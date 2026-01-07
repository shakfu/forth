/**
 * @file async.c
 * @brief Asynchronous event playback for Alda interpreter.
 *
 * This module enables non-blocking playback using libuv, allowing the REPL
 * to remain responsive while music plays. Supports concurrent mode for
 * polyphonic playback of multiple parts entered separately.
 */

#include "alda/async.h"
#include "alda/scheduler.h"
#include "alda/midi_backend.h"
#include "alda/tsf_backend.h"
#include <uv.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ============================================================================
 * Constants
 * ============================================================================ */

#define MAX_ASYNC_SLOTS 8

/* ============================================================================
 * Playback Slot - One per concurrent playback
 * ============================================================================ */

typedef struct {
    int active;                     /* Is this slot in use? */

    /* Event data (copy of scheduled events) */
    AldaScheduledEvent* events;
    int event_count;
    int event_index;
    int tempo;

    /* Stop request */
    int stop_requested;

    /* libuv handles */
    uv_timer_t timer;
    uv_async_t stop_async;
} AsyncSlot;

/* ============================================================================
 * Async Playback System
 * ============================================================================ */

typedef struct {
    /* libuv event loop and thread */
    uv_loop_t* loop;
    uv_thread_t thread;
    uv_async_t wake_async;
    uv_mutex_t mutex;

    /* System state */
    int running;
    int shutdown_requested;

    /* Playback slots */
    AsyncSlot slots[MAX_ASYNC_SLOTS];
    int active_count;

    /* MIDI output handle (borrowed from context) */
    libremidi_midi_out_handle* midi_out;

    /* Context reference (for TSF routing) */
    AldaContext* ctx;

    /* Concurrent mode flag */
    int concurrent_mode;

} AsyncPlaybackSystem;

static AsyncPlaybackSystem async_sys = {0};

/* ============================================================================
 * Forward Declarations
 * ============================================================================ */

static void on_timer(uv_timer_t* handle);
static void schedule_next_event(AsyncSlot* slot);
static AsyncSlot* find_free_slot(void);

/* ============================================================================
 * Timer Callback - Fires for each MIDI event
 * ============================================================================ */

static void send_event(AldaScheduledEvent* evt) {
    int channel = evt->channel;  /* 0-based in events */
    int channel_1based = channel + 1;  /* TSF/midi_backend expect 1-based */

    /* Route to TSF if enabled (takes priority) */
    if (async_sys.ctx && async_sys.ctx->tsf_enabled && alda_tsf_is_enabled()) {
        switch (evt->type) {
            case ALDA_EVT_NOTE_ON:
                alda_tsf_send_note_on(channel_1based, evt->data1, evt->data2);
                break;
            case ALDA_EVT_NOTE_OFF:
                alda_tsf_send_note_off(channel_1based, evt->data1);
                break;
            case ALDA_EVT_PROGRAM:
                alda_tsf_send_program(channel_1based, evt->data1);
                break;
            case ALDA_EVT_CC:
                alda_tsf_send_cc(channel_1based, evt->data1, evt->data2);
                break;
            case ALDA_EVT_PAN:
                alda_tsf_send_cc(channel_1based, 10, evt->data1);  /* CC 10 = Pan */
                break;
            case ALDA_EVT_TEMPO:
                /* Handled elsewhere */
                break;
        }
        return;
    }

    /* Fall back to MIDI output */
    if (!async_sys.midi_out) return;

    unsigned char msg[3];

    switch (evt->type) {
        case ALDA_EVT_NOTE_ON:
            msg[0] = 0x90 | (channel & 0x0F);
            msg[1] = evt->data1 & 0x7F;
            msg[2] = evt->data2 & 0x7F;
            libremidi_midi_out_send_message(async_sys.midi_out, msg, 3);
            break;

        case ALDA_EVT_NOTE_OFF:
            msg[0] = 0x80 | (channel & 0x0F);
            msg[1] = evt->data1 & 0x7F;
            msg[2] = 0;
            libremidi_midi_out_send_message(async_sys.midi_out, msg, 3);
            break;

        case ALDA_EVT_PROGRAM:
            msg[0] = 0xC0 | (channel & 0x0F);
            msg[1] = evt->data1 & 0x7F;
            libremidi_midi_out_send_message(async_sys.midi_out, msg, 2);
            break;

        case ALDA_EVT_CC:
            msg[0] = 0xB0 | (channel & 0x0F);
            msg[1] = evt->data1 & 0x7F;
            msg[2] = evt->data2 & 0x7F;
            libremidi_midi_out_send_message(async_sys.midi_out, msg, 3);
            break;

        case ALDA_EVT_PAN:
            msg[0] = 0xB0 | (channel & 0x0F);
            msg[1] = 10;  /* CC 10 = Pan */
            msg[2] = evt->data1 & 0x7F;
            libremidi_midi_out_send_message(async_sys.midi_out, msg, 3);
            break;

        case ALDA_EVT_TEMPO:
            /* Tempo events are handled in on_timer, not sent as MIDI */
            break;
    }
}

static void on_timer(uv_timer_t* handle) {
    AsyncSlot* slot = (AsyncSlot*)handle->data;

    if (slot->stop_requested || slot->event_index >= slot->event_count) {
        /* Done playing - mark slot as inactive */
        uv_mutex_lock(&async_sys.mutex);
        slot->active = 0;
        async_sys.active_count--;
        uv_mutex_unlock(&async_sys.mutex);
        return;
    }

    /* Send current event */
    AldaScheduledEvent* evt = &slot->events[slot->event_index];

    if (evt->type == ALDA_EVT_TEMPO) {
        /* Tempo event: update playback tempo for timing calculations */
        slot->tempo = evt->data1;
        if (slot->tempo <= 0) slot->tempo = ALDA_DEFAULT_TEMPO;
    } else {
        send_event(evt);
    }

    slot->event_index++;
    schedule_next_event(slot);
}

static void schedule_next_event(AsyncSlot* slot) {
    if (slot->stop_requested) {
        uv_mutex_lock(&async_sys.mutex);
        slot->active = 0;
        async_sys.active_count--;
        uv_mutex_unlock(&async_sys.mutex);
        return;
    }

    if (slot->event_index >= slot->event_count) {
        /* Done playing */
        uv_mutex_lock(&async_sys.mutex);
        slot->active = 0;
        async_sys.active_count--;
        uv_mutex_unlock(&async_sys.mutex);
        return;
    }

    AldaScheduledEvent* curr = &slot->events[slot->event_index];
    int prev_tick = (slot->event_index > 0) ?
                    slot->events[slot->event_index - 1].tick : 0;

    int delta_ticks = curr->tick - prev_tick;
    int ms = alda_ticks_to_ms(delta_ticks, slot->tempo);
    if (ms < 0) ms = 0;

    uv_timer_start(&slot->timer, on_timer, ms, 0);
}

/* ============================================================================
 * Stop Signal Handler
 * ============================================================================ */

static void on_stop_signal(uv_async_t* handle) {
    AsyncSlot* slot = (AsyncSlot*)handle->data;
    slot->stop_requested = 1;
    uv_timer_stop(&slot->timer);

    if (slot->active) {
        uv_mutex_lock(&async_sys.mutex);
        slot->active = 0;
        async_sys.active_count--;
        uv_mutex_unlock(&async_sys.mutex);
    }
}

/* ============================================================================
 * Wake Handler - Check for shutdown
 * ============================================================================ */

static void on_wake(uv_async_t* handle) {
    (void)handle;
    if (async_sys.shutdown_requested) {
        uv_stop(async_sys.loop);
    }
}

/* ============================================================================
 * Event Loop Thread
 * ============================================================================ */

static void async_thread_fn(void* arg) {
    (void)arg;

    while (!async_sys.shutdown_requested) {
        uv_run(async_sys.loop, UV_RUN_DEFAULT);

        if (!async_sys.shutdown_requested) {
            /* Brief sleep to avoid busy-waiting */
            uv_sleep(10);
        }
    }

    async_sys.running = 0;
}

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

static AsyncSlot* find_free_slot(void) {
    for (int i = 0; i < MAX_ASYNC_SLOTS; i++) {
        if (!async_sys.slots[i].active) {
            return &async_sys.slots[i];
        }
    }
    return NULL;
}

/* ============================================================================
 * Public API
 * ============================================================================ */

int alda_async_init(void) {
    if (async_sys.loop != NULL) {
        return 0;  /* Already initialized */
    }

    async_sys.loop = malloc(sizeof(uv_loop_t));
    if (!async_sys.loop) {
        return -1;
    }

    if (uv_loop_init(async_sys.loop) != 0) {
        free(async_sys.loop);
        async_sys.loop = NULL;
        return -1;
    }

    uv_mutex_init(&async_sys.mutex);

    /* Initialize wake async handle */
    uv_async_init(async_sys.loop, &async_sys.wake_async, on_wake);

    /* Initialize all slots */
    for (int i = 0; i < MAX_ASYNC_SLOTS; i++) {
        AsyncSlot* slot = &async_sys.slots[i];
        slot->active = 0;
        slot->events = NULL;

        uv_timer_init(async_sys.loop, &slot->timer);
        slot->timer.data = slot;

        uv_async_init(async_sys.loop, &slot->stop_async, on_stop_signal);
        slot->stop_async.data = slot;
    }

    /* Start the event loop thread */
    async_sys.running = 1;
    async_sys.shutdown_requested = 0;
    async_sys.active_count = 0;
    async_sys.concurrent_mode = 0;  /* Default: sequential */

    if (uv_thread_create(&async_sys.thread, async_thread_fn, NULL) != 0) {
        async_sys.running = 0;
        uv_loop_close(async_sys.loop);
        free(async_sys.loop);
        async_sys.loop = NULL;
        return -1;
    }

    return 0;
}

void alda_async_cleanup(void) {
    if (!async_sys.loop) return;

    /* Stop all playback */
    alda_async_stop();

    /* Signal shutdown */
    async_sys.shutdown_requested = 1;
    uv_async_send(&async_sys.wake_async);

    /* Wait for thread to finish */
    if (async_sys.running) {
        uv_thread_join(&async_sys.thread);
    }

    /* Close all handles */
    for (int i = 0; i < MAX_ASYNC_SLOTS; i++) {
        AsyncSlot* slot = &async_sys.slots[i];
        uv_close((uv_handle_t*)&slot->timer, NULL);
        uv_close((uv_handle_t*)&slot->stop_async, NULL);

        if (slot->events) {
            free(slot->events);
            slot->events = NULL;
        }
    }
    uv_close((uv_handle_t*)&async_sys.wake_async, NULL);

    /* Run loop to process close callbacks */
    uv_run(async_sys.loop, UV_RUN_DEFAULT);

    uv_mutex_destroy(&async_sys.mutex);
    uv_loop_close(async_sys.loop);
    free(async_sys.loop);
    async_sys.loop = NULL;
}

int alda_events_play_async(AldaContext* ctx) {
    if (!ctx) return -1;

    if (ctx->event_count == 0) {
        return 0;  /* Nothing to play */
    }

    /* Check if any output is available (MIDI or built-in synth) */
    if (!ctx->midi_out && !(ctx->tsf_enabled && alda_tsf_is_enabled())) {
        fprintf(stderr, "No audio output available\n");
        return -1;
    }

    /* Initialize async system if needed */
    if (!async_sys.loop) {
        if (alda_async_init() != 0) {
            fprintf(stderr, "Failed to initialize async playback\n");
            return -1;
        }
    }

    /* In sequential mode, wait for previous playback to complete */
    if (!async_sys.concurrent_mode) {
        while (alda_async_is_playing()) {
            uv_sleep(10);
        }
    }

    /* Find a free slot */
    uv_mutex_lock(&async_sys.mutex);

    AsyncSlot* slot = find_free_slot();
    if (!slot) {
        uv_mutex_unlock(&async_sys.mutex);
        fprintf(stderr, "No free playback slots (max %d concurrent)\n", MAX_ASYNC_SLOTS);
        return -1;
    }

    /* Sort events */
    alda_events_sort(ctx);

    /* Copy events to slot */
    if (slot->events) {
        free(slot->events);
    }

    slot->events = malloc(ctx->event_count * sizeof(AldaScheduledEvent));
    if (!slot->events) {
        uv_mutex_unlock(&async_sys.mutex);
        fprintf(stderr, "Failed to allocate event buffer\n");
        return -1;
    }

    memcpy(slot->events, ctx->events, ctx->event_count * sizeof(AldaScheduledEvent));
    slot->event_count = ctx->event_count;
    slot->event_index = 0;
    slot->tempo = ctx->global_tempo > 0 ? ctx->global_tempo : ALDA_DEFAULT_TEMPO;
    slot->stop_requested = 0;
    slot->active = 1;
    async_sys.active_count++;
    async_sys.midi_out = ctx->midi_out;
    async_sys.ctx = ctx;

    uv_mutex_unlock(&async_sys.mutex);

    /* Schedule first event */
    int first_ms = 0;
    if (slot->event_count > 0 && slot->events[0].tick > 0) {
        first_ms = alda_ticks_to_ms(slot->events[0].tick, slot->tempo);
    }

    uv_timer_start(&slot->timer, on_timer, first_ms, 0);

    /* Wake the event loop thread */
    uv_async_send(&async_sys.wake_async);

    return 0;
}

void alda_async_stop(void) {
    if (!async_sys.loop) return;

    /* Stop all active slots */
    for (int i = 0; i < MAX_ASYNC_SLOTS; i++) {
        if (async_sys.slots[i].active) {
            async_sys.slots[i].stop_requested = 1;
            uv_async_send(&async_sys.slots[i].stop_async);
        }
    }
}

int alda_async_is_playing(void) {
    if (!async_sys.loop) return 0;

    uv_mutex_lock(&async_sys.mutex);
    int count = async_sys.active_count;
    uv_mutex_unlock(&async_sys.mutex);

    return count > 0;
}

int alda_async_active_count(void) {
    if (!async_sys.loop) return 0;

    uv_mutex_lock(&async_sys.mutex);
    int count = async_sys.active_count;
    uv_mutex_unlock(&async_sys.mutex);

    return count;
}

int alda_async_wait(int timeout_ms) {
    if (!async_sys.loop) return 0;

    int waited = 0;
    int interval = 10;

    while (alda_async_is_playing()) {
        uv_sleep(interval);
        waited += interval;

        if (timeout_ms > 0 && waited >= timeout_ms) {
            return -1;  /* Timeout */
        }
    }

    return 0;
}

void alda_async_set_concurrent(int enabled) {
    /* Initialize if needed */
    if (!async_sys.loop) {
        alda_async_init();
    }

    uv_mutex_lock(&async_sys.mutex);
    async_sys.concurrent_mode = enabled ? 1 : 0;
    uv_mutex_unlock(&async_sys.mutex);
}

int alda_async_get_concurrent(void) {
    if (!async_sys.loop) return 0;

    uv_mutex_lock(&async_sys.mutex);
    int mode = async_sys.concurrent_mode;
    uv_mutex_unlock(&async_sys.mutex);

    return mode;
}
