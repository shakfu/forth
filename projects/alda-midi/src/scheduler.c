/**
 * @file scheduler.c
 * @brief Event scheduling and playback for Alda interpreter.
 */

#include "alda/scheduler.h"
#include "alda/midi_backend.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Initial event queue capacity */
#define INITIAL_EVENT_CAPACITY 1024

/* ============================================================================
 * Event Queue Management
 * ============================================================================ */

int alda_events_reserve(AldaContext* ctx, int additional) {
    if (!ctx) return -1;

    int needed = ctx->event_count + additional;
    if (needed <= ctx->event_capacity) {
        return 0;  /* Already have capacity */
    }

    /* Calculate new capacity (double or needed, whichever is larger) */
    int new_capacity = ctx->event_capacity;
    if (new_capacity == 0) {
        new_capacity = INITIAL_EVENT_CAPACITY;
    }
    while (new_capacity < needed) {
        new_capacity *= 2;
    }

    /* Allocate or reallocate */
    AldaScheduledEvent* new_events = realloc(ctx->events,
                                              new_capacity * sizeof(AldaScheduledEvent));
    if (!new_events) {
        fprintf(stderr, "Error: Failed to allocate event queue\n");
        return -1;
    }

    ctx->events = new_events;
    ctx->event_capacity = new_capacity;
    return 0;
}

int alda_schedule_event(AldaContext* ctx, int tick, AldaEventType type,
                        int channel, int data1, int data2, int part_index) {
    if (!ctx) return -1;

    /* Ensure capacity */
    if (alda_events_reserve(ctx, 1) < 0) {
        return -1;
    }

    /* Add event */
    AldaScheduledEvent* evt = &ctx->events[ctx->event_count];
    evt->tick = tick;
    evt->type = type;
    evt->channel = channel;
    evt->data1 = data1;
    evt->data2 = data2;
    evt->part_index = part_index;

    ctx->event_count++;
    return 0;
}

int alda_schedule_note_slurred(AldaContext* ctx, AldaPartState* part,
                               int start_tick, int pitch, int velocity,
                               int duration_ticks, int slurred) {
    if (!ctx || !part) return -1;

    int part_index = (int)(part - ctx->parts);
    int channel = part->channel - 1;  /* Convert to 0-based for storage */

    /* Apply quantization to get actual sounding duration.
     * Slurred notes skip quantization (play full duration). */
    int sounding_ticks;
    if (slurred) {
        sounding_ticks = duration_ticks;  /* Full duration for slurred notes */
    } else {
        int quant = alda_effective_quant(ctx, part);
        sounding_ticks = alda_apply_quant(duration_ticks, quant);
    }

    /* Ensure capacity for both note-on and note-off */
    if (alda_events_reserve(ctx, 2) < 0) {
        return -1;
    }

    /* Schedule note-on */
    if (alda_schedule_event(ctx, start_tick, ALDA_EVT_NOTE_ON,
                            channel, pitch, velocity, part_index) < 0) {
        return -1;
    }

    /* Schedule note-off */
    int end_tick = start_tick + sounding_ticks;
    if (alda_schedule_event(ctx, end_tick, ALDA_EVT_NOTE_OFF,
                            channel, pitch, 0, part_index) < 0) {
        return -1;
    }

    return 0;
}

int alda_schedule_note(AldaContext* ctx, AldaPartState* part,
                       int start_tick, int pitch, int velocity, int duration_ticks) {
    /* Non-slurred note: apply normal quantization */
    return alda_schedule_note_slurred(ctx, part, start_tick, pitch, velocity,
                                      duration_ticks, 0);
}

int alda_schedule_program_change(AldaContext* ctx, AldaPartState* part, int tick) {
    if (!ctx || !part) return -1;

    int part_index = (int)(part - ctx->parts);
    int channel = part->channel - 1;

    return alda_schedule_event(ctx, tick, ALDA_EVT_PROGRAM,
                               channel, part->program, 0, part_index);
}

int alda_schedule_pan(AldaContext* ctx, AldaPartState* part, int tick, int pan) {
    if (!ctx || !part) return -1;

    int part_index = (int)(part - ctx->parts);
    int channel = part->channel - 1;

    return alda_schedule_event(ctx, tick, ALDA_EVT_PAN,
                               channel, pan, 0, part_index);
}

int alda_schedule_tempo(AldaContext* ctx, int tick, int tempo) {
    if (!ctx) return -1;

    /* Tempo events use channel -1 as a sentinel (not channel-specific) */
    return alda_schedule_event(ctx, tick, ALDA_EVT_TEMPO,
                               -1, tempo, 0, -1);
}

void alda_events_clear(AldaContext* ctx) {
    if (!ctx) return;
    ctx->event_count = 0;
}

/* ============================================================================
 * Event Sorting
 * ============================================================================ */

/* Comparison function for qsort */
static int event_compare(const void* a, const void* b) {
    const AldaScheduledEvent* ea = (const AldaScheduledEvent*)a;
    const AldaScheduledEvent* eb = (const AldaScheduledEvent*)b;

    /* Sort by tick first */
    if (ea->tick != eb->tick) {
        return ea->tick - eb->tick;
    }

    /* At same tick: note-offs before note-ons (to avoid stuck notes) */
    /* Program changes before notes */
    int type_order_a = (ea->type == ALDA_EVT_NOTE_OFF) ? 0 :
                       (ea->type == ALDA_EVT_PROGRAM) ? 1 :
                       (ea->type == ALDA_EVT_CC) ? 2 :
                       (ea->type == ALDA_EVT_PAN) ? 3 : 4;
    int type_order_b = (eb->type == ALDA_EVT_NOTE_OFF) ? 0 :
                       (eb->type == ALDA_EVT_PROGRAM) ? 1 :
                       (eb->type == ALDA_EVT_CC) ? 2 :
                       (eb->type == ALDA_EVT_PAN) ? 3 : 4;

    if (type_order_a != type_order_b) {
        return type_order_a - type_order_b;
    }

    /* Same type at same tick: sort by channel for consistency */
    return ea->channel - eb->channel;
}

void alda_events_sort(AldaContext* ctx) {
    if (!ctx || ctx->event_count <= 1) return;

    qsort(ctx->events, ctx->event_count, sizeof(AldaScheduledEvent), event_compare);
}

/* ============================================================================
 * Event Playback
 * ============================================================================ */

int alda_events_play(AldaContext* ctx) {
    if (!ctx) return -1;

    if (ctx->event_count == 0) {
        return 0;  /* Nothing to play */
    }

    /* Sort events by tick */
    alda_events_sort(ctx);

    /* Get tempo for timing conversion */
    int tempo = ctx->global_tempo;
    if (tempo <= 0) tempo = ALDA_DEFAULT_TEMPO;

    int last_tick = 0;

    /* Play events in order */
    for (int i = 0; i < ctx->event_count; i++) {
        AldaScheduledEvent* evt = &ctx->events[i];

        /* Wait until event time */
        if (evt->tick > last_tick) {
            int delta_ticks = evt->tick - last_tick;
            int delta_ms = alda_ticks_to_ms(delta_ticks, tempo);
            alda_midi_sleep_ms(ctx, delta_ms);
            last_tick = evt->tick;
        }

        /* Convert channel back to 1-based for MIDI functions */
        int channel = evt->channel + 1;

        /* Send event */
        switch (evt->type) {
            case ALDA_EVT_NOTE_ON:
                alda_midi_send_note_on(ctx, channel, evt->data1, evt->data2);
                break;

            case ALDA_EVT_NOTE_OFF:
                alda_midi_send_note_off(ctx, channel, evt->data1);
                break;

            case ALDA_EVT_PROGRAM:
                alda_midi_send_program(ctx, channel, evt->data1);
                break;

            case ALDA_EVT_CC:
                alda_midi_send_cc(ctx, channel, evt->data1, evt->data2);
                break;

            case ALDA_EVT_PAN:
                alda_midi_send_cc(ctx, channel, 10, evt->data1);  /* CC 10 = Pan */
                break;

            case ALDA_EVT_TEMPO:
                /* Update playback tempo for subsequent timing calculations */
                tempo = evt->data1;
                if (tempo <= 0) tempo = ALDA_DEFAULT_TEMPO;
                break;
        }
    }

    return 0;
}

/* ============================================================================
 * Duration Calculation
 * ============================================================================ */

int alda_duration_to_ticks(int denominator, int dots) {
    if (denominator <= 0) {
        denominator = 4;  /* Default to quarter note */
    }

    /* Base duration: whole note = 4 * TICKS_PER_QUARTER */
    int base_ticks = (4 * ALDA_TICKS_PER_QUARTER) / denominator;

    /* Apply dots: each dot adds half of the previous value */
    int total = base_ticks;
    int add = base_ticks;
    for (int d = 0; d < dots; d++) {
        add = add / 2;
        total += add;
    }

    return total;
}

int alda_ms_to_ticks(int ms, int tempo) {
    if (tempo <= 0) tempo = ALDA_DEFAULT_TEMPO;

    /* ticks = ms * tempo * TICKS_PER_QUARTER / 60000 */
    return (ms * tempo * ALDA_TICKS_PER_QUARTER) / 60000;
}

int alda_seconds_to_ticks(double seconds, int tempo) {
    return alda_ms_to_ticks((int)(seconds * 1000), tempo);
}

int alda_ticks_to_ms(int ticks, int tempo) {
    if (tempo <= 0) tempo = ALDA_DEFAULT_TEMPO;

    /* ms = ticks * 60000 / (tempo * TICKS_PER_QUARTER) */
    return (ticks * 60000) / (tempo * ALDA_TICKS_PER_QUARTER);
}

int alda_apply_quant(int duration_ticks, int quant) {
    if (quant <= 0) quant = ALDA_DEFAULT_QUANT;
    if (quant > 100) quant = 100;

    return (duration_ticks * quant) / 100;
}
