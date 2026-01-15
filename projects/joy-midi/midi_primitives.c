/*
 * midi_primitives.c - MIDI primitive implementations for Joy
 */

#include "joy_runtime.h"
#include "midi_primitives.h"
#include "music_theory.h"
#include "music_context.h"
#include "music_notation.h"
#include <libremidi/libremidi-c.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <limits.h>

/* Helper macros (matching joy_primitives.c pattern) */
#define REQUIRE(n, op) \
    if (ctx->stack->depth < (n)) { joy_error_underflow(op, n, ctx->stack->depth); return; }

#define POP() joy_stack_pop(ctx->stack)
#define PUSH(v) joy_stack_push(ctx->stack, v)

#define EXPECT_TYPE(v, t, op) \
    if ((v).type != (t)) { joy_error_type(op, #t, (v).type); return; }

/* ============================================================================
 * MIDI State
 * ============================================================================ */

#define MAX_PORTS 64

static libremidi_midi_observer_handle* midi_observer = NULL;
static libremidi_midi_out_handle* midi_out = NULL;
static libremidi_midi_out_port* out_ports[MAX_PORTS];
static int out_port_count = 0;
static int current_channel = 1;

/* ============================================================================
 * Internal Helpers
 * ============================================================================ */

static void on_output_port_found(void* user_ctx, const libremidi_midi_out_port* port) {
    (void)user_ctx;
    if (out_port_count >= MAX_PORTS) return;
    libremidi_midi_out_port_clone(port, &out_ports[out_port_count]);
    out_port_count++;
}

static void ensure_observer(void) {
    if (midi_observer != NULL) return;

    libremidi_observer_configuration observer_conf;
    if (libremidi_midi_observer_configuration_init(&observer_conf) != 0) return;

    observer_conf.track_hardware = true;
    observer_conf.track_virtual = true;
    observer_conf.track_any = true;

    libremidi_api_configuration api_conf;
    if (libremidi_midi_api_configuration_init(&api_conf) != 0) return;

    api_conf.configuration_type = Observer;
    api_conf.api = UNSPECIFIED;

    libremidi_midi_observer_new(&observer_conf, &api_conf, &midi_observer);
}

static void enumerate_ports(void) {
    ensure_observer();
    if (!midi_observer) return;

    /* Free existing ports */
    for (int i = 0; i < out_port_count; i++) {
        libremidi_midi_out_port_free(out_ports[i]);
    }
    out_port_count = 0;

    /* Enumerate */
    libremidi_midi_observer_enumerate_output_ports(midi_observer, NULL, on_output_port_found);
}

void send_note_on(int pitch, int velocity) {
    if (!midi_out) return;
    unsigned char msg[3];
    msg[0] = 0x90 | ((current_channel - 1) & 0x0F);
    msg[1] = pitch & 0x7F;
    msg[2] = velocity & 0x7F;
    libremidi_midi_out_send_message(midi_out, msg, 3);
}

void send_note_off(int pitch) {
    if (!midi_out) return;
    unsigned char msg[3];
    msg[0] = 0x80 | ((current_channel - 1) & 0x0F);
    msg[1] = pitch & 0x7F;
    msg[2] = 0;
    libremidi_midi_out_send_message(midi_out, msg, 3);
}

/* ============================================================================
 * Port Management Primitives
 * ============================================================================ */

void midi_list_(JoyContext* ctx) {
    (void)ctx;
    enumerate_ports();

    printf("MIDI outputs:\n");
    if (out_port_count == 0) {
        printf("  (none - use midi-virtual to create a virtual port)\n");
    } else {
        for (int i = 0; i < out_port_count; i++) {
            const char* name = NULL;
            size_t len = 0;
            if (libremidi_midi_out_port_name(out_ports[i], &name, &len) == 0) {
                printf("  %d: %s\n", i, name);
            }
        }
    }
}

void midi_virtual_(JoyContext* ctx) {
    (void)ctx;

    /* Close existing output if open */
    if (midi_out != NULL) {
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
    }

    libremidi_midi_configuration midi_conf;
    if (libremidi_midi_configuration_init(&midi_conf) != 0) {
        printf("Failed to init MIDI config\n");
        return;
    }

    midi_conf.version = MIDI1;
    midi_conf.virtual_port = true;
    midi_conf.port_name = "JoyMIDI";

    libremidi_api_configuration api_conf;
    if (libremidi_midi_api_configuration_init(&api_conf) != 0) {
        printf("Failed to init API config\n");
        return;
    }

    api_conf.configuration_type = Output;
    api_conf.api = UNSPECIFIED;

    int ret = libremidi_midi_out_new(&midi_conf, &api_conf, &midi_out);
    if (ret != 0) {
        printf("Failed to create virtual MIDI output: %d\n", ret);
        return;
    }

    printf("Created virtual MIDI output: JoyMIDI\n");
}

void midi_open_(JoyContext* ctx) {
    REQUIRE(1, "midi-open");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_INTEGER, "midi-open");

    int port_idx = (int)v.data.integer;
    enumerate_ports();

    if (port_idx < 0 || port_idx >= out_port_count) {
        printf("Invalid port index: %d (have %d ports)\n", port_idx, out_port_count);
        return;
    }

    /* Close existing output if open */
    if (midi_out != NULL) {
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
    }

    libremidi_midi_configuration midi_conf;
    if (libremidi_midi_configuration_init(&midi_conf) != 0) {
        printf("Failed to init MIDI config\n");
        return;
    }

    midi_conf.version = MIDI1;
    midi_conf.out_port = out_ports[port_idx];

    libremidi_api_configuration api_conf;
    if (libremidi_midi_api_configuration_init(&api_conf) != 0) {
        printf("Failed to init API config\n");
        return;
    }

    api_conf.configuration_type = Output;
    api_conf.api = UNSPECIFIED;

    int ret = libremidi_midi_out_new(&midi_conf, &api_conf, &midi_out);
    if (ret != 0) {
        printf("Failed to open MIDI output: %d\n", ret);
        return;
    }

    const char* name = NULL;
    size_t len = 0;
    libremidi_midi_out_port_name(out_ports[port_idx], &name, &len);
    printf("Opened MIDI output: %s\n", name);
}

void midi_close_(JoyContext* ctx) {
    (void)ctx;
    if (midi_out != NULL) {
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
        printf("MIDI output closed\n");
    }
}

/* ============================================================================
 * Note Operations
 * ============================================================================ */

void midi_note_(JoyContext* ctx) {
    REQUIRE(3, "midi-note");

    JoyValue dur_v = POP();
    JoyValue vel_v = POP();
    JoyValue pitch_v = POP();

    EXPECT_TYPE(dur_v, JOY_INTEGER, "midi-note");
    EXPECT_TYPE(vel_v, JOY_INTEGER, "midi-note");
    EXPECT_TYPE(pitch_v, JOY_INTEGER, "midi-note");

    int pitch = (int)pitch_v.data.integer;
    int velocity = (int)vel_v.data.integer;
    int duration = (int)dur_v.data.integer;

    if (!midi_out) {
        printf("No MIDI output open\n");
        return;
    }

    send_note_on(pitch, velocity);
    if (duration > 0) {
        usleep(duration * 1000);
    }
    send_note_off(pitch);
}

void midi_note_on_(JoyContext* ctx) {
    REQUIRE(2, "midi-note-on");

    JoyValue vel_v = POP();
    JoyValue pitch_v = POP();

    EXPECT_TYPE(vel_v, JOY_INTEGER, "midi-note-on");
    EXPECT_TYPE(pitch_v, JOY_INTEGER, "midi-note-on");

    int pitch = (int)pitch_v.data.integer;
    int velocity = (int)vel_v.data.integer;

    send_note_on(pitch, velocity);
}

void midi_note_off_(JoyContext* ctx) {
    REQUIRE(1, "midi-note-off");

    JoyValue pitch_v = POP();
    EXPECT_TYPE(pitch_v, JOY_INTEGER, "midi-note-off");

    int pitch = (int)pitch_v.data.integer;
    send_note_off(pitch);
}

void midi_chord_(JoyContext* ctx) {
    REQUIRE(3, "midi-chord");

    JoyValue dur_v = POP();
    JoyValue vel_v = POP();
    JoyValue list_v = POP();

    EXPECT_TYPE(dur_v, JOY_INTEGER, "midi-chord");
    EXPECT_TYPE(vel_v, JOY_INTEGER, "midi-chord");
    EXPECT_TYPE(list_v, JOY_LIST, "midi-chord");

    int velocity = (int)vel_v.data.integer;
    int duration = (int)dur_v.data.integer;
    JoyList* pitches = list_v.data.list;

    if (!midi_out) {
        printf("No MIDI output open\n");
        joy_value_free(&list_v);
        return;
    }

    /* Note on for all pitches */
    for (size_t i = 0; i < pitches->length; i++) {
        if (pitches->items[i].type == JOY_INTEGER) {
            int pitch = (int)pitches->items[i].data.integer;
            send_note_on(pitch, velocity);
        }
    }

    if (duration > 0) {
        usleep(duration * 1000);
    }

    /* Note off for all pitches */
    for (size_t i = 0; i < pitches->length; i++) {
        if (pitches->items[i].type == JOY_INTEGER) {
            int pitch = (int)pitches->items[i].data.integer;
            send_note_off(pitch);
        }
    }

    joy_value_free(&list_v);
}

/* ============================================================================
 * Control Messages
 * ============================================================================ */

void midi_cc_(JoyContext* ctx) {
    REQUIRE(2, "midi-cc");

    JoyValue val_v = POP();
    JoyValue cc_v = POP();

    EXPECT_TYPE(val_v, JOY_INTEGER, "midi-cc");
    EXPECT_TYPE(cc_v, JOY_INTEGER, "midi-cc");

    int cc = (int)cc_v.data.integer;
    int value = (int)val_v.data.integer;

    if (!midi_out) return;

    unsigned char msg[3];
    msg[0] = 0xB0 | ((current_channel - 1) & 0x0F);
    msg[1] = cc & 0x7F;
    msg[2] = value & 0x7F;
    libremidi_midi_out_send_message(midi_out, msg, 3);
}

void midi_program_(JoyContext* ctx) {
    REQUIRE(1, "midi-program");

    JoyValue prog_v = POP();
    EXPECT_TYPE(prog_v, JOY_INTEGER, "midi-program");

    int program = (int)prog_v.data.integer;

    if (!midi_out) return;

    unsigned char msg[2];
    msg[0] = 0xC0 | ((current_channel - 1) & 0x0F);
    msg[1] = program & 0x7F;
    libremidi_midi_out_send_message(midi_out, msg, 2);
}

void midi_panic_(JoyContext* ctx) {
    (void)ctx;
    if (!midi_out) {
        printf("No MIDI output open\n");
        return;
    }

    /* Send All Notes Off (CC 123) on all channels */
    for (int ch = 0; ch < 16; ch++) {
        unsigned char msg[3];
        msg[0] = 0xB0 | ch;
        msg[1] = 123;
        msg[2] = 0;
        libremidi_midi_out_send_message(midi_out, msg, 3);
    }
}

/* ============================================================================
 * Utilities
 * ============================================================================ */

void midi_sleep_(JoyContext* ctx) {
    REQUIRE(1, "midi-sleep");

    JoyValue ms_v = POP();
    EXPECT_TYPE(ms_v, JOY_INTEGER, "midi-sleep");

    int ms = (int)ms_v.data.integer;
    if (ms > 0) {
        usleep(ms * 1000);
    }
}

void pitch_(JoyContext* ctx) {
    REQUIRE(1, "pitch");

    JoyValue str_v = POP();
    EXPECT_TYPE(str_v, JOY_STRING, "pitch");

    int midi_num = music_parse_pitch(str_v.data.string);
    joy_value_free(&str_v);

    if (midi_num < 0) {
        joy_error("pitch: invalid pitch name");
        return;
    }

    PUSH(joy_integer(midi_num));
}

void tempo_(JoyContext* ctx) {
    REQUIRE(1, "tempo");

    JoyValue bpm_v = POP();
    EXPECT_TYPE(bpm_v, JOY_INTEGER, "tempo");

    int bpm = (int)bpm_v.data.integer;
    if (bpm < 1) bpm = 1;
    if (bpm > 999) bpm = 999;

    MusicContext* mctx = (MusicContext*)ctx->user_data;
    if (mctx) {
        mctx->tempo = bpm;
        /* Recalculate duration based on new tempo */
        /* Default to quarter note duration */
        mctx->duration_ms = music_duration_to_ms(4, bpm);
    }
}

void quant_(JoyContext* ctx) {
    REQUIRE(1, "quant");

    JoyValue q_v = POP();
    EXPECT_TYPE(q_v, JOY_INTEGER, "quant");

    int q = (int)q_v.data.integer;
    if (q < 0) q = 0;
    if (q > 100) q = 100;

    MusicContext* mctx = (MusicContext*)ctx->user_data;
    if (mctx) {
        mctx->quantization = q;
    }
}

void vol_(JoyContext* ctx) {
    REQUIRE(1, "vol");

    JoyValue vol_v = POP();
    EXPECT_TYPE(vol_v, JOY_INTEGER, "vol");

    int vol = (int)vol_v.data.integer;
    if (vol < 0) vol = 0;
    if (vol > 100) vol = 100;

    /* Scale 0-100 to 0-127 */
    int velocity = vol * 127 / 100;

    MusicContext* mctx = (MusicContext*)ctx->user_data;
    if (mctx) {
        mctx->velocity = velocity;
    }
}

/* ============================================================================
 * Music Theory
 * ============================================================================ */

void major_chord_(JoyContext* ctx) {
    REQUIRE(1, "major");

    JoyValue root_v = POP();
    EXPECT_TYPE(root_v, JOY_INTEGER, "major");

    int root = (int)root_v.data.integer;
    int pitches[3];
    music_build_chord(root, CHORD_MAJOR, CHORD_TRIAD_SIZE, pitches);

    JoyList* list = joy_list_new(3);
    for (int i = 0; i < 3; i++) {
        joy_list_push(list, joy_integer(pitches[i]));
    }

    JoyValue result = {.type = JOY_LIST};
    result.data.list = list;
    PUSH(result);
}

void minor_chord_(JoyContext* ctx) {
    REQUIRE(1, "minor");

    JoyValue root_v = POP();
    EXPECT_TYPE(root_v, JOY_INTEGER, "minor");

    int root = (int)root_v.data.integer;
    int pitches[3];
    music_build_chord(root, CHORD_MINOR, CHORD_TRIAD_SIZE, pitches);

    JoyList* list = joy_list_new(3);
    for (int i = 0; i < 3; i++) {
        joy_list_push(list, joy_integer(pitches[i]));
    }

    JoyValue result = {.type = JOY_LIST};
    result.data.list = list;
    PUSH(result);
}

void dim_chord_(JoyContext* ctx) {
    REQUIRE(1, "dim");

    JoyValue root_v = POP();
    EXPECT_TYPE(root_v, JOY_INTEGER, "dim");

    int root = (int)root_v.data.integer;
    int pitches[3];
    music_build_chord(root, CHORD_DIM, CHORD_TRIAD_SIZE, pitches);

    JoyList* list = joy_list_new(3);
    for (int i = 0; i < 3; i++) {
        joy_list_push(list, joy_integer(pitches[i]));
    }

    JoyValue result = {.type = JOY_LIST};
    result.data.list = list;
    PUSH(result);
}

void aug_chord_(JoyContext* ctx) {
    REQUIRE(1, "aug");

    JoyValue root_v = POP();
    EXPECT_TYPE(root_v, JOY_INTEGER, "aug");

    int root = (int)root_v.data.integer;
    int pitches[3];
    music_build_chord(root, CHORD_AUG, CHORD_TRIAD_SIZE, pitches);

    JoyList* list = joy_list_new(3);
    for (int i = 0; i < 3; i++) {
        joy_list_push(list, joy_integer(pitches[i]));
    }

    JoyValue result = {.type = JOY_LIST};
    result.data.list = list;
    PUSH(result);
}

void dom7_chord_(JoyContext* ctx) {
    REQUIRE(1, "dom7");

    JoyValue root_v = POP();
    EXPECT_TYPE(root_v, JOY_INTEGER, "dom7");

    int root = (int)root_v.data.integer;
    int pitches[4];
    music_build_chord(root, CHORD_DOM7, CHORD_7TH_SIZE, pitches);

    JoyList* list = joy_list_new(4);
    for (int i = 0; i < 4; i++) {
        joy_list_push(list, joy_integer(pitches[i]));
    }

    JoyValue result = {.type = JOY_LIST};
    result.data.list = list;
    PUSH(result);
}

void maj7_chord_(JoyContext* ctx) {
    REQUIRE(1, "maj7");

    JoyValue root_v = POP();
    EXPECT_TYPE(root_v, JOY_INTEGER, "maj7");

    int root = (int)root_v.data.integer;
    int pitches[4];
    music_build_chord(root, CHORD_MAJ7, CHORD_7TH_SIZE, pitches);

    JoyList* list = joy_list_new(4);
    for (int i = 0; i < 4; i++) {
        joy_list_push(list, joy_integer(pitches[i]));
    }

    JoyValue result = {.type = JOY_LIST};
    result.data.list = list;
    PUSH(result);
}

void min7_chord_(JoyContext* ctx) {
    REQUIRE(1, "min7");

    JoyValue root_v = POP();
    EXPECT_TYPE(root_v, JOY_INTEGER, "min7");

    int root = (int)root_v.data.integer;
    int pitches[4];
    music_build_chord(root, CHORD_MIN7, CHORD_7TH_SIZE, pitches);

    JoyList* list = joy_list_new(4);
    for (int i = 0; i < 4; i++) {
        joy_list_push(list, joy_integer(pitches[i]));
    }

    JoyValue result = {.type = JOY_LIST};
    result.data.list = list;
    PUSH(result);
}

void transpose_(JoyContext* ctx) {
    REQUIRE(2, "transpose");

    JoyValue n_v = POP();
    JoyValue pitch_v = POP();

    EXPECT_TYPE(n_v, JOY_INTEGER, "transpose");
    EXPECT_TYPE(pitch_v, JOY_INTEGER, "transpose");

    int pitch = (int)pitch_v.data.integer;
    int n = (int)n_v.data.integer;

    int result = pitch + n;
    /* Clamp to MIDI range */
    if (result < 0) result = 0;
    if (result > 127) result = 127;

    PUSH(joy_integer(result));
}

/* ============================================================================
 * Channel Operations
 * ============================================================================ */

void channel_(JoyContext* ctx) {
    /* N channel - set current MIDI channel (1-16) */
    REQUIRE(1, "channel");

    JoyValue n = POP();
    EXPECT_TYPE(n, JOY_INTEGER, "channel");

    int ch = (int)n.data.integer;
    if (ch < 1) ch = 1;
    if (ch > 16) ch = 16;
    current_channel = ch;
}

void chan_(JoyContext* ctx) {
    /* [P] N chan - execute quotation P on channel N, restore channel after */
    REQUIRE(2, "chan");

    JoyValue n = POP();
    JoyValue p = POP();
    EXPECT_TYPE(n, JOY_INTEGER, "chan");

    int old_channel = current_channel;
    int ch = (int)n.data.integer;
    if (ch < 1) ch = 1;
    if (ch > 16) ch = 16;
    current_channel = ch;

    /* Execute quotation or list */
    if (p.type == JOY_QUOTATION) {
        joy_execute_quotation(ctx, p.data.quotation);
    } else if (p.type == JOY_LIST) {
        /* Treat list as sequence of notes to play */
        MusicContext* mctx = (MusicContext*)ctx->user_data;
        if (!mctx) {
            fprintf(stderr, "chan: no music context\n");
        } else if (!midi_out) {
            fprintf(stderr, "chan: no MIDI output (use midi-virtual or midi-open first)\n");
        } else {
            for (size_t i = 0; i < p.data.list->length; i++) {
                JoyValue item = p.data.list->items[i];
                if (item.type == JOY_INTEGER) {
                    /* Play note as MIDI number */
                    PUSH(joy_integer(item.data.integer));
                    music_play_(ctx);
                } else if (item.type == JOY_SYMBOL) {
                    /* Try to parse symbol as pitch name (e.g., "c4", "C#4") */
                    int pitch = music_parse_pitch(item.data.symbol);
                    if (pitch >= 0) {
                        PUSH(joy_integer(pitch));
                        music_play_(ctx);
                    }
                }
            }
        }
    } else {
        fprintf(stderr, "chan: expected quotation or list, got %s\n",
                p.type == JOY_INTEGER ? "integer" :
                p.type == JOY_FLOAT ? "float" :
                p.type == JOY_SYMBOL ? "symbol" : "unknown");
    }

    current_channel = old_channel;
    joy_value_free(&p);
}

/* ============================================================================
 * Initialization / Cleanup
 * ============================================================================ */

void midi_init(void) {
    ensure_observer();
}

void midi_cleanup(void) {
    /* Close output */
    if (midi_out != NULL) {
        /* Send all notes off first */
        for (int ch = 0; ch < 16; ch++) {
            unsigned char msg[3] = { (unsigned char)(0xB0 | ch), 123, 0 };
            libremidi_midi_out_send_message(midi_out, msg, 3);
        }
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
    }

    /* Free ports */
    for (int i = 0; i < out_port_count; i++) {
        libremidi_midi_out_port_free(out_ports[i]);
    }
    out_port_count = 0;

    /* Free observer */
    if (midi_observer != NULL) {
        libremidi_midi_observer_free(midi_observer);
        midi_observer = NULL;
    }
}

/* ============================================================================
 * Schedule System Implementation
 * ============================================================================ */

/* Scheduling mode state */
static bool g_scheduling_mode = false;
static int g_schedule_channel = 1;
static int g_schedule_time = 0;
static MidiSchedule* g_current_schedule = NULL;

/* Global accumulator state */
static MidiSchedule* g_accumulator = NULL;
static int g_accumulator_offset = 0;

/* Create a new empty schedule */
MidiSchedule* schedule_new(void) {
    MidiSchedule* sched = malloc(sizeof(MidiSchedule));
    sched->events = NULL;
    sched->count = 0;
    sched->capacity = 0;
    sched->total_duration_ms = 0;
    return sched;
}

/* Free a schedule */
void schedule_free(MidiSchedule* sched) {
    if (sched) {
        free(sched->events);
        free(sched);
    }
}

/* Add an event to a schedule */
void schedule_add_event(MidiSchedule* sched, int time_ms, int channel,
                        int pitch, int velocity, int duration_ms) {
    if (!sched) return;

    /* Grow capacity if needed */
    if (sched->count >= sched->capacity) {
        size_t new_cap = sched->capacity == 0 ? 64 : sched->capacity * 2;
        sched->events = realloc(sched->events, new_cap * sizeof(ScheduledEvent));
        sched->capacity = new_cap;
    }

    /* Add the event */
    ScheduledEvent* ev = &sched->events[sched->count++];
    ev->time_ms = time_ms;
    ev->channel = channel;
    ev->pitch = pitch;
    ev->velocity = velocity;
    ev->duration_ms = duration_ms;

    /* Update total duration */
    int end_time = time_ms + duration_ms;
    if (end_time > sched->total_duration_ms) {
        sched->total_duration_ms = end_time;
    }
}

/* Compare events by time for sorting */
static int compare_events(const void* a, const void* b) {
    const ScheduledEvent* ea = (const ScheduledEvent*)a;
    const ScheduledEvent* eb = (const ScheduledEvent*)b;
    return ea->time_ms - eb->time_ms;
}

/* Play a schedule - sorts events and plays them with proper timing */
void schedule_play(MidiSchedule* sched) {
    if (!sched || sched->count == 0 || !midi_out) return;

    /* Sort events by time */
    qsort(sched->events, sched->count, sizeof(ScheduledEvent), compare_events);

    /* Track active notes for note-off scheduling */
    typedef struct { int pitch; int channel; int off_time; } ActiveNote;
    ActiveNote* active = malloc(sched->count * sizeof(ActiveNote));
    size_t active_count = 0;

    int current_time = 0;
    size_t event_idx = 0;

    while (event_idx < sched->count || active_count > 0) {
        /* Find next event time */
        int next_event_time = INT_MAX;
        if (event_idx < sched->count) {
            next_event_time = sched->events[event_idx].time_ms;
        }

        /* Find next note-off time */
        int next_off_time = INT_MAX;
        for (size_t i = 0; i < active_count; i++) {
            if (active[i].off_time < next_off_time) {
                next_off_time = active[i].off_time;
            }
        }

        /* Determine what happens next */
        int next_time = (next_event_time < next_off_time) ? next_event_time : next_off_time;
        if (next_time == INT_MAX) break;

        /* Sleep until next event */
        if (next_time > current_time) {
            usleep((next_time - current_time) * 1000);
            current_time = next_time;
        }

        /* Process note-offs first */
        for (size_t i = 0; i < active_count; ) {
            if (active[i].off_time <= current_time) {
                /* Send note-off */
                unsigned char msg[3];
                msg[0] = 0x80 | ((active[i].channel - 1) & 0x0F);
                msg[1] = active[i].pitch & 0x7F;
                msg[2] = 0;
                libremidi_midi_out_send_message(midi_out, msg, 3);

                /* Remove from active list */
                active[i] = active[--active_count];
            } else {
                i++;
            }
        }

        /* Process note-ons */
        while (event_idx < sched->count &&
               sched->events[event_idx].time_ms <= current_time) {
            ScheduledEvent* ev = &sched->events[event_idx];

            /* Send note-on */
            unsigned char msg[3];
            msg[0] = 0x90 | ((ev->channel - 1) & 0x0F);
            msg[1] = ev->pitch & 0x7F;
            msg[2] = ev->velocity & 0x7F;
            libremidi_midi_out_send_message(midi_out, msg, 3);

            /* Add to active notes */
            active[active_count].pitch = ev->pitch;
            active[active_count].channel = ev->channel;
            active[active_count].off_time = ev->time_ms + ev->duration_ms;
            active_count++;

            event_idx++;
        }
    }

    free(active);
}

/* Begin scheduling mode for a channel */
void schedule_begin(int channel) {
    g_scheduling_mode = true;
    g_schedule_channel = channel;
    g_schedule_time = 0;
    if (!g_current_schedule) {
        g_current_schedule = schedule_new();
    }
}

/* End scheduling mode */
void schedule_end(void) {
    g_scheduling_mode = false;
}

/* Check if in scheduling mode */
bool is_scheduling(void) {
    return g_scheduling_mode;
}

/* Get current scheduling channel */
int get_schedule_channel(void) {
    return g_schedule_channel;
}

/* Get current time offset in schedule */
int get_schedule_time(void) {
    return g_schedule_time;
}

/* Advance time in current schedule */
void advance_schedule_time(int ms) {
    g_schedule_time += ms;
}

/* Get the current schedule being built */
MidiSchedule* get_current_schedule(void) {
    return g_current_schedule;
}

/* Clear the current schedule (for starting a new part) */
void clear_current_schedule(void) {
    if (g_current_schedule) {
        schedule_free(g_current_schedule);
    }
    g_current_schedule = schedule_new();
    g_schedule_time = 0;
}

/* Initialize the accumulator */
void accumulator_init(void) {
    if (!g_accumulator) {
        g_accumulator = schedule_new();
    }
    g_accumulator_offset = 0;
}

/* Add a schedule to the accumulator (with current offset) */
void accumulator_add_schedule(MidiSchedule* sched) {
    if (!sched || !g_accumulator) return;

    for (size_t i = 0; i < sched->count; i++) {
        ScheduledEvent* ev = &sched->events[i];
        schedule_add_event(g_accumulator,
                          ev->time_ms + g_accumulator_offset,
                          ev->channel, ev->pitch, ev->velocity, ev->duration_ms);
    }
}

/* Flush the accumulator - play and clear */
void accumulator_flush(void) {
    if (g_accumulator && g_accumulator->count > 0) {
        schedule_play(g_accumulator);
        schedule_free(g_accumulator);
        g_accumulator = NULL;
    }
    g_accumulator_offset = 0;
}

/* Get current accumulator time offset */
int accumulator_get_offset(void) {
    return g_accumulator_offset;
}

/* Advance accumulator offset for next sequence */
void accumulator_advance(int ms) {
    g_accumulator_offset += ms;
}

/* Execute a sequence definition - called from joy_runtime.c */
void joy_execute_seq(JoyContext* ctx, SeqDefinition* seq) {
    if (!seq || seq->part_count == 0) return;

    /* Initialize accumulator if needed */
    if (!g_accumulator) {
        accumulator_init();
    }

    /* Create a merged schedule for all parts */
    MidiSchedule* merged = schedule_new();

    /* Execute each part in scheduling mode */
    for (size_t i = 0; i < seq->part_count; i++) {
        SeqPart* part = &seq->parts[i];

        /* Clear current schedule and enter scheduling mode */
        clear_current_schedule();
        schedule_begin(part->channel);

        /* Execute the part's quotation */
        joy_execute_quotation(ctx, part->quotation);

        /* If items remain on stack, play them as notes.
         * IMPORTANT: Stack is LIFO, so we must collect all items first
         * and then process in reverse order (bottom to top = original order).
         */
        MidiSchedule* sched = get_current_schedule();
        MusicContext* mctx = (MusicContext*)ctx->user_data;

        if (sched && mctx && ctx->stack->depth > 0) {
            /* Collect all playable items from stack */
            JoyValue* collected = malloc(ctx->stack->depth * sizeof(JoyValue));
            size_t collected_count = 0;

            while (ctx->stack->depth > 0) {
                JoyValue top = joy_stack_peek(ctx->stack);
                if (top.type == JOY_LIST || top.type == JOY_INTEGER) {
                    collected[collected_count++] = joy_stack_pop(ctx->stack);
                } else {
                    break;  /* Unknown type - leave on stack */
                }
            }

            /* Process in reverse order (to restore original sequence) */
            for (size_t j = collected_count; j > 0; j--) {
                JoyValue val = collected[j - 1];
                if (val.type == JOY_LIST) {
                    /* Play the list as sequential notes */
                    for (size_t k = 0; k < val.data.list->length; k++) {
                        if (val.data.list->items[k].type == JOY_INTEGER) {
                            int pitch = (int)val.data.list->items[k].data.integer;
                            int play_dur = mctx->duration_ms * mctx->quantization / 100;
                            schedule_add_event(sched, get_schedule_time(),
                                              get_schedule_channel(), pitch,
                                              mctx->velocity, play_dur);
                            advance_schedule_time(mctx->duration_ms);
                        }
                    }
                    joy_value_free(&val);
                } else if (val.type == JOY_INTEGER) {
                    /* Single note - play it */
                    int pitch = (int)val.data.integer;
                    int play_dur = mctx->duration_ms * mctx->quantization / 100;
                    schedule_add_event(sched, get_schedule_time(),
                                      get_schedule_channel(), pitch,
                                      mctx->velocity, play_dur);
                    advance_schedule_time(mctx->duration_ms);
                }
            }

            free(collected);
        }

        /* Get the schedule built by this part */
        MidiSchedule* part_sched = get_current_schedule();

        /* Merge into combined schedule (all parts start at time 0) */
        if (part_sched) {
            for (size_t j = 0; j < part_sched->count; j++) {
                ScheduledEvent* ev = &part_sched->events[j];
                schedule_add_event(merged, ev->time_ms, ev->channel,
                                  ev->pitch, ev->velocity, ev->duration_ms);
            }
        }

        schedule_end();
    }

    /* Add merged schedule to accumulator with current offset */
    accumulator_add_schedule(merged);

    /* Advance accumulator offset by this sequence's duration */
    accumulator_advance(merged->total_duration_ms);

    /* Clean up */
    schedule_free(merged);
    clear_current_schedule();
}
