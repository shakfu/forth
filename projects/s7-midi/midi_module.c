/*
 * MIDI module for s7 scheme
 * Provides Scheme bindings for MIDI output using libremidi
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

/* Cross-platform sleep */
#ifdef _WIN32
#include <windows.h>
#define usleep(us) Sleep((us) / 1000)
#else
#include <unistd.h>
#endif

#include "s7.h"
#include <libremidi/libremidi-c.h>
#include "scm_prelude.h"
#include "music_theory.h"
#include "midi_file.h"
#include "scheduler.h"

#define MAX_PORTS 64
#define MAX_CAPTURE_EVENTS 4096
#define TICKS_PER_QUARTER 480
#define BASE_TEMPO 120

/* Global state */
static int global_tempo_bpm = 120;

/* Scale duration for current tempo (120 BPM = 1x, 60 BPM = 2x, 240 BPM = 0.5x) */
static int scale_duration_for_tempo(int duration_ms) {
    if (global_tempo_bpm <= 0 || global_tempo_bpm == BASE_TEMPO) {
        return duration_ms;
    }
    return (int)((double)duration_ms * BASE_TEMPO / global_tempo_bpm);
}
static libremidi_midi_observer_handle* midi_observer = NULL;
static libremidi_midi_out_port* out_ports[MAX_PORTS];
static int out_port_count = 0;
static s7_scheme *global_sc = NULL;

/* MidiOut type tag */
static s7_int midi_out_tag = 0;

/* MIDI capture system */
typedef struct {
    uint32_t time_ms;
    uint8_t type;      /* 0=note-on, 1=note-off, 2=cc */
    uint8_t channel;
    uint8_t data1;
    uint8_t data2;
} CapturedEvent;

static CapturedEvent capture_buffer[MAX_CAPTURE_EVENTS];
static int capture_count = 0;
static int capture_active = 0;
static struct timespec capture_start_time;
static int capture_bpm = 120;

/* Get current time in milliseconds since capture start */
static uint32_t capture_get_time_ms(void) {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    uint32_t elapsed = (now.tv_sec - capture_start_time.tv_sec) * 1000 +
                       (now.tv_nsec - capture_start_time.tv_nsec) / 1000000;
    return elapsed;
}

/* Add an event to the capture buffer */
static void capture_add_event(int type, int channel, int data1, int data2) {
    if (!capture_active) return;
    if (capture_count >= MAX_CAPTURE_EVENTS) {
        printf("Capture buffer full!\n");
        capture_active = 0;
        return;
    }
    CapturedEvent* e = &capture_buffer[capture_count++];
    e->time_ms = capture_get_time_ms();
    e->type = type;
    e->channel = channel;
    e->data1 = data1;
    e->data2 = data2;
}

/* MidiOut data structure */
typedef struct {
    libremidi_midi_out_handle* handle;
    int is_virtual;
    char *port_name;
} MidiOutData;

/* Forward declarations */
static void midi_cleanup_observer(void);
static int midi_init_observer(void);

/* ============================================================================
 * Observer and port management
 * ============================================================================ */

static void on_output_port_found(void* ctx, const libremidi_midi_out_port* port) {
    (void)ctx;
    if (out_port_count >= MAX_PORTS) return;
    libremidi_midi_out_port_clone(port, &out_ports[out_port_count]);
    out_port_count++;
}

static int midi_init_observer(void) {
    if (midi_observer != NULL) return 0;

    libremidi_observer_configuration observer_conf;
    int ret = libremidi_midi_observer_configuration_init(&observer_conf);
    if (ret != 0) return ret;

    observer_conf.track_any = true;

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) return ret;

    ret = libremidi_midi_observer_new(&observer_conf, &api_conf, &midi_observer);
    if (ret != 0) return ret;

    out_port_count = 0;
    ret = libremidi_midi_observer_enumerate_output_ports(midi_observer, NULL, on_output_port_found);
    return ret;
}

static void midi_cleanup_observer(void) {
    for (int i = 0; i < out_port_count; i++) {
        libremidi_midi_out_port_free(out_ports[i]);
    }
    out_port_count = 0;

    if (midi_observer != NULL) {
        libremidi_midi_observer_free(midi_observer);
        midi_observer = NULL;
    }
}

/* ============================================================================
 * Pitch parsing - uses common music_theory library
 * ============================================================================ */

/* Wrapper for backward compatibility */
static int parse_pitch(const char* name) {
    return music_parse_pitch(name);
}

/* Get pitch from s7 value (integer, string, or symbol) */
static int get_pitch(s7_scheme *sc, s7_pointer arg) {
    if (s7_is_integer(arg)) {
        return (int)s7_integer(arg);
    } else if (s7_is_string(arg)) {
        return parse_pitch(s7_string(arg));
    } else if (s7_is_symbol(arg)) {
        return parse_pitch(s7_symbol_name(arg));
    }
    return -1;
}

/* ============================================================================
 * MidiOut type functions
 * ============================================================================ */

static void free_midi_out(void *val) {
    MidiOutData *data = (MidiOutData *)val;
    if (data) {
        if (data->handle) {
            /* Send all notes off before closing */
            for (int ch = 0; ch < 16; ch++) {
                uint8_t msg[3] = { 0xB0 | ch, 123, 0 };
                libremidi_midi_out_send_message(data->handle, msg, 3);
            }
            libremidi_midi_out_free(data->handle);
        }
        if (data->port_name) free(data->port_name);
        free(data);
    }
}

static s7_pointer midi_out_to_string(s7_scheme *sc, s7_pointer args) {
    MidiOutData *data = (MidiOutData *)s7_c_object_value(s7_car(args));
    char buf[128];
    if (data->handle) {
        if (data->is_virtual) {
            snprintf(buf, sizeof(buf), "#<midi-out virtual \"%s\">",
                     data->port_name ? data->port_name : "s7MIDI");
        } else {
            snprintf(buf, sizeof(buf), "#<midi-out connected>");
        }
    } else {
        snprintf(buf, sizeof(buf), "#<midi-out closed>");
    }
    return s7_make_string(sc, buf);
}

static s7_pointer is_midi_out(s7_scheme *sc, s7_pointer args) {
    return s7_make_boolean(sc,
        s7_is_c_object(s7_car(args)) &&
        s7_c_object_type(s7_car(args)) == midi_out_tag);
}

/* ============================================================================
 * Module functions
 * ============================================================================ */

/* (midi-list-ports) -> list of (index name) */
static s7_pointer g_midi_list_ports(s7_scheme *sc, s7_pointer args) {
    (void)args;

    if (midi_init_observer() != 0) {
        return s7_error(sc, s7_make_symbol(sc, "midi-error"),
                        s7_list(sc, 1, s7_make_string(sc, "Failed to initialize MIDI observer")));
    }

    /* Re-enumerate ports */
    for (int i = 0; i < out_port_count; i++) {
        libremidi_midi_out_port_free(out_ports[i]);
    }
    out_port_count = 0;
    libremidi_midi_observer_enumerate_output_ports(midi_observer, NULL, on_output_port_found);

    /* Build list */
    s7_pointer result = s7_nil(sc);
    for (int i = out_port_count - 1; i >= 0; i--) {
        const char* name = NULL;
        size_t len = 0;
        if (libremidi_midi_out_port_name(out_ports[i], &name, &len) == 0) {
            s7_pointer entry = s7_list(sc, 2,
                s7_make_integer(sc, i),
                s7_make_string(sc, name));
            result = s7_cons(sc, entry, result);
        }
    }
    return result;
}

/* (midi-open) or (midi-open "name") or (midi-open index) */
static s7_pointer g_midi_open(s7_scheme *sc, s7_pointer args) {
    if (midi_init_observer() != 0) {
        return s7_error(sc, s7_make_symbol(sc, "midi-error"),
                        s7_list(sc, 1, s7_make_string(sc, "Failed to initialize MIDI observer")));
    }

    libremidi_midi_configuration midi_conf;
    int ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        return s7_error(sc, s7_make_symbol(sc, "midi-error"),
                        s7_list(sc, 1, s7_make_string(sc, "Failed to init MIDI config")));
    }

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        return s7_error(sc, s7_make_symbol(sc, "midi-error"),
                        s7_list(sc, 1, s7_make_string(sc, "Failed to init API config")));
    }

    int is_virtual = 0;
    const char *port_name = "s7MIDI";

    if (args == s7_nil(sc)) {
        /* (midi-open) -> virtual port with default name */
        midi_conf.virtual_port = true;
        midi_conf.port_name = port_name;
        is_virtual = 1;
    } else {
        s7_pointer arg = s7_car(args);
        if (s7_is_integer(arg)) {
            /* (midi-open index) -> hardware port */
            int port_idx = (int)s7_integer(arg);
            if (port_idx < 0 || port_idx >= out_port_count) {
                return s7_error(sc, s7_make_symbol(sc, "out-of-range"),
                                s7_list(sc, 1, s7_make_string(sc, "Invalid port index")));
            }
            midi_conf.out_port = out_ports[port_idx];
        } else if (s7_is_string(arg)) {
            /* (midi-open "name") -> virtual port with name */
            port_name = s7_string(arg);
            midi_conf.virtual_port = true;
            midi_conf.port_name = port_name;
            is_virtual = 1;
        } else {
            return s7_wrong_type_error(sc, s7_make_symbol(sc, "midi-open"), 1, arg,
                                       s7_make_string(sc, "integer or string"));
        }
    }

    libremidi_midi_out_handle* handle;
    ret = libremidi_midi_out_new(&midi_conf, &api_conf, &handle);
    if (ret != 0) {
        return s7_error(sc, s7_make_symbol(sc, "midi-error"),
                        s7_list(sc, 1, s7_make_string(sc, "Failed to open MIDI output")));
    }

    MidiOutData *data = (MidiOutData *)malloc(sizeof(MidiOutData));
    data->handle = handle;
    data->is_virtual = is_virtual;
    data->port_name = is_virtual ? strdup(port_name) : NULL;

    return s7_make_c_object(sc, midi_out_tag, (void *)data);
}

/* (note "C4") or (note 'c4) -> MIDI number */
static s7_pointer g_note(s7_scheme *sc, s7_pointer args) {
    s7_pointer arg = s7_car(args);
    int pitch = get_pitch(sc, arg);
    if (pitch < 0) {
        return s7_error(sc, s7_make_symbol(sc, "invalid-note"),
                        s7_list(sc, 1, s7_make_string(sc, "Invalid note name")));
    }
    return s7_make_integer(sc, pitch);
}

/* ============================================================================
 * MidiOut methods
 * ============================================================================ */

#define GET_MIDI_OUT(args) \
    s7_pointer _obj = s7_car(args); \
    if (!s7_is_c_object(_obj) || s7_c_object_type(_obj) != midi_out_tag) { \
        return s7_wrong_type_error(sc, s7_make_symbol(sc, __func__), 1, _obj, \
                                   s7_make_string(sc, "midi-out")); \
    } \
    MidiOutData *data = (MidiOutData *)s7_c_object_value(_obj); \
    if (!data->handle) { \
        return s7_error(sc, s7_make_symbol(sc, "midi-error"), \
                        s7_list(sc, 1, s7_make_string(sc, "MIDI output is closed"))); \
    }

/* (midi-close m) */
static s7_pointer g_midi_close(s7_scheme *sc, s7_pointer args) {
    s7_pointer obj = s7_car(args);
    if (!s7_is_c_object(obj) || s7_c_object_type(obj) != midi_out_tag) {
        return s7_wrong_type_error(sc, s7_make_symbol(sc, "midi-close"), 1, obj,
                                   s7_make_string(sc, "midi-out"));
    }
    MidiOutData *data = (MidiOutData *)s7_c_object_value(obj);
    if (data->handle) {
        /* Send all notes off */
        for (int ch = 0; ch < 16; ch++) {
            uint8_t msg[3] = { 0xB0 | ch, 123, 0 };
            libremidi_midi_out_send_message(data->handle, msg, 3);
        }
        libremidi_midi_out_free(data->handle);
        data->handle = NULL;
    }
    return s7_unspecified(sc);
}

/* (midi-open? m) */
static s7_pointer g_midi_open_p(s7_scheme *sc, s7_pointer args) {
    s7_pointer obj = s7_car(args);
    if (!s7_is_c_object(obj) || s7_c_object_type(obj) != midi_out_tag) {
        return s7_f(sc);
    }
    MidiOutData *data = (MidiOutData *)s7_c_object_value(obj);
    return s7_make_boolean(sc, data->handle != NULL);
}

/* (midi-note-on m pitch [velocity] [channel]) */
static s7_pointer g_midi_note_on(s7_scheme *sc, s7_pointer args) {
    GET_MIDI_OUT(args);
    args = s7_cdr(args);

    int pitch = get_pitch(sc, s7_car(args));
    if (pitch < 0 || pitch > 127) {
        return s7_error(sc, s7_make_symbol(sc, "out-of-range"),
                        s7_list(sc, 1, s7_make_string(sc, "pitch must be 0-127")));
    }
    args = s7_cdr(args);

    int velocity = 80;
    if (args != s7_nil(sc)) {
        velocity = (int)s7_integer(s7_car(args));
        args = s7_cdr(args);
    }

    int channel = 1;
    if (args != s7_nil(sc)) {
        channel = (int)s7_integer(s7_car(args));
    }

    if (velocity < 0 || velocity > 127) velocity = 80;
    if (channel < 1 || channel > 16) channel = 1;

    uint8_t msg[3] = {
        0x90 | ((channel - 1) & 0x0F),
        pitch & 0x7F,
        velocity & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);
    capture_add_event(0, channel - 1, pitch, velocity);

    return s7_unspecified(sc);
}

/* (midi-note-off m pitch [velocity] [channel]) */
static s7_pointer g_midi_note_off(s7_scheme *sc, s7_pointer args) {
    GET_MIDI_OUT(args);
    args = s7_cdr(args);

    int pitch = get_pitch(sc, s7_car(args));
    if (pitch < 0 || pitch > 127) {
        return s7_error(sc, s7_make_symbol(sc, "out-of-range"),
                        s7_list(sc, 1, s7_make_string(sc, "pitch must be 0-127")));
    }
    args = s7_cdr(args);

    int velocity = 0;
    if (args != s7_nil(sc)) {
        velocity = (int)s7_integer(s7_car(args));
        args = s7_cdr(args);
    }

    int channel = 1;
    if (args != s7_nil(sc)) {
        channel = (int)s7_integer(s7_car(args));
    }

    if (velocity < 0 || velocity > 127) velocity = 0;
    if (channel < 1 || channel > 16) channel = 1;

    uint8_t msg[3] = {
        0x80 | ((channel - 1) & 0x0F),
        pitch & 0x7F,
        velocity & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);
    capture_add_event(1, channel - 1, pitch, 0);

    return s7_unspecified(sc);
}

/* (midi-note m pitch [velocity] [duration] [channel]) - high level */
static s7_pointer g_midi_note(s7_scheme *sc, s7_pointer args) {
    GET_MIDI_OUT(args);
    args = s7_cdr(args);

    int pitch = get_pitch(sc, s7_car(args));
    if (pitch < 0 || pitch > 127) {
        return s7_error(sc, s7_make_symbol(sc, "out-of-range"),
                        s7_list(sc, 1, s7_make_string(sc, "pitch must be 0-127")));
    }
    args = s7_cdr(args);

    int velocity = 80;
    if (args != s7_nil(sc)) {
        velocity = (int)s7_integer(s7_car(args));
        args = s7_cdr(args);
    }

    int duration = 500;
    if (args != s7_nil(sc)) {
        duration = (int)s7_integer(s7_car(args));
        args = s7_cdr(args);
    }

    int channel = 1;
    if (args != s7_nil(sc)) {
        channel = (int)s7_integer(s7_car(args));
    }

    if (velocity < 0 || velocity > 127) velocity = 80;
    if (duration < 0) duration = 500;
    if (channel < 1 || channel > 16) channel = 1;

    /* Note on */
    uint8_t msg[3] = {
        0x90 | ((channel - 1) & 0x0F),
        pitch & 0x7F,
        velocity & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);
    capture_add_event(0, channel - 1, pitch, velocity);

    /* Wait (scaled for tempo) */
    int scaled_dur = scale_duration_for_tempo(duration);
    if (scaled_dur > 0) {
        usleep(scaled_dur * 1000);
    }

    /* Note off */
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[2] = 0;
    libremidi_midi_out_send_message(data->handle, msg, 3);
    capture_add_event(1, channel - 1, pitch, 0);

    return s7_unspecified(sc);
}

/* (midi-chord m pitches [velocity] [duration] [channel]) */
static s7_pointer g_midi_chord(s7_scheme *sc, s7_pointer args) {
    GET_MIDI_OUT(args);
    args = s7_cdr(args);

    s7_pointer pitches_list = s7_car(args);
    if (!s7_is_list(sc, pitches_list)) {
        return s7_wrong_type_error(sc, s7_make_symbol(sc, "midi-chord"), 2, pitches_list,
                                   s7_make_string(sc, "list"));
    }
    args = s7_cdr(args);

    int velocity = 80;
    if (args != s7_nil(sc)) {
        velocity = (int)s7_integer(s7_car(args));
        args = s7_cdr(args);
    }

    int duration = 500;
    if (args != s7_nil(sc)) {
        duration = (int)s7_integer(s7_car(args));
        args = s7_cdr(args);
    }

    int channel = 1;
    if (args != s7_nil(sc)) {
        channel = (int)s7_integer(s7_car(args));
    }

    if (velocity < 0 || velocity > 127) velocity = 80;
    if (duration < 0) duration = 500;
    if (channel < 1 || channel > 16) channel = 1;

    /* Collect pitches */
    int pitches[128];
    int count = 0;
    s7_pointer p = pitches_list;
    while (p != s7_nil(sc) && count < 128) {
        int pitch = get_pitch(sc, s7_car(p));
        if (pitch >= 0 && pitch <= 127) {
            pitches[count++] = pitch;
        }
        p = s7_cdr(p);
    }

    if (count == 0) {
        return s7_unspecified(sc);
    }

    /* Send note-ons */
    uint8_t msg[3];
    msg[0] = 0x90 | ((channel - 1) & 0x0F);
    msg[2] = velocity & 0x7F;
    for (int i = 0; i < count; i++) {
        msg[1] = pitches[i] & 0x7F;
        libremidi_midi_out_send_message(data->handle, msg, 3);
        capture_add_event(0, channel - 1, pitches[i], velocity);
    }

    /* Wait (scaled for tempo) */
    int scaled_dur = scale_duration_for_tempo(duration);
    if (scaled_dur > 0) {
        usleep(scaled_dur * 1000);
    }

    /* Send note-offs */
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[2] = 0;
    for (int i = 0; i < count; i++) {
        msg[1] = pitches[i] & 0x7F;
        libremidi_midi_out_send_message(data->handle, msg, 3);
        capture_add_event(1, channel - 1, pitches[i], 0);
    }

    return s7_unspecified(sc);
}

/* (midi-cc m control value [channel]) */
static s7_pointer g_midi_cc(s7_scheme *sc, s7_pointer args) {
    GET_MIDI_OUT(args);
    args = s7_cdr(args);

    int control = (int)s7_integer(s7_car(args));
    args = s7_cdr(args);
    int value = (int)s7_integer(s7_car(args));
    args = s7_cdr(args);

    int channel = 1;
    if (args != s7_nil(sc)) {
        channel = (int)s7_integer(s7_car(args));
    }

    if (control < 0 || control > 127) control = 0;
    if (value < 0 || value > 127) value = 0;
    if (channel < 1 || channel > 16) channel = 1;

    uint8_t msg[3] = {
        0xB0 | ((channel - 1) & 0x0F),
        control & 0x7F,
        value & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);
    capture_add_event(2, channel - 1, control, value);

    return s7_unspecified(sc);
}

/* (midi-program m program [channel]) */
static s7_pointer g_midi_program(s7_scheme *sc, s7_pointer args) {
    GET_MIDI_OUT(args);
    args = s7_cdr(args);

    int program = (int)s7_integer(s7_car(args));
    args = s7_cdr(args);

    int channel = 1;
    if (args != s7_nil(sc)) {
        channel = (int)s7_integer(s7_car(args));
    }

    if (program < 0 || program > 127) program = 0;
    if (channel < 1 || channel > 16) channel = 1;

    uint8_t msg[2] = {
        0xC0 | ((channel - 1) & 0x0F),
        program & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 2);

    return s7_unspecified(sc);
}

/* (midi-all-notes-off m [channel]) */
static s7_pointer g_midi_all_notes_off(s7_scheme *sc, s7_pointer args) {
    GET_MIDI_OUT(args);
    args = s7_cdr(args);

    int start_ch = 1, end_ch = 16;
    if (args != s7_nil(sc)) {
        int ch = (int)s7_integer(s7_car(args));
        if (ch >= 1 && ch <= 16) {
            start_ch = end_ch = ch;
        }
    }

    for (int ch = start_ch; ch <= end_ch; ch++) {
        uint8_t msg[3] = {
            0xB0 | ((ch - 1) & 0x0F),
            123,
            0
        };
        libremidi_midi_out_send_message(data->handle, msg, 3);
    }

    return s7_unspecified(sc);
}

/* (midi-sleep ms) */
static s7_pointer g_midi_sleep(s7_scheme *sc, s7_pointer args) {
    int ms = (int)s7_integer(s7_car(args));
    int scaled_ms = scale_duration_for_tempo(ms);
    if (scaled_ms > 0) {
        usleep(scaled_ms * 1000);
    }
    return s7_unspecified(sc);
}

/* (set-tempo-c! bpm) - Set C layer tempo for duration scaling */
static s7_pointer g_set_tempo_c(s7_scheme *sc, s7_pointer args) {
    int bpm = (int)s7_integer(s7_car(args));
    if (bpm > 0) {
        global_tempo_bpm = bpm;
    }
    return s7_unspecified(sc);
}

/* (get-tempo-c) - Get C layer tempo */
static s7_pointer g_get_tempo_c(s7_scheme *sc, s7_pointer args) {
    (void)args;
    return s7_make_integer(sc, global_tempo_bpm);
}

/* ============================================================================
 * Scale functions - using common music_theory library
 * ============================================================================ */

/* Helper to read interval list from s7 list */
static int get_intervals_from_list(s7_scheme *sc, s7_pointer lst, int* intervals, int max_size) {
    int count = 0;
    s7_pointer p = lst;
    while (p != s7_nil(sc) && count < max_size) {
        if (s7_is_integer(s7_car(p))) {
            intervals[count++] = (int)s7_integer(s7_car(p));
        }
        p = s7_cdr(p);
    }
    return count;
}

/* (build-scale root intervals) -> list of pitches */
static s7_pointer g_build_scale(s7_scheme *sc, s7_pointer args) {
    int root = get_pitch(sc, s7_car(args));
    if (root < 0 || root > 127) {
        return s7_error(sc, s7_make_symbol(sc, "out-of-range"),
                        s7_list(sc, 1, s7_make_string(sc, "Invalid root pitch")));
    }
    args = s7_cdr(args);

    s7_pointer intervals_list = s7_car(args);
    if (!s7_is_list(sc, intervals_list)) {
        return s7_wrong_type_error(sc, s7_make_symbol(sc, "build-scale"), 2,
                                   intervals_list, s7_make_string(sc, "list"));
    }

    int intervals[16];
    int num_intervals = get_intervals_from_list(sc, intervals_list, intervals, 16);
    if (num_intervals == 0) {
        return s7_nil(sc);
    }

    int pitches[16];
    int count = music_build_scale(root, intervals, num_intervals, pitches);

    /* Build result list */
    s7_pointer result = s7_nil(sc);
    for (int i = count - 1; i >= 0; i--) {
        result = s7_cons(sc, s7_make_integer(sc, pitches[i]), result);
    }
    return result;
}

/* (scale-degree root intervals degree) -> pitch */
static s7_pointer g_scale_degree(s7_scheme *sc, s7_pointer args) {
    int root = get_pitch(sc, s7_car(args));
    if (root < 0 || root > 127) {
        return s7_error(sc, s7_make_symbol(sc, "out-of-range"),
                        s7_list(sc, 1, s7_make_string(sc, "Invalid root pitch")));
    }
    args = s7_cdr(args);

    s7_pointer intervals_list = s7_car(args);
    if (!s7_is_list(sc, intervals_list)) {
        return s7_wrong_type_error(sc, s7_make_symbol(sc, "scale-degree"), 2,
                                   intervals_list, s7_make_string(sc, "list"));
    }
    args = s7_cdr(args);

    int degree = (int)s7_integer(s7_car(args));

    int intervals[16];
    int num_intervals = get_intervals_from_list(sc, intervals_list, intervals, 16);
    if (num_intervals == 0) {
        return s7_error(sc, s7_make_symbol(sc, "invalid-scale"),
                        s7_list(sc, 1, s7_make_string(sc, "Empty intervals list")));
    }

    int pitch = music_scale_degree(root, intervals, num_intervals, degree);
    if (pitch < 0) {
        return s7_error(sc, s7_make_symbol(sc, "out-of-range"),
                        s7_list(sc, 1, s7_make_string(sc, "Scale degree out of range")));
    }
    return s7_make_integer(sc, pitch);
}

/* (in-scale? pitch root intervals) -> boolean */
static s7_pointer g_in_scale(s7_scheme *sc, s7_pointer args) {
    int pitch = get_pitch(sc, s7_car(args));
    if (pitch < 0 || pitch > 127) {
        return s7_f(sc);
    }
    args = s7_cdr(args);

    int root = get_pitch(sc, s7_car(args));
    if (root < 0 || root > 127) {
        return s7_f(sc);
    }
    args = s7_cdr(args);

    s7_pointer intervals_list = s7_car(args);
    if (!s7_is_list(sc, intervals_list)) {
        return s7_wrong_type_error(sc, s7_make_symbol(sc, "in-scale?"), 3,
                                   intervals_list, s7_make_string(sc, "list"));
    }

    int intervals[16];
    int num_intervals = get_intervals_from_list(sc, intervals_list, intervals, 16);
    if (num_intervals == 0) {
        return s7_f(sc);
    }

    int result = music_in_scale(pitch, root, intervals, num_intervals);
    return s7_make_boolean(sc, result);
}

/* (quantize-to-scale pitch root intervals) -> pitch */
static s7_pointer g_quantize_to_scale(s7_scheme *sc, s7_pointer args) {
    int pitch = get_pitch(sc, s7_car(args));
    if (pitch < 0 || pitch > 127) {
        return s7_error(sc, s7_make_symbol(sc, "out-of-range"),
                        s7_list(sc, 1, s7_make_string(sc, "Invalid pitch")));
    }
    args = s7_cdr(args);

    int root = get_pitch(sc, s7_car(args));
    if (root < 0 || root > 127) {
        return s7_error(sc, s7_make_symbol(sc, "out-of-range"),
                        s7_list(sc, 1, s7_make_string(sc, "Invalid root pitch")));
    }
    args = s7_cdr(args);

    s7_pointer intervals_list = s7_car(args);
    if (!s7_is_list(sc, intervals_list)) {
        return s7_wrong_type_error(sc, s7_make_symbol(sc, "quantize-to-scale"), 3,
                                   intervals_list, s7_make_string(sc, "list"));
    }

    int intervals[16];
    int num_intervals = get_intervals_from_list(sc, intervals_list, intervals, 16);
    if (num_intervals == 0) {
        return s7_make_integer(sc, pitch);
    }

    int result = music_quantize_to_scale(pitch, root, intervals, num_intervals);
    return s7_make_integer(sc, result);
}

/* (midi-pitch-bend m cents [channel]) - send pitch bend for microtonal */
static s7_pointer g_midi_pitch_bend(s7_scheme *sc, s7_pointer args) {
    GET_MIDI_OUT(args);
    args = s7_cdr(args);

    int cents = (int)s7_integer(s7_car(args));
    args = s7_cdr(args);

    int channel = 1;
    if (args != s7_nil(sc)) {
        channel = (int)s7_integer(s7_car(args));
    }

    if (channel < 1 || channel > 16) channel = 1;

    int bend = music_cents_to_bend(cents);
    uint8_t msg[3] = {
        0xE0 | ((channel - 1) & 0x0F),
        bend & 0x7F,
        (bend >> 7) & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);

    return s7_unspecified(sc);
}

/* (help) */
static s7_pointer g_help(s7_scheme *sc, s7_pointer args) {
    (void)args;
    const char *help_text =
        "s7_midi - Scheme MIDI language\n"
        "\n"
        "Port management:\n"
        "  (midi-list-ports)           List available MIDI ports\n"
        "  (midi-open)                 Open virtual port \"s7MIDI\"\n"
        "  (midi-open \"name\")          Open virtual port with name\n"
        "  (midi-open index)           Open hardware port by index\n"
        "  (midi-close m)              Close MIDI port\n"
        "  (midi-open? m)              Check if port is open\n"
        "  (midi-out? x)               Check if x is a midi-out\n"
        "\n"
        "Note playing:\n"
        "  (midi-note m pitch [vel] [dur] [ch])     Play note\n"
        "  (midi-chord m pitches [vel] [dur] [ch])  Play chord\n"
        "  (midi-note-on m pitch [vel] [ch])        Note on\n"
        "  (midi-note-off m pitch [vel] [ch])       Note off\n"
        "\n"
        "Control:\n"
        "  (midi-cc m ctrl val [ch])   Control change\n"
        "  (midi-program m prog [ch])  Program change\n"
        "  (midi-all-notes-off m [ch]) All notes off\n"
        "\n"
        "Recording and File I/O:\n"
        "  (record-midi [bpm])         Start recording\n"
        "  (record-stop)               Stop recording\n"
        "  (save-midi filename)        Save as Scheme file\n"
        "  (write-mid filename [ppqn]) Save as standard MIDI file\n"
        "  (read-mid filename)         Read MIDI file\n"
        "\n"
        "Utilities:\n"
        "  (note \"C4\") or (note 'c4)  Parse note name to MIDI number\n"
        "  (midi-sleep ms)             Sleep for milliseconds\n"
        "\n"
        "Pitch constants: c0-c8, cs0-cs8 (sharps), etc.\n"
        "Dynamics: ppp pp p mp mf f ff fff\n"
        "Durations: whole half quarter eighth sixteenth\n"
        "\n"
        "Chord builders:\n"
        "  (major root) (minor root) (dim root) (aug root)\n"
        "  (dom7 root) (maj7 root) (min7 root)\n"
        "\n"
        "Scale functions:\n"
        "  (build-scale root intervals)      Build scale pitches\n"
        "  (scale-degree root intervals n)   Get nth scale degree\n"
        "  (in-scale? pitch root intervals)  Check if pitch in scale\n"
        "  (quantize-to-scale pitch root intervals) Snap to scale\n"
        "  (midi-pitch-bend m cents [ch])    Pitch bend (microtonal)\n"
        "\n"
        "Scale helpers (via prelude):\n"
        "  (scale root name)        Build scale, e.g. (scale c4 'major)\n"
        "  (degree root name n)     Get scale degree\n"
        "  scale-major, scale-minor, scale-dorian, scale-blues, etc.\n"
        "\n"
        "Example:\n"
        "  (define m (midi-open))\n"
        "  (midi-note m c4 mf quarter)\n"
        "  (midi-chord m (major c4) mf half)\n"
        "  (midi-close m)\n";

    printf("%s", help_text);
    return s7_unspecified(sc);
}

/* ============================================================================
 * MIDI Recording functions
 * ============================================================================ */

/* (record-midi [bpm]) - Start recording MIDI events */
static s7_pointer g_record_midi(s7_scheme *sc, s7_pointer args) {
    if (capture_active) {
        printf("Already recording (use (record-stop) first)\n");
        return s7_unspecified(sc);
    }
    capture_bpm = 120;
    if (args != s7_nil(sc)) {
        capture_bpm = (int)s7_integer(s7_car(args));
    }
    capture_count = 0;
    capture_active = 1;
    clock_gettime(CLOCK_MONOTONIC, &capture_start_time);
    printf("MIDI recording started at %d BPM. Play notes, then (record-stop) and (save-midi filename).\n", capture_bpm);
    return s7_unspecified(sc);
}

/* (record-stop) - Stop recording MIDI events */
static s7_pointer g_record_stop(s7_scheme *sc, s7_pointer args) {
    (void)args;
    if (!capture_active) {
        printf("Not recording\n");
        return s7_unspecified(sc);
    }
    capture_active = 0;
    printf("MIDI recording stopped. %d events recorded. Use (save-midi filename) to save.\n", capture_count);
    return s7_unspecified(sc);
}

/* (save-midi filename) - Save recorded MIDI to a Scheme file */
static s7_pointer g_save_midi(s7_scheme *sc, s7_pointer args) {
    const char* filename = s7_string(s7_car(args));

    if (capture_count == 0) {
        printf("Nothing to save (recording is empty)\n");
        return s7_unspecified(sc);
    }

    FILE* f = fopen(filename, "w");
    if (f == NULL) {
        return s7_error(sc, s7_make_symbol(sc, "file-error"),
                        s7_list(sc, 1, s7_make_string(sc, "Cannot create file")));
    }

    /* Write header */
    fprintf(f, ";; MIDI recording\n");
    fprintf(f, ";; %d events recorded at %d BPM\n\n", capture_count, capture_bpm);
    fprintf(f, "(define m (midi-open))\n\n");
    fprintf(f, ";; Notes: (start-ms pitch velocity duration-ms channel)\n");
    fprintf(f, "(define notes '(\n");

    /* Track note-on events to pair with note-offs */
    int note_on_time[16][128];
    int note_on_vel[16][128];
    for (int ch = 0; ch < 16; ch++) {
        for (int p = 0; p < 128; p++) {
            note_on_time[ch][p] = -1;
            note_on_vel[ch][p] = 0;
        }
    }

    int notes_written = 0;

    for (int i = 0; i < capture_count; i++) {
        CapturedEvent* e = &capture_buffer[i];

        if (e->type == 0) {  /* Note-on */
            note_on_time[e->channel][e->data1] = e->time_ms;
            note_on_vel[e->channel][e->data1] = e->data2;
        } else if (e->type == 1) {  /* Note-off */
            int start_ms = note_on_time[e->channel][e->data1];
            if (start_ms >= 0) {
                int dur_ms = e->time_ms - start_ms;
                if (dur_ms < 1) dur_ms = 1;
                int vel = note_on_vel[e->channel][e->data1];

                fprintf(f, "  (%d %d %d %d %d)\n",
                        start_ms, e->data1, vel, dur_ms, e->channel + 1);
                notes_written++;

                note_on_time[e->channel][e->data1] = -1;
            }
        }
    }

    fprintf(f, "))\n\n");

    /* Write playback code with proper timing using delta sleeps */
    fprintf(f, ";; Playback with original timing (uses delta delays between notes)\n");
    fprintf(f, "(define (play-timed)\n");
    fprintf(f, "  (let loop ((ns notes) (last-time 0))\n");
    fprintf(f, "    (if (not (null? ns))\n");
    fprintf(f, "        (let* ((n (car ns))\n");
    fprintf(f, "               (start-ms (car n))\n");
    fprintf(f, "               (pitch (cadr n))\n");
    fprintf(f, "               (vel (caddr n))\n");
    fprintf(f, "               (dur (cadddr n))\n");
    fprintf(f, "               (ch (car (cddddr n)))\n");
    fprintf(f, "               (delay (- start-ms last-time)))\n");
    fprintf(f, "          (if (> delay 0) (midi-sleep delay))\n");
    fprintf(f, "          (midi-note-on m pitch vel ch)\n");
    fprintf(f, "          (midi-sleep dur)\n");
    fprintf(f, "          (midi-note-off m pitch ch)\n");
    fprintf(f, "          (loop (cdr ns) (+ start-ms dur))))))\n\n");

    fprintf(f, ";; Run: (play-timed)\n");
    fprintf(f, ";; Or sequential playback: (for-each (lambda (n) (midi-note m (cadr n) (caddr n) (cadddr n) (car (cddddr n)))) notes)\n");
    fprintf(f, ";; Close when done: (midi-close m)\n");
    fprintf(f, ";; %d notes written\n", notes_written);

    fclose(f);
    printf("Saved %d notes to '%s'\n", notes_written, filename);
    return s7_unspecified(sc);
}

/* (record-status) - Get recording status */
static s7_pointer g_record_status(s7_scheme *sc, s7_pointer args) {
    (void)args;
    return s7_list(sc, 3,
                   s7_cons(sc, s7_make_symbol(sc, "active"), s7_make_boolean(sc, capture_active)),
                   s7_cons(sc, s7_make_symbol(sc, "events"), s7_make_integer(sc, capture_count)),
                   s7_cons(sc, s7_make_symbol(sc, "bpm"), s7_make_integer(sc, capture_bpm)));
}

/* ============================================================================
 * MIDI File I/O (using libremidi reader/writer)
 * ============================================================================ */

/* (write-mid filename [ppqn]) - Write captured events to standard MIDI file */
static s7_pointer g_write_mid(s7_scheme *sc, s7_pointer args) {
    const char* filename = s7_string(s7_car(args));
    args = s7_cdr(args);

    int ppqn = TICKS_PER_QUARTER;
    if (args != s7_nil(sc)) {
        ppqn = (int)s7_integer(s7_car(args));
    }

    if (capture_count == 0) {
        printf("Nothing to save (no events recorded)\n");
        return s7_unspecified(sc);
    }

    midi_file_writer* writer = NULL;
    if (midi_file_writer_new(&writer) != 0) {
        return s7_error(sc, s7_make_symbol(sc, "midi-error"),
                        s7_list(sc, 1, s7_make_string(sc, "Failed to create MIDI file writer")));
    }

    midi_file_writer_set_ppqn(writer, (uint16_t)ppqn);
    midi_file_writer_add_track(writer);

    /* Add tempo event at the beginning */
    midi_file_writer_tempo_bpm(writer, 0, 0, capture_bpm);

    /* Convert milliseconds to ticks: tick = ms * ppqn * bpm / 60000 */
    double ms_to_tick = (double)ppqn * capture_bpm / 60000.0;

    for (int i = 0; i < capture_count; i++) {
        CapturedEvent* e = &capture_buffer[i];
        int tick = (int)(e->time_ms * ms_to_tick);
        int channel = e->channel + 1;  /* 1-based for midi_file API */

        switch (e->type) {
            case 0:  /* note-on */
                midi_file_writer_note_on(writer, tick, 0, channel, e->data1, e->data2);
                break;
            case 1:  /* note-off */
                midi_file_writer_note_off(writer, tick, 0, channel, e->data1, 0);
                break;
            case 2:  /* cc */
                midi_file_writer_cc(writer, tick, 0, channel, e->data1, e->data2);
                break;
            default:
                break;
        }
    }

    int result = midi_file_writer_save(writer, filename);
    midi_file_writer_free(writer);

    if (result != 0) {
        return s7_error(sc, s7_make_symbol(sc, "file-error"),
                        s7_list(sc, 1, s7_make_string(sc, "Failed to write MIDI file")));
    }

    printf("Saved %d events to '%s'\n", capture_count, filename);
    return s7_unspecified(sc);
}

/* Callback context for reading MIDI file */
typedef struct {
    s7_scheme *sc;
    s7_pointer events_list;
} ReadMidContext;

static void read_mid_callback(void* ctx, const midi_file_event* event) {
    ReadMidContext* rctx = (ReadMidContext*)ctx;
    s7_scheme* sc = rctx->sc;

    /* Skip meta events and system messages */
    if (event->type < 0x80 || event->type >= 0xF0) return;

    /* Decode message type */
    const char* type_name = "unknown";
    switch (event->type) {
        case 0x80: type_name = "note-off"; break;
        case 0x90: type_name = (event->data2 > 0) ? "note-on" : "note-off"; break;
        case 0xA0: type_name = "poly-pressure"; break;
        case 0xB0: type_name = "cc"; break;
        case 0xC0: type_name = "program"; break;
        case 0xD0: type_name = "aftertouch"; break;
        case 0xE0: type_name = "pitch-bend"; break;
        default: break;
    }

    /* Create event as alist */
    s7_pointer event_list = s7_nil(sc);

    /* Add fields in reverse order since cons prepends */
    if (event->type == 0x80 || event->type == 0x90) {
        event_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "velocity"),
                                         s7_make_integer(sc, event->data2)), event_list);
        event_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "pitch"),
                                         s7_make_integer(sc, event->data1)), event_list);
    } else if (event->type == 0xB0) {
        event_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "value"),
                                         s7_make_integer(sc, event->data2)), event_list);
        event_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "control"),
                                         s7_make_integer(sc, event->data1)), event_list);
    } else if (event->type == 0xC0) {
        event_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "program"),
                                         s7_make_integer(sc, event->data1)), event_list);
    } else if (event->type == 0xE0) {
        int bend = event->data1 | (event->data2 << 7);
        event_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "value"),
                                         s7_make_integer(sc, bend)), event_list);
    } else {
        event_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "data2"),
                                         s7_make_integer(sc, event->data2)), event_list);
        event_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "data1"),
                                         s7_make_integer(sc, event->data1)), event_list);
    }

    event_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "type"),
                                     s7_make_symbol(sc, type_name)), event_list);
    event_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "channel"),
                                     s7_make_integer(sc, event->channel)), event_list);
    event_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "tick"),
                                     s7_make_integer(sc, event->tick)), event_list);
    event_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "track"),
                                     s7_make_integer(sc, event->track)), event_list);

    /* Add event to list */
    rctx->events_list = s7_cons(sc, event_list, rctx->events_list);
}

/* (read-mid filename) - Read MIDI file and return alist with events */
static s7_pointer g_read_mid(s7_scheme *sc, s7_pointer args) {
    const char* filename = s7_string(s7_car(args));

    midi_file_reader* reader = NULL;
    if (midi_file_reader_new(&reader) != 0) {
        return s7_error(sc, s7_make_symbol(sc, "midi-error"),
                        s7_list(sc, 1, s7_make_string(sc, "Failed to create MIDI file reader")));
    }

    int result = midi_file_reader_load(reader, filename);
    if (result == 0) {
        midi_file_reader_free(reader);
        return s7_error(sc, s7_make_symbol(sc, "file-error"),
                        s7_list(sc, 1, s7_make_string(sc, "Failed to parse MIDI file")));
    }

    /* Gather events */
    ReadMidContext ctx = { sc, s7_nil(sc) };
    s7_gc_protect(sc, ctx.events_list);
    midi_file_reader_for_each(reader, &ctx, read_mid_callback);

    /* Reverse list to get chronological order */
    s7_pointer events = s7_reverse(sc, ctx.events_list);

    /* Build result alist */
    s7_pointer result_list = s7_nil(sc);
    result_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "events"), events), result_list);
    result_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "format"),
                                      s7_make_integer(sc, midi_file_reader_format(reader))), result_list);
    result_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "duration"),
                                      s7_make_real(sc, midi_file_reader_duration(reader))), result_list);
    result_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "tempo"),
                                      s7_make_real(sc, midi_file_reader_tempo(reader))), result_list);
    result_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "ppqn"),
                                      s7_make_real(sc, midi_file_reader_ppqn(reader))), result_list);
    result_list = s7_cons(sc, s7_cons(sc, s7_make_symbol(sc, "num-tracks"),
                                      s7_make_integer(sc, midi_file_reader_num_tracks(reader))), result_list);

    midi_file_reader_free(reader);
    return result_list;
}

/* ============================================================================
 * Module initialization
 * ============================================================================ */

void s7_midi_init(s7_scheme *sc) {
    global_sc = sc;

    /* Create midi-out type */
    midi_out_tag = s7_make_c_type(sc, "midi-out");
    s7_c_type_set_free(sc, midi_out_tag, free_midi_out);
    s7_c_type_set_to_string(sc, midi_out_tag, midi_out_to_string);

    /* Module functions */
    s7_define_function(sc, "midi-list-ports", g_midi_list_ports, 0, 0, false,
                       "(midi-list-ports) returns list of (index name) for available MIDI ports");

    s7_define_function(sc, "midi-open", g_midi_open, 0, 1, false,
                       "(midi-open) or (midi-open name) or (midi-open index) opens a MIDI output");

    s7_define_function(sc, "midi-close", g_midi_close, 1, 0, false,
                       "(midi-close m) closes the MIDI output");

    s7_define_function(sc, "midi-open?", g_midi_open_p, 1, 0, false,
                       "(midi-open? m) returns #t if the MIDI output is open");

    s7_define_function(sc, "midi-out?", is_midi_out, 1, 0, false,
                       "(midi-out? x) returns #t if x is a midi-out object");

    s7_define_function(sc, "note", g_note, 1, 0, false,
                       "(note name) parses note name to MIDI number, e.g. (note \"C4\") -> 60");

    /* MidiOut methods */
    s7_define_function(sc, "midi-note-on", g_midi_note_on, 2, 2, false,
                       "(midi-note-on m pitch [velocity] [channel]) sends note-on");

    s7_define_function(sc, "midi-note-off", g_midi_note_off, 2, 2, false,
                       "(midi-note-off m pitch [velocity] [channel]) sends note-off");

    s7_define_function(sc, "midi-note", g_midi_note, 2, 3, false,
                       "(midi-note m pitch [velocity] [duration] [channel]) plays a note");

    s7_define_function(sc, "midi-chord", g_midi_chord, 2, 3, false,
                       "(midi-chord m pitches [velocity] [duration] [channel]) plays a chord");

    s7_define_function(sc, "midi-cc", g_midi_cc, 3, 1, false,
                       "(midi-cc m control value [channel]) sends control change");

    s7_define_function(sc, "midi-program", g_midi_program, 2, 1, false,
                       "(midi-program m program [channel]) sends program change");

    s7_define_function(sc, "midi-all-notes-off", g_midi_all_notes_off, 1, 1, false,
                       "(midi-all-notes-off m [channel]) sends all-notes-off");

    s7_define_function(sc, "midi-sleep", g_midi_sleep, 1, 0, false,
                       "(midi-sleep ms) sleeps for given milliseconds");

    s7_define_function(sc, "set-tempo-c!", g_set_tempo_c, 1, 0, false,
                       "(set-tempo-c! bpm) sets C layer tempo for duration scaling");

    s7_define_function(sc, "get-tempo-c", g_get_tempo_c, 0, 0, false,
                       "(get-tempo-c) gets C layer tempo");

    /* Scale functions */
    s7_define_function(sc, "build-scale", g_build_scale, 2, 0, false,
                       "(build-scale root intervals) builds a scale from root and interval list");

    s7_define_function(sc, "scale-degree", g_scale_degree, 3, 0, false,
                       "(scale-degree root intervals n) returns the nth degree of the scale");

    s7_define_function(sc, "in-scale?", g_in_scale, 3, 0, false,
                       "(in-scale? pitch root intervals) returns #t if pitch is in scale");

    s7_define_function(sc, "quantize-to-scale", g_quantize_to_scale, 3, 0, false,
                       "(quantize-to-scale pitch root intervals) snaps pitch to nearest scale tone");

    s7_define_function(sc, "midi-pitch-bend", g_midi_pitch_bend, 2, 1, false,
                       "(midi-pitch-bend m cents [channel]) sends pitch bend for microtonal");

    s7_define_function(sc, "help", g_help, 0, 0, false,
                       "(help) displays available functions");

    /* Recording functions */
    s7_define_function(sc, "record-midi", g_record_midi, 0, 1, false,
                       "(record-midi [bpm]) starts MIDI recording");

    s7_define_function(sc, "record-stop", g_record_stop, 0, 0, false,
                       "(record-stop) stops MIDI recording");

    s7_define_function(sc, "save-midi", g_save_midi, 1, 0, false,
                       "(save-midi filename) saves recorded MIDI to file");

    s7_define_function(sc, "record-status", g_record_status, 0, 0, false,
                       "(record-status) returns recording status");

    /* MIDI File I/O functions */
    s7_define_function(sc, "write-mid", g_write_mid, 1, 1, false,
                       "(write-mid filename [ppqn]) writes captured events to standard MIDI file");

    s7_define_function(sc, "read-mid", g_read_mid, 1, 0, false,
                       "(read-mid filename) reads MIDI file and returns alist with events");

    /* Register scheduler functions (spawn, run, stop, voices, scheduler-status) */
    s7_scheduler_register(sc);

    /* Load Scheme prelude */
    s7_load_c_string(sc, SCHEME_PRELUDE_MODULE, strlen(SCHEME_PRELUDE_MODULE));
}

void s7_midi_cleanup(void) {
    s7_scheduler_cleanup();
    midi_cleanup_observer();
    global_sc = NULL;
}
