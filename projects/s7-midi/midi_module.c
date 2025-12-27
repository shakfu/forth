/*
 * MIDI module for s7 scheme
 * Provides Scheme bindings for MIDI output using libremidi
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include "s7.h"
#include <libremidi/libremidi-c.h>

#define MAX_PORTS 64

/* Global state */
static libremidi_midi_observer_handle* midi_observer = NULL;
static libremidi_midi_out_port* out_ports[MAX_PORTS];
static int out_port_count = 0;
static s7_scheme *global_sc = NULL;

/* MidiOut type tag */
static s7_int midi_out_tag = 0;

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
 * Pitch parsing
 * ============================================================================ */

static int parse_pitch(const char* name) {
    if (name == NULL || name[0] == '\0') return -1;

    int note;
    char c = toupper(name[0]);
    switch (c) {
        case 'C': note = 0; break;
        case 'D': note = 2; break;
        case 'E': note = 4; break;
        case 'F': note = 5; break;
        case 'G': note = 7; break;
        case 'A': note = 9; break;
        case 'B': note = 11; break;
        default: return -1;
    }

    const char* p = name + 1;

    if (*p == '#' || *p == 's') {
        note++;
        p++;
    } else if (*p == 'b') {
        note--;
        p++;
    }

    if (*p == '\0') return -1;
    int octave = atoi(p);
    if (octave < -1 || octave > 9) return -1;

    int midi_note = (octave + 1) * 12 + note;
    if (midi_note < 0 || midi_note > 127) return -1;

    return midi_note;
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

    /* Wait */
    if (duration > 0) {
        usleep(duration * 1000);
    }

    /* Note off */
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[2] = 0;
    libremidi_midi_out_send_message(data->handle, msg, 3);

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
    }

    /* Wait */
    if (duration > 0) {
        usleep(duration * 1000);
    }

    /* Send note-offs */
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[2] = 0;
    for (int i = 0; i < count; i++) {
        msg[1] = pitches[i] & 0x7F;
        libremidi_midi_out_send_message(data->handle, msg, 3);
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
    if (ms > 0) {
        usleep(ms * 1000);
    }
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
        "Example:\n"
        "  (define m (midi-open))\n"
        "  (midi-note m c4 mf quarter)\n"
        "  (midi-chord m (major c4) mf half)\n"
        "  (midi-close m)\n";

    printf("%s", help_text);
    return s7_unspecified(sc);
}

/* ============================================================================
 * Scheme prelude - constants and helper functions
 * ============================================================================ */

static const char *scheme_prelude =
    ";; Dynamics (velocity values)\n"
    "(define ppp 16)\n"
    "(define pp 33)\n"
    "(define p 49)\n"
    "(define mp 64)\n"
    "(define mf 80)\n"
    "(define f 96)\n"
    "(define ff 112)\n"
    "(define fff 127)\n"
    "\n"
    ";; Durations (milliseconds at 120 BPM)\n"
    "(define whole 2000)\n"
    "(define half 1000)\n"
    "(define quarter 500)\n"
    "(define eighth 250)\n"
    "(define sixteenth 125)\n"
    "\n"
    "(define (dotted dur) (floor (* dur 1.5)))\n"
    "\n"
    ";; Tempo\n"
    "(define *bpm* 120)\n"
    "\n"
    "(define (set-tempo! bpm)\n"
    "  (set! *bpm* bpm)\n"
    "  (let ((beat-ms (floor (/ 60000 bpm))))\n"
    "    (set! quarter beat-ms)\n"
    "    (set! half (* beat-ms 2))\n"
    "    (set! whole (* beat-ms 4))\n"
    "    (set! eighth (floor (/ beat-ms 2)))\n"
    "    (set! sixteenth (floor (/ beat-ms 4)))))\n"
    "\n"
    "(define (get-tempo) *bpm*)\n"
    "\n"
    "(define (bpm tempo) (floor (/ 60000 tempo)))\n"
    "\n"
    ";; Pitch constants\n"
    "(define c0 12) (define cs0 13) (define db0 13) (define d0 14)\n"
    "(define ds0 15) (define eb0 15) (define e0 16) (define f0 17)\n"
    "(define fs0 18) (define gb0 18) (define g0 19) (define gs0 20)\n"
    "(define ab0 20) (define a0 21) (define as0 22) (define bb0 22) (define b0 23)\n"
    "\n"
    "(define c1 24) (define cs1 25) (define db1 25) (define d1 26)\n"
    "(define ds1 27) (define eb1 27) (define e1 28) (define f1 29)\n"
    "(define fs1 30) (define gb1 30) (define g1 31) (define gs1 32)\n"
    "(define ab1 32) (define a1 33) (define as1 34) (define bb1 34) (define b1 35)\n"
    "\n"
    "(define c2 36) (define cs2 37) (define db2 37) (define d2 38)\n"
    "(define ds2 39) (define eb2 39) (define e2 40) (define f2 41)\n"
    "(define fs2 42) (define gb2 42) (define g2 43) (define gs2 44)\n"
    "(define ab2 44) (define a2 45) (define as2 46) (define bb2 46) (define b2 47)\n"
    "\n"
    "(define c3 48) (define cs3 49) (define db3 49) (define d3 50)\n"
    "(define ds3 51) (define eb3 51) (define e3 52) (define f3 53)\n"
    "(define fs3 54) (define gb3 54) (define g3 55) (define gs3 56)\n"
    "(define ab3 56) (define a3 57) (define as3 58) (define bb3 58) (define b3 59)\n"
    "\n"
    "(define c4 60) (define cs4 61) (define db4 61) (define d4 62)\n"
    "(define ds4 63) (define eb4 63) (define e4 64) (define f4 65)\n"
    "(define fs4 66) (define gb4 66) (define g4 67) (define gs4 68)\n"
    "(define ab4 68) (define a4 69) (define as4 70) (define bb4 70) (define b4 71)\n"
    "\n"
    "(define c5 72) (define cs5 73) (define db5 73) (define d5 74)\n"
    "(define ds5 75) (define eb5 75) (define e5 76) (define f5 77)\n"
    "(define fs5 78) (define gb5 78) (define g5 79) (define gs5 80)\n"
    "(define ab5 80) (define a5 81) (define as5 82) (define bb5 82) (define b5 83)\n"
    "\n"
    "(define c6 84) (define cs6 85) (define db6 85) (define d6 86)\n"
    "(define ds6 87) (define eb6 87) (define e6 88) (define f6 89)\n"
    "(define fs6 90) (define gb6 90) (define g6 91) (define gs6 92)\n"
    "(define ab6 92) (define a6 93) (define as6 94) (define bb6 94) (define b6 95)\n"
    "\n"
    "(define c7 96) (define cs7 97) (define db7 97) (define d7 98)\n"
    "(define ds7 99) (define eb7 99) (define e7 100) (define f7 101)\n"
    "(define fs7 102) (define gb7 102) (define g7 103) (define gs7 104)\n"
    "(define ab7 104) (define a7 105) (define as7 106) (define bb7 106) (define b7 107)\n"
    "\n"
    "(define c8 108) (define cs8 109) (define db8 109) (define d8 110)\n"
    "(define ds8 111) (define eb8 111) (define e8 112) (define f8 113)\n"
    "(define fs8 114) (define gb8 114) (define g8 115) (define gs8 116)\n"
    "(define ab8 116) (define a8 117) (define as8 118) (define bb8 118) (define b8 119)\n"
    "\n"
    ";; Chord builders\n"
    "(define (major root)\n"
    "  (let ((r (if (number? root) root (note root))))\n"
    "    (list r (+ r 4) (+ r 7))))\n"
    "\n"
    "(define (minor root)\n"
    "  (let ((r (if (number? root) root (note root))))\n"
    "    (list r (+ r 3) (+ r 7))))\n"
    "\n"
    "(define (dim root)\n"
    "  (let ((r (if (number? root) root (note root))))\n"
    "    (list r (+ r 3) (+ r 6))))\n"
    "\n"
    "(define (aug root)\n"
    "  (let ((r (if (number? root) root (note root))))\n"
    "    (list r (+ r 4) (+ r 8))))\n"
    "\n"
    "(define (dom7 root)\n"
    "  (let ((r (if (number? root) root (note root))))\n"
    "    (list r (+ r 4) (+ r 7) (+ r 10))))\n"
    "\n"
    "(define (maj7 root)\n"
    "  (let ((r (if (number? root) root (note root))))\n"
    "    (list r (+ r 4) (+ r 7) (+ r 11))))\n"
    "\n"
    "(define (min7 root)\n"
    "  (let ((r (if (number? root) root (note root))))\n"
    "    (list r (+ r 3) (+ r 7) (+ r 10))))\n"
    "\n"
    ";; Transpose helpers\n"
    "(define (transpose pitch semitones)\n"
    "  (let ((p (if (number? pitch) pitch (note pitch))))\n"
    "    (+ p semitones)))\n"
    "\n"
    "(define (octave-up pitch) (transpose pitch 12))\n"
    "(define (octave-down pitch) (transpose pitch -12))\n"
    "\n"
    ";; Arpeggio helper\n"
    "(define (midi-arpeggio m pitches . args)\n"
    "  (let ((vel (if (>= (length args) 1) (car args) mf))\n"
    "        (dur (if (>= (length args) 2) (cadr args) eighth))\n"
    "        (ch (if (>= (length args) 3) (caddr args) 1)))\n"
    "    (for-each (lambda (p) (midi-note m p vel dur ch)) pitches)))\n"
    "\n"
    ";; Rest helper\n"
    "(define (rest . args)\n"
    "  (let ((dur (if (null? args) quarter (car args))))\n"
    "    (midi-sleep dur)))\n"
    "\n"
    ";; ============================================================================\n"
    ";; REPL convenience functions (use global *midi* output)\n"
    ";; ============================================================================\n"
    "(define *midi* #f)\n"
    "\n"
    "(define (open . args)\n"
    "  \"Open MIDI port and set as default. (open) or (open name) or (open index)\"\n"
    "  (if *midi* (midi-close *midi*))\n"
    "  (set! *midi* (if (null? args) (midi-open) (midi-open (car args))))\n"
    "  *midi*)\n"
    "\n"
    "(define (close)\n"
    "  \"Close default MIDI port\"\n"
    "  (if *midi* (begin (midi-close *midi*) (set! *midi* #f))))\n"
    "\n"
    "(define (n pitch . args)\n"
    "  \"Play note on default port: (n pitch [vel] [dur] [ch])\"\n"
    "  (if (not *midi*) (error 'no-midi-port \"No MIDI port open. Use (open) first.\"))\n"
    "  (let ((vel (if (>= (length args) 1) (car args) mf))\n"
    "        (dur (if (>= (length args) 2) (cadr args) quarter))\n"
    "        (ch (if (>= (length args) 3) (caddr args) 1)))\n"
    "    (midi-note *midi* pitch vel dur ch)))\n"
    "\n"
    "(define (ch pitches . args)\n"
    "  \"Play chord on default port: (ch pitches [vel] [dur] [channel])\"\n"
    "  (if (not *midi*) (error 'no-midi-port \"No MIDI port open. Use (open) first.\"))\n"
    "  (let ((vel (if (>= (length args) 1) (car args) mf))\n"
    "        (dur (if (>= (length args) 2) (cadr args) quarter))\n"
    "        (channel (if (>= (length args) 3) (caddr args) 1)))\n"
    "    (midi-chord *midi* pitches vel dur channel)))\n"
    "\n"
    "(define (arp pitches . args)\n"
    "  \"Arpeggiate on default port: (arp pitches [vel] [dur] [ch])\"\n"
    "  (if (not *midi*) (error 'no-midi-port \"No MIDI port open. Use (open) first.\"))\n"
    "  (let ((vel (if (>= (length args) 1) (car args) mf))\n"
    "        (dur (if (>= (length args) 2) (cadr args) eighth))\n"
    "        (channel (if (>= (length args) 3) (caddr args) 1)))\n"
    "    (midi-arpeggio *midi* pitches vel dur channel)))\n"
    ;

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

    s7_define_function(sc, "help", g_help, 0, 0, false,
                       "(help) displays available functions");

    /* Load Scheme prelude */
    s7_load_c_string(sc, scheme_prelude, strlen(scheme_prelude));
}

void s7_midi_cleanup(void) {
    midi_cleanup_observer();
    global_sc = NULL;
}
