/*
 * MIDI module for pocketpy
 * Provides a Pythonic API for MIDI output using libremidi
 */

#define PK_IS_PUBLIC_INCLUDE
#include "pocketpy.h"
#include <libremidi/libremidi-c.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#define MAX_PORTS 64

// Global state
static libremidi_midi_observer_handle* midi_observer = NULL;
static libremidi_midi_out_port* out_ports[MAX_PORTS];
static int out_port_count = 0;

// MidiOut type
static py_Type tp_MidiOut;

// MidiOut userdata
typedef struct {
    libremidi_midi_out_handle* handle;
    int is_virtual;
} MidiOutData;

// Forward declarations
static void midi_cleanup_observer(void);
static int midi_init_observer(void);
static void MidiOut_dtor(void* ud);

// Callback for port enumeration
static void on_output_port_found(void* ctx, const libremidi_midi_out_port* port) {
    (void)ctx;
    if (out_port_count >= MAX_PORTS) return;
    libremidi_midi_out_port_clone(port, &out_ports[out_port_count]);
    out_port_count++;
}

// Initialize MIDI observer
static int midi_init_observer(void) {
    if (midi_observer != NULL) return 0;  // Already initialized

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

// Cleanup observer
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

// Parse pitch name to MIDI number (e.g., "C4" -> 60, "C#4" -> 61)
static int parse_pitch(const char* name) {
    if (name == NULL || name[0] == '\0') return -1;

    // Parse note letter
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

    // Check for accidentals
    if (*p == '#' || *p == 's') {
        note++;
        p++;
    } else if (*p == 'b') {
        note--;
        p++;
    }

    // Parse octave
    if (*p == '\0') return -1;
    int octave = atoi(p);
    if (octave < -1 || octave > 9) return -1;

    int midi_note = (octave + 1) * 12 + note;
    if (midi_note < 0 || midi_note > 127) return -1;

    return midi_note;
}

// ============================================================================
// Module functions
// ============================================================================

// midi.list_ports() -> list of (index, name) tuples
static bool midi_list_ports(int argc, py_StackRef argv) {
    PY_CHECK_ARGC(0);

    if (midi_init_observer() != 0) {
        return py_exception(tp_RuntimeError, "Failed to initialize MIDI observer");
    }

    // Re-enumerate ports
    for (int i = 0; i < out_port_count; i++) {
        libremidi_midi_out_port_free(out_ports[i]);
    }
    out_port_count = 0;
    libremidi_midi_observer_enumerate_output_ports(midi_observer, NULL, on_output_port_found);

    // Build list
    py_newlist(py_retval());
    for (int i = 0; i < out_port_count; i++) {
        const char* name = NULL;
        size_t len = 0;
        if (libremidi_midi_out_port_name(out_ports[i], &name, &len) == 0) {
            py_newtuple(py_pushtmp(), 2);
            py_Ref tuple = py_peek(0);
            py_newint(py_tuple_getitem(tuple, 0), i);
            py_newstr(py_tuple_getitem(tuple, 1), name);
            py_list_append(py_retval(), tuple);
            py_pop();
        }
    }
    return true;
}

// midi.open(port=None) -> MidiOut
// - open() or open("name") -> virtual port
// - open(index) -> hardware port by index
static bool midi_open(int argc, py_StackRef argv) {
    if (midi_init_observer() != 0) {
        return py_exception(tp_RuntimeError, "Failed to initialize MIDI observer");
    }

    libremidi_midi_configuration midi_conf;
    int ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        return py_exception(tp_RuntimeError, "Failed to init MIDI config");
    }

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        return py_exception(tp_RuntimeError, "Failed to init API config");
    }

    int is_virtual = 0;

    if (argc == 0) {
        // open() -> virtual port with default name
        midi_conf.virtual_port = true;
        midi_conf.port_name = "pktpyMIDI";
        is_virtual = 1;
    } else if (argc == 1) {
        if (py_isint(py_arg(0))) {
            // open(index) -> hardware port
            int port_idx = (int)py_toint(py_arg(0));
            if (port_idx < 0 || port_idx >= out_port_count) {
                return py_exception(tp_ValueError, "Invalid port index: %d (have %d ports)",
                                   port_idx, out_port_count);
            }
            midi_conf.out_port = out_ports[port_idx];
        } else if (py_isstr(py_arg(0))) {
            // open("name") -> virtual port with name
            midi_conf.virtual_port = true;
            midi_conf.port_name = py_tostr(py_arg(0));
            is_virtual = 1;
        } else {
            return py_exception(tp_TypeError, "open() argument must be int or str");
        }
    } else {
        return py_exception(tp_TypeError, "open() takes 0 or 1 arguments");
    }

    libremidi_midi_out_handle* handle;
    ret = libremidi_midi_out_new(&midi_conf, &api_conf, &handle);
    if (ret != 0) {
        if (is_virtual) {
            return py_exception(tp_RuntimeError, "Failed to create virtual MIDI output: %d", ret);
        } else {
            return py_exception(tp_RuntimeError, "Failed to open MIDI output: %d", ret);
        }
    }

    MidiOutData* data = py_newobject(py_retval(), tp_MidiOut, 0, sizeof(MidiOutData));
    data->handle = handle;
    data->is_virtual = is_virtual;
    return true;
}

// midi.note(name: str) -> int  (e.g., "C4" -> 60)
static bool midi_note(int argc, py_StackRef argv) {
    PY_CHECK_ARGC(1);
    PY_CHECK_ARG_TYPE(0, tp_str);

    const char* name = py_tostr(py_arg(0));
    int note = parse_pitch(name);

    if (note < 0) {
        return py_exception(tp_ValueError, "Invalid note name: %s", name);
    }

    py_newint(py_retval(), note);
    return true;
}

// ============================================================================
// MidiOut methods
// ============================================================================

static MidiOutData* MidiOut_get(py_Ref self) {
    if (!py_checktype(self, tp_MidiOut)) return NULL;
    return py_touserdata(self);
}

// MidiOut destructor
static void MidiOut_dtor(void* ud) {
    MidiOutData* data = (MidiOutData*)ud;
    if (data->handle != NULL) {
        libremidi_midi_out_free(data->handle);
        data->handle = NULL;
    }
}

// MidiOut.note_on(pitch, velocity=80, channel=1)
static bool MidiOut_note_on(int argc, py_StackRef argv) {
    if (argc < 2 || argc > 4) {
        return py_exception(tp_TypeError, "note_on() takes 1-3 arguments");
    }

    MidiOutData* data = MidiOut_get(py_arg(0));
    if (!data) return false;
    if (!data->handle) {
        return py_exception(tp_RuntimeError, "MIDI output is closed");
    }

    PY_CHECK_ARG_TYPE(1, tp_int);
    int pitch = (int)py_toint(py_arg(1));

    int velocity = 80;
    if (argc >= 3) {
        PY_CHECK_ARG_TYPE(2, tp_int);
        velocity = (int)py_toint(py_arg(2));
    }

    int channel = 1;
    if (argc >= 4) {
        PY_CHECK_ARG_TYPE(3, tp_int);
        channel = (int)py_toint(py_arg(3));
    }

    if (pitch < 0 || pitch > 127) {
        return py_exception(tp_ValueError, "pitch must be 0-127, got %d", pitch);
    }
    if (velocity < 0 || velocity > 127) {
        return py_exception(tp_ValueError, "velocity must be 0-127, got %d", velocity);
    }
    if (channel < 1 || channel > 16) {
        return py_exception(tp_ValueError, "channel must be 1-16, got %d", channel);
    }

    uint8_t msg[3] = {
        0x90 | ((channel - 1) & 0x0F),
        pitch & 0x7F,
        velocity & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);

    py_newnone(py_retval());
    return true;
}

// MidiOut.note_off(pitch, velocity=0, channel=1)
static bool MidiOut_note_off(int argc, py_StackRef argv) {
    if (argc < 2 || argc > 4) {
        return py_exception(tp_TypeError, "note_off() takes 1-3 arguments");
    }

    MidiOutData* data = MidiOut_get(py_arg(0));
    if (!data) return false;
    if (!data->handle) {
        return py_exception(tp_RuntimeError, "MIDI output is closed");
    }

    PY_CHECK_ARG_TYPE(1, tp_int);
    int pitch = (int)py_toint(py_arg(1));

    int velocity = 0;
    if (argc >= 3) {
        PY_CHECK_ARG_TYPE(2, tp_int);
        velocity = (int)py_toint(py_arg(2));
    }

    int channel = 1;
    if (argc >= 4) {
        PY_CHECK_ARG_TYPE(3, tp_int);
        channel = (int)py_toint(py_arg(3));
    }

    if (pitch < 0 || pitch > 127) {
        return py_exception(tp_ValueError, "pitch must be 0-127, got %d", pitch);
    }
    if (velocity < 0 || velocity > 127) {
        return py_exception(tp_ValueError, "velocity must be 0-127, got %d", velocity);
    }
    if (channel < 1 || channel > 16) {
        return py_exception(tp_ValueError, "channel must be 1-16, got %d", channel);
    }

    uint8_t msg[3] = {
        0x80 | ((channel - 1) & 0x0F),
        pitch & 0x7F,
        velocity & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);

    py_newnone(py_retval());
    return true;
}

// MidiOut.note(pitch, velocity=80, duration=500, channel=1)
// pitch can be int (MIDI number) or str (note name like "C4")
static bool MidiOut_note(int argc, py_StackRef argv) {
    if (argc < 2 || argc > 5) {
        return py_exception(tp_TypeError, "note() takes 1-4 arguments");
    }

    MidiOutData* data = MidiOut_get(py_arg(0));
    if (!data) return false;
    if (!data->handle) {
        return py_exception(tp_RuntimeError, "MIDI output is closed");
    }

    // Parse pitch - can be int or str
    int pitch;
    if (py_isint(py_arg(1))) {
        pitch = (int)py_toint(py_arg(1));
    } else if (py_isstr(py_arg(1))) {
        pitch = parse_pitch(py_tostr(py_arg(1)));
        if (pitch < 0) {
            return py_exception(tp_ValueError, "Invalid note name: %s", py_tostr(py_arg(1)));
        }
    } else {
        return py_exception(tp_TypeError, "pitch must be int or str");
    }

    int velocity = 80;
    if (argc >= 3) {
        PY_CHECK_ARG_TYPE(2, tp_int);
        velocity = (int)py_toint(py_arg(2));
    }

    int duration = 500;
    if (argc >= 4) {
        PY_CHECK_ARG_TYPE(3, tp_int);
        duration = (int)py_toint(py_arg(3));
    }

    int channel = 1;
    if (argc >= 5) {
        PY_CHECK_ARG_TYPE(4, tp_int);
        channel = (int)py_toint(py_arg(4));
    }

    if (pitch < 0 || pitch > 127) {
        return py_exception(tp_ValueError, "pitch must be 0-127, got %d", pitch);
    }
    if (velocity < 0 || velocity > 127) {
        return py_exception(tp_ValueError, "velocity must be 0-127, got %d", velocity);
    }
    if (duration < 0) {
        return py_exception(tp_ValueError, "duration must be >= 0, got %d", duration);
    }
    if (channel < 1 || channel > 16) {
        return py_exception(tp_ValueError, "channel must be 1-16, got %d", channel);
    }

    // Note on
    uint8_t msg[3] = {
        0x90 | ((channel - 1) & 0x0F),
        pitch & 0x7F,
        velocity & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);

    // Wait
    if (duration > 0) {
        usleep(duration * 1000);
    }

    // Note off
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[2] = 0;
    libremidi_midi_out_send_message(data->handle, msg, 3);

    py_newnone(py_retval());
    return true;
}

// Helper to parse a single pitch from a py_Ref (int or str)
static int parse_pitch_arg(py_Ref arg) {
    if (py_isint(arg)) {
        return (int)py_toint(arg);
    } else if (py_isstr(arg)) {
        return parse_pitch(py_tostr(arg));
    }
    return -1;
}

// MidiOut.chord(pitches, velocity=80, duration=500, channel=1)
// pitches is a list of int or str
static bool MidiOut_chord(int argc, py_StackRef argv) {
    if (argc < 2 || argc > 5) {
        return py_exception(tp_TypeError, "chord() takes 1-4 arguments");
    }

    MidiOutData* data = MidiOut_get(py_arg(0));
    if (!data) return false;
    if (!data->handle) {
        return py_exception(tp_RuntimeError, "MIDI output is closed");
    }

    // First arg must be a list or tuple
    py_Ref pitches_arg = py_arg(1);
    if (!py_istype(pitches_arg, tp_list) && !py_istype(pitches_arg, tp_tuple)) {
        return py_exception(tp_TypeError, "pitches must be a list or tuple");
    }

    int velocity = 80;
    if (argc >= 3) {
        PY_CHECK_ARG_TYPE(2, tp_int);
        velocity = (int)py_toint(py_arg(2));
    }

    int duration = 500;
    if (argc >= 4) {
        PY_CHECK_ARG_TYPE(3, tp_int);
        duration = (int)py_toint(py_arg(3));
    }

    int channel = 1;
    if (argc >= 5) {
        PY_CHECK_ARG_TYPE(4, tp_int);
        channel = (int)py_toint(py_arg(4));
    }

    if (velocity < 0 || velocity > 127) {
        return py_exception(tp_ValueError, "velocity must be 0-127, got %d", velocity);
    }
    if (duration < 0) {
        return py_exception(tp_ValueError, "duration must be >= 0, got %d", duration);
    }
    if (channel < 1 || channel > 16) {
        return py_exception(tp_ValueError, "channel must be 1-16, got %d", channel);
    }

    // Get pitches count
    int count;
    if (py_istype(pitches_arg, tp_list)) {
        count = py_list_len(pitches_arg);
    } else {
        count = py_tuple_len(pitches_arg);
    }

    if (count == 0) {
        py_newnone(py_retval());
        return true;
    }

    // Parse and validate all pitches, store them
    int pitches[128];  // Max reasonable chord size
    if (count > 128) count = 128;

    for (int i = 0; i < count; i++) {
        py_Ref item;
        if (py_istype(pitches_arg, tp_list)) {
            item = py_list_getitem(pitches_arg, i);
        } else {
            item = py_tuple_getitem(pitches_arg, i);
        }

        int pitch = parse_pitch_arg(item);
        if (pitch < 0 || pitch > 127) {
            return py_exception(tp_ValueError, "invalid pitch at index %d", i);
        }
        pitches[i] = pitch;
    }

    // Send all note-ons
    uint8_t msg[3];
    msg[0] = 0x90 | ((channel - 1) & 0x0F);
    msg[2] = velocity & 0x7F;
    for (int i = 0; i < count; i++) {
        msg[1] = pitches[i] & 0x7F;
        libremidi_midi_out_send_message(data->handle, msg, 3);
    }

    // Wait
    if (duration > 0) {
        usleep(duration * 1000);
    }

    // Send all note-offs
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[2] = 0;
    for (int i = 0; i < count; i++) {
        msg[1] = pitches[i] & 0x7F;
        libremidi_midi_out_send_message(data->handle, msg, 3);
    }

    py_newnone(py_retval());
    return true;
}

// MidiOut.cc(control, value, channel=1)
static bool MidiOut_cc(int argc, py_StackRef argv) {
    if (argc < 3 || argc > 4) {
        return py_exception(tp_TypeError, "cc() takes 2-3 arguments");
    }

    MidiOutData* data = MidiOut_get(py_arg(0));
    if (!data) return false;
    if (!data->handle) {
        return py_exception(tp_RuntimeError, "MIDI output is closed");
    }

    PY_CHECK_ARG_TYPE(1, tp_int);
    PY_CHECK_ARG_TYPE(2, tp_int);
    int control = (int)py_toint(py_arg(1));
    int value = (int)py_toint(py_arg(2));

    int channel = 1;
    if (argc >= 4) {
        PY_CHECK_ARG_TYPE(3, tp_int);
        channel = (int)py_toint(py_arg(3));
    }

    if (control < 0 || control > 127) {
        return py_exception(tp_ValueError, "control must be 0-127, got %d", control);
    }
    if (value < 0 || value > 127) {
        return py_exception(tp_ValueError, "value must be 0-127, got %d", value);
    }
    if (channel < 1 || channel > 16) {
        return py_exception(tp_ValueError, "channel must be 1-16, got %d", channel);
    }

    uint8_t msg[3] = {
        0xB0 | ((channel - 1) & 0x0F),
        control & 0x7F,
        value & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);

    py_newnone(py_retval());
    return true;
}

// MidiOut.program_change(program, channel=1)
static bool MidiOut_program_change(int argc, py_StackRef argv) {
    if (argc < 2 || argc > 3) {
        return py_exception(tp_TypeError, "program_change() takes 1-2 arguments");
    }

    MidiOutData* data = MidiOut_get(py_arg(0));
    if (!data) return false;
    if (!data->handle) {
        return py_exception(tp_RuntimeError, "MIDI output is closed");
    }

    PY_CHECK_ARG_TYPE(1, tp_int);
    int program = (int)py_toint(py_arg(1));

    int channel = 1;
    if (argc >= 3) {
        PY_CHECK_ARG_TYPE(2, tp_int);
        channel = (int)py_toint(py_arg(2));
    }

    if (program < 0 || program > 127) {
        return py_exception(tp_ValueError, "program must be 0-127, got %d", program);
    }
    if (channel < 1 || channel > 16) {
        return py_exception(tp_ValueError, "channel must be 1-16, got %d", channel);
    }

    uint8_t msg[2] = {
        0xC0 | ((channel - 1) & 0x0F),
        program & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 2);

    py_newnone(py_retval());
    return true;
}

// MidiOut.all_notes_off(channel=None)  - if channel is None, all channels
static bool MidiOut_all_notes_off(int argc, py_StackRef argv) {
    if (argc < 1 || argc > 2) {
        return py_exception(tp_TypeError, "all_notes_off() takes 0-1 arguments");
    }

    MidiOutData* data = MidiOut_get(py_arg(0));
    if (!data) return false;
    if (!data->handle) {
        return py_exception(tp_RuntimeError, "MIDI output is closed");
    }

    int start_ch = 1, end_ch = 16;
    if (argc == 2 && !py_istype(py_arg(1), tp_NoneType)) {
        PY_CHECK_ARG_TYPE(1, tp_int);
        int ch = (int)py_toint(py_arg(1));
        if (ch < 1 || ch > 16) {
            return py_exception(tp_ValueError, "channel must be 1-16, got %d", ch);
        }
        start_ch = end_ch = ch;
    }

    for (int ch = start_ch; ch <= end_ch; ch++) {
        uint8_t msg[3] = {
            0xB0 | ((ch - 1) & 0x0F),
            123,  // All Notes Off CC
            0
        };
        libremidi_midi_out_send_message(data->handle, msg, 3);
    }

    py_newnone(py_retval());
    return true;
}

// MidiOut.close()
static bool MidiOut_close(int argc, py_StackRef argv) {
    PY_CHECK_ARGC(1);

    MidiOutData* data = MidiOut_get(py_arg(0));
    if (!data) return false;

    if (data->handle != NULL) {
        libremidi_midi_out_free(data->handle);
        data->handle = NULL;
    }

    py_newnone(py_retval());
    return true;
}

// MidiOut.is_open property getter
static bool MidiOut_is_open_getter(int argc, py_StackRef argv) {
    PY_CHECK_ARGC(1);

    MidiOutData* data = MidiOut_get(py_arg(0));
    if (!data) return false;

    py_newbool(py_retval(), data->handle != NULL);
    return true;
}

// MidiOut.__enter__() - for context manager
static bool MidiOut__enter__(int argc, py_StackRef argv) {
    PY_CHECK_ARGC(1);
    py_assign(py_retval(), py_arg(0));
    return true;
}

// MidiOut.__exit__(*args) - for context manager
static bool MidiOut__exit__(int argc, py_StackRef argv) {
    // Close the MIDI output
    MidiOutData* data = MidiOut_get(py_arg(0));
    if (!data) return false;

    if (data->handle != NULL) {
        // Send all notes off before closing
        for (int ch = 0; ch < 16; ch++) {
            uint8_t msg[3] = { 0xB0 | ch, 123, 0 };
            libremidi_midi_out_send_message(data->handle, msg, 3);
        }
        libremidi_midi_out_free(data->handle);
        data->handle = NULL;
    }

    py_newnone(py_retval());
    return true;
}

// MidiOut.__repr__()
static bool MidiOut__repr__(int argc, py_StackRef argv) {
    PY_CHECK_ARGC(1);

    MidiOutData* data = MidiOut_get(py_arg(0));
    if (!data) return false;

    if (data->handle) {
        if (data->is_virtual) {
            py_newstr(py_retval(), "<MidiOut virtual>");
        } else {
            py_newstr(py_retval(), "<MidiOut connected>");
        }
    } else {
        py_newstr(py_retval(), "<MidiOut closed>");
    }
    return true;
}

// ============================================================================
// Module initialization
// ============================================================================

void pk_midi_module_init(void) {
    // Create the midi module
    py_GlobalRef mod = py_newmodule("midi");

    // Bind module functions
    py_bindfunc(mod, "list_ports", midi_list_ports);
    py_bindfunc(mod, "open", midi_open);
    py_bindfunc(mod, "note", midi_note);

    // Create MidiOut type
    tp_MidiOut = py_newtype("MidiOut", tp_object, mod, MidiOut_dtor);

    // Bind MidiOut methods
    py_bindmethod(tp_MidiOut, "note", MidiOut_note);
    py_bindmethod(tp_MidiOut, "chord", MidiOut_chord);
    py_bindmethod(tp_MidiOut, "note_on", MidiOut_note_on);
    py_bindmethod(tp_MidiOut, "note_off", MidiOut_note_off);
    py_bindmethod(tp_MidiOut, "cc", MidiOut_cc);
    py_bindmethod(tp_MidiOut, "program_change", MidiOut_program_change);
    py_bindmethod(tp_MidiOut, "all_notes_off", MidiOut_all_notes_off);
    py_bindmethod(tp_MidiOut, "close", MidiOut_close);

    // Bind properties
    py_bindproperty(tp_MidiOut, "is_open", MidiOut_is_open_getter, NULL);

    // Bind magic methods for context manager
    py_bindmagic(tp_MidiOut, py_name("__enter__"), MidiOut__enter__);
    py_bindmagic(tp_MidiOut, py_name("__exit__"), MidiOut__exit__);
    py_bindmagic(tp_MidiOut, py_name("__repr__"), MidiOut__repr__);

    // Execute Python prelude to add helper functions and constants
    static const char* prelude =
        "import midi\n"
        "import time as _time\n"
        "\n"
        "# ============================================================================\n"
        "# Dynamics (velocity values)\n"
        "# ============================================================================\n"
        "midi.ppp = 16\n"
        "midi.pp = 33\n"
        "midi.p = 49\n"
        "midi.mp = 64\n"
        "midi.mf = 80\n"
        "midi.f = 96\n"
        "midi.ff = 112\n"
        "midi.fff = 127\n"
        "\n"
        "# ============================================================================\n"
        "# Durations (milliseconds at 120 BPM)\n"
        "# ============================================================================\n"
        "midi.whole = 2000\n"
        "midi.half = 1000\n"
        "midi.quarter = 500\n"
        "midi.eighth = 250\n"
        "midi.sixteenth = 125\n"
        "\n"
        "def _dotted(duration):\n"
        "    '''Return dotted duration (1.5x)'''\n"
        "    return int(duration * 1.5)\n"
        "midi.dotted = _dotted\n"
        "\n"
        "# ============================================================================\n"
        "# Tempo\n"
        "# ============================================================================\n"
        "midi._bpm = 120\n"
        "\n"
        "def _set_tempo(bpm):\n"
        "    '''Set tempo in BPM and update duration constants'''\n"
        "    midi._bpm = bpm\n"
        "    beat_ms = 60000 // bpm\n"
        "    midi.quarter = beat_ms\n"
        "    midi.half = beat_ms * 2\n"
        "    midi.whole = beat_ms * 4\n"
        "    midi.eighth = beat_ms // 2\n"
        "    midi.sixteenth = beat_ms // 4\n"
        "midi.set_tempo = _set_tempo\n"
        "\n"
        "def _get_tempo():\n"
        "    '''Get current tempo in BPM'''\n"
        "    return midi._bpm\n"
        "midi.get_tempo = _get_tempo\n"
        "\n"
        "def _bpm_to_ms(tempo):\n"
        "    '''Return quarter note duration in ms for given tempo'''\n"
        "    return 60000 // tempo\n"
        "midi.bpm = _bpm_to_ms\n"
        "\n"
        "# ============================================================================\n"
        "# Timing helpers\n"
        "# ============================================================================\n"
        "def _sleep(ms):\n"
        "    '''Sleep for given milliseconds'''\n"
        "    _time.sleep(ms / 1000.0)\n"
        "midi.sleep = _sleep\n"
        "\n"
        "def _rest(duration=None):\n"
        "    '''Rest (silence) for given duration, default is quarter note'''\n"
        "    if duration is None:\n"
        "        duration = midi.quarter\n"
        "    _time.sleep(duration / 1000.0)\n"
        "midi.rest = _rest\n"
        "\n"
        "# ============================================================================\n"
        "# Chord builders (return list of MIDI pitches)\n"
        "# ============================================================================\n"
        "def _major(root):\n"
        "    '''Build major triad from root'''\n"
        "    if isinstance(root, str):\n"
        "        root = midi.note(root)\n"
        "    return [root, root + 4, root + 7]\n"
        "midi.major = _major\n"
        "\n"
        "def _minor(root):\n"
        "    '''Build minor triad from root'''\n"
        "    if isinstance(root, str):\n"
        "        root = midi.note(root)\n"
        "    return [root, root + 3, root + 7]\n"
        "midi.minor = _minor\n"
        "\n"
        "def _dim(root):\n"
        "    '''Build diminished triad from root'''\n"
        "    if isinstance(root, str):\n"
        "        root = midi.note(root)\n"
        "    return [root, root + 3, root + 6]\n"
        "midi.dim = _dim\n"
        "\n"
        "def _aug(root):\n"
        "    '''Build augmented triad from root'''\n"
        "    if isinstance(root, str):\n"
        "        root = midi.note(root)\n"
        "    return [root, root + 4, root + 8]\n"
        "midi.aug = _aug\n"
        "\n"
        "def _dom7(root):\n"
        "    '''Build dominant 7th chord from root'''\n"
        "    if isinstance(root, str):\n"
        "        root = midi.note(root)\n"
        "    return [root, root + 4, root + 7, root + 10]\n"
        "midi.dom7 = _dom7\n"
        "\n"
        "def _maj7(root):\n"
        "    '''Build major 7th chord from root'''\n"
        "    if isinstance(root, str):\n"
        "        root = midi.note(root)\n"
        "    return [root, root + 4, root + 7, root + 11]\n"
        "midi.maj7 = _maj7\n"
        "\n"
        "def _min7(root):\n"
        "    '''Build minor 7th chord from root'''\n"
        "    if isinstance(root, str):\n"
        "        root = midi.note(root)\n"
        "    return [root, root + 3, root + 7, root + 10]\n"
        "midi.min7 = _min7\n"
        "\n"
        "# ============================================================================\n"
        "# Pitch helpers\n"
        "# ============================================================================\n"
        "def _transpose(pitch, semitones):\n"
        "    '''Transpose pitch by semitones'''\n"
        "    if isinstance(pitch, str):\n"
        "        pitch = midi.note(pitch)\n"
        "    return pitch + semitones\n"
        "midi.transpose = _transpose\n"
        "\n"
        "def _octave_up(pitch):\n"
        "    '''Transpose pitch up one octave'''\n"
        "    return _transpose(pitch, 12)\n"
        "midi.octave_up = _octave_up\n"
        "\n"
        "def _octave_down(pitch):\n"
        "    '''Transpose pitch down one octave'''\n"
        "    return _transpose(pitch, -12)\n"
        "midi.octave_down = _octave_down\n"
        "\n"
        "# ============================================================================\n"
        "# Pitch constants (all octaves)\n"
        "# ============================================================================\n"
        "for _oct in range(9):\n"
        "    setattr(midi, f'c{_oct}', 12 + _oct * 12)\n"
        "    setattr(midi, f'cs{_oct}', 13 + _oct * 12)\n"
        "    setattr(midi, f'db{_oct}', 13 + _oct * 12)\n"
        "    setattr(midi, f'd{_oct}', 14 + _oct * 12)\n"
        "    setattr(midi, f'ds{_oct}', 15 + _oct * 12)\n"
        "    setattr(midi, f'eb{_oct}', 15 + _oct * 12)\n"
        "    setattr(midi, f'e{_oct}', 16 + _oct * 12)\n"
        "    setattr(midi, f'f{_oct}', 17 + _oct * 12)\n"
        "    setattr(midi, f'fs{_oct}', 18 + _oct * 12)\n"
        "    setattr(midi, f'gb{_oct}', 18 + _oct * 12)\n"
        "    setattr(midi, f'g{_oct}', 19 + _oct * 12)\n"
        "    setattr(midi, f'gs{_oct}', 20 + _oct * 12)\n"
        "    setattr(midi, f'ab{_oct}', 20 + _oct * 12)\n"
        "    setattr(midi, f'a{_oct}', 21 + _oct * 12)\n"
        "    setattr(midi, f'as{_oct}', 22 + _oct * 12)\n"
        "    setattr(midi, f'bb{_oct}', 22 + _oct * 12)\n"
        "    setattr(midi, f'b{_oct}', 23 + _oct * 12)\n"
        "del _oct\n"
        "\n"
        "# ============================================================================\n"
        "# CC helpers (common control changes)\n"
        "# ============================================================================\n"
        "midi.CC_MODULATION = 1\n"
        "midi.CC_BREATH = 2\n"
        "midi.CC_VOLUME = 7\n"
        "midi.CC_PAN = 10\n"
        "midi.CC_EXPRESSION = 11\n"
        "midi.CC_SUSTAIN = 64\n"
        "midi.CC_REVERB = 91\n"
        "midi.CC_CHORUS = 93\n"
        "\n"
        "# ============================================================================\n"
        "# MidiOut extensions\n"
        "# ============================================================================\n"
        "def _arpeggio(self, pitches, velocity=80, note_duration=None, spacing=None, channel=1):\n"
        "    '''Play notes sequentially (arpeggiated)'''\n"
        "    if note_duration is None:\n"
        "        note_duration = midi.eighth\n"
        "    if spacing is None:\n"
        "        spacing = note_duration\n"
        "    for p in pitches:\n"
        "        self.note(p, velocity, note_duration, channel)\n"
        "        if spacing > note_duration:\n"
        "            midi.sleep(spacing - note_duration)\n"
        "midi.MidiOut.arpeggio = _arpeggio\n"
        "\n"
        "def _modulation(self, value, channel=1):\n"
        "    '''Set modulation wheel (CC 1)'''\n"
        "    self.cc(1, value, channel)\n"
        "midi.MidiOut.modulation = _modulation\n"
        "\n"
        "def _volume(self, value, channel=1):\n"
        "    '''Set channel volume (CC 7)'''\n"
        "    self.cc(7, value, channel)\n"
        "midi.MidiOut.volume = _volume\n"
        "\n"
        "def _pan(self, value, channel=1):\n"
        "    '''Set pan position (CC 10): 0=left, 64=center, 127=right'''\n"
        "    self.cc(10, value, channel)\n"
        "midi.MidiOut.pan = _pan\n"
        "\n"
        "def _sustain(self, on=True, channel=1):\n"
        "    '''Set sustain pedal (CC 64)'''\n"
        "    self.cc(64, 127 if on else 0, channel)\n"
        "midi.MidiOut.sustain = _sustain\n"
        ;

    py_Ref midi_mod = py_getmodule("midi");
    if (!py_exec(prelude, "<midi_prelude>", EXEC_MODE, midi_mod)) {
        py_printexc();
    }
}

void pk_midi_module_cleanup(void) {
    midi_cleanup_observer();
}
