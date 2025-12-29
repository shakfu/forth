/*
 * MIDI module for pocketpy
 * Provides a Pythonic API for MIDI output using libremidi
 */

#define PK_IS_PUBLIC_INCLUDE
#include "pocketpy.h"
#include <libremidi/libremidi-c.h>

#include "py_prelude.h"
#include "music_theory.h"
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

// Parse pitch name to MIDI number - uses common music_theory library
static int parse_pitch(const char* name) {
    return music_parse_pitch(name);
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
// Scale functions - using common music_theory library
// ============================================================================

// Helper to read intervals from a Python list/tuple
static int get_intervals_from_py(py_Ref list_arg, int* intervals, int max_size) {
    int count = 0;
    int len;

    if (py_istype(list_arg, tp_list)) {
        len = py_list_len(list_arg);
        for (int i = 0; i < len && count < max_size; i++) {
            py_Ref item = py_list_getitem(list_arg, i);
            if (py_isint(item)) {
                intervals[count++] = (int)py_toint(item);
            }
        }
    } else if (py_istype(list_arg, tp_tuple)) {
        len = py_tuple_len(list_arg);
        for (int i = 0; i < len && count < max_size; i++) {
            py_Ref item = py_tuple_getitem(list_arg, i);
            if (py_isint(item)) {
                intervals[count++] = (int)py_toint(item);
            }
        }
    }
    return count;
}

// midi.build_scale(root, intervals) -> list of pitches
static bool midi_build_scale(int argc, py_StackRef argv) {
    PY_CHECK_ARGC(2);
    PY_CHECK_ARG_TYPE(0, tp_int);

    int root = (int)py_toint(py_arg(0));
    if (root < 0 || root > 127) {
        return py_exception(tp_ValueError, "root must be 0-127, got %d", root);
    }

    py_Ref intervals_arg = py_arg(1);
    if (!py_istype(intervals_arg, tp_list) && !py_istype(intervals_arg, tp_tuple)) {
        return py_exception(tp_TypeError, "intervals must be a list or tuple");
    }

    int intervals[16];
    int num_intervals = get_intervals_from_py(intervals_arg, intervals, 16);
    if (num_intervals == 0) {
        py_newlist(py_retval());
        return true;
    }

    int pitches[16];
    int count = music_build_scale(root, intervals, num_intervals, pitches);

    py_newlistn(py_retval(), count);
    for (int i = 0; i < count; i++) {
        py_newint(py_list_getitem(py_retval(), i), pitches[i]);
    }
    return true;
}

// midi.scale_degree(root, intervals, degree) -> pitch
static bool midi_scale_degree(int argc, py_StackRef argv) {
    PY_CHECK_ARGC(3);
    PY_CHECK_ARG_TYPE(0, tp_int);
    PY_CHECK_ARG_TYPE(2, tp_int);

    int root = (int)py_toint(py_arg(0));
    if (root < 0 || root > 127) {
        return py_exception(tp_ValueError, "root must be 0-127, got %d", root);
    }

    py_Ref intervals_arg = py_arg(1);
    if (!py_istype(intervals_arg, tp_list) && !py_istype(intervals_arg, tp_tuple)) {
        return py_exception(tp_TypeError, "intervals must be a list or tuple");
    }

    int degree = (int)py_toint(py_arg(2));

    int intervals[16];
    int num_intervals = get_intervals_from_py(intervals_arg, intervals, 16);
    if (num_intervals == 0) {
        return py_exception(tp_ValueError, "intervals list is empty");
    }

    int pitch = music_scale_degree(root, intervals, num_intervals, degree);
    if (pitch < 0) {
        return py_exception(tp_ValueError, "scale degree out of range");
    }

    py_newint(py_retval(), pitch);
    return true;
}

// midi.in_scale(pitch, root, intervals) -> bool
static bool midi_in_scale(int argc, py_StackRef argv) {
    PY_CHECK_ARGC(3);
    PY_CHECK_ARG_TYPE(0, tp_int);
    PY_CHECK_ARG_TYPE(1, tp_int);

    int pitch = (int)py_toint(py_arg(0));
    int root = (int)py_toint(py_arg(1));

    py_Ref intervals_arg = py_arg(2);
    if (!py_istype(intervals_arg, tp_list) && !py_istype(intervals_arg, tp_tuple)) {
        return py_exception(tp_TypeError, "intervals must be a list or tuple");
    }

    int intervals[16];
    int num_intervals = get_intervals_from_py(intervals_arg, intervals, 16);
    if (num_intervals == 0) {
        py_newbool(py_retval(), false);
        return true;
    }

    int result = music_in_scale(pitch, root, intervals, num_intervals);
    py_newbool(py_retval(), result);
    return true;
}

// midi.quantize_to_scale(pitch, root, intervals) -> pitch
static bool midi_quantize_to_scale(int argc, py_StackRef argv) {
    PY_CHECK_ARGC(3);
    PY_CHECK_ARG_TYPE(0, tp_int);
    PY_CHECK_ARG_TYPE(1, tp_int);

    int pitch = (int)py_toint(py_arg(0));
    int root = (int)py_toint(py_arg(1));

    py_Ref intervals_arg = py_arg(2);
    if (!py_istype(intervals_arg, tp_list) && !py_istype(intervals_arg, tp_tuple)) {
        return py_exception(tp_TypeError, "intervals must be a list or tuple");
    }

    int intervals[16];
    int num_intervals = get_intervals_from_py(intervals_arg, intervals, 16);
    if (num_intervals == 0) {
        py_newint(py_retval(), pitch);
        return true;
    }

    int result = music_quantize_to_scale(pitch, root, intervals, num_intervals);
    py_newint(py_retval(), result);
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

// MidiOut.pitch_bend(cents, channel=1) - for microtonal playback
static bool MidiOut_pitch_bend(int argc, py_StackRef argv) {
    if (argc < 2 || argc > 3) {
        return py_exception(tp_TypeError, "pitch_bend() takes 1-2 arguments");
    }

    MidiOutData* data = MidiOut_get(py_arg(0));
    if (!data) return false;
    if (!data->handle) {
        return py_exception(tp_RuntimeError, "MIDI output is closed");
    }

    PY_CHECK_ARG_TYPE(1, tp_int);
    int cents = (int)py_toint(py_arg(1));

    int channel = 1;
    if (argc >= 3) {
        PY_CHECK_ARG_TYPE(2, tp_int);
        channel = (int)py_toint(py_arg(2));
    }

    if (channel < 1 || channel > 16) {
        return py_exception(tp_ValueError, "channel must be 1-16, got %d", channel);
    }

    int bend = music_cents_to_bend(cents);
    uint8_t msg[3] = {
        0xE0 | ((channel - 1) & 0x0F),
        bend & 0x7F,
        (bend >> 7) & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);

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

    // Scale functions
    py_bindfunc(mod, "build_scale", midi_build_scale);
    py_bindfunc(mod, "scale_degree", midi_scale_degree);
    py_bindfunc(mod, "in_scale", midi_in_scale);
    py_bindfunc(mod, "quantize_to_scale", midi_quantize_to_scale);

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
    py_bindmethod(tp_MidiOut, "pitch_bend", MidiOut_pitch_bend);
    py_bindmethod(tp_MidiOut, "close", MidiOut_close);

    // Bind properties
    py_bindproperty(tp_MidiOut, "is_open", MidiOut_is_open_getter, NULL);

    // Bind magic methods for context manager
    py_bindmagic(tp_MidiOut, py_name("__enter__"), MidiOut__enter__);
    py_bindmagic(tp_MidiOut, py_name("__exit__"), MidiOut__exit__);
    py_bindmagic(tp_MidiOut, py_name("__repr__"), MidiOut__repr__);

    // Execute Python prelude to add helper functions and constants
    py_Ref midi_mod = py_getmodule("midi");
    if (!py_exec(PY_PRELUDE_MODULE, "<midi_prelude>", EXEC_MODE, midi_mod)) {
        py_printexc();
    }
}

void pk_midi_module_cleanup(void) {
    midi_cleanup_observer();
}
