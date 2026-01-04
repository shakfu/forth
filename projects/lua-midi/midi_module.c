/*
 * MIDI module for Lua 5.5
 * Provides Lua bindings for MIDI output using libremidi
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <time.h>

#include "lua.h"
#include "lauxlib.h"
#include <libremidi/libremidi-c.h>

#include "lua_prelude.h"
#include "music_theory.h"
#include "midi_file.h"

#define MAX_PORTS 64
#define MIDI_OUT_MT "MidiOut"
#define MAX_CAPTURE_EVENTS 4096
#define TICKS_PER_QUARTER 480

/* Global state */
static libremidi_midi_observer_handle* midi_observer = NULL;
static libremidi_midi_out_port* out_ports[MAX_PORTS];
static int out_port_count = 0;
static lua_State *global_L = NULL;

/* Default MIDI output for REPL convenience functions */
static libremidi_midi_out_handle* default_midi_out = NULL;

/* Tempo state for duration scaling */
static int global_tempo_bpm = 120;
#define BASE_TEMPO_BPM 120

/* Scale duration based on current tempo */
static inline int scale_duration_for_tempo(int duration_ms) {
    if (global_tempo_bpm == BASE_TEMPO_BPM || duration_ms <= 0) {
        return duration_ms;
    }
    /* At 60 BPM, durations double (120/60 = 2x)
       At 240 BPM, durations halve (120/240 = 0.5x) */
    return (int)((int64_t)duration_ms * BASE_TEMPO_BPM / global_tempo_bpm);
}

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

/* MidiOut userdata structure */
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

/* Get pitch from Lua value (integer or string) */
static int get_pitch(lua_State *L, int idx) {
    if (lua_isinteger(L, idx)) {
        return (int)lua_tointeger(L, idx);
    } else if (lua_isnumber(L, idx)) {
        return (int)lua_tonumber(L, idx);
    } else if (lua_isstring(L, idx)) {
        return parse_pitch(lua_tostring(L, idx));
    }
    return -1;
}

/* ============================================================================
 * MidiOut userdata functions
 * ============================================================================ */

static MidiOutData* check_midi_out(lua_State *L, int idx) {
    return (MidiOutData *)luaL_checkudata(L, idx, MIDI_OUT_MT);
}

static int midi_out_gc(lua_State *L) {
    MidiOutData *data = check_midi_out(L, 1);
    if (data->handle) {
        /* Send all notes off before closing */
        for (int ch = 0; ch < 16; ch++) {
            uint8_t msg[3] = { 0xB0 | ch, 123, 0 };
            libremidi_midi_out_send_message(data->handle, msg, 3);
        }
        libremidi_midi_out_free(data->handle);
        data->handle = NULL;
    }
    if (data->port_name) {
        free(data->port_name);
        data->port_name = NULL;
    }
    return 0;
}

static int midi_out_tostring(lua_State *L) {
    MidiOutData *data = check_midi_out(L, 1);
    if (data->handle) {
        if (data->is_virtual) {
            lua_pushfstring(L, "MidiOut(virtual, \"%s\")",
                           data->port_name ? data->port_name : "luaMIDI");
        } else {
            lua_pushliteral(L, "MidiOut(connected)");
        }
    } else {
        lua_pushliteral(L, "MidiOut(closed)");
    }
    return 1;
}

/* ============================================================================
 * Port management functions
 * ============================================================================ */

/* midi.list_ports() -> {{index, name}, ...} */
static int l_list_ports(lua_State *L) {
    if (midi_init_observer() != 0) {
        return luaL_error(L, "Failed to initialize MIDI observer");
    }

    /* Re-enumerate ports */
    for (int i = 0; i < out_port_count; i++) {
        libremidi_midi_out_port_free(out_ports[i]);
    }
    out_port_count = 0;
    libremidi_midi_observer_enumerate_output_ports(midi_observer, NULL, on_output_port_found);

    /* Build table */
    lua_newtable(L);
    for (int i = 0; i < out_port_count; i++) {
        const char* name = NULL;
        size_t len = 0;
        if (libremidi_midi_out_port_name(out_ports[i], &name, &len) == 0) {
            lua_newtable(L);
            lua_pushinteger(L, i);
            lua_rawseti(L, -2, 1);
            lua_pushstring(L, name);
            lua_rawseti(L, -2, 2);
            lua_rawseti(L, -2, i + 1);
        }
    }
    return 1;
}

/* midi.open([name_or_index]) -> MidiOut */
static int l_open(lua_State *L) {
    MidiOutData *data;
    libremidi_midi_out_handle* handle = NULL;
    int is_virtual = 0;
    char *port_name_copy = NULL;
    const char *port_name = "luaMIDI";

    if (midi_init_observer() != 0) {
        return luaL_error(L, "Failed to initialize MIDI observer");
    }

    libremidi_midi_configuration midi_conf;
    int ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        return luaL_error(L, "Failed to initialize MIDI configuration");
    }

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        return luaL_error(L, "Failed to initialize API configuration");
    }

    if (lua_gettop(L) == 0 || lua_isnil(L, 1)) {
        /* No argument - create virtual port */
        midi_conf.virtual_port = true;
        midi_conf.port_name = port_name;
        is_virtual = 1;
        port_name_copy = strdup(port_name);
    } else if (lua_isstring(L, 1)) {
        /* String argument - create named virtual port */
        port_name = lua_tostring(L, 1);
        midi_conf.virtual_port = true;
        midi_conf.port_name = port_name;
        is_virtual = 1;
        port_name_copy = strdup(port_name);
    } else if (lua_isinteger(L, 1)) {
        /* Integer argument - open hardware port by index */
        int idx = (int)lua_tointeger(L, 1);
        if (idx < 0 || idx >= out_port_count) {
            return luaL_error(L, "Invalid port index %d (0-%d available)", idx, out_port_count - 1);
        }
        midi_conf.out_port = out_ports[idx];
        is_virtual = 0;
    } else {
        return luaL_error(L, "Expected string (port name) or integer (port index)");
    }

    ret = libremidi_midi_out_new(&midi_conf, &api_conf, &handle);
    if (ret != 0 || !handle) {
        if (port_name_copy) free(port_name_copy);
        return luaL_error(L, "Failed to create MIDI output");
    }

    /* Create userdata */
    data = (MidiOutData *)lua_newuserdata(L, sizeof(MidiOutData));
    data->handle = handle;
    data->is_virtual = is_virtual;
    data->port_name = port_name_copy;

    luaL_getmetatable(L, MIDI_OUT_MT);
    lua_setmetatable(L, -2);

    return 1;
}

/* ============================================================================
 * MidiOut methods
 * ============================================================================ */

/* midiout:close() */
static int l_close(lua_State *L) {
    MidiOutData *data = check_midi_out(L, 1);
    if (data->handle) {
        /* Send all notes off */
        for (int ch = 0; ch < 16; ch++) {
            uint8_t msg[3] = { 0xB0 | ch, 123, 0 };
            libremidi_midi_out_send_message(data->handle, msg, 3);
        }
        libremidi_midi_out_free(data->handle);
        data->handle = NULL;
    }
    return 0;
}

/* midiout:is_open() -> boolean */
static int l_is_open(lua_State *L) {
    MidiOutData *data = check_midi_out(L, 1);
    lua_pushboolean(L, data->handle != NULL);
    return 1;
}

/* midiout:note_on(pitch, [velocity], [channel]) */
static int l_note_on(lua_State *L) {
    MidiOutData *data = check_midi_out(L, 1);
    if (!data->handle) {
        return luaL_error(L, "MIDI port is closed");
    }

    int pitch = get_pitch(L, 2);
    if (pitch < 0 || pitch > 127) {
        return luaL_error(L, "Invalid pitch");
    }

    int velocity = luaL_optinteger(L, 3, 80);
    int channel = luaL_optinteger(L, 4, 1);

    if (velocity < 0 || velocity > 127) velocity = 80;
    if (channel < 1 || channel > 16) channel = 1;

    uint8_t msg[3] = {
        0x90 | ((channel - 1) & 0x0F),
        pitch & 0x7F,
        velocity & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);
    capture_add_event(0, channel - 1, pitch, velocity);
    return 0;
}

/* midiout:note_off(pitch, [velocity], [channel]) */
static int l_note_off(lua_State *L) {
    MidiOutData *data = check_midi_out(L, 1);
    if (!data->handle) {
        return luaL_error(L, "MIDI port is closed");
    }

    int pitch = get_pitch(L, 2);
    if (pitch < 0 || pitch > 127) {
        return luaL_error(L, "Invalid pitch");
    }

    int velocity = luaL_optinteger(L, 3, 0);
    int channel = luaL_optinteger(L, 4, 1);

    if (velocity < 0 || velocity > 127) velocity = 0;
    if (channel < 1 || channel > 16) channel = 1;

    uint8_t msg[3] = {
        0x80 | ((channel - 1) & 0x0F),
        pitch & 0x7F,
        velocity & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);
    capture_add_event(1, channel - 1, pitch, 0);
    return 0;
}

/* midiout:note(pitch, [velocity], [duration], [channel]) */
static int l_note(lua_State *L) {
    MidiOutData *data = check_midi_out(L, 1);
    if (!data->handle) {
        return luaL_error(L, "MIDI port is closed");
    }

    int pitch = get_pitch(L, 2);
    if (pitch < 0 || pitch > 127) {
        return luaL_error(L, "Invalid pitch");
    }

    int velocity = luaL_optinteger(L, 3, 80);
    int duration = luaL_optinteger(L, 4, 500);
    int channel = luaL_optinteger(L, 5, 1);

    if (velocity < 0 || velocity > 127) velocity = 80;
    if (channel < 1 || channel > 16) channel = 1;

    /* Note on */
    uint8_t msg[3] = {
        0x90 | ((channel - 1) & 0x0F),
        pitch & 0x7F,
        velocity & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);
    capture_add_event(0, channel - 1, pitch, velocity);

    /* Sleep (scaled by tempo) */
    usleep(scale_duration_for_tempo(duration) * 1000);

    /* Note off */
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[2] = 0;
    libremidi_midi_out_send_message(data->handle, msg, 3);
    capture_add_event(1, channel - 1, pitch, 0);

    return 0;
}

/* midiout:chord(pitches, [velocity], [duration], [channel]) */
static int l_chord(lua_State *L) {
    MidiOutData *data = check_midi_out(L, 1);
    if (!data->handle) {
        return luaL_error(L, "MIDI port is closed");
    }

    luaL_checktype(L, 2, LUA_TTABLE);
    int velocity = luaL_optinteger(L, 3, 80);
    int duration = luaL_optinteger(L, 4, 500);
    int channel = luaL_optinteger(L, 5, 1);

    if (velocity < 0 || velocity > 127) velocity = 80;
    if (channel < 1 || channel > 16) channel = 1;

    /* Collect pitches */
    int pitches[128];
    int count = 0;

    lua_pushnil(L);
    while (lua_next(L, 2) != 0 && count < 128) {
        int p = get_pitch(L, -1);
        if (p >= 0 && p <= 127) {
            pitches[count++] = p;
        }
        lua_pop(L, 1);
    }

    /* Note on for all */
    for (int i = 0; i < count; i++) {
        uint8_t msg[3] = {
            0x90 | ((channel - 1) & 0x0F),
            pitches[i] & 0x7F,
            velocity & 0x7F
        };
        libremidi_midi_out_send_message(data->handle, msg, 3);
        capture_add_event(0, channel - 1, pitches[i], velocity);
    }

    /* Sleep (scaled by tempo) */
    usleep(scale_duration_for_tempo(duration) * 1000);

    /* Note off for all */
    for (int i = 0; i < count; i++) {
        uint8_t msg[3] = {
            0x80 | ((channel - 1) & 0x0F),
            pitches[i] & 0x7F,
            0
        };
        libremidi_midi_out_send_message(data->handle, msg, 3);
        capture_add_event(1, channel - 1, pitches[i], 0);
    }

    return 0;
}

/* midiout:arpeggio(pitches, [velocity], [duration], [channel]) */
static int l_arpeggio(lua_State *L) {
    MidiOutData *data = check_midi_out(L, 1);
    if (!data->handle) {
        return luaL_error(L, "MIDI port is closed");
    }

    luaL_checktype(L, 2, LUA_TTABLE);
    int velocity = luaL_optinteger(L, 3, 80);
    int duration = luaL_optinteger(L, 4, 250);
    int channel = luaL_optinteger(L, 5, 1);

    if (velocity < 0 || velocity > 127) velocity = 80;
    if (channel < 1 || channel > 16) channel = 1;

    /* Iterate through table */
    lua_pushnil(L);
    while (lua_next(L, 2) != 0) {
        int pitch = get_pitch(L, -1);
        if (pitch >= 0 && pitch <= 127) {
            /* Note on */
            uint8_t msg[3] = {
                0x90 | ((channel - 1) & 0x0F),
                pitch & 0x7F,
                velocity & 0x7F
            };
            libremidi_midi_out_send_message(data->handle, msg, 3);
            capture_add_event(0, channel - 1, pitch, velocity);

            /* Sleep (scaled by tempo) */
            usleep(scale_duration_for_tempo(duration) * 1000);

            /* Note off */
            msg[0] = 0x80 | ((channel - 1) & 0x0F);
            msg[2] = 0;
            libremidi_midi_out_send_message(data->handle, msg, 3);
            capture_add_event(1, channel - 1, pitch, 0);
        }
        lua_pop(L, 1);
    }

    return 0;
}

/* midiout:cc(control, value, [channel]) */
static int l_cc(lua_State *L) {
    MidiOutData *data = check_midi_out(L, 1);
    if (!data->handle) {
        return luaL_error(L, "MIDI port is closed");
    }

    int control = luaL_checkinteger(L, 2);
    int value = luaL_checkinteger(L, 3);
    int channel = luaL_optinteger(L, 4, 1);

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
    return 0;
}

/* midiout:program(program, [channel]) */
static int l_program(lua_State *L) {
    MidiOutData *data = check_midi_out(L, 1);
    if (!data->handle) {
        return luaL_error(L, "MIDI port is closed");
    }

    int program = luaL_checkinteger(L, 2);
    int channel = luaL_optinteger(L, 3, 1);

    if (program < 0 || program > 127) program = 0;
    if (channel < 1 || channel > 16) channel = 1;

    uint8_t msg[2] = {
        0xC0 | ((channel - 1) & 0x0F),
        program & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 2);
    return 0;
}

/* midiout:all_notes_off([channel]) */
static int l_all_notes_off(lua_State *L) {
    MidiOutData *data = check_midi_out(L, 1);
    if (!data->handle) {
        return luaL_error(L, "MIDI port is closed");
    }

    if (lua_gettop(L) >= 2 && !lua_isnil(L, 2)) {
        int channel = luaL_checkinteger(L, 2);
        if (channel >= 1 && channel <= 16) {
            uint8_t msg[3] = { 0xB0 | ((channel - 1) & 0x0F), 123, 0 };
            libremidi_midi_out_send_message(data->handle, msg, 3);
        }
    } else {
        for (int ch = 0; ch < 16; ch++) {
            uint8_t msg[3] = { 0xB0 | ch, 123, 0 };
            libremidi_midi_out_send_message(data->handle, msg, 3);
        }
    }
    return 0;
}

/* ============================================================================
 * Utility functions
 * ============================================================================ */

/* midi.note(name) -> MIDI number */
static int l_parse_note(lua_State *L) {
    const char *name = luaL_checkstring(L, 1);
    int pitch = parse_pitch(name);
    if (pitch < 0) {
        return luaL_error(L, "Invalid note name: %s", name);
    }
    lua_pushinteger(L, pitch);
    return 1;
}

/* midi.sleep(ms) */
static int l_sleep(lua_State *L) {
    int ms = luaL_checkinteger(L, 1);
    if (ms > 0) {
        usleep(scale_duration_for_tempo(ms) * 1000);
    }
    return 0;
}

/* midi._set_c_tempo(bpm) - Set tempo in C layer for duration scaling */
static int l_set_c_tempo(lua_State *L) {
    int bpm = luaL_checkinteger(L, 1);
    if (bpm < 1) bpm = 1;
    if (bpm > 999) bpm = 999;
    global_tempo_bpm = bpm;
    return 0;
}

/* midi._get_c_tempo() -> bpm - Get current C layer tempo */
static int l_get_c_tempo(lua_State *L) {
    lua_pushinteger(L, global_tempo_bpm);
    return 1;
}

/* midi.transpose(pitch, semitones) -> pitch */
static int l_transpose(lua_State *L) {
    int pitch = get_pitch(L, 1);
    int semitones = luaL_checkinteger(L, 2);
    int result = pitch + semitones;
    if (result < 0) result = 0;
    if (result > 127) result = 127;
    lua_pushinteger(L, result);
    return 1;
}

/* midi.octave_up(pitch) -> pitch */
static int l_octave_up(lua_State *L) {
    int pitch = get_pitch(L, 1);
    int result = pitch + 12;
    if (result > 127) result = 127;
    lua_pushinteger(L, result);
    return 1;
}

/* midi.octave_down(pitch) -> pitch */
static int l_octave_down(lua_State *L) {
    int pitch = get_pitch(L, 1);
    int result = pitch - 12;
    if (result < 0) result = 0;
    lua_pushinteger(L, result);
    return 1;
}

/* Chord builders */
static int l_major(lua_State *L) {
    int root = get_pitch(L, 1);
    if (root < 0) return luaL_error(L, "Invalid pitch");
    lua_newtable(L);
    lua_pushinteger(L, root); lua_rawseti(L, -2, 1);
    lua_pushinteger(L, root + 4); lua_rawseti(L, -2, 2);
    lua_pushinteger(L, root + 7); lua_rawseti(L, -2, 3);
    return 1;
}

static int l_minor(lua_State *L) {
    int root = get_pitch(L, 1);
    if (root < 0) return luaL_error(L, "Invalid pitch");
    lua_newtable(L);
    lua_pushinteger(L, root); lua_rawseti(L, -2, 1);
    lua_pushinteger(L, root + 3); lua_rawseti(L, -2, 2);
    lua_pushinteger(L, root + 7); lua_rawseti(L, -2, 3);
    return 1;
}

static int l_dim(lua_State *L) {
    int root = get_pitch(L, 1);
    if (root < 0) return luaL_error(L, "Invalid pitch");
    lua_newtable(L);
    lua_pushinteger(L, root); lua_rawseti(L, -2, 1);
    lua_pushinteger(L, root + 3); lua_rawseti(L, -2, 2);
    lua_pushinteger(L, root + 6); lua_rawseti(L, -2, 3);
    return 1;
}

static int l_aug(lua_State *L) {
    int root = get_pitch(L, 1);
    if (root < 0) return luaL_error(L, "Invalid pitch");
    lua_newtable(L);
    lua_pushinteger(L, root); lua_rawseti(L, -2, 1);
    lua_pushinteger(L, root + 4); lua_rawseti(L, -2, 2);
    lua_pushinteger(L, root + 8); lua_rawseti(L, -2, 3);
    return 1;
}

static int l_dom7(lua_State *L) {
    int root = get_pitch(L, 1);
    if (root < 0) return luaL_error(L, "Invalid pitch");
    lua_newtable(L);
    lua_pushinteger(L, root); lua_rawseti(L, -2, 1);
    lua_pushinteger(L, root + 4); lua_rawseti(L, -2, 2);
    lua_pushinteger(L, root + 7); lua_rawseti(L, -2, 3);
    lua_pushinteger(L, root + 10); lua_rawseti(L, -2, 4);
    return 1;
}

static int l_maj7(lua_State *L) {
    int root = get_pitch(L, 1);
    if (root < 0) return luaL_error(L, "Invalid pitch");
    lua_newtable(L);
    lua_pushinteger(L, root); lua_rawseti(L, -2, 1);
    lua_pushinteger(L, root + 4); lua_rawseti(L, -2, 2);
    lua_pushinteger(L, root + 7); lua_rawseti(L, -2, 3);
    lua_pushinteger(L, root + 11); lua_rawseti(L, -2, 4);
    return 1;
}

static int l_min7(lua_State *L) {
    int root = get_pitch(L, 1);
    if (root < 0) return luaL_error(L, "Invalid pitch");
    lua_newtable(L);
    lua_pushinteger(L, root); lua_rawseti(L, -2, 1);
    lua_pushinteger(L, root + 3); lua_rawseti(L, -2, 2);
    lua_pushinteger(L, root + 7); lua_rawseti(L, -2, 3);
    lua_pushinteger(L, root + 10); lua_rawseti(L, -2, 4);
    return 1;
}

/* ============================================================================
 * Scale functions - using common music_theory library
 * ============================================================================ */

/* Helper to read interval array from Lua table */
static int get_intervals(lua_State *L, int idx, int* intervals, int max_size) {
    if (!lua_istable(L, idx)) return 0;
    int count = 0;
    lua_pushnil(L);
    while (lua_next(L, idx) != 0 && count < max_size) {
        if (lua_isinteger(L, -1)) {
            intervals[count++] = (int)lua_tointeger(L, -1);
        }
        lua_pop(L, 1);
    }
    return count;
}

/* midi.build_scale(root, intervals) -> {pitches...} */
static int l_build_scale(lua_State *L) {
    int root = get_pitch(L, 1);
    if (root < 0) return luaL_error(L, "Invalid root pitch");

    int intervals[16];
    int num_intervals = get_intervals(L, 2, intervals, 16);
    if (num_intervals == 0) {
        return luaL_error(L, "Expected intervals table");
    }

    int pitches[16];
    int count = music_build_scale(root, intervals, num_intervals, pitches);

    lua_newtable(L);
    for (int i = 0; i < count; i++) {
        lua_pushinteger(L, pitches[i]);
        lua_rawseti(L, -2, i + 1);
    }
    return 1;
}

/* midi.scale_degree(root, intervals, degree) -> pitch */
static int l_scale_degree(lua_State *L) {
    int root = get_pitch(L, 1);
    if (root < 0) return luaL_error(L, "Invalid root pitch");

    int intervals[16];
    int num_intervals = get_intervals(L, 2, intervals, 16);
    if (num_intervals == 0) {
        return luaL_error(L, "Expected intervals table");
    }

    int degree = luaL_checkinteger(L, 3);
    int pitch = music_scale_degree(root, intervals, num_intervals, degree);

    if (pitch < 0) {
        return luaL_error(L, "Scale degree out of range");
    }
    lua_pushinteger(L, pitch);
    return 1;
}

/* midi.in_scale(pitch, root, intervals) -> boolean */
static int l_in_scale(lua_State *L) {
    int pitch = get_pitch(L, 1);
    if (pitch < 0) return luaL_error(L, "Invalid pitch");

    int root = get_pitch(L, 2);
    if (root < 0) return luaL_error(L, "Invalid root pitch");

    int intervals[16];
    int num_intervals = get_intervals(L, 3, intervals, 16);
    if (num_intervals == 0) {
        return luaL_error(L, "Expected intervals table");
    }

    int result = music_in_scale(pitch, root, intervals, num_intervals);
    lua_pushboolean(L, result);
    return 1;
}

/* midi.quantize(pitch, root, intervals) -> pitch */
static int l_quantize(lua_State *L) {
    int pitch = get_pitch(L, 1);
    if (pitch < 0) return luaL_error(L, "Invalid pitch");

    int root = get_pitch(L, 2);
    if (root < 0) return luaL_error(L, "Invalid root pitch");

    int intervals[16];
    int num_intervals = get_intervals(L, 3, intervals, 16);
    if (num_intervals == 0) {
        return luaL_error(L, "Expected intervals table");
    }

    int result = music_quantize_to_scale(pitch, root, intervals, num_intervals);
    lua_pushinteger(L, result);
    return 1;
}

/* midi.pitch_bend(cents, [channel]) - send pitch bend for microtonal */
static int l_pitch_bend(lua_State *L) {
    MidiOutData *data = check_midi_out(L, 1);
    if (!data->handle) {
        return luaL_error(L, "MIDI port is closed");
    }

    int cents = luaL_checkinteger(L, 2);
    int channel = luaL_optinteger(L, 3, 1);

    if (channel < 1 || channel > 16) channel = 1;

    int bend = music_cents_to_bend(cents);
    uint8_t msg[3] = {
        0xE0 | ((channel - 1) & 0x0F),
        bend & 0x7F,
        (bend >> 7) & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);
    return 0;
}

/* ============================================================================
 * MIDI Capture functions
 * ============================================================================ */

/* midi.record_midi([bpm]) - Start recording MIDI events */
static int l_record_midi(lua_State *L) {
    if (capture_active) {
        printf("Already recording (use record_stop() first)\n");
        return 0;
    }
    capture_bpm = luaL_optinteger(L, 1, 120);
    capture_count = 0;
    capture_active = 1;
    clock_gettime(CLOCK_MONOTONIC, &capture_start_time);
    printf("MIDI recording started at %d BPM. Play notes, then record_stop() and save_midi(filename).\n", capture_bpm);
    return 0;
}

/* midi.record_stop() - Stop recording MIDI events */
static int l_record_stop(lua_State *L) {
    (void)L;
    if (!capture_active) {
        printf("Not recording\n");
        return 0;
    }
    capture_active = 0;
    printf("MIDI recording stopped. %d events recorded. Use save_midi(filename) to save.\n", capture_count);
    return 0;
}

/* midi.save_midi(filename) - Save recorded MIDI to a Lua file */
static int l_save_midi(lua_State *L) {
    const char* filename = luaL_checkstring(L, 1);

    if (capture_count == 0) {
        printf("Nothing to save (no events recorded)\n");
        return 0;
    }

    FILE* f = fopen(filename, "w");
    if (f == NULL) {
        return luaL_error(L, "Cannot create file '%s'", filename);
    }

    /* Write header */
    fprintf(f, "-- MIDI capture\n");
    fprintf(f, "-- %d events captured at %d BPM\n\n", capture_count, capture_bpm);
    fprintf(f, "local m = midi.open()\n\n");
    fprintf(f, "-- Notes with timing (start_ms, pitch, velocity, duration_ms, channel)\n");
    fprintf(f, "local notes = {\n");

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

                fprintf(f, "  {%d, %d, %d, %d, %d},\n",
                        start_ms, e->data1, vel, dur_ms, e->channel + 1);
                notes_written++;

                note_on_time[e->channel][e->data1] = -1;
            }
        }
    }

    fprintf(f, "}\n\n");

    /* Write playback code - use sleep deltas for accurate wall-clock timing */
    fprintf(f, "-- Timed playback (preserves original timing between notes)\n");
    fprintf(f, "local function play_timed()\n");
    fprintf(f, "  local last_start = 0\n");
    fprintf(f, "  for _, n in ipairs(notes) do\n");
    fprintf(f, "    -- Wait until this note's start time\n");
    fprintf(f, "    local delay = n[1] - last_start\n");
    fprintf(f, "    if delay > 0 then\n");
    fprintf(f, "      midi.sleep(delay)\n");
    fprintf(f, "    end\n");
    fprintf(f, "    last_start = n[1]\n");
    fprintf(f, "    -- Play note (blocking for duration)\n");
    fprintf(f, "    m:note(n[2], n[3], n[4], n[5])\n");
    fprintf(f, "  end\n");
    fprintf(f, "end\n\n");

    fprintf(f, "-- Sequential playback (plays notes back-to-back)\n");
    fprintf(f, "for _, n in ipairs(notes) do\n");
    fprintf(f, "  m:note(n[2], n[3], n[4], n[5])\n");
    fprintf(f, "end\n\n");

    fprintf(f, "m:close()\n");
    fprintf(f, "-- %d notes written\n", notes_written);

    fclose(f);
    printf("Saved %d notes to '%s'\n", notes_written, filename);
    return 0;
}

/* midi.record_status() - Get recording status */
static int l_record_status(lua_State *L) {
    lua_newtable(L);
    lua_pushboolean(L, capture_active);
    lua_setfield(L, -2, "active");
    lua_pushinteger(L, capture_count);
    lua_setfield(L, -2, "events");
    lua_pushinteger(L, capture_bpm);
    lua_setfield(L, -2, "bpm");
    return 1;
}

/* ============================================================================
 * MIDI File I/O (using libremidi reader/writer)
 * ============================================================================ */

/* midi.write_mid(filename, [ppqn]) - Write captured events to standard MIDI file */
static int l_write_mid(lua_State *L) {
    const char* filename = luaL_checkstring(L, 1);
    int ppqn = luaL_optinteger(L, 2, TICKS_PER_QUARTER);

    if (capture_count == 0) {
        printf("Nothing to save (no events recorded)\n");
        return 0;
    }

    midi_file_writer* writer = NULL;
    if (midi_file_writer_new(&writer) != 0) {
        return luaL_error(L, "Failed to create MIDI file writer");
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
        }
    }

    int result = midi_file_writer_save(writer, filename);
    midi_file_writer_free(writer);

    if (result != 0) {
        return luaL_error(L, "Failed to write MIDI file '%s'", filename);
    }

    printf("Saved %d events to '%s'\n", capture_count, filename);
    return 0;
}

/* Callback context for reading MIDI file */
typedef struct {
    lua_State *L;
    int table_idx;
    int event_num;
} ReadMidContext;

static void read_mid_callback(void* ctx, const midi_file_event* event) {
    ReadMidContext* rctx = (ReadMidContext*)ctx;
    lua_State *L = rctx->L;

    /* Skip meta events and system messages */
    if (event->type < 0x80 || event->type >= 0xF0) return;

    rctx->event_num++;

    /* Create event table */
    lua_newtable(L);

    lua_pushinteger(L, event->track);
    lua_setfield(L, -2, "track");

    lua_pushinteger(L, event->tick);
    lua_setfield(L, -2, "tick");

    lua_pushinteger(L, event->channel);
    lua_setfield(L, -2, "channel");

    /* Decode message type */
    const char* type_name = "unknown";
    switch (event->type) {
        case 0x80: type_name = "note_off"; break;
        case 0x90: type_name = (event->data2 > 0) ? "note_on" : "note_off"; break;
        case 0xA0: type_name = "poly_pressure"; break;
        case 0xB0: type_name = "cc"; break;
        case 0xC0: type_name = "program"; break;
        case 0xD0: type_name = "aftertouch"; break;
        case 0xE0: type_name = "pitch_bend"; break;
    }
    lua_pushstring(L, type_name);
    lua_setfield(L, -2, "type");

    /* Add data fields based on type */
    if (event->type == 0x80 || event->type == 0x90) {
        lua_pushinteger(L, event->data1);
        lua_setfield(L, -2, "pitch");
        lua_pushinteger(L, event->data2);
        lua_setfield(L, -2, "velocity");
    } else if (event->type == 0xB0) {
        lua_pushinteger(L, event->data1);
        lua_setfield(L, -2, "control");
        lua_pushinteger(L, event->data2);
        lua_setfield(L, -2, "value");
    } else if (event->type == 0xC0) {
        lua_pushinteger(L, event->data1);
        lua_setfield(L, -2, "program");
    } else if (event->type == 0xE0) {
        int bend = event->data1 | (event->data2 << 7);
        lua_pushinteger(L, bend);
        lua_setfield(L, -2, "value");
    } else {
        lua_pushinteger(L, event->data1);
        lua_setfield(L, -2, "data1");
        lua_pushinteger(L, event->data2);
        lua_setfield(L, -2, "data2");
    }

    /* Add to events array */
    lua_rawseti(L, rctx->table_idx, rctx->event_num);
}

/* midi.read_mid(filename) - Read MIDI file and return table of events */
static int l_read_mid(lua_State *L) {
    const char* filename = luaL_checkstring(L, 1);

    midi_file_reader* reader = NULL;
    if (midi_file_reader_new(&reader) != 0) {
        return luaL_error(L, "Failed to create MIDI file reader");
    }

    int result = midi_file_reader_load(reader, filename);
    if (result == 0) {
        midi_file_reader_free(reader);
        return luaL_error(L, "Failed to parse MIDI file '%s'", filename);
    }

    /* Create result table */
    lua_newtable(L);
    int result_idx = lua_gettop(L);

    /* Add metadata */
    lua_pushinteger(L, midi_file_reader_num_tracks(reader));
    lua_setfield(L, result_idx, "num_tracks");

    lua_pushnumber(L, midi_file_reader_ppqn(reader));
    lua_setfield(L, result_idx, "ppqn");

    lua_pushnumber(L, midi_file_reader_tempo(reader));
    lua_setfield(L, result_idx, "tempo");

    lua_pushnumber(L, midi_file_reader_duration(reader));
    lua_setfield(L, result_idx, "duration");

    lua_pushinteger(L, midi_file_reader_format(reader));
    lua_setfield(L, result_idx, "format");

    /* Create events array */
    lua_newtable(L);
    int events_idx = lua_gettop(L);

    ReadMidContext ctx = { L, events_idx, 0 };
    midi_file_reader_for_each(reader, &ctx, read_mid_callback);

    lua_setfield(L, result_idx, "events");

    midi_file_reader_free(reader);
    return 1;
}

/* help() function */
static int l_help(lua_State *L) {
    (void)L;
    printf("lua_midi - Lua MIDI language\n\n");
    printf("Port management:\n");
    printf("  midi.list_ports()           List available MIDI ports\n");
    printf("  midi.open()                 Open virtual port 'luaMIDI'\n");
    printf("  midi.open(\"name\")           Open named virtual port\n");
    printf("  midi.open(index)            Open hardware port by index\n");
    printf("  m:close()                   Close MIDI port\n");
    printf("  m:is_open()                 Check if port is open\n");
    printf("\n");
    printf("Note playing:\n");
    printf("  m:note(pitch, [vel], [dur], [ch])     Play note (blocking)\n");
    printf("  m:chord(pitches, [vel], [dur], [ch])  Play chord\n");
    printf("  m:arpeggio(pitches, [vel], [dur], [ch]) Arpeggiate\n");
    printf("  m:note_on(pitch, [vel], [ch])         Send note on\n");
    printf("  m:note_off(pitch, [vel], [ch])        Send note off\n");
    printf("\n");
    printf("Control messages:\n");
    printf("  m:cc(control, value, [ch])  Control change\n");
    printf("  m:program(prog, [ch])       Program change\n");
    printf("  m:all_notes_off([ch])       All notes off\n");
    printf("\n");
    printf("Utilities:\n");
    printf("  midi.note(\"C4\")             Parse note name to MIDI number\n");
    printf("  midi.sleep(ms)              Sleep for milliseconds\n");
    printf("  midi.transpose(pitch, n)    Transpose by semitones\n");
    printf("  midi.octave_up(pitch)       Transpose up one octave\n");
    printf("  midi.octave_down(pitch)     Transpose down one octave\n");
    printf("\n");
    printf("Chord builders:\n");
    printf("  midi.major(root)            Major triad\n");
    printf("  midi.minor(root)            Minor triad\n");
    printf("  midi.dim(root)              Diminished triad\n");
    printf("  midi.aug(root)              Augmented triad\n");
    printf("  midi.dom7(root)             Dominant 7th\n");
    printf("  midi.maj7(root)             Major 7th\n");
    printf("  midi.min7(root)             Minor 7th\n");
    printf("\n");
    printf("Scale functions:\n");
    printf("  midi.build_scale(root, intervals)    Build scale pitches\n");
    printf("  midi.scale_degree(root, intervals, n) Get nth scale degree\n");
    printf("  midi.in_scale(pitch, root, intervals) Check if pitch in scale\n");
    printf("  midi.quantize(pitch, root, intervals) Snap to nearest scale tone\n");
    printf("  m:pitch_bend(cents, [ch])   Send pitch bend (for microtonal)\n");
    printf("\n");
    printf("Scale helpers (via prelude):\n");
    printf("  scale(root, scale_type)     Build scale (e.g., scale(c4, 'major'))\n");
    printf("  degree(root, scale_type, n) Get scale degree\n");
    printf("\n");
    printf("MIDI Recording:\n");
    printf("  midi.record_midi([bpm])     Start recording MIDI events\n");
    printf("  midi.record_stop()          Stop recording\n");
    printf("  midi.save_midi(filename)    Save recording to Lua file\n");
    printf("  midi.record_status()        Get recording status\n");
    printf("\n");
    printf("MIDI File I/O:\n");
    printf("  midi.write_mid(filename)    Write recording to .mid file\n");
    printf("  midi.read_mid(filename)     Read .mid file -> {events, ppqn, tempo, ...}\n");
    printf("\n");
    printf("Constants: midi.c0-c8, midi.cs0-cs8, midi.d0-d8, etc.\n");
    printf("Dynamics: midi.ppp, pp, p, mp, mf, f, ff, fff\n");
    printf("Durations: midi.whole, half, quarter, eighth, sixteenth\n");
    printf("Scales: midi.scales.major, minor, dorian, blues, pentatonic, etc.\n");
    return 0;
}

/* ============================================================================
 * Module registration
 * ============================================================================ */

static const luaL_Reg midi_methods[] = {
    {"close", l_close},
    {"is_open", l_is_open},
    {"note", l_note},
    {"note_on", l_note_on},
    {"note_off", l_note_off},
    {"chord", l_chord},
    {"arpeggio", l_arpeggio},
    {"cc", l_cc},
    {"program", l_program},
    {"all_notes_off", l_all_notes_off},
    {"pitch_bend", l_pitch_bend},
    {NULL, NULL}
};

static const luaL_Reg midi_funcs[] = {
    {"list_ports", l_list_ports},
    {"open", l_open},
    {"note", l_parse_note},
    {"sleep", l_sleep},
    {"_set_c_tempo", l_set_c_tempo},
    {"_get_c_tempo", l_get_c_tempo},
    {"transpose", l_transpose},
    {"octave_up", l_octave_up},
    {"octave_down", l_octave_down},
    /* Chord builders */
    {"major", l_major},
    {"minor", l_minor},
    {"dim", l_dim},
    {"aug", l_aug},
    {"dom7", l_dom7},
    {"maj7", l_maj7},
    {"min7", l_min7},
    /* Scale functions */
    {"build_scale", l_build_scale},
    {"scale_degree", l_scale_degree},
    {"in_scale", l_in_scale},
    {"quantize", l_quantize},
    /* Recording functions */
    {"record_midi", l_record_midi},
    {"record_stop", l_record_stop},
    {"save_midi", l_save_midi},
    {"record_status", l_record_status},
    /* MIDI file I/O */
    {"write_mid", l_write_mid},
    {"read_mid", l_read_mid},
    {"help", l_help},
    {NULL, NULL}
};


int luaopen_midi(lua_State *L) {
    global_L = L;

    /* Create MidiOut metatable */
    luaL_newmetatable(L, MIDI_OUT_MT);
    lua_pushvalue(L, -1);
    lua_setfield(L, -2, "__index");  /* mt.__index = mt */
    lua_pushcfunction(L, midi_out_gc);
    lua_setfield(L, -2, "__gc");
    lua_pushcfunction(L, midi_out_tostring);
    lua_setfield(L, -2, "__tostring");

    /* Add methods to metatable */
    luaL_setfuncs(L, midi_methods, 0);
    lua_pop(L, 1);  /* pop metatable */

    /* Create module table */
    luaL_newlib(L, midi_funcs);

    /* Run prelude to set up constants */
    lua_setglobal(L, "midi");  /* temporarily set as global for prelude */
    if (luaL_dostring(L, LUA_PRELUDE_MODULE) != LUA_OK) {
        fprintf(stderr, "Error loading MIDI prelude: %s\n", lua_tostring(L, -1));
        lua_pop(L, 1);
    }
    lua_getglobal(L, "midi");  /* get it back on stack */

    return 1;
}

void lua_midi_cleanup(void) {
    if (default_midi_out) {
        /* Send all notes off */
        for (int ch = 0; ch < 16; ch++) {
            uint8_t msg[3] = { 0xB0 | ch, 123, 0 };
            libremidi_midi_out_send_message(default_midi_out, msg, 3);
        }
        libremidi_midi_out_free(default_midi_out);
        default_midi_out = NULL;
    }
    midi_cleanup_observer();
}
