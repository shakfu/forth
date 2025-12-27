/*
 * MIDI module for Lua 5.5
 * Provides Lua bindings for MIDI output using libremidi
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include "lua.h"
#include "lauxlib.h"
#include <libremidi/libremidi-c.h>

#define MAX_PORTS 64
#define MIDI_OUT_MT "MidiOut"

/* Global state */
static libremidi_midi_observer_handle* midi_observer = NULL;
static libremidi_midi_out_port* out_ports[MAX_PORTS];
static int out_port_count = 0;
static lua_State *global_L = NULL;

/* Default MIDI output for REPL convenience functions */
static libremidi_midi_out_handle* default_midi_out = NULL;

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

    /* Sleep */
    usleep(duration * 1000);

    /* Note off */
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[2] = 0;
    libremidi_midi_out_send_message(data->handle, msg, 3);

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
    }

    /* Sleep */
    usleep(duration * 1000);

    /* Note off for all */
    for (int i = 0; i < count; i++) {
        uint8_t msg[3] = {
            0x80 | ((channel - 1) & 0x0F),
            pitches[i] & 0x7F,
            0
        };
        libremidi_midi_out_send_message(data->handle, msg, 3);
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

            usleep(duration * 1000);

            /* Note off */
            msg[0] = 0x80 | ((channel - 1) & 0x0F);
            msg[2] = 0;
            libremidi_midi_out_send_message(data->handle, msg, 3);
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
        usleep(ms * 1000);
    }
    return 0;
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
    printf("Constants: midi.c0-c8, midi.cs0-cs8, midi.d0-d8, etc.\n");
    printf("Dynamics: midi.ppp, pp, p, mp, mf, f, ff, fff\n");
    printf("Durations: midi.whole, half, quarter, eighth, sixteenth\n");
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
    {NULL, NULL}
};

static const luaL_Reg midi_funcs[] = {
    {"list_ports", l_list_ports},
    {"open", l_open},
    {"note", l_parse_note},
    {"sleep", l_sleep},
    {"transpose", l_transpose},
    {"octave_up", l_octave_up},
    {"octave_down", l_octave_down},
    {"major", l_major},
    {"minor", l_minor},
    {"dim", l_dim},
    {"aug", l_aug},
    {"dom7", l_dom7},
    {"maj7", l_maj7},
    {"min7", l_min7},
    {"help", l_help},
    {NULL, NULL}
};

/* Lua prelude - defines constants and convenience functions */
static const char *lua_prelude =
    "-- Pitch constants\n"
    "for oct = 0, 8 do\n"
    "  midi['c'..oct] = 12 + oct * 12\n"
    "  midi['cs'..oct] = 13 + oct * 12\n"
    "  midi['d'..oct] = 14 + oct * 12\n"
    "  midi['ds'..oct] = 15 + oct * 12\n"
    "  midi['e'..oct] = 16 + oct * 12\n"
    "  midi['f'..oct] = 17 + oct * 12\n"
    "  midi['fs'..oct] = 18 + oct * 12\n"
    "  midi['g'..oct] = 19 + oct * 12\n"
    "  midi['gs'..oct] = 20 + oct * 12\n"
    "  midi['a'..oct] = 21 + oct * 12\n"
    "  midi['as'..oct] = 22 + oct * 12\n"
    "  midi['b'..oct] = 23 + oct * 12\n"
    "  -- Flat aliases\n"
    "  midi['db'..oct] = 13 + oct * 12\n"
    "  midi['eb'..oct] = 15 + oct * 12\n"
    "  midi['gb'..oct] = 18 + oct * 12\n"
    "  midi['ab'..oct] = 20 + oct * 12\n"
    "  midi['bb'..oct] = 22 + oct * 12\n"
    "end\n"
    "\n"
    "-- Dynamics (velocity values)\n"
    "midi.ppp = 16\n"
    "midi.pp = 33\n"
    "midi.p = 49\n"
    "midi.mp = 64\n"
    "midi.mf = 80\n"
    "midi.f = 96\n"
    "midi.ff = 112\n"
    "midi.fff = 127\n"
    "\n"
    "-- Duration constants (ms at 120 BPM)\n"
    "midi.whole = 2000\n"
    "midi.half = 1000\n"
    "midi.quarter = 500\n"
    "midi.eighth = 250\n"
    "midi.sixteenth = 125\n"
    "\n"
    "-- Tempo state\n"
    "midi._tempo = 120\n"
    "\n"
    "function midi.set_tempo(bpm)\n"
    "  midi._tempo = bpm\n"
    "  local q = math.floor(60000 / bpm)\n"
    "  midi.quarter = q\n"
    "  midi.whole = q * 4\n"
    "  midi.half = q * 2\n"
    "  midi.eighth = math.floor(q / 2)\n"
    "  midi.sixteenth = math.floor(q / 4)\n"
    "end\n"
    "\n"
    "function midi.get_tempo()\n"
    "  return midi._tempo\n"
    "end\n"
    "\n"
    "function midi.bpm(tempo)\n"
    "  return math.floor(60000 / tempo)\n"
    "end\n"
    "\n"
    "function midi.dotted(dur)\n"
    "  return math.floor(dur * 1.5)\n"
    "end\n"
    "\n"
    "function midi.rest(dur)\n"
    "  midi.sleep(dur or midi.quarter)\n"
    "end\n"
    "\n"
    "-- Global aliases for concise syntax\n"
    "-- Pitch constants as globals\n"
    "for oct = 0, 8 do\n"
    "  _G['c'..oct] = midi['c'..oct]\n"
    "  _G['cs'..oct] = midi['cs'..oct]\n"
    "  _G['d'..oct] = midi['d'..oct]\n"
    "  _G['ds'..oct] = midi['ds'..oct]\n"
    "  _G['e'..oct] = midi['e'..oct]\n"
    "  _G['f'..oct] = midi['f'..oct]\n"
    "  _G['fs'..oct] = midi['fs'..oct]\n"
    "  _G['g'..oct] = midi['g'..oct]\n"
    "  _G['gs'..oct] = midi['gs'..oct]\n"
    "  _G['a'..oct] = midi['a'..oct]\n"
    "  _G['as'..oct] = midi['as'..oct]\n"
    "  _G['b'..oct] = midi['b'..oct]\n"
    "  _G['db'..oct] = midi['db'..oct]\n"
    "  _G['eb'..oct] = midi['eb'..oct]\n"
    "  _G['gb'..oct] = midi['gb'..oct]\n"
    "  _G['ab'..oct] = midi['ab'..oct]\n"
    "  _G['bb'..oct] = midi['bb'..oct]\n"
    "end\n"
    "\n"
    "-- Dynamics as globals\n"
    "ppp, pp, p, mp, mf, f, ff, fff = midi.ppp, midi.pp, midi.p, midi.mp, midi.mf, midi.f, midi.ff, midi.fff\n"
    "\n"
    "-- Durations as globals\n"
    "whole, half, quarter, eighth, sixteenth = midi.whole, midi.half, midi.quarter, midi.eighth, midi.sixteenth\n"
    "\n"
    "-- Chord builders as globals\n"
    "major, minor, dim, aug, dom7, maj7, min7 = midi.major, midi.minor, midi.dim, midi.aug, midi.dom7, midi.maj7, midi.min7\n"
    "\n"
    "-- Other utilities as globals\n"
    "transpose, octave_up, octave_down = midi.transpose, midi.octave_up, midi.octave_down\n"
    "dotted, rest, sleep = midi.dotted, midi.rest, midi.sleep\n"
    "\n"
    "-- Global help function\n"
    "function help() midi.help() end\n"
    "\n"
    "-- REPL convenience: global midi output\n"
    "midi._out = nil\n"
    "\n"
    "function open(arg)\n"
    "  if midi._out then midi._out:close() end\n"
    "  midi._out = midi.open(arg)\n"
    "  return midi._out\n"
    "end\n"
    "\n"
    "function close()\n"
    "  if midi._out then\n"
    "    midi._out:close()\n"
    "    midi._out = nil\n"
    "  end\n"
    "end\n"
    "\n"
    "function n(pitch, vel, dur, ch)\n"
    "  if not midi._out then error('No MIDI port open. Use open() first.') end\n"
    "  midi._out:note(pitch, vel or midi.mf, dur or midi.quarter, ch or 1)\n"
    "end\n"
    "\n"
    "function ch(pitches, vel, dur, channel)\n"
    "  if not midi._out then error('No MIDI port open. Use open() first.') end\n"
    "  midi._out:chord(pitches, vel or midi.mf, dur or midi.quarter, channel or 1)\n"
    "end\n"
    "\n"
    "function arp(pitches, vel, dur, channel)\n"
    "  if not midi._out then error('No MIDI port open. Use open() first.') end\n"
    "  midi._out:arpeggio(pitches, vel or midi.mf, dur or midi.eighth, channel or 1)\n"
    "end\n"
;

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
    if (luaL_dostring(L, lua_prelude) != LUA_OK) {
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
