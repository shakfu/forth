/*
 * lua_midi - Lua-based MIDI language using Lua 5.5
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#include "lua.h"

#ifdef USE_READLINE
/* ============================================================================
 * Readline autocomplete
 * ============================================================================ */

/* Lua keywords */
static const char* lua_keywords[] = {
    "and", "break", "do", "else", "elseif", "end", "false", "for",
    "function", "goto", "if", "in", "local", "nil", "not", "or",
    "repeat", "return", "then", "true", "until", "while",
    NULL
};

/* Lua standard library functions */
static const char* lua_stdlib[] = {
    "assert", "collectgarbage", "dofile", "error", "getmetatable",
    "ipairs", "load", "loadfile", "next", "pairs", "pcall", "print",
    "rawequal", "rawget", "rawlen", "rawset", "require", "select",
    "setmetatable", "tonumber", "tostring", "type", "xpcall",
    "coroutine", "debug", "io", "math", "os", "package", "string", "table", "utf8",
    NULL
};

/* MIDI module functions */
static const char* midi_functions[] = {
    "midi.open", "midi.list_ports", "midi.note", "midi.sleep",
    "midi.build_scale", "midi.scale_degree", "midi.in_scale", "midi.quantize",
    "midi.cents_to_note", "midi.help",
    "midi.transpose", "midi.octave_up", "midi.octave_down",
    "midi.major", "midi.minor", "midi.dim", "midi.aug",
    "midi.dom7", "midi.maj7", "midi.min7",
    "midi.set_tempo", "midi.get_tempo", "midi.bpm", "midi.dotted", "midi.rest",
    "midi.scales", "midi.scales_cents",
    NULL
};

/* MidiOut methods */
static const char* midiout_methods[] = {
    ":note", ":chord", ":arpeggio", ":note_on", ":note_off",
    ":cc", ":program", ":pitch_bend", ":all_notes_off", ":close", ":is_open",
    NULL
};

/* Prelude convenience functions */
static const char* prelude_functions[] = {
    "open", "close", "n", "ch", "arp", "help", "quit", "exit",
    "scale", "degree", "in_scale", "quantize",
    "major", "minor", "dim", "aug", "dom7", "maj7", "min7",
    "transpose", "octave_up", "octave_down",
    "dotted", "rest", "sleep",
    NULL
};

/* Dynamics */
static const char* dynamics[] = {
    "ppp", "pp", "p", "mp", "mf", "f", "ff", "fff",
    "midi.ppp", "midi.pp", "midi.p", "midi.mp", "midi.mf", "midi.f", "midi.ff", "midi.fff",
    NULL
};

/* Durations */
static const char* durations[] = {
    "whole", "half", "quarter", "eighth", "sixteenth",
    "midi.whole", "midi.half", "midi.quarter", "midi.eighth", "midi.sixteenth",
    NULL
};

/* Pitch names (c0-c8 with sharps and flats) */
static const char* pitch_names[] = {
    "c0", "cs0", "db0", "d0", "ds0", "eb0", "e0", "f0", "fs0", "gb0", "g0", "gs0", "ab0", "a0", "as0", "bb0", "b0",
    "c1", "cs1", "db1", "d1", "ds1", "eb1", "e1", "f1", "fs1", "gb1", "g1", "gs1", "ab1", "a1", "as1", "bb1", "b1",
    "c2", "cs2", "db2", "d2", "ds2", "eb2", "e2", "f2", "fs2", "gb2", "g2", "gs2", "ab2", "a2", "as2", "bb2", "b2",
    "c3", "cs3", "db3", "d3", "ds3", "eb3", "e3", "f3", "fs3", "gb3", "g3", "gs3", "ab3", "a3", "as3", "bb3", "b3",
    "c4", "cs4", "db4", "d4", "ds4", "eb4", "e4", "f4", "fs4", "gb4", "g4", "gs4", "ab4", "a4", "as4", "bb4", "b4",
    "c5", "cs5", "db5", "d5", "ds5", "eb5", "e5", "f5", "fs5", "gb5", "g5", "gs5", "ab5", "a5", "as5", "bb5", "b5",
    "c6", "cs6", "db6", "d6", "ds6", "eb6", "e6", "f6", "fs6", "gb6", "g6", "gs6", "ab6", "a6", "as6", "bb6", "b6",
    "c7", "cs7", "db7", "d7", "ds7", "eb7", "e7", "f7", "fs7", "gb7", "g7", "gs7", "ab7", "a7", "as7", "bb7", "b7",
    "c8", "cs8", "db8", "d8", "ds8", "eb8", "e8", "f8", "fs8", "gb8", "g8", "gs8", "ab8", "a8", "as8", "bb8", "b8",
    NULL
};

/* Scale names (for midi.scales table) */
static const char* scale_names[] = {
    "major", "ionian", "dorian", "phrygian", "lydian", "mixolydian",
    "minor", "aeolian", "locrian", "harmonic_minor", "melodic_minor",
    "pentatonic", "pentatonic_major", "pentatonic_minor", "blues",
    "whole_tone", "chromatic", "diminished_hw", "diminished_wh", "augmented",
    "bebop_dominant", "bebop_major", "bebop_minor",
    "hungarian_minor", "double_harmonic", "neapolitan_major", "neapolitan_minor",
    "phrygian_dominant", "persian", "altered", "enigmatic",
    "hirajoshi", "in_sen", "iwato", "kumoi",
    "egyptian", "romanian_minor", "spanish_8_tone",
    "maqam_hijaz", "maqam_nahawand", "maqam_nikriz",
    "raga_bhairav", "raga_todi", "raga_marwa",
    NULL
};

/* Generator function for readline completion */
static char* completion_generator(const char* text, int state) {
    static int kw_idx, stdlib_idx, midi_idx, method_idx, prelude_idx;
    static int dyn_idx, dur_idx, pitch_idx, scale_idx;
    static size_t len;

    if (state == 0) {
        kw_idx = 0;
        stdlib_idx = 0;
        midi_idx = 0;
        method_idx = 0;
        prelude_idx = 0;
        dyn_idx = 0;
        dur_idx = 0;
        pitch_idx = 0;
        scale_idx = 0;
        len = strlen(text);
    }

    /* Search Lua keywords */
    while (lua_keywords[kw_idx] != NULL) {
        const char* name = lua_keywords[kw_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search Lua stdlib */
    while (lua_stdlib[stdlib_idx] != NULL) {
        const char* name = lua_stdlib[stdlib_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search MIDI functions */
    while (midi_functions[midi_idx] != NULL) {
        const char* name = midi_functions[midi_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search MidiOut methods */
    while (midiout_methods[method_idx] != NULL) {
        const char* name = midiout_methods[method_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search prelude functions */
    while (prelude_functions[prelude_idx] != NULL) {
        const char* name = prelude_functions[prelude_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search dynamics */
    while (dynamics[dyn_idx] != NULL) {
        const char* name = dynamics[dyn_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search durations */
    while (durations[dur_idx] != NULL) {
        const char* name = durations[dur_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search pitch names */
    while (pitch_names[pitch_idx] != NULL) {
        const char* name = pitch_names[pitch_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search scale names */
    while (scale_names[scale_idx] != NULL) {
        const char* name = scale_names[scale_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    return NULL;
}

/* Custom completion function */
static char** lua_completion(const char* text, int start, int end) {
    (void)start;
    (void)end;

    /* Disable default filename completion */
    rl_attempted_completion_over = 1;

    return rl_completion_matches(text, completion_generator);
}

/* Initialize readline completion */
static void init_readline_completion(void) {
    rl_attempted_completion_function = lua_completion;
    /* Lua uses parens, brackets, and dots as word boundaries */
    rl_completer_word_break_characters = " \t\n()[]{}:.,;'\"";
}
#endif /* USE_READLINE */

#include "lauxlib.h"
#include "lualib.h"

/* External functions from midi_module.c */
extern int luaopen_midi(lua_State *L);
extern void lua_midi_cleanup(void);

static void print_usage(const char *prog) {
    fprintf(stderr, "Usage: %s [options] [file.lua]\n", prog);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -e EXPR    Execute Lua statement\n");
    fprintf(stderr, "  --version  Show version\n");
    fprintf(stderr, "  --help     Show this help\n");
    fprintf(stderr, "\nWithout arguments, starts an interactive REPL.\n");
}

static void print_error(lua_State *L) {
    const char *msg = lua_tostring(L, -1);
    if (msg) {
        fprintf(stderr, "Error: %s\n", msg);
    }
    lua_pop(L, 1);
}

static int run_string(lua_State *L, const char *code) {
    int status = luaL_loadstring(L, code);
    if (status == LUA_OK) {
        status = lua_pcall(L, 0, LUA_MULTRET, 0);
    }
    if (status != LUA_OK) {
        print_error(L);
        return 1;
    }
    /* Print results if any */
    int n = lua_gettop(L);
    if (n > 0) {
        luaL_checkstack(L, LUA_MINSTACK, "too many results to print");
        lua_getglobal(L, "print");
        lua_insert(L, 1);
        if (lua_pcall(L, n, 0, 0) != LUA_OK) {
            fprintf(stderr, "error calling 'print' (%s)\n", lua_tostring(L, -1));
        }
    }
    return 0;
}

static int run_file(lua_State *L, const char *filename) {
    int status = luaL_loadfile(L, filename);
    if (status == LUA_OK) {
        status = lua_pcall(L, 0, LUA_MULTRET, 0);
    }
    if (status != LUA_OK) {
        print_error(L);
        return 1;
    }
    return 0;
}

static void repl(lua_State *L) {
    printf("lua_midi - Lua MIDI language (Lua 5.5)\n");
    printf("Type help() for available functions, quit() or Ctrl-D to exit\n\n");

#ifdef USE_READLINE
    init_readline_completion();
#endif

    while (1) {
#ifdef USE_READLINE
        char *line = readline("> ");
        if (!line) {
            printf("\n");
            break;  /* EOF or error */
        }

        /* Skip empty lines */
        if (line[0] == '\0') {
            free(line);
            continue;
        }

        /* Add to history */
        add_history(line);

        /* Check for quit */
        if (strcmp(line, "quit()") == 0 || strcmp(line, "exit()") == 0) {
            free(line);
            break;
        }

        /* Copy to buffer for processing */
        char buffer[4096];
        strncpy(buffer, line, sizeof(buffer) - 1);
        buffer[sizeof(buffer) - 1] = '\0';
        free(line);
#else
        char buffer[4096];
        printf("> ");
        fflush(stdout);

        if (!fgets(buffer, sizeof(buffer), stdin)) {
            printf("\n");
            break;  /* EOF or error */
        }

        /* Skip empty lines */
        size_t len = strlen(buffer);
        if (len == 0 || (len == 1 && buffer[0] == '\n')) {
            continue;
        }

        /* Remove trailing newline */
        if (buffer[len - 1] == '\n') {
            buffer[len - 1] = '\0';
        }

        /* Check for quit */
        if (strcmp(buffer, "quit()") == 0 || strcmp(buffer, "exit()") == 0) {
            break;
        }
#endif

        /* Try as expression first (prepend return) */
        char expr_buffer[4096 + 16];
        snprintf(expr_buffer, sizeof(expr_buffer), "return %s", buffer);

        int status = luaL_loadstring(L, expr_buffer);
        if (status != LUA_OK) {
            /* Failed as expression, try as statement */
            lua_pop(L, 1);  /* pop error message */
            status = luaL_loadstring(L, buffer);
        }

        if (status == LUA_OK) {
            status = lua_pcall(L, 0, LUA_MULTRET, 0);
        }

        if (status != LUA_OK) {
            print_error(L);
        } else {
            /* Print results if any */
            int n = lua_gettop(L);
            if (n > 0) {
                for (int i = 1; i <= n; i++) {
                    if (lua_isnil(L, i)) {
                        printf("nil");
                    } else if (lua_isboolean(L, i)) {
                        printf("%s", lua_toboolean(L, i) ? "true" : "false");
                    } else if (lua_isnumber(L, i)) {
                        if (lua_isinteger(L, i)) {
                            printf("%lld", (long long)lua_tointeger(L, i));
                        } else {
                            printf("%g", lua_tonumber(L, i));
                        }
                    } else if (lua_isstring(L, i)) {
                        printf("%s", lua_tostring(L, i));
                    } else if (lua_istable(L, i)) {
                        /* Print table contents */
                        printf("{");
                        int first = 1;
                        lua_pushnil(L);
                        while (lua_next(L, i) != 0) {
                            if (!first) printf(", ");
                            first = 0;
                            /* Key at -2, value at -1 */
                            if (lua_isinteger(L, -2)) {
                                printf("[%lld]=", (long long)lua_tointeger(L, -2));
                            } else if (lua_isstring(L, -2)) {
                                printf("%s=", lua_tostring(L, -2));
                            }
                            if (lua_isnumber(L, -1)) {
                                if (lua_isinteger(L, -1)) {
                                    printf("%lld", (long long)lua_tointeger(L, -1));
                                } else {
                                    printf("%g", lua_tonumber(L, -1));
                                }
                            } else if (lua_isstring(L, -1)) {
                                printf("\"%s\"", lua_tostring(L, -1));
                            } else {
                                printf("%s", luaL_typename(L, -1));
                            }
                            lua_pop(L, 1);  /* pop value, keep key */
                        }
                        printf("}");
                    } else {
                        /* For other types, use tostring */
                        lua_getglobal(L, "tostring");
                        lua_pushvalue(L, i);
                        if (lua_pcall(L, 1, 1, 0) == LUA_OK) {
                            printf("%s", lua_tostring(L, -1));
                            lua_pop(L, 1);
                        } else {
                            printf("%s: %p", luaL_typename(L, i), lua_topointer(L, i));
                            lua_pop(L, 1);
                        }
                    }
                    if (i < n) printf("\t");
                }
                printf("\n");
                lua_settop(L, 0);  /* clear stack */
            }
        }
    }
}

int main(int argc, char **argv) {
    lua_State *L;

    /* Initialize Lua */
    L = luaL_newstate();
    if (!L) {
        fprintf(stderr, "Failed to initialize Lua\n");
        return 1;
    }

    /* Open standard libraries */
    luaL_openlibs(L);

    /* Register MIDI module */
    luaL_requiref(L, "midi", luaopen_midi, 1);
    lua_pop(L, 1);  /* remove module from stack */

    /* Process arguments */
    if (argc >= 2) {
        for (int i = 1; i < argc; i++) {
            if (strcmp(argv[i], "-e") == 0) {
                /* Execute statement */
                if (i + 1 >= argc) {
                    fprintf(stderr, "Error: -e requires an expression\n");
                    lua_midi_cleanup();
                    lua_close(L);
                    return 1;
                }
                if (run_string(L, argv[++i]) != 0) {
                    lua_midi_cleanup();
                    lua_close(L);
                    return 1;
                }
            } else if (strcmp(argv[i], "--version") == 0) {
                printf("lua_midi using %s\n", LUA_VERSION);
            } else if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
                print_usage(argv[0]);
            } else {
                /* Load and run file */
                if (run_file(L, argv[i]) != 0) {
                    lua_midi_cleanup();
                    lua_close(L);
                    return 1;
                }
            }
        }
    } else {
        /* Interactive REPL */
        repl(L);
    }

    /* Cleanup */
    lua_midi_cleanup();
    lua_close(L);

    return 0;
}
