#include <stdbool.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#include "pocketpy.h"

#ifdef USE_READLINE
/* ============================================================================
 * Readline autocomplete
 * ============================================================================ */

/* Python keywords */
static const char* python_keywords[] = {
    "False", "None", "True", "and", "as", "assert", "async", "await",
    "break", "class", "continue", "def", "del", "elif", "else", "except",
    "finally", "for", "from", "global", "if", "import", "in", "is",
    "lambda", "nonlocal", "not", "or", "pass", "raise", "return", "try",
    "while", "with", "yield",
    NULL
};

/* Python builtins */
static const char* python_builtins[] = {
    "abs", "all", "any", "bin", "bool", "bytes", "callable", "chr",
    "dict", "dir", "divmod", "enumerate", "eval", "exec", "exit",
    "filter", "float", "format", "getattr", "globals", "hasattr", "hash",
    "help", "hex", "id", "input", "int", "isinstance", "iter", "len",
    "list", "locals", "map", "max", "min", "next", "object", "oct",
    "open", "ord", "pow", "print", "range", "repr", "reversed", "round",
    "set", "setattr", "slice", "sorted", "str", "sum", "super", "tuple",
    "type", "vars", "zip",
    NULL
};

/* MIDI module functions */
static const char* midi_functions[] = {
    "midi.open", "midi.list_ports", "midi.note", "midi.help",
    "midi.build_scale", "midi.scale_degree", "midi.in_scale", "midi.quantize_to_scale",
    "midi.scale", "midi.degree", "midi.in_scale_named", "midi.quantize",
    "midi.cents_to_note",
    "midi.transpose", "midi.octave_up", "midi.octave_down",
    "midi.major", "midi.minor", "midi.dim", "midi.aug",
    "midi.dom7", "midi.maj7", "midi.min7",
    "midi.set_tempo", "midi.get_tempo", "midi.bpm", "midi.dotted",
    "midi.sleep", "midi.rest",
    "midi.scales", "midi.scales_cents",
    "midi.MidiOut",
    NULL
};

/* MidiOut methods */
static const char* midiout_methods[] = {
    ".note", ".chord", ".arpeggio", ".note_on", ".note_off",
    ".cc", ".program_change", ".pitch_bend", ".all_notes_off",
    ".close", ".is_open",
    ".modulation", ".volume", ".pan", ".sustain",
    NULL
};

/* Dynamics */
static const char* dynamics[] = {
    "midi.ppp", "midi.pp", "midi.p", "midi.mp", "midi.mf", "midi.f", "midi.ff", "midi.fff",
    NULL
};

/* Durations */
static const char* durations[] = {
    "midi.whole", "midi.half", "midi.quarter", "midi.eighth", "midi.sixteenth",
    NULL
};

/* Pitch names (c0-c8 with sharps and flats) - midi.prefix */
static const char* pitch_names[] = {
    "midi.c0", "midi.cs0", "midi.db0", "midi.d0", "midi.ds0", "midi.eb0", "midi.e0", "midi.f0", "midi.fs0", "midi.gb0", "midi.g0", "midi.gs0", "midi.ab0", "midi.a0", "midi.as0", "midi.bb0", "midi.b0",
    "midi.c1", "midi.cs1", "midi.db1", "midi.d1", "midi.ds1", "midi.eb1", "midi.e1", "midi.f1", "midi.fs1", "midi.gb1", "midi.g1", "midi.gs1", "midi.ab1", "midi.a1", "midi.as1", "midi.bb1", "midi.b1",
    "midi.c2", "midi.cs2", "midi.db2", "midi.d2", "midi.ds2", "midi.eb2", "midi.e2", "midi.f2", "midi.fs2", "midi.gb2", "midi.g2", "midi.gs2", "midi.ab2", "midi.a2", "midi.as2", "midi.bb2", "midi.b2",
    "midi.c3", "midi.cs3", "midi.db3", "midi.d3", "midi.ds3", "midi.eb3", "midi.e3", "midi.f3", "midi.fs3", "midi.gb3", "midi.g3", "midi.gs3", "midi.ab3", "midi.a3", "midi.as3", "midi.bb3", "midi.b3",
    "midi.c4", "midi.cs4", "midi.db4", "midi.d4", "midi.ds4", "midi.eb4", "midi.e4", "midi.f4", "midi.fs4", "midi.gb4", "midi.g4", "midi.gs4", "midi.ab4", "midi.a4", "midi.as4", "midi.bb4", "midi.b4",
    "midi.c5", "midi.cs5", "midi.db5", "midi.d5", "midi.ds5", "midi.eb5", "midi.e5", "midi.f5", "midi.fs5", "midi.gb5", "midi.g5", "midi.gs5", "midi.ab5", "midi.a5", "midi.as5", "midi.bb5", "midi.b5",
    "midi.c6", "midi.cs6", "midi.db6", "midi.d6", "midi.ds6", "midi.eb6", "midi.e6", "midi.f6", "midi.fs6", "midi.gb6", "midi.g6", "midi.gs6", "midi.ab6", "midi.a6", "midi.as6", "midi.bb6", "midi.b6",
    "midi.c7", "midi.cs7", "midi.db7", "midi.d7", "midi.ds7", "midi.eb7", "midi.e7", "midi.f7", "midi.fs7", "midi.gb7", "midi.g7", "midi.gs7", "midi.ab7", "midi.a7", "midi.as7", "midi.bb7", "midi.b7",
    "midi.c8", "midi.cs8", "midi.db8", "midi.d8", "midi.ds8", "midi.eb8", "midi.e8", "midi.f8", "midi.fs8", "midi.gb8", "midi.g8", "midi.gs8", "midi.ab8", "midi.a8", "midi.as8", "midi.bb8", "midi.b8",
    NULL
};

/* Scale constants */
static const char* scale_names[] = {
    "midi.SCALE_MAJOR", "midi.SCALE_MINOR", "midi.SCALE_DORIAN", "midi.SCALE_PHRYGIAN",
    "midi.SCALE_LYDIAN", "midi.SCALE_MIXOLYDIAN", "midi.SCALE_LOCRIAN",
    "midi.SCALE_HARMONIC_MINOR", "midi.SCALE_MELODIC_MINOR",
    "midi.SCALE_PENTATONIC", "midi.SCALE_PENTATONIC_MINOR", "midi.SCALE_BLUES",
    "midi.SCALE_WHOLE_TONE", "midi.SCALE_CHROMATIC",
    "midi.SCALE_DIMINISHED_HW", "midi.SCALE_DIMINISHED_WH", "midi.SCALE_AUGMENTED",
    "midi.SCALE_BEBOP_DOMINANT", "midi.SCALE_BEBOP_MAJOR", "midi.SCALE_BEBOP_MINOR",
    "midi.SCALE_HUNGARIAN_MINOR", "midi.SCALE_DOUBLE_HARMONIC",
    "midi.SCALE_HIRAJOSHI", "midi.SCALE_IN_SEN", "midi.SCALE_IWATO", "midi.SCALE_KUMOI",
    NULL
};

/* CC constants */
static const char* cc_constants[] = {
    "midi.CC_MODULATION", "midi.CC_BREATH", "midi.CC_VOLUME", "midi.CC_PAN",
    "midi.CC_EXPRESSION", "midi.CC_SUSTAIN", "midi.CC_REVERB", "midi.CC_CHORUS",
    NULL
};

/* Generator function for readline completion */
static char* completion_generator(const char* text, int state) {
    static int kw_idx, builtin_idx, midi_idx, method_idx;
    static int dyn_idx, dur_idx, pitch_idx, scale_idx, cc_idx;
    static size_t len;

    if (state == 0) {
        kw_idx = 0;
        builtin_idx = 0;
        midi_idx = 0;
        method_idx = 0;
        dyn_idx = 0;
        dur_idx = 0;
        pitch_idx = 0;
        scale_idx = 0;
        cc_idx = 0;
        len = strlen(text);
    }

    /* Search Python keywords */
    while (python_keywords[kw_idx] != NULL) {
        const char* name = python_keywords[kw_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search Python builtins */
    while (python_builtins[builtin_idx] != NULL) {
        const char* name = python_builtins[builtin_idx++];
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

    /* Search CC constants */
    while (cc_constants[cc_idx] != NULL) {
        const char* name = cc_constants[cc_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    return NULL;
}

/* Custom completion function */
static char** pktpy_completion(const char* text, int start, int end) {
    (void)start;
    (void)end;

    /* Disable default filename completion */
    rl_attempted_completion_over = 1;

    return rl_completion_matches(text, completion_generator);
}

/* Initialize readline completion */
static void init_readline_completion(void) {
    rl_attempted_completion_function = pktpy_completion;
    /* Python uses parens, brackets, and dots as word boundaries */
    rl_completer_word_break_characters = " \t\n()[]{}:.,;'\"=+-*/<>";
}
#endif /* USE_READLINE */

// MIDI module declarations
extern void pk_midi_module_init(void);
extern void pk_midi_module_cleanup(void);

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

static char* read_file(const char* path) {
    FILE* file = fopen(path, "rb");
    if(file == NULL) {
        printf("Error: file not found\n");
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    char* buffer = PK_MALLOC(size + 1);
    size = fread(buffer, 1, size, file);
    buffer[size] = 0;
    return buffer;
}

static char buf[2048];

int main(int argc, char** argv) {
#if _WIN32
    SetConsoleCP(CP_UTF8);
    SetConsoleOutputCP(CP_UTF8);
#endif

    bool profile = false;
    bool debug = false;
    const char* filename = NULL;

    for(int i = 1; i < argc; i++) {
        if(strcmp(argv[i], "--profile") == 0) {
            profile = true;
            continue;
        }
        if(strcmp(argv[i], "--debug") == 0) {
            debug = true;
            continue;
        }
        if(filename == NULL) {
            filename = argv[i];
            continue;
        }
        printf("Usage: pocketpy [--profile] [--debug] filename\n");
    }

    if(debug && profile) {
        printf("Error: --debug and --profile cannot be used together.\n");
        return 1;
    }

    py_initialize();
    pk_midi_module_init();
    py_sys_setargv(argc, argv);

    if(filename == NULL) {
        if(profile) printf("Warning: --profile is ignored in REPL mode.\n");
        if(debug) printf("Warning: --debug is ignored in REPL mode.\n");

        printf("pktpy_midi - pocketpy " PK_VERSION " with MIDI support\n");
        printf("[%d bit] on %s", (int)(sizeof(void*) * 8), PY_SYS_PLATFORM_STRING);
#ifndef NDEBUG
        printf(" (DEBUG)");
#endif
        printf("\n");
        printf("Type \"exit()\" to exit. Use \"import midi\" to access MIDI.\n");

#ifdef USE_READLINE
        init_readline_completion();

        while(true) {
            char *line = readline(">>> ");
            if(line == NULL) {  // Ctrl-D (i.e. EOF)
                printf("\n");
                break;
            }

            /* Skip empty lines */
            if(line[0] == '\0') {
                free(line);
                continue;
            }

            /* Add to history */
            add_history(line);

            /* Copy to buffer for execution */
            strncpy(buf, line, sizeof(buf) - 1);
            buf[sizeof(buf) - 1] = '\0';
            free(line);

            py_StackRef p0 = py_peek(0);
            if(!py_exec(buf, "<stdin>", SINGLE_MODE, NULL)) {
                py_printexc();
                py_clearexc(p0);
            }
        }
#else
        while(true) {
            int size = py_replinput(buf, sizeof(buf));
            if(size == -1) {  // Ctrl-D (i.e. EOF)
                printf("\n");
                break;
            }
            assert(size < sizeof(buf));
            if(size >= 0) {
                py_StackRef p0 = py_peek(0);
                if(!py_exec(buf, "<stdin>", SINGLE_MODE, NULL)) {
                    py_printexc();
                    py_clearexc(p0);
                }
            }
        }
#endif
    } else {
        if(profile) py_profiler_begin();
        if(debug) py_debugger_waitforattach("127.0.0.1", 6110);

        char* source = read_file(filename);
        if(source) {
            if(!py_exec(source, filename, EXEC_MODE, NULL)) py_printexc();

            if(profile) {
                char* json_report = py_profiler_report();
                FILE* report_file = fopen("profiler_report.json", "w");
                if(report_file) {
                    fprintf(report_file, "%s", json_report);
                    fclose(report_file);
                }
                PK_FREE(json_report);
            }

            PK_FREE(source);
        }
    }

    int code = py_checkexc() ? 1 : 0;
    pk_midi_module_cleanup();
    py_finalize();

    if(debug) py_debugger_exit(code);
    return code;
}
