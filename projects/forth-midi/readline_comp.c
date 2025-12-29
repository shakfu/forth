/* readline_comp.c - Readline completion for MIDI Forth interpreter */

#include "forth_midi.h"
#include <readline/readline.h>
#include <readline/history.h>

/* Static pitch names for completion */
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

/* Additional built-in words not in dictionary */
static const char* builtin_words[] = {
    "quit", "help", "if", "else", "then", ":", ";", "load", "words",
    "rec", "stop", "save", "capture", "save-midi", "midi-open-as",
    NULL
};

/* Generator function for readline completion */
static char* completion_generator(const char* text, int state) {
    static int dict_index;
    static int pitch_index;
    static int builtin_index;
    static size_t len;

    if (state == 0) {
        dict_index = 0;
        pitch_index = 0;
        builtin_index = 0;
        len = strlen(text);
    }

    /* Search dictionary words */
    while (dict_index < dict_count) {
        const char* name = dictionary[dict_index].name;
        dict_index++;
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search pitch names */
    while (pitch_names[pitch_index] != NULL) {
        const char* name = pitch_names[pitch_index];
        pitch_index++;
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search builtin words */
    while (builtin_words[builtin_index] != NULL) {
        const char* name = builtin_words[builtin_index];
        builtin_index++;
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    return NULL;
}

/* Custom completion function */
static char** forth_completion(const char* text, int start, int end) {
    (void)start;
    (void)end;

    /* Disable default filename completion */
    rl_attempted_completion_over = 1;

    return rl_completion_matches(text, completion_generator);
}

/* Initialize readline completion */
void init_readline_completion(void) {
    rl_attempted_completion_function = forth_completion;
    /* Use space as word break character */
    rl_completer_word_break_characters = " \t\n";
}
