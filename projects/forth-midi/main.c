/* main.c - Main entry point for MIDI Forth interpreter */

#include "forth_midi.h"
#include <readline/readline.h>
#include <readline/history.h>

/* Interactive interpreter loop */
static void interpreter_loop(void) {
    char* input;

    printf("MIDI Forth (type 'help' for commands, 'quit' to exit)\n");

    while (1) {
        input = readline("> ");

        if (input == NULL) {
            /* EOF (Ctrl-D) */
            printf("\n");
            break;
        }

        /* Skip empty lines */
        if (input[0] == '\0') {
            free(input);
            continue;
        }

        /* Add to history */
        add_history(input);

        if (strcmp(input, "quit") == 0) {
            free(input);
            break;
        }

        if (strcmp(input, "help") == 0) {
            op_help(&stack);
            free(input);
            continue;
        }

        /* Record input if recording is active */
        if (recording_active) {
            /* Check if first word is a control command */
            char first_word[32];
            int j = 0;
            const char* p = input;
            while (*p && isspace(*p)) p++;  /* Skip leading whitespace */
            while (*p && !isspace(*p) && j < 31) {
                first_word[j++] = *p++;
            }
            first_word[j] = '\0';

            /* Only record if not a control command */
            if (strcmp(first_word, "rec") != 0 &&
                strcmp(first_word, "stop") != 0 &&
                strcmp(first_word, "save") != 0 &&
                strcmp(first_word, "save-midi") != 0 &&
                strcmp(first_word, "capture") != 0 &&
                strcmp(first_word, "load") != 0) {
                recording_add_line(input);
            }
        }

        interpret(input);
        printf(" ok\n");

        free(input);
    }
}

/* Cleanup function */
static void cleanup(void) {
    midi_cleanup_observer();
    recording_clear();
    capture_clear();

    /* Free block storage */
    for (int i = 0; i < block_count; i++) {
        if (block_storage[i]) {
            free(block_storage[i]);
            block_storage[i] = NULL;
        }
    }

    /* Free user-defined word bodies */
    for (int i = 0; i < dict_count; i++) {
        if (!dictionary[i].is_primitive && dictionary[i].body) {
            free(dictionary[i].body);
            dictionary[i].body = NULL;
        }
    }
}

int main(int argc, char* argv[]) {
    /* Seed random number generator */
    srand((unsigned int)time(NULL));

    /* Initialize stack */
    stack.top = -1;

    /* Initialize dictionary with primitives */
    init_dictionary();

    /* Initialize readline autocomplete */
    init_readline_completion();

    /* If a file is provided as argument, load it */
    if (argc > 1) {
        for (int i = 1; i < argc; i++) {
            load_file(argv[i]);
        }
    }

    /* Start interactive interpreter */
    interpreter_loop();

    /* Cleanup */
    cleanup();

    return 0;
}
