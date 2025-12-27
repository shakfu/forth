/*
 * s7_midi - Scheme-based MIDI language using s7 scheme
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "s7.h"

/* External functions from midi_module.c */
extern void s7_midi_init(s7_scheme *sc);
extern void s7_midi_cleanup(void);

static void print_usage(const char *prog) {
    fprintf(stderr, "Usage: %s [options] [file.scm]\n", prog);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -e EXPR    Evaluate expression and print result\n");
    fprintf(stderr, "  --version  Show version\n");
    fprintf(stderr, "  --help     Show this help\n");
    fprintf(stderr, "\nWithout arguments, starts an interactive REPL.\n");
}

int main(int argc, char **argv) {
    s7_scheme *sc;

    /* Initialize s7 */
    sc = s7_init();
    if (!sc) {
        fprintf(stderr, "Failed to initialize s7\n");
        return 1;
    }

    /* Initialize MIDI module */
    s7_midi_init(sc);

    /* Process arguments */
    if (argc >= 2) {
        for (int i = 1; i < argc; i++) {
            if (strcmp(argv[i], "-e") == 0) {
                /* Evaluate expression */
                if (i + 1 >= argc) {
                    fprintf(stderr, "Error: -e requires an expression\n");
                    s7_midi_cleanup();
                    s7_free(sc);
                    return 1;
                }
                s7_pointer result = s7_eval_c_string(sc, argv[++i]);
                char *str = s7_object_to_c_string(sc, result);
                printf("%s\n", str);
                free(str);
            } else if (strcmp(argv[i], "--version") == 0) {
                printf("s7_midi using s7: %s, %s\n", S7_VERSION, S7_DATE);
            } else if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
                print_usage(argv[0]);
            } else {
                /* Load file */
                if (!s7_load(sc, argv[i])) {
                    fprintf(stderr, "Error: could not load %s\n", argv[i]);
                    s7_midi_cleanup();
                    s7_free(sc);
                    return 1;
                }
            }
        }
    } else {
        /* Interactive REPL */
        printf("s7_midi - Scheme MIDI language\n");
        printf("Type (help) for available functions, (quit) to exit\n\n");

        /* Simple REPL loop */
        char buffer[4096];
        while (1) {
            printf("> ");
            fflush(stdout);

            if (!fgets(buffer, sizeof(buffer), stdin)) {
                break;  /* EOF or error */
            }

            /* Skip empty lines */
            if (buffer[0] == '\n' || buffer[0] == '\0') {
                continue;
            }

            /* Check for quit */
            if (strncmp(buffer, "(quit)", 6) == 0 ||
                strncmp(buffer, "(exit)", 6) == 0) {
                break;
            }

            /* Evaluate and print result */
            s7_pointer result = s7_eval_c_string(sc, buffer);
            if (result != s7_unspecified(sc)) {
                char *str = s7_object_to_c_string(sc, result);
                printf("%s\n", str);
                free(str);
            }
        }
    }

    /* Cleanup */
    s7_midi_cleanup();
    s7_free(sc);

    return 0;
}
