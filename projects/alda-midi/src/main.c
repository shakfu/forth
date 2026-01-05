/**
 * @file main.c
 * @brief Alda-MIDI main executable - CLI, file playback, and REPL.
 */

#include "alda/alda.h"
#include "alda/context.h"
#include "alda/midi_backend.h"
#include "alda/scheduler.h"
#include "alda/interpreter.h"
#include "alda/async.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <readline/readline.h>
#include <readline/history.h>

/* ============================================================================
 * Usage and Help
 * ============================================================================ */

static void print_usage(const char* prog) {
    printf("Usage: %s [options] [file.alda]\n", prog);
    printf("\n");
    printf("Alda music language interpreter with MIDI output.\n");
    printf("If no file is provided, starts an interactive REPL.\n");
    printf("\n");
    printf("Options:\n");
    printf("  -h, --help        Show this help message\n");
    printf("  -v, --verbose     Enable verbose output\n");
    printf("  -l, --list        List available MIDI ports\n");
    printf("  -p, --port N      Use MIDI port N (0-based index)\n");
    printf("  -o, --output NAME Use MIDI port matching NAME\n");
    printf("  --virtual NAME    Create virtual MIDI port with NAME\n");
    printf("  --no-sleep        Disable timing delays (for testing)\n");
    printf("  -s, --sequential  Use sequential playback mode (wait for each input)\n");
    printf("\n");
    printf("By default, connects to the first available MIDI port (or creates a virtual\n");
    printf("port if none exist) and uses concurrent mode for polyphonic playback.\n");
    printf("\n");
    printf("Examples:\n");
    printf("  %s                            Start interactive REPL\n", prog);
    printf("  %s song.alda                  Play an Alda file\n", prog);
    printf("  %s -l                         List MIDI ports\n", prog);
    printf("  %s -p 0 song.alda             Play using port 0\n", prog);
    printf("  %s --virtual iMIDI song.alda  Create virtual port + play song\n", prog);
    printf("\n");
}

static void print_repl_help(void) {
    printf("Alda REPL Commands:\n");
    printf("  help              Show this help\n");
    printf("  quit, exit        Exit the REPL\n");
    printf("  list              List MIDI ports\n");
    printf("  stop              Stop current playback\n");
    printf("  panic             All notes off\n");
    printf("  sequential        Enable sequential mode (wait for each input)\n");
    printf("  concurrent        Enable concurrent mode (default, polyphony)\n");
    printf("\n");
    printf("Alda Syntax Examples:\n");
    printf("  piano:            Select piano instrument\n");
    printf("  c d e f g         Play notes C D E F G\n");
    printf("  c4 d8 e8 f4       Quarter, eighths, quarter\n");
    printf("  c/e/g             Play C major chord\n");
    printf("  (tempo 140)       Set tempo to 140 BPM\n");
    printf("  (mf)              Set dynamics to mezzo-forte\n");
    printf("  o5 c d e          Octave 5, then notes\n");
    printf("  > c < c           Octave up, C, octave down, C\n");
    printf("\n");
}

/* ============================================================================
 * REPL
 * ============================================================================ */

static void repl_loop(AldaContext* ctx) {
    char* input;

    printf("Alda MIDI (type 'help' for commands, 'quit' to exit)\n");
    if (!alda_async_get_concurrent()) {
        printf("Mode: sequential\n");
    }

    while (1) {
        input = readline("alda> ");

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

        /* Handle special commands */
        if (strcmp(input, "quit") == 0 || strcmp(input, "exit") == 0) {
            free(input);
            break;
        }

        if (strcmp(input, "help") == 0) {
            print_repl_help();
            free(input);
            continue;
        }

        if (strcmp(input, "list") == 0) {
            alda_midi_list_ports(ctx);
            free(input);
            continue;
        }

        if (strcmp(input, "stop") == 0) {
            alda_async_stop();
            alda_midi_all_notes_off(ctx);
            printf("Playback stopped\n");
            free(input);
            continue;
        }

        if (strcmp(input, "panic") == 0) {
            alda_async_stop();
            alda_midi_all_notes_off(ctx);
            printf("All notes off\n");
            free(input);
            continue;
        }

        if (strcmp(input, "concurrent") == 0) {
            alda_async_set_concurrent(1);
            printf("Concurrent mode enabled (polyphony)\n");
            free(input);
            continue;
        }

        if (strcmp(input, "sequential") == 0) {
            alda_async_set_concurrent(0);
            printf("Sequential mode enabled (wait for each input)\n");
            free(input);
            continue;
        }

        /* Clear previous events (keep parts and state) */
        alda_events_clear(ctx);

        /* Parse and interpret the input */
        int result = alda_interpret_string(ctx, input, "<repl>");

        if (result < 0) {
            /* Error already printed by interpret_string */
            free(input);
            continue;
        }

        /* Play events asynchronously if any were scheduled */
        if (ctx->event_count > 0) {
            if (ctx->verbose_mode) {
                printf("Playing %d events...\n", ctx->event_count);
            }
            alda_events_play_async(ctx);
        }

        free(input);
    }
}

/* ============================================================================
 * Main
 * ============================================================================ */

int main(int argc, char* argv[]) {
    int verbose = 0;
    int list_ports = 0;
    int port_index = -1;
    const char* port_name = NULL;
    const char* virtual_name = NULL;
    int no_sleep = 0;
    int sequential = 0;
    const char* input_file = NULL;

    /* Long options */
    static struct option long_options[] = {
        {"help",       no_argument,       0, 'h'},
        {"verbose",    no_argument,       0, 'v'},
        {"list",       no_argument,       0, 'l'},
        {"port",       required_argument, 0, 'p'},
        {"output",     required_argument, 0, 'o'},
        {"virtual",    required_argument, 0, 'V'},
        {"no-sleep",   no_argument,       0, 'S'},
        {"sequential", no_argument,       0, 's'},
        {0, 0, 0, 0}
    };

    /* Parse options */
    int opt;
    int option_index = 0;
    while ((opt = getopt_long(argc, argv, "hvlsp:o:", long_options, &option_index)) != -1) {
        switch (opt) {
            case 'h':
                print_usage(argv[0]);
                return 0;

            case 'v':
                verbose = 1;
                break;

            case 'l':
                list_ports = 1;
                break;

            case 'p':
                port_index = atoi(optarg);
                break;

            case 'o':
                port_name = optarg;
                break;

            case 'V':
                virtual_name = optarg;
                break;

            case 'S':
                no_sleep = 1;
                break;

            case 's':
                sequential = 1;
                break;

            default:
                print_usage(argv[0]);
                return 1;
        }
    }

    /* Get input file if provided */
    if (optind < argc) {
        input_file = argv[optind];
    }

    /* Initialize context */
    AldaContext ctx;
    alda_context_init(&ctx);
    ctx.verbose_mode = verbose;
    ctx.no_sleep_mode = no_sleep;

    /* Initialize MIDI observer */
    alda_midi_init_observer(&ctx);

    /* Handle --list */
    if (list_ports) {
        alda_midi_list_ports(&ctx);
        alda_midi_cleanup(&ctx);
        alda_context_cleanup(&ctx);
        return 0;
    }

    /* Open MIDI output */
    int midi_opened = 0;

    if (virtual_name) {
        /* Create virtual port */
        if (alda_midi_open_virtual(&ctx, virtual_name) == 0) {
            midi_opened = 1;
            if (verbose) {
                printf("Created virtual MIDI output: %s\n", virtual_name);
            }
        }
    } else if (port_name) {
        /* Open by name */
        if (alda_midi_open_by_name(&ctx, port_name) == 0) {
            midi_opened = 1;
        }
    } else if (port_index >= 0) {
        /* Open by index */
        if (alda_midi_open_port(&ctx, port_index) == 0) {
            midi_opened = 1;
        }
    } else {
        /* Default: auto-select first available port, or create virtual */
        if (alda_midi_open_auto(&ctx, "AldaMIDI") == 0) {
            midi_opened = 1;
        }
    }

    if (!midi_opened) {
        fprintf(stderr, "Warning: Failed to open MIDI output\n");
    }

    /* Concurrent mode is the default; disable if sequential requested */
    if (!sequential) {
        alda_async_set_concurrent(1);
    }
    if (verbose) {
        printf("Playback mode: %s\n", sequential ? "sequential" : "concurrent");
    }

    int result = 0;

    if (input_file) {
        /* File mode: interpret and play the file */
        if (verbose) {
            printf("Playing: %s\n", input_file);
        }

        result = alda_interpret_file(&ctx, input_file);

        if (result < 0) {
            fprintf(stderr, "Error: Failed to interpret file\n");
            alda_midi_cleanup(&ctx);
            alda_context_cleanup(&ctx);
            return 1;
        }

        /* Play the scheduled events */
        if (verbose) {
            printf("Scheduled %d events\n", ctx.event_count);
        }

        result = alda_events_play(&ctx);

        if (result < 0) {
            fprintf(stderr, "Error: Failed to play events\n");
        }
    } else {
        /* REPL mode: interactive input */
        repl_loop(&ctx);
    }

    /* Cleanup */
    alda_async_cleanup();
    alda_midi_cleanup(&ctx);
    alda_context_cleanup(&ctx);

    return result < 0 ? 1 : 0;
}
