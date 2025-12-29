/* interpreter.c - Interpreter and tokenizer for MIDI Forth interpreter */

#include "forth_midi.h"

/* Core interpreter state - defined here */
Stack stack;

/* Compile mode state */
int compile_mode = 0;
char current_definition_name[MAX_WORD_LENGTH];
char current_definition_body[MAX_DEFINITION_LENGTH];
int definition_body_len = 0;

/* Anonymous block state */
char* block_storage[MAX_BLOCKS];
int block_count = 0;
int block_capture_mode = 0;
char current_block_body[MAX_DEFINITION_LENGTH];
int block_body_len = 0;
int block_nesting = 0;

/* Conditional execution state */
int cond_skip_mode = 0;
int cond_skip_nesting = 0;
int cond_in_true_branch = 0;

/* Track last executed word for 'times' loop */
char last_executed_word[MAX_WORD_LENGTH] = "";

/* File loading depth */
int load_depth = 0;

/* Forward declarations */
void interpret(const char* input);

/* Check if character is a special single-char token */
static int is_special_char(char c) {
    return c == ',' || c == '(' || c == ')' || c == '|' || c == '[' || c == ']' || c == '%'
        || c == '{' || c == '}';
}

/* Append a word to the current block body */
static void append_to_block(const char* word) {
    int len = strlen(word);
    if (block_body_len + len + 2 >= MAX_DEFINITION_LENGTH) {
        printf("Block too long\n");
        return;
    }
    if (block_body_len > 0) {
        current_block_body[block_body_len++] = ' ';
    }
    strcpy(current_block_body + block_body_len, word);
    block_body_len += len;
}

/* Append a word to the current definition body */
static void append_to_definition(const char* word) {
    int len = strlen(word);
    if (definition_body_len + len + 2 >= MAX_DEFINITION_LENGTH) {
        printf("Definition too long\n");
        return;
    }
    if (definition_body_len > 0) {
        current_definition_body[definition_body_len++] = ' ';
    }
    strcpy(current_definition_body + definition_body_len, word);
    definition_body_len += len;
}

/* End compilation and register the word */
static void end_definition(void) {
    if (!compile_mode) {
        printf("Not in compile mode\n");
        return;
    }

    current_definition_body[definition_body_len] = '\0';
    register_user_word(current_definition_name, current_definition_body);

    /* Reset compile state */
    compile_mode = 0;
    current_definition_name[0] = '\0';
    definition_body_len = 0;
}

/* Load and execute a Forth file */
int load_file(const char* filename) {
    if (load_depth >= MAX_LOAD_DEPTH) {
        printf("Error: load depth exceeded (max %d nested loads)\n", MAX_LOAD_DEPTH);
        return -1;
    }

    FILE* f = fopen(filename, "r");
    if (f == NULL) {
        printf("Error: cannot open file '%s'\n", filename);
        return -1;
    }

    load_depth++;

    char line[MAX_INPUT_LENGTH * 4];
    int line_num = 0;

    while (fgets(line, sizeof(line), f) != NULL) {
        line_num++;

        /* Remove trailing newline */
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
            len--;
        }
        if (len > 0 && line[len - 1] == '\r') {
            line[len - 1] = '\0';
        }

        /* Skip empty lines */
        if (line[0] == '\0') continue;

        /* Skip whitespace at start */
        char* start = line;
        while (*start && isspace(*start)) start++;

        /* Skip comment lines (Forth uses \ for comments) */
        if (*start == '\\') continue;

        /* Execute the line */
        interpret(start);
    }

    fclose(f);
    load_depth--;

    return 0;
}

/* : ( -- ) Start word definition */
void op_colon(Stack* stack) {
    (void)stack;
    /* Handled specially in interpret() */
}

/* ; ( -- ) End word definition */
void op_semicolon(Stack* stack) {
    (void)stack;
    /* Handled specially in interpret() */
}

/* if ( flag -- ) Conditional execution */
void op_if(Stack* stack) {
    (void)stack;
    /* Handled specially in interpret() */
}

/* else ( -- ) Alternative branch */
void op_else(Stack* stack) {
    (void)stack;
    /* Handled specially in interpret() */
}

/* then ( -- ) End conditional */
void op_then(Stack* stack) {
    (void)stack;
    /* Handled specially in interpret() */
}

/* { ( -- ) Start anonymous block */
void op_block_open(Stack* stack) {
    (void)stack;
    /* Handled specially in interpret() */
}

/* } ( -- ) End anonymous block */
void op_block_close(Stack* stack) {
    (void)stack;
    /* Handled specially in interpret() */
}

/* times ( n -- ) Repeat last word n times */
void op_times(Stack* stack) {
    int32_t n = pop(stack);
    if (n <= 0) return;

    if (last_executed_word[0] == '\0') {
        printf("No word to repeat\n");
        return;
    }

    Word* word = find_word(last_executed_word);
    if (word == NULL) {
        printf("Word '%s' not found\n", last_executed_word);
        return;
    }

    /* Already executed once, so repeat n-1 more times */
    for (int i = 1; i < n; i++) {
        if (word->is_primitive) {
            word->function(stack);
        } else if (word->body) {
            interpret(word->body);
        }
    }
}

/* * ( block-id|a b -- result ) Block repeat or multiplication */
void op_star(Stack* stack) {
    if (stack->top < 1) {
        printf("* needs two values\n");
        return;
    }
    int32_t n = pop(stack);
    int32_t block_id = pop(stack);

    /* Check if it's a valid block reference */
    if ((block_id & BLOCK_MARKER) != BLOCK_MARKER) {
        /* Not a block - treat as multiplication */
        push(stack, block_id * n);
        return;
    }

    int idx = block_id & 0x0FFFFFFF;
    if (idx < 0 || idx >= block_count || block_storage[idx] == NULL) {
        printf("Invalid block reference\n");
        return;
    }

    /* Execute block n times */
    for (int i = 0; i < n; i++) {
        interpret(block_storage[idx]);
    }
}

/* load ( filename -- ) Load a file (handled specially) */
void op_load(Stack* stack) {
    (void)stack;
    /* Handled specially in interpret() */
}

/* help ( -- ) Print help text */
void op_help(Stack* stack) {
    (void)stack;

    printf("\nMIDI Forth - A Forth for MIDI sequence generation\n\n");

    printf("Concise Notation:\n");
    printf("  c4,                     Play C4 with defaults\n");
    printf("  c#4, db4, 60,           Sharps, flats, or MIDI numbers\n");
    printf("  (c4 e4 g4),             Chord (concurrent)\n");
    printf("  r,                      Rest (silence)\n");
    printf("  ch! vel! dur!           Set default channel/velocity/duration\n");
    printf("\n");
    printf("Word Definitions:\n");
    printf("  : name ... ;            Define a new word\n");
    printf("  name N times            Execute word N times total\n");
    printf("\n");
    printf("File Operations:\n");
    printf("  load filename           Load and execute a .4th file\n");
    printf("  rec / stop / save       Record commands to file\n");
    printf("  capture / stop / save-midi  Capture MIDI events\n");
    printf("\n");
    printf("Generative:\n");
    printf("  c4|e4,                  Alternative (random selection)\n");
    printf("  c4 75%%,                 75%% chance to play\n");
    printf("  seed! seed@ next-random  PRNG functions\n");
    printf("  [[ ... ]] list-len      List operations\n");
    printf("  euclidean shuffle pick  Pattern operations\n");
    printf("\n");
    printf("Dynamics: ppp pp p mp mf f ff fff\n");
    printf("Articulation: c4. (staccato) c4> (accent) c4- (tenuto)\n");
    printf("\n");
    printf("MIDI: midi-open midi-close midi-list panic\n");
    printf("Sequences: seq-new seq-note seq-play seq-show\n");
    printf("Scales: scale-major scale-minor scale-blues ... scales\n");
    printf("\n");
    printf("Type 'words' to see all available words.\n");
    printf("Type 'quit' to exit.\n");
}

/* Process a single token */
void process_token(const char* token) {
    Word* word = find_word(token);

    if (word == NULL) {
        /* Try to parse as relative interval (+N or -N) */
        if ((token[0] == '+' || token[0] == '-') && strlen(token) > 1) {
            char* end;
            long interval = strtol(token, &end, 10);
            if (*end == '\0') {
                int new_pitch = current_pitch + (int)interval;
                if (new_pitch < 0) new_pitch = 0;
                if (new_pitch > 127) new_pitch = 127;
                push(&stack, (int32_t)new_pitch);
                return;
            }
        }

        /* Try to parse as a number */
        char* endptr;
        long num = strtol(token, &endptr, 10);
        if (*endptr == '\0') {
            push(&stack, (int32_t)num);
            return;
        }

        /* Try to parse as a pitch name */
        int pitch = parse_pitch(token);
        if (pitch >= 0) {
            push(&stack, (int32_t)pitch);
            return;
        }

        printf("Unknown word: %s\n", token);
        return;
    }

    /* Store the word name for 'times' loop (only for user-defined words) */
    if (!word->is_primitive) {
        strncpy(last_executed_word, token, MAX_WORD_LENGTH - 1);
        last_executed_word[MAX_WORD_LENGTH - 1] = '\0';
    }

    /* Execute the word */
    if (word->is_primitive) {
        word->function(&stack);
    } else {
        if (word->body) {
            int initial_depth = stack.top;
            interpret(word->body);
            int final_depth = stack.top;
            if (final_depth > initial_depth) {
                printf("Note: '%s' left %d item(s) on stack\n",
                       token, final_depth - initial_depth);
            }
        }
    }
}

/* Parse and execute a command line */
void interpret(const char* input) {
    char word[MAX_INPUT_LENGTH];
    int i = 0;
    int awaiting_name = 0;
    int awaiting_filename = 0;

    while (input[i] != '\0') {
        /* Skip whitespace */
        while (isspace(input[i])) {
            i++;
        }

        if (input[i] == '\0') break;

        /* Extract the word */
        int start = i;
        int len = 0;

        /* Special single-character tokens */
        if (is_special_char(input[i])) {
            word[0] = input[i];
            word[1] = '\0';
            len = 1;
            i++;
        } else {
            /* Regular word */
            while (!isspace(input[i]) && input[i] != '\0' && !is_special_char(input[i])) {
                i++;
            }
            len = i - start;
            if (len >= MAX_INPUT_LENGTH) {
                printf("Word too long\n");
                return;
            }
            strncpy(word, input + start, len);
            word[len] = '\0';
        }

        /* Handle compile mode */
        if (awaiting_name) {
            strncpy(current_definition_name, word, MAX_WORD_LENGTH - 1);
            current_definition_name[MAX_WORD_LENGTH - 1] = '\0';
            awaiting_name = 0;
            continue;
        }

        /* Handle file operations */
        if (awaiting_filename) {
            if (awaiting_filename == 1) {
                load_file(word);
            } else if (awaiting_filename == 2) {
                recording_save(word);
            } else if (awaiting_filename == 3) {
                capture_save_midi(word);
            } else if (awaiting_filename == 4) {
                open_virtual_port(word);
            }
            awaiting_filename = 0;
            continue;
        }

        if (compile_mode) {
            if (strcmp(word, ";") == 0) {
                end_definition();
            } else {
                append_to_definition(word);
            }
            continue;
        }

        /* Handle block capture mode */
        if (block_capture_mode) {
            if (strcmp(word, "{") == 0) {
                block_nesting++;
                append_to_block(word);
            } else if (strcmp(word, "}") == 0) {
                if (block_nesting > 0) {
                    block_nesting--;
                    append_to_block(word);
                } else {
                    current_block_body[block_body_len] = '\0';
                    if (block_count >= MAX_BLOCKS) {
                        printf("Too many blocks\n");
                        block_capture_mode = 0;
                    } else {
                        block_storage[block_count] = strdup(current_block_body);
                        push(&stack, BLOCK_MARKER | block_count);
                        block_count++;
                    }
                    block_capture_mode = 0;
                    block_body_len = 0;
                }
            } else {
                append_to_block(word);
            }
            continue;
        }

        /* Interpret mode: check for block start */
        if (strcmp(word, "{") == 0) {
            block_capture_mode = 1;
            block_nesting = 0;
            block_body_len = 0;
            current_block_body[0] = '\0';
            continue;
        }

        if (strcmp(word, "}") == 0) {
            printf("Unexpected '}' outside of block\n");
            continue;
        }

        /* Handle conditional skip mode */
        if (cond_skip_mode) {
            if (strcmp(word, "if") == 0) {
                cond_skip_nesting++;
            } else if (strcmp(word, "then") == 0) {
                if (cond_skip_nesting > 0) {
                    cond_skip_nesting--;
                } else {
                    cond_skip_mode = 0;
                    cond_in_true_branch = 0;
                }
            } else if (strcmp(word, "else") == 0) {
                if (cond_skip_nesting == 0) {
                    if (!cond_in_true_branch) {
                        cond_skip_mode = 0;
                    }
                }
            }
            continue;
        }

        /* Handle if/else/then */
        if (strcmp(word, "if") == 0) {
            if (stack.top < 0) {
                printf("if needs a condition on stack\n");
                continue;
            }
            int32_t cond = pop(&stack);
            if (cond == 0) {
                cond_skip_mode = 1;
                cond_skip_nesting = 0;
                cond_in_true_branch = 0;
            } else {
                cond_in_true_branch = 1;
            }
            continue;
        }

        if (strcmp(word, "else") == 0) {
            if (cond_in_true_branch) {
                cond_skip_mode = 1;
                cond_skip_nesting = 0;
            }
            continue;
        }

        if (strcmp(word, "then") == 0) {
            cond_in_true_branch = 0;
            continue;
        }

        /* Interpret mode */
        if (strcmp(word, ":") == 0) {
            compile_mode = 1;
            awaiting_name = 1;
            definition_body_len = 0;
            current_definition_body[0] = '\0';
            continue;
        }

        if (strcmp(word, ";") == 0) {
            printf("Unexpected ';' outside of definition\n");
            continue;
        }

        if (strcmp(word, "load") == 0) {
            awaiting_filename = 1;
            continue;
        }

        if (strcmp(word, "rec") == 0) {
            op_rec_start(&stack);
            continue;
        }

        if (strcmp(word, "capture") == 0) {
            op_capture_start(&stack);
            continue;
        }

        if (strcmp(word, "stop") == 0) {
            op_rec_stop(&stack);
            op_capture_stop(&stack);
            continue;
        }

        if (strcmp(word, "save") == 0) {
            awaiting_filename = 2;
            continue;
        }

        if (strcmp(word, "save-midi") == 0) {
            awaiting_filename = 3;
            continue;
        }

        if (strcmp(word, "midi-open-as") == 0) {
            awaiting_filename = 4;
            continue;
        }

        /* Execute the word */
        process_token(word);
    }
}
