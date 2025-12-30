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

/* Named parameter system state */
int default_gate = 100;           /* Gate percentage (1-100) */
int pending_channel = -1;         /* One-shot channel override (-1 if not set) */
int pending_velocity = -1;        /* One-shot velocity override (-1 if not set) */
int pending_duration = -1;        /* One-shot duration override (-1 if not set) */
int pending_gate = -1;            /* One-shot gate override (-1 if not set) */

/* Bracket sequence system */
BracketSequence* bracket_seq_storage[MAX_BRACKET_SEQS];
int bracket_seq_count = 0;
int seq_capture_mode = 0;
int seq_capture_count = 0;
int seq_capture_chord_mode = 0;
int seq_capture_chord_count = 0;
int16_t seq_capture_chord_buffer[8];
BracketSequence* current_bracket_seq = NULL;

/* Forward declarations */
void interpret(const char* input);

/* ============================================================================
 * Bracket Sequence Memory Management
 * ============================================================================ */

/* Allocate a new bracket sequence with ref_count=1 */
BracketSequence* seq_alloc(void) {
    BracketSequence* seq = malloc(sizeof(BracketSequence));
    if (seq) {
        seq->count = 0;
        seq->ref_count = 1;
    }
    return seq;
}

/* Increment reference count */
void seq_retain(BracketSequence* seq) {
    if (seq) {
        seq->ref_count++;
    }
}

/* Decrement reference count and free if zero */
void seq_release(BracketSequence* seq) {
    if (seq && --seq->ref_count <= 0) {
        free(seq);
    }
}

/* Clean up all sequences in storage */
void seq_cleanup_all(void) {
    for (int i = 0; i < bracket_seq_count; i++) {
        if (bracket_seq_storage[i]) {
            seq_release(bracket_seq_storage[i]);
            bracket_seq_storage[i] = NULL;
        }
    }
    bracket_seq_count = 0;
}

/* seq-gc ( -- ) Garbage collect unreferenced sequences */
void op_seq_gc(Stack* stack) {
    (void)stack;
    int freed = 0;
    for (int i = 0; i < bracket_seq_count; i++) {
        if (bracket_seq_storage[i] && bracket_seq_storage[i]->ref_count <= 1) {
            /* Only referenced by storage, free it */
            free(bracket_seq_storage[i]);
            bracket_seq_storage[i] = NULL;
            freed++;
        }
    }
    /* Compact the storage array */
    int write = 0;
    for (int read = 0; read < bracket_seq_count; read++) {
        if (bracket_seq_storage[read]) {
            if (write != read) {
                bracket_seq_storage[write] = bracket_seq_storage[read];
                bracket_seq_storage[read] = NULL;
            }
            write++;
        }
    }
    bracket_seq_count = write;
    printf("seq-gc: freed %d sequences, %d remaining\n", freed, bracket_seq_count);
}

/* Check if character is a special single-char token */
static int is_special_char(char c) {
    return c == ',' || c == '(' || c == ')' || c == '|' || c == '[' || c == ']' || c == '%'
        || c == '{' || c == '}';
}

/* ============================================================================
 * Named Parameter System
 * ============================================================================ */

/* Get effective channel (pending override or default) */
int effective_channel(void) {
    return (pending_channel >= 0) ? pending_channel : default_channel;
}

/* Get effective velocity (pending override or default) */
int effective_velocity(void) {
    return (pending_velocity >= 0) ? pending_velocity : default_velocity;
}

/* Get effective duration (pending override or default) */
int effective_duration(void) {
    return (pending_duration >= 0) ? pending_duration : default_duration;
}

/* Get effective gate (pending override or default) */
int effective_gate(void) {
    return (pending_gate >= 0) ? pending_gate : default_gate;
}

/* Clear one-shot pending parameters after use */
void clear_pending_params(void) {
    pending_channel = -1;
    pending_velocity = -1;
    pending_duration = -1;
    pending_gate = -1;
}

/* Parse duration value (number or duration word) */
static int parse_duration_value(const char* str) {
    /* Check for duration words */
    if (strcmp(str, "whole") == 0) return 2000;
    if (strcmp(str, "half") == 0) return 1000;
    if (strcmp(str, "quarter") == 0) return 500;
    if (strcmp(str, "eighth") == 0) return 250;
    if (strcmp(str, "sixteenth") == 0) return 125;
    /* Otherwise parse as number */
    return atoi(str);
}

/* Parse parameter name, returns param type or 0 if unknown */
static int parse_param_name(const char* name, int name_len, int* cc_number) {
    *cc_number = -1;

    if (name_len == 2 && strncmp(name, "ch", 2) == 0) return PARAM_CHANNEL;
    if (name_len == 3 && strncmp(name, "vel", 3) == 0) return PARAM_VELOCITY;
    if (name_len == 3 && strncmp(name, "dur", 3) == 0) return PARAM_DURATION;
    if (name_len == 4 && strncmp(name, "gate", 4) == 0) return PARAM_GATE;
    if (name_len == 3 && strncmp(name, "bpm", 3) == 0) return PARAM_BPM;
    if (name_len == 4 && strncmp(name, "prog", 4) == 0) return PARAM_PROGRAM;
    if (name_len == 3 && strncmp(name, "pan", 3) == 0) return PARAM_PAN;

    /* Check for ccN pattern (e.g., cc64, cc1, cc127) */
    if (name_len >= 3 && strncmp(name, "cc", 2) == 0) {
        char num_buf[8];
        int num_len = name_len - 2;
        if (num_len > 0 && num_len < 8) {
            strncpy(num_buf, name + 2, num_len);
            num_buf[num_len] = '\0';
            *cc_number = atoi(num_buf);
            if (*cc_number >= 0 && *cc_number <= 127) {
                return PARAM_CC;
            }
        }
    }

    return 0;  /* Unknown parameter */
}

/* Apply a parameter value based on type and scope */
static void apply_param(int param_type, int value, int scope, int cc_number) {
    if (scope == SCOPE_ONESHOT) {
        /* One-shot: set pending value */
        switch (param_type) {
            case PARAM_CHANNEL:  pending_channel = value; break;
            case PARAM_VELOCITY: pending_velocity = value; break;
            case PARAM_DURATION: pending_duration = value; break;
            case PARAM_GATE:     pending_gate = value; break;
            default:
                /* Global params don't support one-shot, fall through to persistent */
                break;
        }
    }

    if (scope == SCOPE_PERSISTENT || param_type >= PARAM_BPM) {
        /* Persistent: set default/context value */
        switch (param_type) {
            case PARAM_CHANNEL:  default_channel = value; break;
            case PARAM_VELOCITY: default_velocity = value; break;
            case PARAM_DURATION: default_duration = value; break;
            case PARAM_GATE:     default_gate = value; break;
            case PARAM_BPM:
                global_bpm = value;
                /* Update duration constants proportionally (120 BPM base) */
                default_duration = 60000 / value / 2;  /* quarter note */
                break;
            case PARAM_PROGRAM:
                if (midi_out) {
                    midi_send_program_change(value, default_channel);
                }
                break;
            case PARAM_PAN:
                if (midi_out) {
                    midi_send_cc(10, value, default_channel);  /* CC 10 = Pan */
                }
                break;
            case PARAM_CC:
                if (midi_out && cc_number >= 0) {
                    midi_send_cc(cc_number, value, default_channel);
                }
                break;
        }
    }
}

/* Parse parameter assignment (both = and :=)
 * Returns 1 if parsed successfully, 0 otherwise */
int parse_param_assign(const char* token) {
    int cc_number = -1;
    int param_type, value, scope;
    const char* assign;
    int name_len;
    const char* val_str;

    /* Check for := (persistent) first */
    assign = strstr(token, ":=");
    if (assign) {
        scope = SCOPE_PERSISTENT;
        name_len = assign - token;
        val_str = assign + 2;

        param_type = parse_param_name(token, name_len, &cc_number);
        if (param_type == 0) return 0;

        value = (param_type == PARAM_DURATION)
            ? parse_duration_value(val_str)
            : atoi(val_str);

        apply_param(param_type, value, scope, cc_number);
        return 1;
    }

    /* Check for = (one-shot) */
    assign = strchr(token, '=');
    if (assign) {
        scope = SCOPE_ONESHOT;
        name_len = assign - token;
        val_str = assign + 1;

        param_type = parse_param_name(token, name_len, &cc_number);
        if (param_type == 0) return 0;

        /* Global params (bpm, prog, pan, cc) don't support one-shot */
        if (param_type >= PARAM_BPM) {
            scope = SCOPE_PERSISTENT;
        }

        value = (param_type == PARAM_DURATION)
            ? parse_duration_value(val_str)
            : atoi(val_str);

        apply_param(param_type, value, scope, cc_number);
        return 1;
    }

    return 0;  /* Not a parameter assignment */
}

/* ============================================================================
 * Bracket Sequence System
 * ============================================================================ */

/* Check if token is a dynamic word */
static int is_dynamic_word(const char* word) {
    return strcmp(word, "ppp") == 0 || strcmp(word, "pp") == 0 ||
           strcmp(word, "p") == 0 || strcmp(word, "mp") == 0 ||
           strcmp(word, "mf") == 0 || strcmp(word, "f") == 0 ||
           strcmp(word, "ff") == 0 || strcmp(word, "fff") == 0;
}

/* Convert dynamic word to velocity */
static int dynamic_to_velocity(const char* word) {
    if (strcmp(word, "ppp") == 0) return 16;
    if (strcmp(word, "pp") == 0) return 33;
    if (strcmp(word, "p") == 0) return 49;
    if (strcmp(word, "mp") == 0) return 64;
    if (strcmp(word, "mf") == 0) return 80;
    if (strcmp(word, "f") == 0) return 96;
    if (strcmp(word, "ff") == 0) return 112;
    if (strcmp(word, "fff") == 0) return 127;
    return 80;  /* default to mf */
}

/* Check if token is a duration word */
static int is_duration_word(const char* word) {
    return strcmp(word, "whole") == 0 || strcmp(word, "half") == 0 ||
           strcmp(word, "quarter") == 0 || strcmp(word, "eighth") == 0 ||
           strcmp(word, "sixteenth") == 0;
}

/* Convert duration word to milliseconds */
static int duration_word_to_ms(const char* word) {
    if (strcmp(word, "whole") == 0) return 2000;
    if (strcmp(word, "half") == 0) return 1000;
    if (strcmp(word, "quarter") == 0) return 500;
    if (strcmp(word, "eighth") == 0) return 250;
    if (strcmp(word, "sixteenth") == 0) return 125;
    return 500;  /* default quarter */
}

/* Parse pitch with optional articulation suffix for sequences */
static int parse_pitch_with_artic(const char* word) {
    /* Just use the base parse_pitch which already handles articulation */
    return parse_pitch(word);
}

/* Add element to current bracket sequence */
static void seq_add_element(uint8_t type, int16_t value) {
    if (!current_bracket_seq || seq_capture_count >= MAX_SEQ_ELEMENTS) {
        printf("Sequence full or not active\n");
        return;
    }
    SeqElement* elem = &current_bracket_seq->elements[seq_capture_count++];
    elem->type = type;
    elem->value = value;
    elem->chord_count = 0;
}

/* Add chord to current bracket sequence */
static void seq_add_chord(int16_t* pitches, int count) {
    if (!current_bracket_seq || seq_capture_count >= MAX_SEQ_ELEMENTS) {
        printf("Sequence full or not active\n");
        return;
    }
    SeqElement* elem = &current_bracket_seq->elements[seq_capture_count++];
    elem->type = SEQ_ELEM_CHORD;
    elem->value = pitches[0];  /* Store first pitch in value too */
    elem->chord_count = count;
    for (int i = 0; i < count && i < 8; i++) {
        elem->chord_pitches[i] = pitches[i];
    }
}

/* Finalize current bracket sequence and return its index */
static int finalize_sequence(void) {
    if (!current_bracket_seq) return -1;

    current_bracket_seq->count = seq_capture_count;

    /* Store in bracket_seq_storage */
    if (bracket_seq_count >= MAX_BRACKET_SEQS) {
        printf("Too many sequences\n");
        free(current_bracket_seq);
        current_bracket_seq = NULL;
        return -1;
    }

    int idx = bracket_seq_count++;
    bracket_seq_storage[idx] = current_bracket_seq;
    current_bracket_seq = NULL;
    seq_capture_count = 0;
    return idx;
}

/* Execute a bracket sequence */
void execute_bracket_sequence(BracketSequence* seq) {
    if (!seq) return;

    int vel = effective_velocity();
    int dur = effective_duration();
    int ch = effective_channel();
    int gate = effective_gate();
    int last_pitch = current_pitch;

    for (int i = 0; i < seq->count; i++) {
        SeqElement* elem = &seq->elements[i];

        switch (elem->type) {
            case SEQ_ELEM_PITCH:
                last_pitch = elem->value;
                current_pitch = last_pitch;
                midi_send_note_on(last_pitch, vel, ch);
                midi_sleep_ms(dur * gate / 100);
                midi_send_note_off(last_pitch, ch);
                if (gate < 100) {
                    midi_sleep_ms(dur * (100 - gate) / 100);
                }
                break;

            case SEQ_ELEM_INTERVAL:
                last_pitch = current_pitch + elem->value;
                if (last_pitch < 0) last_pitch = 0;
                if (last_pitch > 127) last_pitch = 127;
                current_pitch = last_pitch;
                midi_send_note_on(last_pitch, vel, ch);
                midi_sleep_ms(dur * gate / 100);
                midi_send_note_off(last_pitch, ch);
                if (gate < 100) {
                    midi_sleep_ms(dur * (100 - gate) / 100);
                }
                break;

            case SEQ_ELEM_REST:
                midi_sleep_ms(dur);
                break;

            case SEQ_ELEM_DYNAMIC:
                vel = elem->value;
                break;

            case SEQ_ELEM_DURATION:
                dur = elem->value;
                break;

            case SEQ_ELEM_CHORD: {
                /* Send all note-ons */
                for (int j = 0; j < elem->chord_count; j++) {
                    int pitch = elem->chord_pitches[j];
                    midi_send_note_on(pitch, vel, ch);
                }
                /* Track the highest note for interval reference */
                int highest = elem->chord_pitches[0];
                for (int j = 1; j < elem->chord_count; j++) {
                    if (elem->chord_pitches[j] > highest) {
                        highest = elem->chord_pitches[j];
                    }
                }
                current_pitch = highest;

                /* Wait for duration */
                midi_sleep_ms(dur * gate / 100);

                /* Send all note-offs */
                for (int j = 0; j < elem->chord_count; j++) {
                    int pitch = elem->chord_pitches[j];
                    midi_send_note_off(pitch, ch);
                }
                if (gate < 100) {
                    midi_sleep_ms(dur * (100 - gate) / 100);
                }
                break;
            }

            case SEQ_ELEM_NUMBER:
                /* Numbers are skipped when playing - they're for generative ops */
                /* Could push to stack if needed: push(&stack, elem->value); */
                break;
        }
    }

    clear_pending_params();
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

/* * ( seq|block|a n -- result ) Sequence/block repeat or multiplication */
void op_star(Stack* stack) {
    if (stack->top < 1) {
        printf("* needs two values\n");
        return;
    }
    int32_t n = pop(stack);
    int32_t val = pop(stack);

    /* Check if it's a bracket sequence */
    if ((val & 0xFF000000) == SEQ_MARKER) {
        int idx = val & 0x00FFFFFF;
        if (idx < 0 || idx >= bracket_seq_count || !bracket_seq_storage[idx]) {
            printf("Invalid sequence\n");
            return;
        }
        /* Execute sequence n times */
        for (int i = 0; i < n; i++) {
            execute_bracket_sequence(bracket_seq_storage[idx]);
        }
        return;
    }

    /* Check if it's a block reference */
    if ((val & BLOCK_MARKER) == BLOCK_MARKER) {
        int idx = val & 0x0FFFFFFF;
        if (idx < 0 || idx >= block_count || block_storage[idx] == NULL) {
            printf("Invalid block reference\n");
            return;
        }
        /* Execute block n times */
        for (int i = 0; i < n; i++) {
            interpret(block_storage[idx]);
        }
        return;
    }

    /* Otherwise treat as multiplication */
    push(stack, val * n);
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
    printf("  rec-midi / stop / save-midi  Record MIDI events (Forth format)\n");
    printf("  write-mid filename      Write captured events to .mid file\n");
    printf("  read-mid filename       Read and display .mid file info\n");
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
        /* Try to parse as parameter assignment (vel=100, ch:=1, etc.) */
        if (strchr(token, '=') != NULL) {
            if (parse_param_assign(token)) {
                return;
            }
        }

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

/* ============================================================================
 * Tokenizer
 * ============================================================================ */

/* Get next token from input, advancing position.
 * Returns 1 if token found, 0 if end of input, -1 on error.
 * Token is written to `out` buffer (must be MAX_INPUT_LENGTH). */
static int next_token(const char* input, int* pos, char* out) {
    int i = *pos;

    /* Skip whitespace */
    while (isspace(input[i])) {
        i++;
    }

    if (input[i] == '\0') {
        *pos = i;
        return 0;  /* End of input */
    }

    int start = i;
    int len = 0;

    /* Quoted string - extract content without quotes */
    if (input[i] == '"') {
        i++;  /* Skip opening quote */
        start = i;
        while (input[i] != '"' && input[i] != '\0') {
            i++;
        }
        len = i - start;
        if (len >= MAX_INPUT_LENGTH) {
            printf("String too long\n");
            *pos = i;
            return -1;
        }
        strncpy(out, input + start, len);
        out[len] = '\0';
        if (input[i] == '"') i++;  /* Skip closing quote */
    }
    /* Special single-character tokens */
    else if (is_special_char(input[i])) {
        out[0] = input[i];
        out[1] = '\0';
        i++;
    } else {
        /* Regular word */
        while (!isspace(input[i]) && input[i] != '\0' && !is_special_char(input[i])) {
            i++;
        }
        len = i - start;
        if (len >= MAX_INPUT_LENGTH) {
            printf("Word too long\n");
            *pos = i;
            return -1;
        }
        strncpy(out, input + start, len);
        out[len] = '\0';
    }

    *pos = i;
    return 1;  /* Token found */
}

/* ============================================================================
 * Sequence Capture Handler
 * ============================================================================ */

/* Handle a token while in sequence capture mode.
 * Returns 1 if token was consumed, 0 if sequence ended. */
static int handle_seq_capture_token(const char* word) {
    if (strcmp(word, "[") == 0) {
        printf("Nested sequences not allowed\n");
        return 1;
    }

    if (strcmp(word, "]") == 0) {
        /* End of sequence - finalize it */
        if (seq_capture_chord_mode) {
            printf("Unclosed '(' in sequence\n");
            seq_capture_chord_mode = 0;
            seq_capture_chord_count = 0;
        }
        int idx = finalize_sequence();
        if (idx >= 0) {
            push(&stack, SEQ_MARKER | idx);
        }
        seq_capture_mode = 0;
        return 0;  /* Sequence ended */
    }

    if (strcmp(word, "(") == 0) {
        seq_capture_chord_mode = 1;
        seq_capture_chord_count = 0;
        return 1;
    }

    if (strcmp(word, ")") == 0) {
        if (seq_capture_chord_mode && seq_capture_chord_count > 0) {
            seq_add_chord(seq_capture_chord_buffer, seq_capture_chord_count);
        }
        seq_capture_chord_mode = 0;
        seq_capture_chord_count = 0;
        return 1;
    }

    if (strcmp(word, "r") == 0) {
        seq_add_element(SEQ_ELEM_REST, 0);
        return 1;
    }

    if (is_dynamic_word(word)) {
        seq_add_element(SEQ_ELEM_DYNAMIC, dynamic_to_velocity(word));
        return 1;
    }

    if (is_duration_word(word)) {
        seq_add_element(SEQ_ELEM_DURATION, duration_word_to_ms(word));
        return 1;
    }

    if ((word[0] == '+' || word[0] == '-') && strlen(word) > 1) {
        char* end;
        long interval = strtol(word, &end, 10);
        if (*end == '\0') {
            seq_add_element(SEQ_ELEM_INTERVAL, (int16_t)interval);
            return 1;
        }
    }

    /* Try to parse as pitch */
    int pitch = parse_pitch_with_artic(word);
    if (pitch >= 0) {
        if (seq_capture_chord_mode) {
            if (seq_capture_chord_count < 8) {
                seq_capture_chord_buffer[seq_capture_chord_count++] = pitch;
            }
        } else {
            seq_add_element(SEQ_ELEM_PITCH, (int16_t)pitch);
        }
        return 1;
    }

    /* Try to parse as plain number */
    {
        char* endptr;
        long num = strtol(word, &endptr, 10);
        if (*endptr == '\0') {
            seq_add_element(SEQ_ELEM_NUMBER, (int16_t)num);
            return 1;
        }
    }

    printf("Unknown element in sequence: %s\n", word);
    return 1;
}

/* ============================================================================
 * Main Interpreter
 * ============================================================================ */

/* Parse and execute a command line */
void interpret(const char* input) {
    char word[MAX_INPUT_LENGTH];
    int i = 0;
    int awaiting_name = 0;
    int awaiting_filename = 0;
    int result;

    while ((result = next_token(input, &i, word)) > 0) {

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
            } else if (awaiting_filename == 5) {
                capture_write_mid(word);
            } else if (awaiting_filename == 6) {
                capture_read_mid(word);
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

        /* Handle bracket sequence capture mode */
        if (seq_capture_mode) {
            handle_seq_capture_token(word);
            continue;
        }

        /* Interpret mode: check for bracket sequence start */
        if (strcmp(word, "[") == 0) {
            seq_capture_mode = 1;
            seq_capture_count = 0;
            seq_capture_chord_mode = 0;
            seq_capture_chord_count = 0;
            current_bracket_seq = seq_alloc();
            continue;
        }

        if (strcmp(word, "]") == 0) {
            printf("Unexpected ']' outside of sequence\n");
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

        if (strcmp(word, "rec-midi") == 0) {
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

        if (strcmp(word, "midi-open") == 0) {
            /* Check if next non-whitespace is a quoted string */
            int peek = i;
            while (isspace(input[peek])) peek++;
            if (input[peek] == '"') {
                awaiting_filename = 4;  /* Next token will be the port name */
            } else {
                open_virtual_port("ForthMIDI");  /* Default name */
            }
            continue;
        }

        if (strcmp(word, "write-mid") == 0) {
            awaiting_filename = 5;
            continue;
        }

        if (strcmp(word, "read-mid") == 0) {
            awaiting_filename = 6;
            continue;
        }

        /* Execute the word */
        process_token(word);
    }
}
