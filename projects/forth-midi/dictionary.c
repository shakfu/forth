/* dictionary.c - Dictionary operations for MIDI Forth interpreter */

#include "forth_midi.h"

/* All globals now accessed via g_ctx macros defined in forth_midi.h */

/* Find a word in the dictionary */
Word* find_word(const char* name) {
    for (int i = 0; i < dict_count; i++) {
        if (strcmp(dictionary[i].name, name) == 0) {
            return &dictionary[i];
        }
    }
    return NULL;
}

/* Add a primitive word to the dictionary */
void add_word(const char* name, void (*func)(Stack*), int is_primitive) {
    if (dict_count >= MAX_WORDS) {
        printf("Dictionary full!\n");
        return;
    }

    strncpy(dictionary[dict_count].name, name, MAX_WORD_LENGTH - 1);
    dictionary[dict_count].name[MAX_WORD_LENGTH - 1] = '\0';
    dictionary[dict_count].function = func;
    dictionary[dict_count].body = NULL;
    dictionary[dict_count].is_primitive = is_primitive;
    dict_count++;
}

/* Add a user-defined word to the dictionary */
static void add_user_word(const char* name, const char* body) {
    if (dict_count >= MAX_WORDS) {
        printf("Dictionary full!\n");
        return;
    }

    /* Check if word already exists and warn */
    Word* existing = find_word(name);
    if (existing != NULL) {
        printf("Warning: redefining '%s'\n", name);
        /* Free old body if it was user-defined */
        if (!existing->is_primitive && existing->body) {
            free(existing->body);
        }
        existing->function = NULL;
        existing->body = strdup(body);
        existing->is_primitive = 0;
        return;
    }

    strncpy(dictionary[dict_count].name, name, MAX_WORD_LENGTH - 1);
    dictionary[dict_count].name[MAX_WORD_LENGTH - 1] = '\0';
    dictionary[dict_count].function = NULL;
    dictionary[dict_count].body = strdup(body);
    dictionary[dict_count].is_primitive = 0;
    dict_count++;
}

/* Register a user-defined word (called from interpreter) */
void register_user_word(const char* name, const char* body) {
    add_user_word(name, body);
}

/* words ( -- ) List all words in the dictionary */
void op_words(Stack* s) {
    (void)stack;
    printf("Words (%d):\n", dict_count);
    for (int i = 0; i < dict_count; i++) {
        printf("%s ", dictionary[i].name);
        if ((i + 1) % 8 == 0) printf("\n");
    }
    if (dict_count % 8 != 0) printf("\n");
}

/* reset ( -- ) Reset interpreter runtime state */
void op_reset(Stack* s) {
    (void)s;
    /* Reset the legacy globals to match fresh state */
    stack.top = -1;
    compile_mode = 0;
    block_capture_mode = 0;
    block_count = 0;
    cond_skip_mode = 0;
    seq_capture_mode = 0;

    /* Also reset g_ctx for future use */
    forth_context_reset(&g_ctx);

    printf("Interpreter state reset\n");
}

/* Dynamics - set default velocity */
static void op_ppp(Stack* s) { (void)stack; default_velocity = DYN_PPP; }
static void op_pp(Stack* s)  { (void)stack; default_velocity = DYN_PP; }
static void op_p(Stack* s)   { (void)stack; default_velocity = DYN_P; }
static void op_mp(Stack* s)  { (void)stack; default_velocity = DYN_MP; }
static void op_mf(Stack* s)  { (void)stack; default_velocity = DYN_MF; }
static void op_f(Stack* s)   { (void)stack; default_velocity = DYN_F; }
static void op_ff(Stack* s)  { (void)stack; default_velocity = DYN_FF; }
static void op_fff(Stack* s) { (void)stack; default_velocity = DYN_FFF; }

/* Rest - push rest marker */
static void op_rest(Stack* s) {
    push(&stack, REST_MARKER);
}

/* op_random is declared in generative.c */

/* Explicit parameter brackets */
static void op_bracket_open(Stack* s) {
    push(&stack, EXPLICIT_MARKER);
}

static void op_bracket_close(Stack* s) {
    (void)stack;
}

/* cc ( ch cc# value -- ) Send Control Change */
static void op_cc(Stack* s) {
    if (stack.top < 2) {
        printf("cc needs 3 values: channel cc# value\n");
        return;
    }

    int32_t value = pop(&stack);
    int32_t cc_num = pop(&stack);
    int32_t channel = pop(&stack);

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    if (channel < 1 || channel > 16) {
        printf("Channel must be 1-16\n");
        return;
    }

    midi_send_cc(cc_num, value, channel);
    capture_add_event(2, channel - 1, cc_num, value);
}

/* Initialize the dictionary with all primitive words */
void init_dictionary(void) {
    /* Arithmetic */
    add_word("+", op_add, 1);
    add_word("-", op_sub, 1);
    add_word("*", op_star, 1);
    add_word("/", op_div, 1);
    add_word("mod", op_mod, 1);
    add_word("abs", op_abs, 1);
    add_word("negate", op_negate, 1);
    add_word("min", op_min, 1);
    add_word("max", op_max, 1);

    /* Stack manipulation */
    add_word("swap", op_swap, 1);
    add_word("dup", op_dup, 1);
    add_word("drop", op_drop, 1);
    add_word("clear", op_clear, 1);
    add_word("over", op_over, 1);
    add_word("rot", op_rot, 1);
    add_word("depth", op_depth, 1);
    add_word(".s", op_print_stack, 1);

    /* Output */
    add_word(".", op_print, 1);
    add_word("cr", op_cr, 1);
    add_word("space", op_space, 1);
    add_word("emit", op_emit, 1);

    /* Bitwise */
    add_word("and", op_and, 1);
    add_word("or", op_or, 1);
    add_word("xor", op_xor, 1);
    add_word("not", op_not, 1);

    /* Comparison */
    add_word("=", op_eq, 1);
    add_word("<", op_lt, 1);
    add_word(">", op_gt, 1);
    add_word("<=", op_le, 1);
    add_word(">=", op_ge, 1);
    add_word("<>", op_ne, 1);

    /* Loop control */
    add_word("do", op_do, 1);
    add_word("loop", op_loop, 1);
    add_word("+loop", op_plus_loop, 1);
    add_word("i", op_i, 1);
    add_word("j", op_j, 1);
    add_word("begin", op_begin, 1);
    add_word("until", op_until, 1);
    add_word("while", op_while, 1);
    add_word("repeat", op_repeat, 1);
    add_word("leave", op_leave, 1);

    /* Timing */
    add_word("ms", op_sleep, 1);

    /* MIDI output words */
    add_word("midi-output-list", op_midi_output_list, 1);
    add_word("midi-output-open", op_midi_output_open, 1);
    add_word("midi-output-close", op_midi_output_close, 1);
    /* Aliases for backward compatibility */
    add_word("midi-list", op_midi_output_list, 1);
    add_word("midi-open-port", op_midi_output_open, 1);
    add_word("midi-close", op_midi_output_close, 1);
    /* midi-open / midi-output-virtual / midi-open-as / midi-output-open-as
       handled specially in interpreter for optional string arg */
    add_word("cc", op_cc, 1);
    add_word("panic", op_all_notes_off, 1);

    /* MIDI input words */
    add_word("midi-input-list", op_midi_input_list, 1);
    add_word("midi-input-open", op_midi_input_open, 1);
    add_word("midi-input-virtual", op_midi_input_open_virtual, 1);
    add_word("midi-input-close", op_midi_input_close, 1);
    add_word("midi-input?", op_midi_input_pending, 1);
    add_word("midi-input@", op_midi_input_read, 1);
    add_word("midi-input-flush", op_midi_input_flush, 1);

    /* Concise notation */
    add_word(",", op_comma, 1);
    add_word("(", op_chord_open, 1);
    add_word(")", op_chord_close, 1);
    add_word("ch!", op_ch_store, 1);
    add_word("vel!", op_vel_store, 1);
    add_word("dur!", op_dur_store, 1);
    add_word("gate!", op_gate_store, 1);
    add_word("gate@", op_gate_fetch, 1);
    add_word("r", op_rest, 1);
    add_word("times", op_times, 1);

    /* Generative / Expression */
    add_word("|", op_alt_open, 1);
    add_word("[", op_bracket_open, 1);
    add_word("]", op_bracket_close, 1);
    add_word("%", op_percent, 1);

    /* Dynamics */
    add_word("ppp", op_ppp, 1);
    add_word("pp", op_pp, 1);
    add_word("p", op_p, 1);
    add_word("mp", op_mp, 1);
    add_word("mf", op_mf, 1);
    add_word("f", op_f, 1);
    add_word("ff", op_ff, 1);
    add_word("fff", op_fff, 1);

    /* Convenience */
    add_word("^", op_octave_up, 1);
    add_word("v", op_octave_down, 1);
    add_word("pc", op_program_change, 1);

    /* Advanced */
    add_word("pb", op_pitch_bend, 1);
    add_word("random", op_random, 1);

    /* Packed notes */
    add_word("note", op_note, 1);
    add_word("pitch@", op_pitch_fetch, 1);
    add_word("vel@", op_vel_fetch, 1);
    add_word("ch@", op_ch_fetch, 1);
    add_word("dur@", op_dur_fetch, 1);
    add_word("note.", op_note_print, 1);
    add_word("transpose", op_transpose, 1);
    add_word("note!", op_note_play, 1);
    add_word("bpm!", op_bpm_store, 1);
    add_word("bpm@", op_bpm_fetch, 1);
    add_word("ctx@", op_ctx_fetch, 1);

    /* Sequences */
    add_word("seq-new", op_seq_new, 1);
    add_word("seq-new!", op_seq_new_store, 1);
    add_word("seq", op_seq_select, 1);
    add_word("seq@", op_seq_current, 1);
    add_word("seq-start", op_seq_start, 1);
    add_word("seq-end", op_seq_end, 1);
    add_word("seq-length", op_seq_length, 1);
    add_word("seq-clear", op_seq_clear, 1);
    add_word("seq-play", op_seq_play, 1);
    add_word("seq-transpose", op_seq_transpose, 1);
    add_word("seq-show", op_seq_show, 1);
    add_word("seq-reverse", op_seq_reverse, 1);
    add_word("seq-stretch", op_seq_stretch, 1);
    add_word("seq-gc", op_seq_gc, 1);

    /* Async playback */
    add_word("seq-play&", op_seq_play_async, 1);
    add_word("seq-loop&", op_seq_loop_async, 1);
    add_word("seq-stop", op_seq_stop, 1);
    add_word("seq-stop-all", op_seq_stop_all, 1);
    add_word("seq-playing?", op_seq_playing, 1);
    add_word("seq-active", op_seq_active, 1);

    /* Pattern DSL */
    add_word("quarter", op_quarter, 1);
    add_word("half", op_half, 1);
    add_word("whole", op_whole, 1);
    add_word("eighth", op_eighth, 1);
    add_word("sixteenth", op_sixteenth, 1);

    add_word("major", op_chord_major, 1);
    add_word("minor", op_chord_minor, 1);
    add_word("dim", op_chord_dim, 1);
    add_word("aug", op_chord_aug, 1);
    add_word("dom7", op_chord_7, 1);
    add_word("maj7", op_chord_maj7, 1);
    add_word("min7", op_chord_min7, 1);

    add_word("play-chord", op_play_chord, 1);
    add_word("chord>seq", op_chord_to_seq, 1);
    add_word("arp>seq", op_arp_to_seq, 1);

    /* Generative music */
    add_word("seed!", op_seed_store, 1);
    add_word("seed@", op_seed_fetch, 1);
    add_word("next-random", op_next_random, 1);
    add_word("srand-range", op_srand_range, 1);
    add_word("chance", op_chance, 1);
    /* Note: [[ and ]] removed - use [ ] for sequences which also serve as lists */
    add_word("euclidean", op_euclidean, 1);
    add_word("reverse", op_reverse, 1);
    add_word("arp-up", op_arp_up, 1);
    add_word("arp-down", op_arp_down, 1);
    add_word("arp-up-down", op_arp_up_down, 1);
    add_word("retrograde", op_retrograde, 1);
    add_word("invert", op_invert, 1);
    add_word("shuffle", op_shuffle, 1);
    add_word("pick", op_pick_random, 1);
    add_word("pick-n", op_pick_n, 1);
    add_word("random-walk", op_random_walk, 1);
    add_word("drunk-walk", op_drunk_walk, 1);
    add_word("weighted-pick", op_weighted_pick, 1);
    add_word("concat", op_concat, 1);

    /* Scales */
    register_scale_words();

    /* Misc */
    add_word("words", op_words, 1);
    add_word("reset", op_reset, 1);
}
