/* forth_midi.h - Main header for MIDI Forth interpreter
 *
 * This header contains all shared types, constants, and declarations
 * used across the modular implementation.
 */

#ifndef FORTH_MIDI_H
#define FORTH_MIDI_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <unistd.h>
#include <time.h>

#include <libremidi/libremidi-c.h>
#include "music_theory.h"

/* ============================================================================
 * Constants
 * ============================================================================ */

#define MAX_STACK_SIZE 256
#define MAX_WORD_LENGTH 32
#define MAX_WORDS 200
#define MAX_INPUT_LENGTH 256
#define MAX_PORTS 64
#define MAX_DEFINITION_LENGTH 4096
#define MAX_BLOCKS 32
#define MAX_IF_NESTING 16
#define MAX_LOAD_DEPTH 8

/* Sequence system constants */
#define MAX_EVENTS 4096
#define MAX_SEQUENCES 64
#define MAX_SEQ_EVENTS 256
#define TICKS_PER_QUARTER 480

/* Recording system constants */
#define MAX_RECORDING_LINES 1024
#define MAX_RECORDING_LINE_LEN 512
#define MAX_CAPTURE_EVENTS 4096

/* Stack markers (sentinel values) */
#define BLOCK_MARKER    0x40000000
#define SEQ_MARKER      0x50000000
#define REST_MARKER     0x7FFFFFFE
#define CHORD_MARKER    0x7FFFFFFF
#define ALT_MARKER      0x7FFFFFFD
#define EXPLICIT_MARKER 0x7FFFFFFC

/* Parameter types for named parameter syntax */
#define PARAM_CHANNEL   1
#define PARAM_VELOCITY  2
#define PARAM_DURATION  3
#define PARAM_GATE      4
#define PARAM_BPM       5
#define PARAM_PROGRAM   6
#define PARAM_PAN       7
#define PARAM_CC        8

/* Parameter scope */
#define SCOPE_ONESHOT    0
#define SCOPE_PERSISTENT 1

/* Bracket sequence element types */
#define SEQ_ELEM_PITCH    0
#define SEQ_ELEM_INTERVAL 1
#define SEQ_ELEM_REST     2
#define SEQ_ELEM_DYNAMIC  3
#define SEQ_ELEM_DURATION 4
#define SEQ_ELEM_CHORD    5
#define SEQ_ELEM_NUMBER   6   /* Plain number for generative ops */

/* Bracket sequence constants */
#define MAX_BRACKET_SEQS   32
#define MAX_SEQ_ELEMENTS   256

/* MIDI event types */
#define EVT_NOTE_ON  0
#define EVT_NOTE_OFF 1
#define EVT_CC       2

/* Packed note format bit masks */
#define NOTE_PITCH_MASK     0x0000007F
#define NOTE_VEL_MASK       0x00003F80
#define NOTE_VEL_SHIFT      7
#define NOTE_CH_MASK        0x0003C000
#define NOTE_CH_SHIFT       14
#define NOTE_DUR_MASK       0xFFFC0000
#define NOTE_DUR_SHIFT      18

/* ============================================================================
 * Types
 * ============================================================================ */

/* Stack structure */
typedef struct {
    int32_t data[MAX_STACK_SIZE];
    int top;
} Stack;

/* Word structure for dictionary */
typedef struct {
    char name[MAX_WORD_LENGTH];
    void (*function)(Stack* stack);
    char* body;             /* For user-defined words: the token string */
    int is_primitive;       /* 1 = C function, 0 = user-defined */
} Word;

/* MIDI event for sequences */
typedef struct {
    uint16_t time;      /* tick offset from sequence start */
    uint8_t  type;      /* 0=note-on, 1=note-off, 2=cc */
    uint8_t  channel;   /* 0-15 */
    uint8_t  data1;     /* pitch or cc number */
    uint8_t  data2;     /* velocity or cc value */
} MidiEvent;

/* Sequence structure */
typedef struct {
    MidiEvent events[MAX_SEQ_EVENTS];
    int length;
    int bpm;            /* tempo in BPM */
} Sequence;

/* Captured event for recording */
typedef struct {
    uint32_t time_ms;   /* Timestamp in milliseconds from capture start */
    uint8_t  type;      /* 0=note-on, 1=note-off, 2=cc */
    uint8_t  channel;   /* 0-15 */
    uint8_t  data1;     /* pitch or cc number */
    uint8_t  data2;     /* velocity or cc value */
} CapturedEvent;

/* Bracket sequence element */
typedef struct {
    uint8_t type;           /* SEQ_ELEM_* */
    int16_t value;          /* pitch, interval, velocity, or duration */
    uint8_t chord_count;    /* for SEQ_ELEM_CHORD */
    int16_t chord_pitches[8];
} SeqElement;

/* Bracket sequence structure */
typedef struct {
    SeqElement elements[MAX_SEQ_ELEMENTS];
    int count;
} BracketSequence;

/* ============================================================================
 * Global Variables (extern declarations)
 * ============================================================================ */

/* Core interpreter state */
extern Stack stack;
extern Word dictionary[MAX_WORDS];
extern int dict_count;

/* Compile mode state */
extern int compile_mode;
extern char current_definition_name[MAX_WORD_LENGTH];
extern char current_definition_body[MAX_DEFINITION_LENGTH];
extern int definition_body_len;

/* Anonymous block state */
extern char* block_storage[MAX_BLOCKS];
extern int block_count;
extern int block_capture_mode;
extern char current_block_body[MAX_DEFINITION_LENGTH];
extern int block_body_len;
extern int block_nesting;

/* Conditional execution state */
extern int cond_skip_mode;
extern int cond_skip_nesting;
extern int cond_in_true_branch;

/* Track last executed word for 'times' loop */
extern char last_executed_word[MAX_WORD_LENGTH];

/* File loading depth */
extern int load_depth;

/* MIDI globals */
extern libremidi_midi_observer_handle* midi_observer;
extern libremidi_midi_out_handle* midi_out;
extern libremidi_midi_out_port* out_ports[MAX_PORTS];
extern int out_port_count;

/* Context defaults for concise notation */
extern int default_channel;
extern int default_velocity;
extern int default_duration;
extern int current_pitch;

/* Articulation flags */
extern int articulation_staccato;
extern int articulation_accent;
extern int articulation_tenuto;

/* Sequences */
extern Sequence sequences[MAX_SEQUENCES];
extern int sequence_count;
extern int current_seq;
extern int global_bpm;

/* Recording system */
extern char* recording_buffer[MAX_RECORDING_LINES];
extern int recording_count;
extern int recording_active;

/* MIDI capture system */
extern CapturedEvent capture_buffer[MAX_CAPTURE_EVENTS];
extern int capture_count;
extern int capture_active;
extern struct timespec capture_start_time;

/* Generative music PRNG state */
extern int32_t prng_seed;

/* Named parameter system state */
extern int default_gate;          /* Gate percentage (1-100) */
extern int pending_channel;       /* One-shot channel override (-1 if not set) */
extern int pending_velocity;      /* One-shot velocity override (-1 if not set) */
extern int pending_duration;      /* One-shot duration override (-1 if not set) */
extern int pending_gate;          /* One-shot gate override (-1 if not set) */

/* Bracket sequence system */
extern BracketSequence* bracket_seq_storage[MAX_BRACKET_SEQS];
extern int bracket_seq_count;
extern int seq_capture_mode;
extern int seq_capture_count;
extern int seq_capture_chord_mode;
extern int seq_capture_chord_count;
extern int16_t seq_capture_chord_buffer[8];
extern BracketSequence* current_bracket_seq;

/* ============================================================================
 * Inline Functions - Packed Notes
 * ============================================================================ */

static inline int32_t pack_note(int pitch, int vel, int ch, int dur) {
    return (pitch & 0x7F)
         | ((vel & 0x7F) << NOTE_VEL_SHIFT)
         | ((ch & 0x0F) << NOTE_CH_SHIFT)
         | ((dur & 0x3FFF) << NOTE_DUR_SHIFT);
}

static inline int note_pitch(int32_t n) { return n & NOTE_PITCH_MASK; }
static inline int note_vel(int32_t n)   { return (n & NOTE_VEL_MASK) >> NOTE_VEL_SHIFT; }
static inline int note_ch(int32_t n)    { return (n & NOTE_CH_MASK) >> NOTE_CH_SHIFT; }
static inline int note_dur(int32_t n)   { return (n & NOTE_DUR_MASK) >> NOTE_DUR_SHIFT; }

/* ============================================================================
 * Function Declarations - Stack (stack.c)
 * ============================================================================ */

void push(Stack* s, int32_t value);
int32_t pop(Stack* s);
int32_t peek(Stack* s);
void op_dup(Stack* stack);
void op_drop(Stack* stack);
void op_swap(Stack* stack);
void op_over(Stack* stack);
void op_rot(Stack* stack);
void op_depth(Stack* stack);
void op_clear(Stack* stack);
void op_print_stack(Stack* stack);

/* ============================================================================
 * Function Declarations - MIDI Core (midi_core.c)
 * ============================================================================ */

void midi_init_observer(void);
void midi_cleanup_observer(void);
void midi_send_note_on(int pitch, int velocity, int channel);
void midi_send_note_off(int pitch, int channel);
void midi_send_cc(int cc, int value, int channel);
void midi_send_program_change(int program, int channel);
void midi_send_pitch_bend(int value, int channel);
void midi_sleep_ms(int ms);

int open_virtual_port(const char* name);
void op_midi_list(Stack* stack);
void op_midi_open(Stack* stack);
void op_midi_open_port(Stack* stack);
void op_midi_close(Stack* stack);
void op_all_notes_off(Stack* stack);
void op_sleep(Stack* stack);

/* ============================================================================
 * Function Declarations - Notation (notation.c)
 * ============================================================================ */

int parse_pitch(const char* token);
void play_single_note(Stack* stack, int pitch);
void play_chord_notes(Stack* stack);
void op_comma(Stack* stack);
void op_chord_open(Stack* stack);
void op_chord_close(Stack* stack);
void op_alt_open(Stack* stack);
void op_percent(Stack* stack);
void op_pitch_bend(Stack* stack);
void op_ch_store(Stack* stack);
void op_ch_fetch_default(Stack* stack);
void op_vel_store(Stack* stack);
void op_vel_fetch_default(Stack* stack);
void op_dur_store(Stack* stack);
void op_dur_fetch_default(Stack* stack);
void op_gate_store(Stack* stack);
void op_gate_fetch(Stack* stack);
void op_octave_up(Stack* stack);
void op_octave_down(Stack* stack);
void op_program_change(Stack* stack);

/* ============================================================================
 * Function Declarations - Generative (generative.c)
 * ============================================================================ */

void op_seed_store(Stack* stack);
void op_seed_fetch(Stack* stack);
void op_next_random(Stack* stack);
void op_srand_range(Stack* stack);
void op_chance(Stack* stack);
void op_random(Stack* stack);
void op_euclidean(Stack* stack);
void op_reverse(Stack* stack);
void op_arp_up(Stack* stack);
void op_arp_down(Stack* stack);
void op_arp_up_down(Stack* stack);
void op_retrograde(Stack* stack);
void op_invert(Stack* stack);
void op_shuffle(Stack* stack);
void op_pick_random(Stack* stack);
void op_pick_n(Stack* stack);
void op_random_walk(Stack* stack);
void op_drunk_walk(Stack* stack);
void op_weighted_pick(Stack* stack);
void op_concat(Stack* stack);
void op_btranspose(Stack* stack);

/* ============================================================================
 * Function Declarations - Sequences (sequences.c)
 * ============================================================================ */

void op_seq_new(Stack* stack);
void op_seq_select(Stack* stack);
void op_seq_current(Stack* stack);
void op_seq_note(Stack* stack);
void op_seq_note_ch(Stack* stack);
void op_seq_add(Stack* stack);
void op_seq_length(Stack* stack);
void op_seq_clear(Stack* stack);
void op_seq_play(Stack* stack);
void op_seq_transpose(Stack* stack);
void op_seq_show(Stack* stack);
void op_seq_reverse(Stack* stack);
void op_seq_stretch(Stack* stack);
void op_bpm_store(Stack* stack);
void op_bpm_fetch(Stack* stack);

/* ============================================================================
 * Function Declarations - Packed Notes (packed_notes.c)
 * ============================================================================ */

void op_note(Stack* stack);
void op_pitch_fetch(Stack* stack);
void op_vel_fetch(Stack* stack);
void op_ch_fetch(Stack* stack);
void op_dur_fetch(Stack* stack);
void op_note_print(Stack* stack);
void op_transpose(Stack* stack);
void op_note_play(Stack* stack);

/* ============================================================================
 * Function Declarations - Patterns (patterns.c)
 * ============================================================================ */

void op_quarter(Stack* stack);
void op_half(Stack* stack);
void op_whole(Stack* stack);
void op_eighth(Stack* stack);
void op_sixteenth(Stack* stack);
void op_chord_major(Stack* stack);
void op_chord_minor(Stack* stack);
void op_chord_dim(Stack* stack);
void op_chord_aug(Stack* stack);
void op_chord_7(Stack* stack);
void op_chord_maj7(Stack* stack);
void op_chord_min7(Stack* stack);
void op_play_chord(Stack* stack);
void op_chord_to_seq(Stack* stack);
void op_arp_to_seq(Stack* stack);

/* ============================================================================
 * Function Declarations - Scales (scales.c)
 * ============================================================================ */

void op_scale_major(Stack* stack);
void op_scale_dorian(Stack* stack);
void op_scale_phrygian(Stack* stack);
void op_scale_lydian(Stack* stack);
void op_scale_mixolydian(Stack* stack);
void op_scale_minor(Stack* stack);
void op_scale_locrian(Stack* stack);
void op_scale_harmonic_minor(Stack* stack);
void op_scale_melodic_minor(Stack* stack);
void op_scale_pentatonic(Stack* stack);
void op_scale_pentatonic_minor(Stack* stack);
void op_scale_blues(Stack* stack);
void op_scale_whole_tone(Stack* stack);
void op_scale_chromatic(Stack* stack);
void op_scale_diminished_hw(Stack* stack);
void op_scale_diminished_wh(Stack* stack);
void op_scale_augmented_scale(Stack* stack);
void op_scale_bebop_dominant(Stack* stack);
void op_scale_bebop_major(Stack* stack);
void op_scale_bebop_minor(Stack* stack);
void op_scale_hungarian_minor(Stack* stack);
void op_scale_double_harmonic(Stack* stack);
void op_scale_neapolitan_major(Stack* stack);
void op_scale_neapolitan_minor(Stack* stack);
void register_scale_words(void);

/* ============================================================================
 * Function Declarations - Recording (recording.c)
 * ============================================================================ */

void capture_add_event(int type, int channel, int data1, int data2);
void recording_add_line(const char* line);
int recording_save(const char* filename);
void recording_clear(void);
int capture_save_midi(const char* filename);
void capture_clear(void);
void op_rec_start(Stack* stack);
void op_rec_stop(Stack* stack);
void op_rec_save(Stack* stack);
void op_capture_start(Stack* stack);
void op_capture_stop(Stack* stack);
void op_save_midi(Stack* stack);

/* Standard MIDI file I/O */
int capture_write_mid(const char* filename);
int capture_read_mid(const char* filename);
void op_write_mid(Stack* stack);
void op_read_mid(Stack* stack);

/* ============================================================================
 * Function Declarations - Dictionary (dictionary.c)
 * ============================================================================ */

void add_word(const char* name, void (*func)(Stack*), int is_primitive);
void register_user_word(const char* name, const char* body);
Word* find_word(const char* name);
void init_dictionary(void);
void op_words(Stack* stack);

/* ============================================================================
 * Function Declarations - Interpreter (interpreter.c)
 * ============================================================================ */

void process_token(const char* token);
void interpret(const char* input);
int load_file(const char* filename);
void op_colon(Stack* stack);
void op_semicolon(Stack* stack);
void op_if(Stack* stack);
void op_else(Stack* stack);
void op_then(Stack* stack);
void op_block_open(Stack* stack);
void op_block_close(Stack* stack);
void op_times(Stack* stack);
void op_star(Stack* stack);
void op_load(Stack* stack);
void op_help(Stack* stack);

/* Parameter system functions */
int parse_param_assign(const char* token);
int effective_channel(void);
int effective_velocity(void);
int effective_duration(void);
int effective_gate(void);
void clear_pending_params(void);

/* Bracket sequence functions */
void execute_bracket_sequence(BracketSequence* seq);

/* ============================================================================
 * Function Declarations - Arithmetic (primitives.c)
 * ============================================================================ */

void op_add(Stack* stack);
void op_sub(Stack* stack);
void op_mul(Stack* stack);
void op_div(Stack* stack);
void op_mod(Stack* stack);
void op_abs(Stack* stack);
void op_negate(Stack* stack);
void op_min(Stack* stack);
void op_max(Stack* stack);
void op_and(Stack* stack);
void op_or(Stack* stack);
void op_xor(Stack* stack);
void op_not(Stack* stack);
void op_eq(Stack* stack);
void op_lt(Stack* stack);
void op_gt(Stack* stack);
void op_le(Stack* stack);
void op_ge(Stack* stack);
void op_ne(Stack* stack);
void op_print(Stack* stack);
void op_cr(Stack* stack);
void op_space(Stack* stack);
void op_emit(Stack* stack);

/* ============================================================================
 * Function Declarations - Readline (readline_comp.c)
 * ============================================================================ */

void init_readline_completion(void);

#endif /* FORTH_MIDI_H */
