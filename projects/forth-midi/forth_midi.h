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
#define MAX_LOOP_NESTING 8
#define MAX_LOOP_BODY 2048

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

/* MIDI input constants */
#define MAX_INPUT_PORTS 64
#define MIDI_INPUT_QUEUE_SIZE 256

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
    void (*function)(Stack* s);
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
    int ref_count;  /* Reference count for memory management */
} BracketSequence;

/* MIDI input message (received from callback) */
typedef struct {
    uint8_t  status;    /* MIDI status byte (includes channel for channel messages) */
    uint8_t  data1;     /* First data byte (pitch, CC number, etc.) */
    uint8_t  data2;     /* Second data byte (velocity, CC value, etc.) */
    uint8_t  length;    /* Message length (1-3) */
} MidiInputMessage;

/* Thread-safe ring buffer for MIDI input messages */
typedef struct {
    MidiInputMessage messages[MIDI_INPUT_QUEUE_SIZE];
    volatile int head;  /* Write position (producer) */
    volatile int tail;  /* Read position (consumer) */
} MidiInputQueue;

/* ============================================================================
 * ForthContext - Encapsulates all interpreter state
 * ============================================================================ */

typedef struct ForthContext {
    /* Core interpreter state */
    Stack stack;
    Word dictionary[MAX_WORDS];
    int dict_count;

    /* Compile mode state */
    int compile_mode;
    char current_definition_name[MAX_WORD_LENGTH];
    char current_definition_body[MAX_DEFINITION_LENGTH];
    int definition_body_len;

    /* Anonymous block state */
    char* block_storage[MAX_BLOCKS];
    int block_count;
    int block_capture_mode;
    char current_block_body[MAX_DEFINITION_LENGTH];
    int block_body_len;
    int block_nesting;

    /* Conditional execution state */
    int cond_skip_mode;
    int cond_skip_nesting;
    int cond_in_true_branch;

    /* Track last executed word */
    char last_executed_word[MAX_WORD_LENGTH];

    /* File loading depth and error context */
    int load_depth;
    const char* current_file;  /* Current file being loaded (NULL if REPL) */
    int current_line;          /* Current line number in file */

    /* MIDI output handles */
    libremidi_midi_observer_handle* midi_observer;
    libremidi_midi_out_handle* midi_out;
    libremidi_midi_out_port* out_ports[MAX_PORTS];
    int out_port_count;

    /* MIDI input handles */
    libremidi_midi_in_handle* midi_in;
    libremidi_midi_in_port* in_ports[MAX_INPUT_PORTS];
    int in_port_count;
    MidiInputQueue input_queue;

    /* Context defaults for concise notation */
    int default_channel;
    int default_velocity;
    int default_duration;
    int current_pitch;

    /* Articulation flags */
    int articulation_staccato;
    int articulation_accent;
    int articulation_tenuto;

    /* Sequences */
    Sequence sequences[MAX_SEQUENCES];
    int sequence_count;
    int current_seq;
    int global_bpm;

    /* Recording system */
    char* recording_buffer[MAX_RECORDING_LINES];
    int recording_count;
    int recording_active;

    /* MIDI capture system */
    CapturedEvent capture_buffer[MAX_CAPTURE_EVENTS];
    int capture_count;
    int capture_active;
    struct timespec capture_start_time;

    /* Generative music PRNG state */
    int32_t prng_seed;

    /* Sleep disable flag (for CI/testing) */
    int no_sleep_mode;

    /* Named parameter system state */
    int default_gate;
    int pending_channel;
    int pending_velocity;
    int pending_duration;
    int pending_gate;

    /* Bracket sequence system */
    BracketSequence* bracket_seq_storage[MAX_BRACKET_SEQS];
    int bracket_seq_count;
    int seq_capture_mode;
    int seq_capture_count;
    int seq_capture_chord_mode;
    int seq_capture_chord_count;
    int16_t seq_capture_chord_buffer[8];
    BracketSequence* current_bracket_seq;

    /* Sequence recording mode (seq-start/seq-end) */
    int seq_recording_mode;      /* 1 if recording to a sequence */
    int seq_recording_id;        /* Which sequence we're recording to */
    int seq_recording_time;      /* Current time offset in ticks */

    /* Loop control state */
    int loop_capture_mode;       /* 1 = capturing do...loop, 2 = begin...until, 3 = begin...while */
    int loop_nesting;            /* Nesting depth for nested loops */
    char loop_body[MAX_LOOP_BODY];
    int loop_body_len;
    char loop_cond[MAX_LOOP_BODY];  /* For begin...while...repeat: condition part */
    int loop_cond_len;

    /* Return stack for do...loop (holds limit and index pairs) */
    int32_t return_stack[MAX_LOOP_NESTING * 2];
    int return_stack_top;
} ForthContext;

/* Global context instance */
extern ForthContext g_ctx;

/* Initialize context with default values */
void forth_context_init(ForthContext* ctx);

/* Reset runtime state (preserves dictionary and MIDI connections) */
void forth_context_reset(ForthContext* ctx);

/* ============================================================================
 * Global Variable Macros - Redirect to g_ctx fields
 * These macros allow existing code to work unchanged while using g_ctx
 * Define FORTH_NO_MACROS before including to access ctx members directly.
 * ============================================================================ */

#ifndef FORTH_NO_MACROS

/* Core interpreter state */
#define stack                       (g_ctx.stack)
#define dictionary                  (g_ctx.dictionary)
#define dict_count                  (g_ctx.dict_count)

/* Compile mode state */
#define compile_mode                (g_ctx.compile_mode)
#define current_definition_name     (g_ctx.current_definition_name)
#define current_definition_body     (g_ctx.current_definition_body)
#define definition_body_len         (g_ctx.definition_body_len)

/* Anonymous block state */
#define block_storage               (g_ctx.block_storage)
#define block_count                 (g_ctx.block_count)
#define block_capture_mode          (g_ctx.block_capture_mode)
#define current_block_body          (g_ctx.current_block_body)
#define block_body_len              (g_ctx.block_body_len)
#define block_nesting               (g_ctx.block_nesting)

/* Conditional execution state */
#define cond_skip_mode              (g_ctx.cond_skip_mode)
#define cond_skip_nesting           (g_ctx.cond_skip_nesting)
#define cond_in_true_branch         (g_ctx.cond_in_true_branch)

/* Track last executed word for 'times' loop */
#define last_executed_word          (g_ctx.last_executed_word)

/* File loading depth */
#define load_depth                  (g_ctx.load_depth)
#define current_file                (g_ctx.current_file)
#define current_line                (g_ctx.current_line)

/* MIDI output globals */
#define midi_observer               (g_ctx.midi_observer)
#define midi_out                    (g_ctx.midi_out)
#define out_ports                   (g_ctx.out_ports)
#define out_port_count              (g_ctx.out_port_count)

/* MIDI input globals */
#define midi_in                     (g_ctx.midi_in)
#define in_ports                    (g_ctx.in_ports)
#define in_port_count               (g_ctx.in_port_count)
#define input_queue                 (g_ctx.input_queue)

/* Context defaults for concise notation */
#define default_channel             (g_ctx.default_channel)
#define default_velocity            (g_ctx.default_velocity)
#define default_duration            (g_ctx.default_duration)
#define current_pitch               (g_ctx.current_pitch)

/* Articulation flags */
#define articulation_staccato       (g_ctx.articulation_staccato)
#define articulation_accent         (g_ctx.articulation_accent)
#define articulation_tenuto         (g_ctx.articulation_tenuto)

/* Sequences */
#define sequences                   (g_ctx.sequences)
#define sequence_count              (g_ctx.sequence_count)
#define current_seq                 (g_ctx.current_seq)
#define global_bpm                  (g_ctx.global_bpm)

/* Recording system */
#define recording_buffer            (g_ctx.recording_buffer)
#define recording_count             (g_ctx.recording_count)
#define recording_active            (g_ctx.recording_active)

/* MIDI capture system */
#define capture_buffer              (g_ctx.capture_buffer)
#define capture_count               (g_ctx.capture_count)
#define capture_active              (g_ctx.capture_active)
#define capture_start_time          (g_ctx.capture_start_time)

/* Generative music PRNG state */
#define prng_seed                   (g_ctx.prng_seed)

/* Sleep disable flag */
#define no_sleep_mode               (g_ctx.no_sleep_mode)

/* Named parameter system state */
#define default_gate                (g_ctx.default_gate)
#define pending_channel             (g_ctx.pending_channel)
#define pending_velocity            (g_ctx.pending_velocity)
#define pending_duration            (g_ctx.pending_duration)
#define pending_gate                (g_ctx.pending_gate)

/* Bracket sequence system */
#define bracket_seq_storage         (g_ctx.bracket_seq_storage)
#define bracket_seq_count           (g_ctx.bracket_seq_count)
#define seq_capture_mode            (g_ctx.seq_capture_mode)
#define seq_capture_count           (g_ctx.seq_capture_count)
#define seq_capture_chord_mode      (g_ctx.seq_capture_chord_mode)
#define seq_capture_chord_count     (g_ctx.seq_capture_chord_count)
#define seq_capture_chord_buffer    (g_ctx.seq_capture_chord_buffer)
#define current_bracket_seq         (g_ctx.current_bracket_seq)

/* Sequence recording mode */
#define seq_recording_mode          (g_ctx.seq_recording_mode)
#define seq_recording_id            (g_ctx.seq_recording_id)
#define seq_recording_time          (g_ctx.seq_recording_time)

/* Loop control state */
#define loop_capture_mode           (g_ctx.loop_capture_mode)
#define loop_nesting                (g_ctx.loop_nesting)
#define loop_body                   (g_ctx.loop_body)
#define loop_body_len               (g_ctx.loop_body_len)
#define loop_cond                   (g_ctx.loop_cond)
#define loop_cond_len               (g_ctx.loop_cond_len)
#define return_stack                (g_ctx.return_stack)
#define return_stack_top            (g_ctx.return_stack_top)

#endif /* FORTH_NO_MACROS */

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
void op_dup(Stack* s);
void op_drop(Stack* s);
void op_swap(Stack* s);
void op_over(Stack* s);
void op_rot(Stack* s);
void op_depth(Stack* s);
void op_clear(Stack* s);
void op_print_stack(Stack* s);

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
int forth_no_sleep(void);       /* Returns 1 if --no-sleep mode is active */
void forth_set_no_sleep(int v); /* Set no-sleep mode (call from main) */

int open_virtual_port(const char* name);
void op_midi_output_list(Stack* s);
void op_midi_output_open(Stack* s);
void op_midi_output_virtual(Stack* s);
void op_midi_output_close(Stack* s);
void op_all_notes_off(Stack* s);
void op_sleep(Stack* s);

/* MIDI input functions */
void midi_input_init(void);
void midi_input_cleanup(void);
int midi_input_pending(void);
int midi_input_dequeue(MidiInputMessage* msg);
void op_midi_input_list(Stack* s);
void op_midi_input_open(Stack* s);
void op_midi_input_open_virtual(Stack* s);
void op_midi_input_close(Stack* s);
void op_midi_input_pending(Stack* s);
void op_midi_input_read(Stack* s);
void op_midi_input_flush(Stack* s);

/* ============================================================================
 * Function Declarations - Notation (notation.c)
 * ============================================================================ */

int parse_pitch(const char* token);
void play_single_note(Stack* s, int pitch);
void play_chord_notes(Stack* s);
void op_comma(Stack* s);
void op_chord_open(Stack* s);
void op_chord_close(Stack* s);
void op_alt_open(Stack* s);
void op_percent(Stack* s);
void op_pitch_bend(Stack* s);
void op_ch_store(Stack* s);
void op_ch_fetch_default(Stack* s);
void op_vel_store(Stack* s);
void op_vel_fetch_default(Stack* s);
void op_dur_store(Stack* s);
void op_dur_fetch_default(Stack* s);
void op_gate_store(Stack* s);
void op_gate_fetch(Stack* s);
void op_octave_up(Stack* s);
void op_octave_down(Stack* s);
void op_program_change(Stack* s);
void op_ctx_fetch(Stack* s);

/* ============================================================================
 * Function Declarations - Generative (generative.c)
 * ============================================================================ */

void op_seed_store(Stack* s);
void op_seed_fetch(Stack* s);
void op_next_random(Stack* s);
void op_srand_range(Stack* s);
void op_chance(Stack* s);
void op_random(Stack* s);
void op_euclidean(Stack* s);
void op_reverse(Stack* s);
void op_arp_up(Stack* s);
void op_arp_down(Stack* s);
void op_arp_up_down(Stack* s);
void op_retrograde(Stack* s);
void op_invert(Stack* s);
void op_shuffle(Stack* s);
void op_pick_random(Stack* s);
void op_pick_n(Stack* s);
void op_random_walk(Stack* s);
void op_drunk_walk(Stack* s);
void op_weighted_pick(Stack* s);
void op_concat(Stack* s);

/* ============================================================================
 * Function Declarations - Sequences (sequences.c)
 * ============================================================================ */

void op_seq_new(Stack* s);
void op_seq_new_store(Stack* s);
void op_seq_select(Stack* s);
void op_seq_current(Stack* s);
void op_seq_start(Stack* s);
void op_seq_end(Stack* s);
void op_seq_length(Stack* s);
void op_seq_clear(Stack* s);
void op_seq_play(Stack* s);
void op_seq_transpose(Stack* s);
void op_seq_show(Stack* s);
void op_seq_reverse(Stack* s);
void op_seq_stretch(Stack* s);
void op_bpm_store(Stack* s);
void op_bpm_fetch(Stack* s);
int seq_write_mid(const char* filename);
int seq_save(const char* filename);
void op_seq_write_mid(Stack* s);
void op_seq_save(Stack* s);

/* ============================================================================
 * Function Declarations - Packed Notes (packed_notes.c)
 * ============================================================================ */

void op_note(Stack* s);
void op_pitch_fetch(Stack* s);
void op_vel_fetch(Stack* s);
void op_ch_fetch(Stack* s);
void op_dur_fetch(Stack* s);
void op_note_print(Stack* s);
void op_transpose(Stack* s);
void op_note_play(Stack* s);

/* ============================================================================
 * Function Declarations - Patterns (patterns.c)
 * ============================================================================ */

void op_quarter(Stack* s);
void op_half(Stack* s);
void op_whole(Stack* s);
void op_eighth(Stack* s);
void op_sixteenth(Stack* s);
void op_chord_major(Stack* s);
void op_chord_minor(Stack* s);
void op_chord_dim(Stack* s);
void op_chord_aug(Stack* s);
void op_chord_7(Stack* s);
void op_chord_maj7(Stack* s);
void op_chord_min7(Stack* s);
void op_play_chord(Stack* s);
void op_chord_to_seq(Stack* s);
void op_arp_to_seq(Stack* s);

/* ============================================================================
 * Function Declarations - Scales (scales.c)
 * ============================================================================ */

void op_scale_major(Stack* s);
void op_scale_dorian(Stack* s);
void op_scale_phrygian(Stack* s);
void op_scale_lydian(Stack* s);
void op_scale_mixolydian(Stack* s);
void op_scale_minor(Stack* s);
void op_scale_locrian(Stack* s);
void op_scale_harmonic_minor(Stack* s);
void op_scale_melodic_minor(Stack* s);
void op_scale_pentatonic(Stack* s);
void op_scale_pentatonic_minor(Stack* s);
void op_scale_blues(Stack* s);
void op_scale_whole_tone(Stack* s);
void op_scale_chromatic(Stack* s);
void op_scale_diminished_hw(Stack* s);
void op_scale_diminished_wh(Stack* s);
void op_scale_augmented_scale(Stack* s);
void op_scale_bebop_dominant(Stack* s);
void op_scale_bebop_major(Stack* s);
void op_scale_bebop_minor(Stack* s);
void op_scale_hungarian_minor(Stack* s);
void op_scale_double_harmonic(Stack* s);
void op_scale_neapolitan_major(Stack* s);
void op_scale_neapolitan_minor(Stack* s);
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
void op_rec_start(Stack* s);
void op_rec_stop(Stack* s);
void op_rec_save(Stack* s);
void op_capture_start(Stack* s);
void op_capture_stop(Stack* s);
void op_save_midi(Stack* s);

/* Standard MIDI file I/O */
int capture_write_mid(const char* filename);
int capture_read_mid(const char* filename);
void op_write_mid(Stack* s);
void op_read_mid(Stack* s);

/* ============================================================================
 * Function Declarations - Dictionary (dictionary.c)
 * ============================================================================ */

void add_word(const char* name, void (*func)(Stack*), int is_primitive);
void register_user_word(const char* name, const char* body);
Word* find_word(const char* name);
void init_dictionary(void);
void op_words(Stack* s);
void op_reset(Stack* s);

/* ============================================================================
 * Function Declarations - Interpreter (interpreter.c)
 * ============================================================================ */

void process_token(const char* token);
void interpret(const char* input);
int load_file(const char* filename);
void op_colon(Stack* s);
void op_semicolon(Stack* s);
void op_if(Stack* s);
void op_else(Stack* s);
void op_then(Stack* s);
void op_block_open(Stack* s);
void op_block_close(Stack* s);
void op_times(Stack* s);
void op_star(Stack* s);
void op_load(Stack* s);
void op_help(Stack* s);

/* Loop control words */
void op_do(Stack* s);
void op_loop(Stack* s);
void op_plus_loop(Stack* s);
void op_i(Stack* s);
void op_j(Stack* s);
void op_begin(Stack* s);
void op_until(Stack* s);
void op_while(Stack* s);
void op_repeat(Stack* s);
void op_leave(Stack* s);

/* Parameter system functions */
int parse_param_assign(const char* token);
int effective_channel(void);
int effective_velocity(void);
int effective_duration(void);
int effective_gate(void);
void clear_pending_params(void);

/* Bracket sequence functions */
void execute_bracket_sequence(BracketSequence* seq);
BracketSequence* seq_alloc(void);
void seq_retain(BracketSequence* seq);
void seq_release(BracketSequence* seq);
void seq_cleanup_all(void);
void op_seq_gc(Stack* s);

/* ============================================================================
 * Function Declarations - Arithmetic (primitives.c)
 * ============================================================================ */

void op_add(Stack* s);
void op_sub(Stack* s);
void op_mul(Stack* s);
void op_div(Stack* s);
void op_mod(Stack* s);
void op_abs(Stack* s);
void op_negate(Stack* s);
void op_min(Stack* s);
void op_max(Stack* s);
void op_and(Stack* s);
void op_or(Stack* s);
void op_xor(Stack* s);
void op_not(Stack* s);
void op_eq(Stack* s);
void op_lt(Stack* s);
void op_gt(Stack* s);
void op_le(Stack* s);
void op_ge(Stack* s);
void op_ne(Stack* s);
void op_print(Stack* s);
void op_cr(Stack* s);
void op_space(Stack* s);
void op_emit(Stack* s);

/* ============================================================================
 * Function Declarations - Async Player (async_player.c)
 * ============================================================================ */

int async_player_init(void);
void async_player_cleanup(void);
int async_player_start(int seq_id);
int async_player_loop(int seq_id);
int async_player_stop_seq(int seq_id);
void async_player_stop_all(void);
void async_player_stop(void);
int async_player_is_playing(void);
int async_player_active_count(void);

void op_seq_play_async(Stack* s);
void op_seq_loop_async(Stack* s);
void op_seq_stop(Stack* s);
void op_seq_stop_all(Stack* s);
void op_seq_playing(Stack* s);
void op_seq_active(Stack* s);

/* ============================================================================
 * Function Declarations - Readline (readline_comp.c)
 * ============================================================================ */

void init_readline_completion(void);

#endif /* FORTH_MIDI_H */
