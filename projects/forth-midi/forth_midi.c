#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <unistd.h>
#include <time.h>
#include <readline/readline.h>
#include <readline/history.h>

#include <libremidi/libremidi-c.h>

#define MAX_STACK_SIZE 256
#define MAX_WORD_LENGTH 32
#define MAX_WORDS 200
#define MAX_INPUT_LENGTH 256
#define MAX_PORTS 64

// Sequence system constants
#define MAX_EVENTS 4096
#define MAX_SEQUENCES 64
#define MAX_SEQ_EVENTS 256
#define TICKS_PER_QUARTER 480

// Stack structure
typedef struct {
    int32_t data[MAX_STACK_SIZE];
    int top;
} Stack;

// Word structure for dictionary
typedef struct {
    char name[MAX_WORD_LENGTH];
    void (*function)(Stack* stack);
    char* body;             // For user-defined words: the token string
    int is_primitive;       // 1 = C function, 0 = user-defined
} Word;

// Compile mode state
#define MAX_DEFINITION_LENGTH 4096
int compile_mode = 0;
char current_definition_name[MAX_WORD_LENGTH];
char current_definition_body[MAX_DEFINITION_LENGTH];
int definition_body_len = 0;

// Anonymous block state
#define MAX_BLOCKS 32
char* block_storage[MAX_BLOCKS];  // Storage for anonymous blocks
int block_count = 0;
int block_capture_mode = 0;       // 1 = capturing block content
char current_block_body[MAX_DEFINITION_LENGTH];
int block_body_len = 0;
int block_nesting = 0;            // Track nested { }

// Block marker on stack (high bit set to distinguish from other values)
#define BLOCK_MARKER 0x40000000

// Conditional execution state
#define MAX_IF_NESTING 16
int cond_skip_mode = 0;          // 1 = skipping tokens until else/then
int cond_skip_nesting = 0;       // Track nested if within skip
int cond_in_true_branch = 0;     // 1 = currently in true branch (skip at else)

// Rest marker (sentinel value)
#define REST_MARKER 0x7FFFFFFE

// Track last executed word for 'times' loop
char last_executed_word[MAX_WORD_LENGTH] = "";

// File loading - track nested load depth to prevent infinite recursion
#define MAX_LOAD_DEPTH 8
int load_depth = 0;

// Recording system - capture input commands to save as .4th file
#define MAX_RECORDING_LINES 1024
#define MAX_RECORDING_LINE_LEN 512
char* recording_buffer[MAX_RECORDING_LINES];
int recording_count = 0;
int recording_active = 0;

// MIDI capture system - record MIDI events with timing
#define MAX_CAPTURE_EVENTS 4096
typedef struct {
    uint32_t time_ms;    // Timestamp in milliseconds from capture start
    uint8_t  type;       // 0=note-on, 1=note-off, 2=cc
    uint8_t  channel;    // 0-15
    uint8_t  data1;      // pitch or cc number
    uint8_t  data2;      // velocity or cc value
} CapturedEvent;

CapturedEvent capture_buffer[MAX_CAPTURE_EVENTS];
int capture_count = 0;
int capture_active = 0;
struct timespec capture_start_time;

// Forward declaration for capture function (defined later)
void capture_add_event(int type, int channel, int data1, int data2);

// ============================================================================
// Phase 1: Packed Note Format
// ============================================================================
// Packed note: 32 bits
//   bits 0-6:   pitch (0-127)
//   bits 7-13:  velocity (0-127)
//   bits 14-17: channel (0-15, displayed as 1-16)
//   bits 18-31: duration in ticks (0-16383)

#define NOTE_PITCH_MASK     0x0000007F
#define NOTE_VEL_MASK       0x00003F80
#define NOTE_VEL_SHIFT      7
#define NOTE_CH_MASK        0x0003C000
#define NOTE_CH_SHIFT       14
#define NOTE_DUR_MASK       0xFFFC0000
#define NOTE_DUR_SHIFT      18

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

// ============================================================================
// Phase 2: Sequence Data Structures
// ============================================================================

typedef struct {
    uint16_t time;      // tick offset from sequence start
    uint8_t  type;      // 0=note-on, 1=note-off, 2=cc
    uint8_t  channel;   // 0-15
    uint8_t  data1;     // pitch or cc number
    uint8_t  data2;     // velocity or cc value
} MidiEvent;

#define EVT_NOTE_ON  0
#define EVT_NOTE_OFF 1
#define EVT_CC       2

typedef struct {
    MidiEvent events[MAX_SEQ_EVENTS];
    int length;
    int bpm;            // tempo in BPM
} Sequence;

// Global sequence storage
Sequence sequences[MAX_SEQUENCES];
int sequence_count = 0;
int current_seq = -1;   // currently selected sequence

// Timing
int global_bpm = 120;

// Global variables
Stack stack;
Word dictionary[MAX_WORDS];
int dict_count = 0;

// MIDI globals
libremidi_midi_observer_handle* midi_observer = NULL;
libremidi_midi_out_handle* midi_out = NULL;
libremidi_midi_out_port* out_ports[MAX_PORTS];
int out_port_count = 0;

// Context defaults for concise notation
int default_channel = 1;      // 1-16
int default_velocity = 80;    // 0-127
int default_duration = 500;   // milliseconds
int current_pitch = 60;       // Track last played pitch for relative intervals

// Articulation flags (set by pitch parser, consumed by play)
int articulation_staccato = 0;   // Reduce duration to 50%
int articulation_accent = 0;     // Increase velocity by 20

// Chord marker (sentinel value on stack)
#define CHORD_MARKER 0x7FFFFFFF
#define ALT_MARKER   0x7FFFFFFD    // Start of alternatives group
#define EXPLICIT_MARKER 0x7FFFFFFC // Explicit parameter mode

// Stack operations
void push(Stack* s, int32_t value) {
    if (s->top >= MAX_STACK_SIZE - 1) {
        printf("Stack overflow!\n");
        return;
    }
    s->data[++(s->top)] = value;
}

int32_t pop(Stack* s) {
    if (s->top < 0) {
        printf("Stack underflow!\n");
        return 0;
    }
    return s->data[(s->top)--];
}

int32_t peek(Stack* s) {
    if (s->top < 0) {
        printf("Stack underflow!\n");
        return 0;
    }
    return s->data[s->top];
}

// Dictionary operations
int find_word(const char* name) {
    for (int i = 0; i < dict_count; i++) {
        if (strcmp(dictionary[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

void add_word(const char* name, void (*function)(Stack* stack), int is_primitive) {
    if (dict_count >= MAX_WORDS) {
        printf("Dictionary full!\n");
        return;
    }

    strcpy(dictionary[dict_count].name, name);
    dictionary[dict_count].function = function;
    dictionary[dict_count].body = NULL;
    dictionary[dict_count].is_primitive = is_primitive;
    dict_count++;
}

// Add a user-defined word
void add_user_word(const char* name, const char* body) {
    if (dict_count >= MAX_WORDS) {
        printf("Dictionary full!\n");
        return;
    }

    // Check if word already exists and warn
    int existing = find_word(name);
    if (existing >= 0) {
        printf("Warning: redefining '%s'\n", name);
        // Free old body if it was user-defined
        if (!dictionary[existing].is_primitive && dictionary[existing].body) {
            free(dictionary[existing].body);
        }
        // Reuse the slot
        dictionary[existing].function = NULL;
        dictionary[existing].body = strdup(body);
        dictionary[existing].is_primitive = 0;
        return;
    }

    strcpy(dictionary[dict_count].name, name);
    dictionary[dict_count].function = NULL;
    dictionary[dict_count].body = strdup(body);
    dictionary[dict_count].is_primitive = 0;
    dict_count++;
}

// Primitive operations
void op_plus(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a + b);
}

void op_minus(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a - b);
}

void op_multiply(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a * b);
}

void op_divide(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    if (b == 0) {
        printf("Division by zero!\n");
        return;
    }
    push(stack, a / b);
}

void op_swap(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, b);
    push(stack, a);
}

void op_dup(Stack* stack) {
    int32_t a = peek(stack);
    push(stack, a);
}

void op_drop(Stack* stack) {
    pop(stack);
}

void op_clear(Stack* stack) {
    stack->top = -1;
}

void op_over(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = peek(stack);
    push(stack, b);
    push(stack, a);
}

void op_rot(Stack* stack) {
    int32_t c = pop(stack);
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, b);
    push(stack, c);
    push(stack, a);
}

void op_dot(Stack* stack) {
    int32_t value = pop(stack);
    printf("%d ", value);
}

void op_cr(Stack* stack) {
    printf("\n");
}

void op_space(Stack* stack) {
    printf(" ");
}

void op_dot_s(Stack* stack) {
    printf("<%d> ", stack->top + 1);
    for (int i = 0; i <= stack->top; i++) {
        printf("%d ", stack->data[i]);
    }
}

void op_and(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a & b);
}

void op_or(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a | b);
}

void op_xor(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a ^ b);
}

void op_not(Stack* stack) {
    int32_t a = pop(stack);
    push(stack, ~a);
}

void op_eq(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, (a == b) ? -1 : 0);
}

void op_lt(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, (a < b) ? -1 : 0);
}

void op_gt(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, (a > b) ? -1 : 0);
}

void op_ms(Stack* stack) {
    int32_t ms = pop(stack);
    if (ms > 0) {
        usleep(ms * 1000);
    }
}

// MIDI operations

void on_output_port_found(void* ctx, const libremidi_midi_out_port* port) {
    if (out_port_count >= MAX_PORTS) return;
    libremidi_midi_out_port_clone(port, &out_ports[out_port_count]);
    out_port_count++;
}

// Debug: list available APIs
void on_api_found(void* ctx, libremidi_api api) {
    const char* name = libremidi_api_display_name(api);
    printf("  %s\n", name ? name : "(unknown)");
}

void op_midi_apis(Stack* stack) {
    printf("Available MIDI APIs:\n");
    libremidi_midi1_available_apis(NULL, on_api_found);
}

int midi_init_observer(void) {
    int ret = 0;

    // Free existing observer if any
    if (midi_observer != NULL) {
        for (int i = 0; i < out_port_count; i++) {
            libremidi_midi_out_port_free(out_ports[i]);
        }
        out_port_count = 0;
        libremidi_midi_observer_free(midi_observer);
        midi_observer = NULL;
    }

    libremidi_observer_configuration observer_conf;
    ret = libremidi_midi_observer_configuration_init(&observer_conf);
    if (ret != 0) {
        printf("Failed to init observer config: %d\n", ret);
        return ret;
    }

    observer_conf.track_hardware = true;
    observer_conf.track_virtual = true;
    observer_conf.track_any = true;

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        printf("Failed to init api config: %d\n", ret);
        return ret;
    }

    api_conf.configuration_type = Observer;
    api_conf.api = UNSPECIFIED;  // Let libremidi auto-detect

    ret = libremidi_midi_observer_new(&observer_conf, &api_conf, &midi_observer);
    if (ret != 0) {
        printf("Failed to create MIDI observer: %d\n", ret);
        return ret;
    }

    // Enumerate output ports
    out_port_count = 0;
    ret = libremidi_midi_observer_enumerate_output_ports(midi_observer, NULL, on_output_port_found);
    if (ret != 0) {
        printf("Failed to enumerate ports: %d\n", ret);
    }

    return ret;
}

void op_midi_list(Stack* stack) {
    if (midi_init_observer() != 0) {
        printf("Failed to initialize MIDI\n");
        return;
    }

    // Clear and re-enumerate
    for (int i = 0; i < out_port_count; i++) {
        libremidi_midi_out_port_free(out_ports[i]);
    }
    out_port_count = 0;
    libremidi_midi_observer_enumerate_output_ports(midi_observer, NULL, on_output_port_found);

    printf("Hardware MIDI outputs:\n");
    if (out_port_count == 0) {
        printf("  (none - use midi-virtual to create a virtual port)\n");
    } else {
        for (int i = 0; i < out_port_count; i++) {
            const char* name = NULL;
            size_t len = 0;
            if (libremidi_midi_out_port_name(out_ports[i], &name, &len) == 0) {
                printf("  %d: %s\n", i, name);
            }
        }
    }
}

void op_midi_open(Stack* stack) {
    int32_t port_idx = pop(stack);

    if (midi_init_observer() != 0) {
        printf("Failed to initialize MIDI\n");
        return;
    }

    if (port_idx < 0 || port_idx >= out_port_count) {
        printf("Invalid port index: %d (have %d ports)\n", port_idx, out_port_count);
        return;
    }

    // Close existing output if open
    if (midi_out != NULL) {
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
    }

    int ret = 0;

    libremidi_midi_configuration midi_conf;
    ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        printf("Failed to init MIDI config\n");
        return;
    }

    midi_conf.version = MIDI1;
    midi_conf.out_port = out_ports[port_idx];

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        printf("Failed to init API config\n");
        return;
    }

    api_conf.configuration_type = Output;
    api_conf.api = UNSPECIFIED;  // Let libremidi auto-detect

    ret = libremidi_midi_out_new(&midi_conf, &api_conf, &midi_out);
    if (ret != 0) {
        printf("Failed to open MIDI output: %d\n", ret);
        return;
    }

    const char* name = NULL;
    size_t len = 0;
    libremidi_midi_out_port_name(out_ports[port_idx], &name, &len);
    printf("Opened MIDI output: %s\n", name);
}

void op_midi_open_virtual(Stack* stack) {
    // Close existing output if open
    if (midi_out != NULL) {
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
    }

    int ret = 0;

    libremidi_midi_configuration midi_conf;
    ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        printf("Failed to init MIDI config\n");
        return;
    }

    midi_conf.version = MIDI1;
    midi_conf.virtual_port = true;
    midi_conf.port_name = "MidiForth";

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        printf("Failed to init API config\n");
        return;
    }

    api_conf.configuration_type = Output;
    api_conf.api = UNSPECIFIED;  // Let libremidi auto-detect

    ret = libremidi_midi_out_new(&midi_conf, &api_conf, &midi_out);
    if (ret != 0) {
        printf("Failed to create virtual MIDI output: %d\n", ret);
        return;
    }

    printf("Created virtual MIDI output: MidiForth\n");
}

void op_midi_close(Stack* stack) {
    if (midi_out != NULL) {
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
        printf("MIDI output closed\n");
    }
}

void op_cc(Stack* stack) {
    if (stack->top < 2) {
        printf("cc needs 3 values: channel cc# value\n");
        return;
    }

    int32_t value = pop(stack);
    int32_t cc_num = pop(stack);
    int32_t channel = pop(stack);

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    if (channel < 1 || channel > 16) {
        printf("Channel must be 1-16\n");
        return;
    }

    unsigned char msg[3];
    msg[0] = 0xB0 | ((channel - 1) & 0x0F);  // Control Change
    msg[1] = cc_num & 0x7F;
    msg[2] = value & 0x7F;

    int ret = libremidi_midi_out_send_message(midi_out, msg, 3);
    if (ret != 0) {
        printf("Failed to send CC\n");
    }
    capture_add_event(2, channel - 1, cc_num, value);  // Capture CC
}

void op_all_notes_off(Stack* stack) {
    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    // Send All Notes Off (CC 123) on all channels
    for (int ch = 0; ch < 16; ch++) {
        unsigned char msg[3];
        msg[0] = 0xB0 | ch;
        msg[1] = 123;  // All Notes Off
        msg[2] = 0;
        libremidi_midi_out_send_message(midi_out, msg, 3);
    }
}

// Pitch bend ( ch value -- ) value is 0-16383, center is 8192
void op_pitch_bend(Stack* stack) {
    if (stack->top < 1) {
        printf("pb needs channel and value (0-16383)\n");
        return;
    }

    int32_t value = pop(stack);
    int32_t channel = pop(stack);

    if (channel < 1 || channel > 16) {
        printf("Channel must be 1-16\n");
        return;
    }
    if (value < 0) value = 0;
    if (value > 16383) value = 16383;

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    // Pitch bend is 14-bit: LSB (bits 0-6) then MSB (bits 7-13)
    unsigned char msg[3];
    msg[0] = 0xE0 | ((channel - 1) & 0x0F);
    msg[1] = value & 0x7F;         // LSB
    msg[2] = (value >> 7) & 0x7F;  // MSB
    libremidi_midi_out_send_message(midi_out, msg, 3);
}

// Random ( -- n ) push random number 0-99
void op_random(Stack* stack) {
    push(stack, rand() % 100);
}

// ============================================================================
// Concise Notation System
// ============================================================================

// Context variable operations
void op_ch_store(Stack* stack) {
    int32_t ch = pop(stack);
    if (ch < 1 || ch > 16) {
        printf("Channel must be 1-16\n");
        return;
    }
    default_channel = ch;
}

void op_ch_fetch_default(Stack* stack) {
    push(stack, default_channel);
}

void op_vel_store(Stack* stack) {
    int32_t vel = pop(stack);
    if (vel < 0 || vel > 127) {
        printf("Velocity must be 0-127\n");
        return;
    }
    default_velocity = vel;
}

void op_vel_fetch_default(Stack* stack) {
    push(stack, default_velocity);
}

void op_dur_store(Stack* stack) {
    int32_t dur = pop(stack);
    if (dur < 1) {
        printf("Duration must be positive\n");
        return;
    }
    default_duration = dur;
}

void op_dur_fetch_default(Stack* stack) {
    push(stack, default_duration);
}

// Chord grouping
void op_chord_open(Stack* stack) {
    push(stack, CHORD_MARKER);
}

void op_chord_close(Stack* stack) {
    // Just a no-op marker - the comma will handle playback
    // We leave the pitches and marker on the stack
}

// Alternative grouping with |
// First | after a value: push ALT_MARKER before the value
// Subsequent |: just continue (values accumulate on stack)
void op_pipe(Stack* stack) {
    if (stack->top < 0) {
        printf("Stack empty for |\n");
        return;
    }

    // Check if we already have an ALT_MARKER in the stack
    int has_alt_marker = 0;
    for (int i = stack->top; i >= 0; i--) {
        if (stack->data[i] == ALT_MARKER) {
            has_alt_marker = 1;
            break;
        }
        // Stop at other markers (chord, explicit)
        if (stack->data[i] == CHORD_MARKER || stack->data[i] == EXPLICIT_MARKER) {
            break;
        }
    }

    if (!has_alt_marker) {
        // First | - insert ALT_MARKER before the top value
        int32_t top_val = pop(stack);
        push(stack, ALT_MARKER);
        push(stack, top_val);
    }
    // Otherwise, just continue - next value will be pushed by following word
}

// Explicit parameter brackets
void op_bracket_open(Stack* stack) {
    push(stack, EXPLICIT_MARKER);
}

void op_bracket_close(Stack* stack) {
    // No-op - comma will handle the explicit params
}

// Pick: select one alternative from ALT_MARKER group (for testing)
// ( ALT_MARKER v1 v2 ... vn -- picked )
void op_pick_alt(Stack* stack) {
    // Find ALT_MARKER
    int alt_pos = -1;
    for (int i = stack->top; i >= 0; i--) {
        if (stack->data[i] == ALT_MARKER) {
            alt_pos = i;
            break;
        }
    }
    if (alt_pos < 0) {
        // No alternatives, leave stack unchanged
        return;
    }
    int alt_count = stack->top - alt_pos;
    if (alt_count < 1) {
        pop(stack);  // Remove marker
        return;
    }
    int pick = rand() % alt_count;
    int32_t picked = stack->data[alt_pos + 1 + pick];
    stack->top = alt_pos - 1;
    push(stack, picked);
}

// Probability: % pops probability (0-100), decides whether to keep the top value
// If random >= probability, drop the top value (skip the note)
void op_percent(Stack* stack) {
    if (stack->top < 1) {  // Need at least probability and a value
        printf("Stack needs value and probability for %%\n");
        return;
    }

    int32_t probability = pop(stack);
    if (probability < 0) probability = 0;
    if (probability > 100) probability = 100;

    int32_t roll = rand() % 100;  // 0-99

    if (roll >= probability) {
        // Failed probability check - drop the value
        pop(stack);
        // Push a "skip" marker so comma knows not to play
        push(stack, REST_MARKER);  // Treat as rest (silence)
    }
    // Otherwise, leave the value on stack to be played
}

// Dynamics - set default velocity
void op_ppp(Stack* stack) { (void)stack; default_velocity = 16; }
void op_pp(Stack* stack)  { (void)stack; default_velocity = 32; }
void op_p(Stack* stack)   { (void)stack; default_velocity = 48; }
void op_mp(Stack* stack)  { (void)stack; default_velocity = 64; }
void op_mf(Stack* stack)  { (void)stack; default_velocity = 80; }
void op_f(Stack* stack)   { (void)stack; default_velocity = 96; }
void op_ff(Stack* stack)  { (void)stack; default_velocity = 112; }
void op_fff(Stack* stack) { (void)stack; default_velocity = 127; }

// Octave shifts - push current pitch shifted by octave
void op_octave_up(Stack* stack) {
    int new_pitch = current_pitch + 12;
    if (new_pitch > 127) new_pitch = 127;
    push(stack, new_pitch);
}

void op_octave_down(Stack* stack) {
    int new_pitch = current_pitch - 12;
    if (new_pitch < 0) new_pitch = 0;
    push(stack, new_pitch);
}

// Program change ( ch prog -- )
void op_program_change(Stack* stack) {
    if (stack->top < 1) {
        printf("pc needs channel and program\n");
        return;
    }

    int program = pop(stack);
    int channel = pop(stack);

    if (channel < 1 || channel > 16) {
        printf("Channel must be 1-16\n");
        return;
    }
    if (program < 0 || program > 127) {
        printf("Program must be 0-127\n");
        return;
    }

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    unsigned char msg[2];
    msg[0] = 0xC0 | ((channel - 1) & 0x0F);
    msg[1] = program & 0x7F;
    libremidi_midi_out_send_message(midi_out, msg, 2);
}

// Helper: play a single note with given params
// Applies and resets articulation flags
static void play_single_note(int channel, int pitch, int velocity, int duration) {
    // Always track current pitch for relative intervals
    current_pitch = pitch;

    // Apply articulation
    if (articulation_staccato) {
        duration = duration / 2;  // 50% duration
        articulation_staccato = 0;
    }
    if (articulation_accent) {
        velocity = velocity + 20;
        if (velocity > 127) velocity = 127;
        articulation_accent = 0;
    }

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    if (channel < 1 || channel > 16) {
        printf("Channel must be 1-16\n");
        return;
    }
    if (pitch < 0 || pitch > 127) {
        printf("Pitch must be 0-127\n");
        return;
    }
    if (velocity < 0 || velocity > 127) {
        printf("Velocity must be 0-127\n");
        return;
    }

    unsigned char msg[3];

    // Note on
    msg[0] = 0x90 | ((channel - 1) & 0x0F);
    msg[1] = pitch & 0x7F;
    msg[2] = velocity & 0x7F;
    libremidi_midi_out_send_message(midi_out, msg, 3);
    capture_add_event(0, channel - 1, pitch, velocity);  // Capture note-on

    // Wait
    if (duration > 0) {
        usleep(duration * 1000);
    }

    // Note off
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[2] = 0;
    libremidi_midi_out_send_message(midi_out, msg, 3);
    capture_add_event(1, channel - 1, pitch, 0);  // Capture note-off
}

// Helper: play chord with given params
// Applies and resets articulation flags
static void play_chord_notes(int* pitches, int count, int channel, int velocity, int duration) {
    // Always track current pitch (use highest note in chord)
    if (count > 0) {
        current_pitch = pitches[count - 1];
    }

    // Apply articulation
    if (articulation_staccato) {
        duration = duration / 2;  // 50% duration
        articulation_staccato = 0;
    }
    if (articulation_accent) {
        velocity = velocity + 20;
        if (velocity > 127) velocity = 127;
        articulation_accent = 0;
    }

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    if (count < 1 || count > 16) {
        printf("Chord must have 1-16 notes\n");
        return;
    }

    unsigned char msg[3];

    // All notes on
    for (int i = 0; i < count; i++) {
        msg[0] = 0x90 | ((channel - 1) & 0x0F);
        msg[1] = pitches[i] & 0x7F;
        msg[2] = velocity & 0x7F;
        libremidi_midi_out_send_message(midi_out, msg, 3);
        capture_add_event(0, channel - 1, pitches[i], velocity);  // Capture note-on
    }

    // Wait
    if (duration > 0) {
        usleep(duration * 1000);
    }

    // All notes off
    for (int i = 0; i < count; i++) {
        msg[0] = 0x80 | ((channel - 1) & 0x0F);
        msg[1] = pitches[i] & 0x7F;
        msg[2] = 0;
        libremidi_midi_out_send_message(midi_out, msg, 3);
        capture_add_event(1, channel - 1, pitches[i], 0);  // Capture note-off
    }
}

// The comma - universal play trigger
void op_comma(Stack* stack) {
    if (stack->top < 0) {
        printf("Stack empty\n");
        return;
    }

    // First, check for alternatives (ALT_MARKER)
    // Stack could be: ALT_MARKER v1 v2 v3 ... (pick one randomly)
    int alt_pos = -1;
    for (int i = stack->top; i >= 0; i--) {
        if (stack->data[i] == ALT_MARKER) {
            alt_pos = i;
            break;
        }
        // Stop at other markers
        if (stack->data[i] == CHORD_MARKER || stack->data[i] == EXPLICIT_MARKER) {
            break;
        }
    }

    if (alt_pos >= 0) {
        // Alternatives mode: pick one randomly
        int alt_count = stack->top - alt_pos;  // number of alternatives
        if (alt_count < 1) {
            printf("Empty alternatives\n");
            pop(stack);  // Remove marker
            return;
        }

        // Pick random index
        int pick = rand() % alt_count;

        // Get the picked value (index from alt_pos + 1)
        int32_t picked = stack->data[alt_pos + 1 + pick];

        // Clear all alternatives and marker from stack
        stack->top = alt_pos - 1;

        // Push the picked value back
        push(stack, picked);

        // Continue to process the single picked value (fall through)
    }

    // Count items and find markers
    int count = 0;
    int chord_pos = -1;
    int explicit_pos = -1;
    for (int i = stack->top; i >= 0; i--) {
        if (stack->data[i] == CHORD_MARKER) {
            chord_pos = i;
            break;
        }
        if (stack->data[i] == EXPLICIT_MARKER) {
            explicit_pos = i;
            break;
        }
        count++;
    }

    if (chord_pos >= 0) {
        // Chord mode: marker found
        // Stack could be: MARKER p1 p2 p3 ,           (defaults)
        //             or: MARKER p1 p2 p3 ch vel dur , (explicit)
        int pitches[16];
        int pitch_count;
        int channel, velocity, duration;

        if (count >= 3) {
            // Check if last 3 items look like ch/vel/dur params
            // Heuristic: if count > 3 and we have at least 1 pitch plus 3 params
            int32_t v1 = stack->data[stack->top - 2];  // potential channel
            int32_t v2 = stack->data[stack->top - 1];  // potential velocity
            int32_t v3 = stack->data[stack->top];      // potential duration

            // Explicit params if: ch in 1-16, vel in 0-127, dur > 0, and at least 1 pitch
            if (count > 3 && v1 >= 1 && v1 <= 16 && v2 >= 0 && v2 <= 127 && v3 > 0) {
                duration = pop(stack);
                velocity = pop(stack);
                channel = pop(stack);
                pitch_count = count - 3;
            } else {
                // Defaults
                channel = default_channel;
                velocity = default_velocity;
                duration = default_duration;
                pitch_count = count;
            }
        } else {
            // Not enough for explicit params, use defaults
            channel = default_channel;
            velocity = default_velocity;
            duration = default_duration;
            pitch_count = count;
        }

        if (pitch_count < 1) {
            printf("Empty chord\n");
            pop(stack);  // Remove marker
            return;
        }

        if (pitch_count > 16) {
            printf("Too many notes in chord (max 16)\n");
            // Clean up: remove all items and marker
            while (stack->top >= chord_pos) pop(stack);
            return;
        }

        // Pop pitches (in reverse order)
        for (int i = pitch_count - 1; i >= 0; i--) {
            pitches[i] = pop(stack);
        }

        // Pop the marker
        pop(stack);

        play_chord_notes(pitches, pitch_count, channel, velocity, duration);
    } else if (explicit_pos >= 0) {
        // Explicit mode with [ ] brackets
        // Stack: EXPLICIT_MARKER ch pitch vel dur
        if (count != 4) {
            printf("Explicit mode [ch pitch vel dur] requires exactly 4 values, got %d\n", count);
            // Clean up
            while (stack->top >= explicit_pos) pop(stack);
            return;
        }
        int duration = pop(stack);
        int velocity = pop(stack);
        int pitch = pop(stack);
        int channel = pop(stack);
        pop(stack);  // Remove EXPLICIT_MARKER
        play_single_note(channel, pitch, velocity, duration);
    } else {
        // Single note mode
        if (count == 1) {
            // Defaults: just pitch on stack
            int pitch = pop(stack);
            if (pitch == REST_MARKER) {
                // Rest: just sleep
                if (default_duration > 0) {
                    usleep(default_duration * 1000);
                }
            } else {
                play_single_note(default_channel, pitch, default_velocity, default_duration);
            }
        } else if (count == 2) {
            // Could be: r dur, (rest with explicit duration)
            int dur_or_pitch = pop(stack);
            int first = pop(stack);
            if (first == REST_MARKER) {
                // Rest with explicit duration
                if (dur_or_pitch > 0) {
                    usleep(dur_or_pitch * 1000);
                }
            } else {
                printf("Invalid note: expected 1 (pitch) or 4 (ch pitch vel dur) items, got 2\n");
            }
        } else if (count == 4) {
            // Explicit: ch pitch vel dur
            int duration = pop(stack);
            int velocity = pop(stack);
            int pitch = pop(stack);
            int channel = pop(stack);
            play_single_note(channel, pitch, velocity, duration);
        } else {
            printf("Invalid note: expected 1 (pitch) or 4 (ch pitch vel dur) items, got %d\n", count);
            // Clean up: remove all items
            for (int i = 0; i < count; i++) pop(stack);
        }
    }
}

// Rest - push rest marker
void op_rest(Stack* stack) {
    push(stack, REST_MARKER);
}

// ============================================================================
// Phase 1: Packed Note Operations
// ============================================================================

// note ( pitch vel ch dur -- packed-note )
void op_note(Stack* stack) {
    int32_t dur = pop(stack);
    int32_t ch = pop(stack);
    int32_t vel = pop(stack);
    int32_t pitch = pop(stack);
    push(stack, pack_note(pitch, vel, ch - 1, dur));  // ch 1-16 -> 0-15
}

// pitch@ ( packed-note -- pitch )
void op_pitch_fetch(Stack* stack) {
    int32_t n = pop(stack);
    push(stack, note_pitch(n));
}

// vel@ ( packed-note -- velocity )
void op_vel_fetch(Stack* stack) {
    int32_t n = pop(stack);
    push(stack, note_vel(n));
}

// ch@ ( packed-note -- channel )  returns 1-16
void op_ch_fetch(Stack* stack) {
    int32_t n = pop(stack);
    push(stack, note_ch(n) + 1);
}

// dur@ ( packed-note -- duration )
void op_dur_fetch(Stack* stack) {
    int32_t n = pop(stack);
    push(stack, note_dur(n));
}

// note. ( packed-note -- ) print note info
void op_note_print(Stack* stack) {
    int32_t n = pop(stack);
    printf("note: pitch=%d vel=%d ch=%d dur=%d",
           note_pitch(n), note_vel(n), note_ch(n) + 1, note_dur(n));
}

// transpose ( packed-note semitones -- packed-note )
void op_transpose(Stack* stack) {
    int32_t semi = pop(stack);
    int32_t n = pop(stack);
    int new_pitch = note_pitch(n) + semi;
    if (new_pitch < 0) new_pitch = 0;
    if (new_pitch > 127) new_pitch = 127;
    push(stack, pack_note(new_pitch, note_vel(n), note_ch(n), note_dur(n)));
}

// note! ( packed-note -- ) play the note immediately (blocking)
void op_note_play(Stack* stack) {
    int32_t n = pop(stack);

    int pitch = note_pitch(n);
    int vel = note_vel(n);
    int ch = note_ch(n);
    int dur = note_dur(n);

    // Update current pitch for relative intervals (unify with concise notation)
    current_pitch = pitch;

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    // Convert ticks to ms: (ticks / PPQ) * (60000 / BPM)
    int ms = (dur * 60000) / (TICKS_PER_QUARTER * global_bpm);

    unsigned char msg[3];

    // Note on
    msg[0] = 0x90 | (ch & 0x0F);
    msg[1] = pitch & 0x7F;
    msg[2] = vel & 0x7F;
    libremidi_midi_out_send_message(midi_out, msg, 3);
    capture_add_event(0, ch, pitch, vel);  // Capture note-on

    // Wait
    if (ms > 0) usleep(ms * 1000);

    // Note off
    msg[0] = 0x80 | (ch & 0x0F);
    msg[2] = 0;
    libremidi_midi_out_send_message(midi_out, msg, 3);
    capture_add_event(1, ch, pitch, 0);  // Capture note-off
}

// bpm! ( n -- ) set tempo
void op_bpm_store(Stack* stack) {
    int32_t bpm = pop(stack);
    if (bpm < 20) bpm = 20;
    if (bpm > 300) bpm = 300;
    global_bpm = bpm;
}

// bpm@ ( -- n ) get tempo
void op_bpm_fetch(Stack* stack) {
    push(stack, global_bpm);
}

// ============================================================================
// Phase 2: Sequence Operations
// ============================================================================

// Helper to sort events by time (simple insertion sort)
static void seq_sort(Sequence* seq) {
    for (int i = 1; i < seq->length; i++) {
        MidiEvent tmp = seq->events[i];
        int j = i - 1;
        while (j >= 0 && seq->events[j].time > tmp.time) {
            seq->events[j + 1] = seq->events[j];
            j--;
        }
        seq->events[j + 1] = tmp;
    }
}

// seq-new ( -- seq-id )
void op_seq_new(Stack* stack) {
    if (sequence_count >= MAX_SEQUENCES) {
        printf("Max sequences reached\n");
        push(stack, -1);
        return;
    }
    int id = sequence_count++;
    sequences[id].length = 0;
    sequences[id].bpm = global_bpm;
    current_seq = id;
    push(stack, id);
}

// seq ( id -- ) select sequence as current
void op_seq_select(Stack* stack) {
    int32_t id = pop(stack);
    if (id < 0 || id >= sequence_count) {
        printf("Invalid sequence id: %d\n", id);
        return;
    }
    current_seq = id;
}

// seq@ ( -- id ) get current sequence id
void op_seq_current(Stack* stack) {
    push(stack, current_seq);
}

// Helper to add event to current sequence
static void add_event(int time, int type, int ch, int d1, int d2) {
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }
    Sequence* seq = &sequences[current_seq];
    if (seq->length >= MAX_SEQ_EVENTS) {
        printf("Sequence full\n");
        return;
    }
    MidiEvent* e = &seq->events[seq->length++];
    e->time = time;
    e->type = type;
    e->channel = ch;
    e->data1 = d1;
    e->data2 = d2;
}

// seq-note ( time pitch vel dur -- ) add note to current sequence
void op_seq_note(Stack* stack) {
    int32_t dur = pop(stack);
    int32_t vel = pop(stack);
    int32_t pitch = pop(stack);
    int32_t time = pop(stack);

    if (current_seq < 0) {
        printf("No sequence selected (use seq-new first)\n");
        return;
    }

    int ch = 0;  // default channel
    add_event(time, EVT_NOTE_ON, ch, pitch, vel);
    add_event(time + dur, EVT_NOTE_OFF, ch, pitch, 0);
}

// seq-note-ch ( time ch pitch vel dur -- ) add note with channel
void op_seq_note_ch(Stack* stack) {
    int32_t dur = pop(stack);
    int32_t vel = pop(stack);
    int32_t pitch = pop(stack);
    int32_t ch = pop(stack) - 1;  // 1-16 -> 0-15
    int32_t time = pop(stack);

    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }

    add_event(time, EVT_NOTE_ON, ch, pitch, vel);
    add_event(time + dur, EVT_NOTE_OFF, ch, pitch, 0);
}

// seq-add ( packed-note time -- ) add packed note at time
void op_seq_add(Stack* stack) {
    int32_t time = pop(stack);
    int32_t n = pop(stack);

    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }

    int ch = note_ch(n);
    int pitch = note_pitch(n);
    int vel = note_vel(n);
    int dur = note_dur(n);

    add_event(time, EVT_NOTE_ON, ch, pitch, vel);
    add_event(time + dur, EVT_NOTE_OFF, ch, pitch, 0);
}

// seq-length ( -- n ) get length of current sequence
void op_seq_length(Stack* stack) {
    if (current_seq < 0) {
        push(stack, 0);
        return;
    }
    push(stack, sequences[current_seq].length);
}

// seq-clear ( -- ) clear current sequence
void op_seq_clear(Stack* stack) {
    if (current_seq < 0) return;
    sequences[current_seq].length = 0;
}

// seq-play ( -- ) play current sequence
void op_seq_play(Stack* stack) {
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }
    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    Sequence* seq = &sequences[current_seq];
    if (seq->length == 0) {
        printf("Sequence empty\n");
        return;
    }

    // Sort events by time
    seq_sort(seq);

    int bpm = seq->bpm > 0 ? seq->bpm : global_bpm;
    int last_time = 0;

    for (int i = 0; i < seq->length; i++) {
        MidiEvent* e = &seq->events[i];

        // Wait until this event's time
        if (e->time > last_time) {
            int ticks = e->time - last_time;
            int ms = (ticks * 60000) / (TICKS_PER_QUARTER * bpm);
            if (ms > 0) usleep(ms * 1000);
            last_time = e->time;
        }

        // Send the event
        unsigned char msg[3];
        switch (e->type) {
            case EVT_NOTE_ON:
                msg[0] = 0x90 | (e->channel & 0x0F);
                msg[1] = e->data1 & 0x7F;
                msg[2] = e->data2 & 0x7F;
                capture_add_event(0, e->channel, e->data1, e->data2);
                break;
            case EVT_NOTE_OFF:
                msg[0] = 0x80 | (e->channel & 0x0F);
                msg[1] = e->data1 & 0x7F;
                msg[2] = 0;
                capture_add_event(1, e->channel, e->data1, 0);
                break;
            case EVT_CC:
                msg[0] = 0xB0 | (e->channel & 0x0F);
                msg[1] = e->data1 & 0x7F;
                msg[2] = e->data2 & 0x7F;
                capture_add_event(2, e->channel, e->data1, e->data2);
                break;
            default:
                continue;
        }
        libremidi_midi_out_send_message(midi_out, msg, 3);
    }
}

// seq-transpose ( semitones -- ) transpose all notes in current sequence
void op_seq_transpose(Stack* stack) {
    int32_t semi = pop(stack);
    if (current_seq < 0) return;

    Sequence* seq = &sequences[current_seq];
    for (int i = 0; i < seq->length; i++) {
        if (seq->events[i].type == EVT_NOTE_ON || seq->events[i].type == EVT_NOTE_OFF) {
            int p = seq->events[i].data1 + semi;
            if (p < 0) p = 0;
            if (p > 127) p = 127;
            seq->events[i].data1 = p;
        }
    }
}

// seq-show ( -- ) show current sequence events
void op_seq_show(Stack* stack) {
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }
    Sequence* seq = &sequences[current_seq];
    printf("Sequence %d: %d events, bpm=%d\n", current_seq, seq->length, seq->bpm);
    for (int i = 0; i < seq->length; i++) {
        MidiEvent* e = &seq->events[i];
        const char* type_str = e->type == EVT_NOTE_ON ? "ON " :
                               e->type == EVT_NOTE_OFF ? "OFF" : "CC ";
        printf("  t=%4d %s ch=%d d1=%3d d2=%3d\n",
               e->time, type_str, e->channel + 1, e->data1, e->data2);
    }
}

// seq-reverse ( -- ) reverse timing of current sequence
void op_seq_reverse(Stack* stack) {
    if (current_seq < 0) return;
    Sequence* seq = &sequences[current_seq];
    if (seq->length == 0) return;

    // Find max time
    int max_time = 0;
    for (int i = 0; i < seq->length; i++) {
        if (seq->events[i].time > max_time)
            max_time = seq->events[i].time;
    }

    // Reverse times and swap note-on/off
    for (int i = 0; i < seq->length; i++) {
        seq->events[i].time = max_time - seq->events[i].time;
        // Swap on/off so notes still make sense
        if (seq->events[i].type == EVT_NOTE_ON)
            seq->events[i].type = EVT_NOTE_OFF;
        else if (seq->events[i].type == EVT_NOTE_OFF)
            seq->events[i].type = EVT_NOTE_ON;
    }
    seq_sort(seq);
}

// seq-stretch ( factor -- ) multiply all times by factor/100
void op_seq_stretch(Stack* stack) {
    int32_t factor = pop(stack);  // percentage: 100 = normal, 200 = double, 50 = half
    if (current_seq < 0) return;

    Sequence* seq = &sequences[current_seq];
    for (int i = 0; i < seq->length; i++) {
        seq->events[i].time = (seq->events[i].time * factor) / 100;
    }
}

// ============================================================================
// Phase 3: Pattern DSL Helpers
// ============================================================================

// quarter ( -- ticks ) push quarter note duration
void op_quarter(Stack* stack) { push(stack, TICKS_PER_QUARTER); }
void op_half(Stack* stack) { push(stack, TICKS_PER_QUARTER * 2); }
void op_whole(Stack* stack) { push(stack, TICKS_PER_QUARTER * 4); }
void op_eighth(Stack* stack) { push(stack, TICKS_PER_QUARTER / 2); }
void op_sixteenth(Stack* stack) { push(stack, TICKS_PER_QUARTER / 4); }

// Chord builders - push pitches relative to root
// Usage: 60 c-major -> pushes 60 64 67

void op_chord_major(Stack* stack) {
    int32_t root = pop(stack);
    push(stack, root);
    push(stack, root + 4);
    push(stack, root + 7);
}

void op_chord_minor(Stack* stack) {
    int32_t root = pop(stack);
    push(stack, root);
    push(stack, root + 3);
    push(stack, root + 7);
}

void op_chord_dim(Stack* stack) {
    int32_t root = pop(stack);
    push(stack, root);
    push(stack, root + 3);
    push(stack, root + 6);
}

void op_chord_aug(Stack* stack) {
    int32_t root = pop(stack);
    push(stack, root);
    push(stack, root + 4);
    push(stack, root + 8);
}

void op_chord_7(Stack* stack) {
    int32_t root = pop(stack);
    push(stack, root);
    push(stack, root + 4);
    push(stack, root + 7);
    push(stack, root + 10);
}

void op_chord_maj7(Stack* stack) {
    int32_t root = pop(stack);
    push(stack, root);
    push(stack, root + 4);
    push(stack, root + 7);
    push(stack, root + 11);
}

void op_chord_min7(Stack* stack) {
    int32_t root = pop(stack);
    push(stack, root);
    push(stack, root + 3);
    push(stack, root + 7);
    push(stack, root + 10);
}

// play-chord ( p1 p2 p3 vel dur n -- ) play n notes as chord
void op_play_chord(Stack* stack) {
    int32_t n = pop(stack);
    int32_t dur = pop(stack);
    int32_t vel = pop(stack);

    if (n < 1 || n > 8) {
        printf("Chord size must be 1-8\n");
        return;
    }

    int pitches[8];
    for (int i = n - 1; i >= 0; i--) {
        pitches[i] = pop(stack);
    }

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    int ms = (dur * 60000) / (TICKS_PER_QUARTER * global_bpm);

    // All notes on
    unsigned char msg[3];
    for (int i = 0; i < n; i++) {
        msg[0] = 0x90;
        msg[1] = pitches[i] & 0x7F;
        msg[2] = vel & 0x7F;
        libremidi_midi_out_send_message(midi_out, msg, 3);
        capture_add_event(0, 0, pitches[i], vel);  // Capture note-on (ch 1)
    }

    if (ms > 0) usleep(ms * 1000);

    // All notes off
    for (int i = 0; i < n; i++) {
        msg[0] = 0x80;
        msg[1] = pitches[i] & 0x7F;
        msg[2] = 0;
        libremidi_midi_out_send_message(midi_out, msg, 3);
        capture_add_event(1, 0, pitches[i], 0);  // Capture note-off (ch 1)
    }
}

// chord>seq ( p1 p2 ... pn vel dur time n -- ) add chord to sequence
void op_chord_to_seq(Stack* stack) {
    int32_t n = pop(stack);
    int32_t time = pop(stack);
    int32_t dur = pop(stack);
    int32_t vel = pop(stack);

    if (n < 1 || n > 8) {
        printf("Chord size must be 1-8\n");
        return;
    }
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }

    int pitches[8];
    for (int i = n - 1; i >= 0; i--) {
        pitches[i] = pop(stack);
    }

    for (int i = 0; i < n; i++) {
        add_event(time, EVT_NOTE_ON, 0, pitches[i], vel);
        add_event(time + dur, EVT_NOTE_OFF, 0, pitches[i], 0);
    }
}

// arp>seq ( p1 p2 ... pn vel notedur spacing time n -- ) add arpeggio to sequence
void op_arp_to_seq(Stack* stack) {
    int32_t n = pop(stack);
    int32_t time = pop(stack);
    int32_t spacing = pop(stack);
    int32_t notedur = pop(stack);
    int32_t vel = pop(stack);

    if (n < 1 || n > 8) {
        printf("Arp size must be 1-8\n");
        return;
    }
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }

    int pitches[8];
    for (int i = n - 1; i >= 0; i--) {
        pitches[i] = pop(stack);
    }

    for (int i = 0; i < n; i++) {
        int t = time + (i * spacing);
        add_event(t, EVT_NOTE_ON, 0, pitches[i], vel);
        add_event(t + notedur, EVT_NOTE_OFF, 0, pitches[i], 0);
    }
}

// Forward declaration for execute_line (defined later)
void execute_line(const char* input);

// Load and execute a Forth file
// Returns 0 on success, -1 on error
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

    char line[MAX_INPUT_LENGTH * 4];  // Allow longer lines in files
    int line_num = 0;

    while (fgets(line, sizeof(line), f) != NULL) {
        line_num++;

        // Remove trailing newline
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
            len--;
        }
        if (len > 0 && line[len - 1] == '\r') {
            line[len - 1] = '\0';
        }

        // Skip empty lines and comments (lines starting with \)
        if (line[0] == '\0') continue;

        // Skip whitespace at start
        char* start = line;
        while (*start && isspace(*start)) start++;

        // Skip comment lines (Forth uses \ for comments)
        if (*start == '\\') continue;

        // Execute the line
        execute_line(start);
    }

    fclose(f);
    load_depth--;

    return 0;
}

// Recording functions

// Start recording input commands
void recording_start(void) {
    if (recording_active) {
        printf("Already recording (use 'stop' first)\n");
        return;
    }
    // Clear any previous recording
    for (int i = 0; i < recording_count; i++) {
        free(recording_buffer[i]);
        recording_buffer[i] = NULL;
    }
    recording_count = 0;
    recording_active = 1;
    printf("Recording started. Use 'stop' to end, 'save filename' to save.\n");
}

// Stop recording
void recording_stop(void) {
    if (!recording_active) {
        printf("Not recording\n");
        return;
    }
    recording_active = 0;
    printf("Recording stopped. %d lines captured. Use 'save filename' to save.\n", recording_count);
}

// Add a line to the recording buffer
void recording_add_line(const char* line) {
    if (!recording_active) return;
    if (recording_count >= MAX_RECORDING_LINES) {
        printf("Recording buffer full!\n");
        recording_active = 0;
        return;
    }
    recording_buffer[recording_count] = strdup(line);
    recording_count++;
}

// Save recording to file
int recording_save(const char* filename) {
    if (recording_count == 0) {
        printf("Nothing to save (recording is empty)\n");
        return -1;
    }

    FILE* f = fopen(filename, "w");
    if (f == NULL) {
        printf("Error: cannot create file '%s'\n", filename);
        return -1;
    }

    // Write header comment
    fprintf(f, "\\ MIDI Forth recording\n");
    fprintf(f, "\\ Generated by midi_forth\n\n");

    // Write all recorded lines
    for (int i = 0; i < recording_count; i++) {
        fprintf(f, "%s\n", recording_buffer[i]);
    }

    fclose(f);
    printf("Saved %d lines to '%s'\n", recording_count, filename);
    return 0;
}

// Clear recording buffer (called on cleanup)
void recording_clear(void) {
    for (int i = 0; i < recording_count; i++) {
        free(recording_buffer[i]);
        recording_buffer[i] = NULL;
    }
    recording_count = 0;
    recording_active = 0;
}

// MIDI capture functions

// Get current time in milliseconds since capture start
static uint32_t capture_get_time_ms(void) {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    uint32_t elapsed = (now.tv_sec - capture_start_time.tv_sec) * 1000 +
                       (now.tv_nsec - capture_start_time.tv_nsec) / 1000000;
    return elapsed;
}

// Start capturing MIDI events
void capture_start(void) {
    if (capture_active) {
        printf("Already capturing (use 'stop' first)\n");
        return;
    }
    capture_count = 0;
    capture_active = 1;
    clock_gettime(CLOCK_MONOTONIC, &capture_start_time);
    printf("MIDI capture started. Play notes, then 'stop' and 'save-midi filename'.\n");
}

// Stop capturing
void capture_stop(void) {
    if (!capture_active) {
        return;  // Silent if not capturing (stop handles both recording and capture)
    }
    capture_active = 0;
    printf("MIDI capture stopped. %d events captured. Use 'save-midi filename' to save.\n", capture_count);
}

// Add an event to the capture buffer (called from MIDI send functions)
void capture_add_event(int type, int channel, int data1, int data2) {
    if (!capture_active) return;
    if (capture_count >= MAX_CAPTURE_EVENTS) {
        printf("Capture buffer full!\n");
        capture_active = 0;
        return;
    }
    CapturedEvent* e = &capture_buffer[capture_count++];
    e->time_ms = capture_get_time_ms();
    e->type = type;
    e->channel = channel;
    e->data1 = data1;
    e->data2 = data2;
}

// Save captured MIDI to file as Forth sequence commands
int capture_save_midi(const char* filename) {
    if (capture_count == 0) {
        printf("Nothing to save (capture is empty)\n");
        return -1;
    }

    FILE* f = fopen(filename, "w");
    if (f == NULL) {
        printf("Error: cannot create file '%s'\n", filename);
        return -1;
    }

    // Write header
    fprintf(f, "\\ MIDI capture\n");
    fprintf(f, "\\ %d events captured\n\n", capture_count);

    // Convert ms times to ticks (assuming current BPM)
    // ticks = ms * (BPM * PPQ) / 60000
    fprintf(f, "\\ Tempo: %d BPM\n", global_bpm);
    fprintf(f, "%d bpm!\n\n", global_bpm);

    // Create a new sequence
    fprintf(f, "seq-new drop\n\n");

    // Write events as seq-note-ch commands
    // Group note-on/off pairs into single notes
    // For simplicity, output raw events and let user clean up if needed

    // Track note-on events to pair with note-offs
    int note_on_time[16][128];  // [channel][pitch] = time_ms, -1 if not active
    for (int ch = 0; ch < 16; ch++) {
        for (int p = 0; p < 128; p++) {
            note_on_time[ch][p] = -1;
        }
    }

    int notes_written = 0;

    for (int i = 0; i < capture_count; i++) {
        CapturedEvent* e = &capture_buffer[i];

        if (e->type == 0) {  // Note-on
            // Record the start time
            note_on_time[e->channel][e->data1] = e->time_ms;
        } else if (e->type == 1) {  // Note-off
            int start_ms = note_on_time[e->channel][e->data1];
            if (start_ms >= 0) {
                // Calculate duration and convert to ticks
                int dur_ms = e->time_ms - start_ms;
                int start_ticks = (start_ms * global_bpm * TICKS_PER_QUARTER) / 60000;
                int dur_ticks = (dur_ms * global_bpm * TICKS_PER_QUARTER) / 60000;
                if (dur_ticks < 1) dur_ticks = 1;

                // Find the velocity from the note-on event
                int vel = 100;  // default
                for (int j = 0; j < i; j++) {
                    if (capture_buffer[j].type == 0 &&
                        capture_buffer[j].channel == e->channel &&
                        capture_buffer[j].data1 == e->data1 &&
                        (int)capture_buffer[j].time_ms == start_ms) {
                        vel = capture_buffer[j].data2;
                        break;
                    }
                }

                // seq-note-ch ( time ch pitch vel dur -- )
                fprintf(f, "%d %d %d %d %d seq-note-ch\n",
                        start_ticks, e->channel + 1, e->data1, vel, dur_ticks);
                notes_written++;

                note_on_time[e->channel][e->data1] = -1;
            }
        } else if (e->type == 2) {  // CC
            // Output CC as comment (could add seq-cc if implemented)
            int time_ticks = (e->time_ms * global_bpm * TICKS_PER_QUARTER) / 60000;
            fprintf(f, "\\ t=%d CC ch=%d cc=%d val=%d\n",
                    time_ticks, e->channel + 1, e->data1, e->data2);
        }
    }

    fprintf(f, "\n\\ %d notes written\n", notes_written);
    fprintf(f, "\\ To play: midi-virtual seq-play midi-close\n");

    fclose(f);
    printf("Saved %d notes to '%s'\n", notes_written, filename);
    return 0;
}

// Clear capture buffer
void capture_clear(void) {
    capture_count = 0;
    capture_active = 0;
}

// Block repeat ( block-id n -- ) execute block n times
void op_block_repeat(Stack* stack) {
    if (stack->top < 1) {
        printf("* needs block and count\n");
        return;
    }
    int32_t n = pop(stack);
    int32_t block_id = pop(stack);

    // Check if it's a valid block reference
    if ((block_id & BLOCK_MARKER) != BLOCK_MARKER) {
        // Not a block - treat as multiplication
        push(stack, block_id * n);
        return;
    }

    int idx = block_id & 0x0FFFFFFF;
    if (idx < 0 || idx >= block_count || block_storage[idx] == NULL) {
        printf("Invalid block reference\n");
        return;
    }

    // Execute block n times
    for (int i = 0; i < n; i++) {
        execute_line(block_storage[idx]);
    }
}

// times - repeat the last user-defined word N times
void op_times(Stack* stack) {
    int32_t n = pop(stack);
    if (n <= 0) return;

    if (last_executed_word[0] == '\0') {
        printf("No word to repeat\n");
        return;
    }

    int index = find_word(last_executed_word);
    if (index < 0) {
        printf("Word '%s' not found\n", last_executed_word);
        return;
    }

    // Already executed once, so repeat n-1 more times
    for (int i = 1; i < n; i++) {
        if (dictionary[index].is_primitive) {
            dictionary[index].function(stack);
        } else if (dictionary[index].body) {
            execute_line(dictionary[index].body);
        }
    }
}

// Initialize the dictionary with primitive words
void init_dictionary(void) {
    // Arithmetic
    add_word("+", op_plus, 1);
    add_word("-", op_minus, 1);
    add_word("*", op_block_repeat, 1);  // Also handles multiplication
    add_word("/", op_divide, 1);

    // Stack manipulation
    add_word("swap", op_swap, 1);
    add_word("dup", op_dup, 1);
    add_word("drop", op_drop, 1);
    add_word("clear", op_clear, 1);
    add_word("over", op_over, 1);
    add_word("rot", op_rot, 1);
    add_word(".s", op_dot_s, 1);

    // Output
    add_word(".", op_dot, 1);
    add_word("cr", op_cr, 1);
    add_word("space", op_space, 1);

    // Bitwise
    add_word("and", op_and, 1);
    add_word("or", op_or, 1);
    add_word("xor", op_xor, 1);
    add_word("not", op_not, 1);

    // Comparison (use -1 for true, 0 for false - standard Forth)
    add_word("=", op_eq, 1);
    add_word("<", op_lt, 1);
    add_word(">", op_gt, 1);

    // Timing
    add_word("ms", op_ms, 1);

    // MIDI words
    add_word("midi-apis", op_midi_apis, 1);
    add_word("midi-list", op_midi_list, 1);
    add_word("midi-open", op_midi_open, 1);
    add_word("midi-virtual", op_midi_open_virtual, 1);
    add_word("midi-close", op_midi_close, 1);
    add_word("cc", op_cc, 1);
    add_word("panic", op_all_notes_off, 1);

    // Concise notation
    add_word(",", op_comma, 1);
    add_word("(", op_chord_open, 1);
    add_word(")", op_chord_close, 1);
    add_word("ch!", op_ch_store, 1);
    add_word("vel!", op_vel_store, 1);
    add_word("dur!", op_dur_store, 1);
    add_word("r", op_rest, 1);
    add_word("times", op_times, 1);

    // Priority 2: Generative / Expression
    add_word("|", op_pipe, 1);
    add_word("[", op_bracket_open, 1);
    add_word("]", op_bracket_close, 1);
    add_word("%", op_percent, 1);
    add_word("pick", op_pick_alt, 1);

    // Dynamics
    add_word("ppp", op_ppp, 1);
    add_word("pp", op_pp, 1);
    add_word("p", op_p, 1);
    add_word("mp", op_mp, 1);
    add_word("mf", op_mf, 1);
    add_word("f", op_f, 1);
    add_word("ff", op_ff, 1);
    add_word("fff", op_fff, 1);

    // Priority 3: Convenience
    add_word("^", op_octave_up, 1);
    add_word("v", op_octave_down, 1);
    add_word("pc", op_program_change, 1);

    // Priority 4: Advanced
    add_word("pb", op_pitch_bend, 1);
    add_word("random", op_random, 1);

    // Phase 1: Packed notes
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

    // Phase 2: Sequences
    add_word("seq-new", op_seq_new, 1);
    add_word("seq", op_seq_select, 1);
    add_word("seq@", op_seq_current, 1);
    add_word("seq-note", op_seq_note, 1);
    add_word("seq-note-ch", op_seq_note_ch, 1);
    add_word("seq-add", op_seq_add, 1);
    add_word("seq-length", op_seq_length, 1);
    add_word("seq-clear", op_seq_clear, 1);
    add_word("seq-play", op_seq_play, 1);
    add_word("seq-transpose", op_seq_transpose, 1);
    add_word("seq-show", op_seq_show, 1);
    add_word("seq-reverse", op_seq_reverse, 1);
    add_word("seq-stretch", op_seq_stretch, 1);

    // Phase 3: Pattern DSL
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
}

// Parse pitch name like c4, C#4, Db5, etc.
// Also handles articulation suffixes: c4. (staccato), c4> (accent), c4- (tenuto)
// Returns MIDI note number (0-127) or -1 if not a valid pitch name
// Sets global articulation flags as side effect
int parse_pitch(const char* token) {
    int note = -1, accidental = 0, octave = -1;
    int i = 0;

    // Note letter (case insensitive)
    switch (tolower(token[i++])) {
        case 'c': note = 0; break;
        case 'd': note = 2; break;
        case 'e': note = 4; break;
        case 'f': note = 5; break;
        case 'g': note = 7; break;
        case 'a': note = 9; break;
        case 'b': note = 11; break;
        default: return -1;
    }

    // Optional accidental
    if (token[i] == '#') {
        accidental = 1;
        i++;
    } else if (token[i] == 'b') {
        accidental = -1;
        i++;
    }

    // Octave digit (required)
    if (token[i] >= '0' && token[i] <= '9') {
        octave = token[i] - '0';
        i++;
    } else {
        return -1;
    }

    // Optional articulation suffix
    if (token[i] == '.') {
        articulation_staccato = 1;
        i++;
    } else if (token[i] == '>') {
        articulation_accent = 1;
        i++;
    } else if (token[i] == '-') {
        // Tenuto: full duration (clear staccato if set, no other effect)
        articulation_staccato = 0;
        i++;
    }

    // Must be end of token
    if (token[i] != '\0') return -1;

    int midi = (octave + 1) * 12 + note + accidental;
    if (midi < 0 || midi > 127) return -1;

    return midi;
}

// Parse and execute a single word
void execute_word(const char* word) {
    int index = find_word(word);

    if (index == -1) {
        // Try to parse as relative interval (+N or -N) FIRST
        // This must come before number parsing since +2 is a valid number
        if ((word[0] == '+' || word[0] == '-') && strlen(word) > 1) {
            char* end;
            long interval = strtol(word, &end, 10);
            if (*end == '\0') {
                // Valid interval - apply to current pitch
                int new_pitch = current_pitch + (int)interval;
                if (new_pitch < 0) new_pitch = 0;
                if (new_pitch > 127) new_pitch = 127;
                push(&stack, (int32_t)new_pitch);
                return;
            }
        }

        // Try to parse as a number
        char* endptr;
        long num = strtol(word, &endptr, 10);
        if (*endptr == '\0') {
            push(&stack, (int32_t)num);
            return;
        }

        // Try to parse as a pitch name (c4, C#4, Db5, etc.)
        int pitch = parse_pitch(word);
        if (pitch >= 0) {
            push(&stack, (int32_t)pitch);
            return;
        }

        printf("Unknown word: %s\n", word);
        return;
    }

    // Store the word name for 'times' loop (only for user-defined words)
    if (!dictionary[index].is_primitive) {
        strcpy(last_executed_word, word);
    }

    // Execute the word
    if (dictionary[index].is_primitive) {
        // Primitive: call the C function
        dictionary[index].function(&stack);
    } else {
        // User-defined: execute the body
        if (dictionary[index].body) {
            int initial_depth = stack.top;
            execute_line(dictionary[index].body);
            int final_depth = stack.top;
            if (final_depth > initial_depth) {
                printf("Note: '%s' left %d item(s) on stack\n",
                       word, final_depth - initial_depth);
            }
        }
    }
}


// Check if character is a special single-char token
static int is_special_char(char c) {
    return c == ',' || c == '(' || c == ')' || c == '|' || c == '[' || c == ']' || c == '%'
        || c == '{' || c == '}';
}

// Append a word to the current block body
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

// Append a word to the current definition body
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

// End compilation and register the word
static void end_definition(void) {
    if (!compile_mode) {
        printf("Not in compile mode\n");
        return;
    }

    current_definition_body[definition_body_len] = '\0';

    // Register the word
    add_user_word(current_definition_name, current_definition_body);

    // Reset compile state
    compile_mode = 0;
    current_definition_name[0] = '\0';
    definition_body_len = 0;
}

// Parse and execute a command line
void execute_line(const char* input) {
    char word[MAX_INPUT_LENGTH];  // Use larger buffer for words (handles filenames)
    int i = 0;
    int awaiting_name = 0;      // After seeing ':', next word is the name
    int awaiting_filename = 0;  // After seeing 'load', next word is the filename

    while (input[i] != '\0') {
        // Skip whitespace
        while (isspace(input[i])) {
            i++;
        }

        if (input[i] == '\0') break;

        // Extract the word
        int start = i;
        int len = 0;

        // Special single-character tokens: , ( )
        if (is_special_char(input[i])) {
            word[0] = input[i];
            word[1] = '\0';
            len = 1;
            i++;
        } else {
            // Regular word (until whitespace or special char)
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

        // Handle compile mode
        if (awaiting_name) {
            // This word is the name of the definition
            strcpy(current_definition_name, word);
            awaiting_name = 0;
            continue;
        }

        // Handle file operations (load, save, save-midi)
        if (awaiting_filename) {
            if (awaiting_filename == 1) {
                load_file(word);
            } else if (awaiting_filename == 2) {
                recording_save(word);
            } else if (awaiting_filename == 3) {
                capture_save_midi(word);
            }
            awaiting_filename = 0;
            continue;
        }

        if (compile_mode) {
            // In compile mode: check for ';' or accumulate
            if (strcmp(word, ";") == 0) {
                end_definition();
            } else {
                append_to_definition(word);
            }
            continue;
        }

        // Handle block capture mode
        if (block_capture_mode) {
            if (strcmp(word, "{") == 0) {
                // Nested block - just accumulate the braces
                block_nesting++;
                append_to_block(word);
            } else if (strcmp(word, "}") == 0) {
                if (block_nesting > 0) {
                    // Closing a nested block
                    block_nesting--;
                    append_to_block(word);
                } else {
                    // End of this block - store it and push reference
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

        // Interpret mode: check for block start
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

        // Handle conditional skip mode
        if (cond_skip_mode) {
            if (strcmp(word, "if") == 0) {
                // Nested if within skip - track depth
                cond_skip_nesting++;
            } else if (strcmp(word, "then") == 0) {
                if (cond_skip_nesting > 0) {
                    cond_skip_nesting--;
                } else {
                    // End of skip - resume normal execution
                    cond_skip_mode = 0;
                    cond_in_true_branch = 0;
                }
            } else if (strcmp(word, "else") == 0) {
                if (cond_skip_nesting == 0) {
                    // At our level - if we were skipping false branch, now execute
                    // if we were in true branch, keep skipping
                    if (!cond_in_true_branch) {
                        cond_skip_mode = 0;  // Start executing the else branch
                    }
                    // Note: if cond_in_true_branch, we keep skipping until then
                }
            }
            // Skip all other tokens
            continue;
        }

        // Handle if/else/then
        if (strcmp(word, "if") == 0) {
            if (stack.top < 0) {
                printf("if needs a condition on stack\n");
                continue;
            }
            int32_t cond = pop(&stack);
            if (cond == 0) {
                // False - skip to else or then
                cond_skip_mode = 1;
                cond_skip_nesting = 0;
                cond_in_true_branch = 0;
            } else {
                // True - execute until else or then
                cond_in_true_branch = 1;
            }
            continue;
        }

        if (strcmp(word, "else") == 0) {
            // If we get here, we were in the true branch - skip to then
            if (cond_in_true_branch) {
                cond_skip_mode = 1;
                cond_skip_nesting = 0;
            }
            // else: we already executed false branch via skip logic
            continue;
        }

        if (strcmp(word, "then") == 0) {
            // End of conditional - reset state
            cond_in_true_branch = 0;
            continue;
        }

        // Interpret mode
        if (strcmp(word, ":") == 0) {
            // Start a new definition
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
            // Load a file - next word is the filename
            awaiting_filename = 1;
            continue;
        }

        if (strcmp(word, "rec") == 0) {
            recording_start();
            continue;
        }

        if (strcmp(word, "capture") == 0) {
            capture_start();
            continue;
        }

        if (strcmp(word, "stop") == 0) {
            recording_stop();  // Stops recording if active
            capture_stop();    // Stops capture if active
            continue;
        }

        if (strcmp(word, "save") == 0) {
            // Save recording - next word is the filename
            awaiting_filename = 2;  // 2 = save mode
            continue;
        }

        if (strcmp(word, "save-midi") == 0) {
            // Save MIDI capture - next word is the filename
            awaiting_filename = 3;  // 3 = save-midi mode
            continue;
        }

        // Execute the word
        execute_word(word);
    }
}

void print_help(void) {
    printf("\nMIDI Forth - A Forth for MIDI sequence generation\n\n");

    printf("Concise Notation:\n");
    printf("  c4,                     Play C4 with defaults\n");
    printf("  c#4, db4, 60,           Sharps, flats, or MIDI numbers\n");
    printf("  c4, e4, g4,             Sequential notes\n");
    printf("  (c4 e4 g4),             Chord (concurrent)\n");
    printf("  1 c4 100 500,           Explicit: ch pitch vel dur\n");
    printf("  (c4 e4 g4) 1 80 500,    Chord with explicit params\n");
    printf("  r,                      Rest (silence) with default duration\n");
    printf("  r 250,                  Rest with explicit duration\n");
    printf("\n");
    printf("  ch!  ( n -- )           Set default channel (1-16)\n");
    printf("  vel! ( n -- )           Set default velocity (0-127)\n");
    printf("  dur! ( n -- )           Set default duration (ms)\n");

    printf("\nWord Definitions:\n");
    printf("  : name ... ;            Define a new word\n");
    printf("  name                    Execute the word\n");
    printf("  name N times            Execute word N times total\n");

    printf("\nFile Operations:\n");
    printf("  load filename           Load and execute a .4th file\n");
    printf("  rec                     Start recording input commands\n");
    printf("  capture                 Start capturing MIDI events with timing\n");
    printf("  stop                    Stop recording/capturing\n");
    printf("  save filename           Save recorded commands to file\n");
    printf("  save-midi filename      Save captured MIDI as sequence file\n");

    printf("\nGenerative / Expression:\n");
    printf("  c4|e4,                  Alternative (50%% each)\n");
    printf("  c4|e4|g4,               Alternative (33%% each)\n");
    printf("  c4|r,                   50%% play, 50%% silence\n");
    printf("  c4 75%%,                 75%% chance to play\n");
    printf("  [1 c4 100 500],         Explicit params with brackets\n");

    printf("\nDynamics:\n");
    printf("  ppp pp p mp mf f ff fff Set velocity (16-127)\n");
    printf("  mf c4, ff g4,           Change dynamics inline\n");

    printf("\nConvenience:\n");
    printf("  +N                      Relative up N semitones\n");
    printf("  -N                      Relative down N semitones\n");
    printf("  c4, +2, +2, +1,         Plays C D E F\n");
    printf("  ^                       Octave up from current pitch\n");
    printf("  v                       Octave down from current pitch\n");
    printf("  c4, ^, v,               Plays C4, C5, C4\n");
    printf("  pc ( ch prog -- )       Program/instrument change\n");

    printf("\nAdvanced:\n");
    printf("  c4.,                    Staccato (50%% duration)\n");
    printf("  c4>,                    Accent (+20 velocity)\n");
    printf("  c4-,                    Tenuto (full duration)\n");
    printf("  pb ( ch val -- )        Pitch bend (0-16383, center=8192)\n");
    printf("  random ( -- n )         Push random number 0-99\n");
    printf("  { c4, e4, } 4 *         Anonymous block, repeat 4 times\n");
    printf("  flag if ... then        Conditional execution\n");
    printf("  flag if ... else ... then   If-else conditional\n");

    printf("\nMIDI Output:\n");
    printf("  midi-list               List available MIDI output ports\n");
    printf("  midi-open ( n -- )      Open MIDI output port by index\n");
    printf("  midi-virtual            Create virtual MIDI output 'MidiForth'\n");
    printf("  midi-close              Close MIDI output\n");
    printf("  cc ( ch cc val -- )     Send Control Change\n");
    printf("  panic                   All notes off on all channels\n");

    printf("\nPacked Notes:\n");
    printf("  note ( pitch vel ch dur -- n )  Pack note into single value\n");
    printf("  note! ( n -- )          Play packed note (blocking)\n");
    printf("  note. ( n -- )          Print note info\n");
    printf("  pitch@ vel@ ch@ dur@    Extract note components\n");
    printf("  transpose ( n semi -- n )  Transpose note by semitones\n");

    printf("\nSequences:\n");
    printf("  seq-new ( -- id )       Create new sequence, select it\n");
    printf("  seq ( id -- )           Select sequence by id\n");
    printf("  seq-note ( t pitch vel dur -- )  Add note at tick t\n");
    printf("  seq-play                Play current sequence\n");
    printf("  seq-show                Show sequence events\n");
    printf("  seq-transpose ( semi -- )  Transpose all notes\n");
    printf("  seq-reverse             Reverse sequence timing\n");

    printf("\nChord Builders:\n");
    printf("  major minor dim aug     Build triad from root\n");
    printf("  dom7 maj7 min7          Build 7th chord from root\n");

    printf("\nTempo:\n");
    printf("  bpm! ( n -- )           Set tempo (default 120)\n");
    printf("  bpm@ ( -- n )           Get current tempo\n");

    printf("\nExamples:\n");
    printf("  midi-virtual c4, e4, g4,\n");
    printf("  midi-virtual (c4 e4 g4),\n");
    printf("  : melody c4, e4, g4, ; melody 4 times\n");
    printf("  c4, r, e4, r, g4,\n");
    printf("  mf c4|e4|g4, c4|e4|g4,   Random note selection\n");
    printf("  : maybe c4 50%%, ; maybe 8 times\n");
    printf("  c4., e4>, g4,            Staccato, accent, normal\n");
    printf("  { c4., e4., g4., } 4 *   Anonymous block, repeat 4x\n");
    printf("  : coin random 50 > if c4, else e4, then ;\n");
}

// ============================================================================
// Readline autocomplete (experimental)
// ============================================================================

// Static pitch names for completion
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

// Additional built-in words not in dictionary
static const char* builtin_words[] = {
    "quit", "help", "if", "else", "then", ":", ";", "load", "words",
    "rec", "stop", "save", "capture", "save-midi",
    NULL
};

// Generator function for readline completion
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

    // Search dictionary words
    while (dict_index < dict_count) {
        const char* name = dictionary[dict_index].name;
        dict_index++;
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    // Search pitch names
    while (pitch_names[pitch_index] != NULL) {
        const char* name = pitch_names[pitch_index];
        pitch_index++;
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    // Search builtin words
    while (builtin_words[builtin_index] != NULL) {
        const char* name = builtin_words[builtin_index];
        builtin_index++;
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    return NULL;
}

// Custom completion function
static char** forth_completion(const char* text, int start, int end) {
    (void)start;
    (void)end;

    // Disable default filename completion
    rl_attempted_completion_over = 1;

    return rl_completion_matches(text, completion_generator);
}

// Initialize readline completion
static void init_readline_completion(void) {
    rl_attempted_completion_function = forth_completion;
    // Use space as word break character (Forth words are space-separated)
    rl_completer_word_break_characters = " \t\n";
}

// Interactive interpreter loop
void interpreter_loop(void) {
    char* input;

    printf("MIDI Forth (type 'help' for commands, 'quit' to exit)\n");

    while (1) {
        input = readline("> ");

        if (input == NULL) {
            // EOF (Ctrl-D)
            printf("\n");
            break;
        }

        // Skip empty lines
        if (input[0] == '\0') {
            free(input);
            continue;
        }

        // Add to history
        add_history(input);

        if (strcmp(input, "quit") == 0) {
            free(input);
            break;
        }

        if (strcmp(input, "help") == 0) {
            print_help();
            free(input);
            continue;
        }

        // Record input if recording is active
        // Don't record control commands (rec, stop, save, quit, help, load)
        if (recording_active) {
            // Check if first word is a control command
            char first_word[32];
            int j = 0;
            const char* p = input;
            while (*p && isspace(*p)) p++;  // Skip leading whitespace
            while (*p && !isspace(*p) && j < 31) {
                first_word[j++] = *p++;
            }
            first_word[j] = '\0';

            // Only record if not a control command
            if (strcmp(first_word, "rec") != 0 &&
                strcmp(first_word, "stop") != 0 &&
                strcmp(first_word, "save") != 0 &&
                strcmp(first_word, "save-midi") != 0 &&
                strcmp(first_word, "capture") != 0 &&
                strcmp(first_word, "load") != 0) {
                recording_add_line(input);
            }
        }

        execute_line(input);
        printf(" ok\n");

        free(input);
    }
}

void cleanup_midi(void) {
    if (midi_out != NULL) {
        op_all_notes_off(NULL);  // Send panic before closing
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
    }

    for (int i = 0; i < out_port_count; i++) {
        libremidi_midi_out_port_free(out_ports[i]);
    }
    out_port_count = 0;

    if (midi_observer != NULL) {
        libremidi_midi_observer_free(midi_observer);
        midi_observer = NULL;
    }

    // Clean up recording and capture buffers
    recording_clear();
    capture_clear();
}

int main(void) {
    // Seed random number generator
    srand((unsigned int)time(NULL));

    // Initialize stack
    stack.top = -1;

    // Initialize dictionary with primitives
    init_dictionary();

    // Initialize readline autocomplete
    init_readline_completion();

    // Start interactive interpreter
    interpreter_loop();

    // Cleanup
    cleanup_midi();

    return 0;
}
