#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <unistd.h>
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
    int is_primitive;
} Word;

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

// Chord marker (sentinel value on stack)
#define CHORD_MARKER 0x7FFFFFFF

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
    dictionary[dict_count].is_primitive = is_primitive;
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

// note-on ( ch pitch vel dur -- ) play note with duration in ms, auto note-off
void op_note_on(Stack* stack) {
    int32_t duration = pop(stack);
    int32_t velocity = pop(stack);
    int32_t pitch = pop(stack);
    int32_t channel = pop(stack);

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
    int ret = libremidi_midi_out_send_message(midi_out, msg, 3);
    if (ret != 0) {
        printf("Failed to send note on\n");
        return;
    }

    // Wait for duration
    if (duration > 0) {
        usleep(duration * 1000);
    }

    // Note off
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[2] = 0;
    libremidi_midi_out_send_message(midi_out, msg, 3);
}

void op_note_off(Stack* stack) {
    int32_t velocity = pop(stack);
    int32_t pitch = pop(stack);
    int32_t channel = pop(stack);

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

    unsigned char msg[3];
    msg[0] = 0x80 | ((channel - 1) & 0x0F);  // Note Off
    msg[1] = pitch & 0x7F;
    msg[2] = velocity & 0x7F;

    int ret = libremidi_midi_out_send_message(midi_out, msg, 3);
    if (ret != 0) {
        printf("Failed to send note off\n");
    }
}

void op_cc(Stack* stack) {
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

// Helper: play a single note with given params
static void play_single_note(int channel, int pitch, int velocity, int duration) {
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

    // Wait
    if (duration > 0) {
        usleep(duration * 1000);
    }

    // Note off
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[2] = 0;
    libremidi_midi_out_send_message(midi_out, msg, 3);
}

// Helper: play chord with given params
static void play_chord_notes(int* pitches, int count, int channel, int velocity, int duration) {
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
    }
}

// The comma - universal play trigger
void op_comma(Stack* stack) {
    if (stack->top < 0) {
        printf("Stack empty\n");
        return;
    }

    // Count items and find chord marker
    int count = 0;
    int marker_pos = -1;
    for (int i = stack->top; i >= 0; i--) {
        if (stack->data[i] == CHORD_MARKER) {
            marker_pos = i;
            break;
        }
        count++;
    }

    if (marker_pos >= 0) {
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
            return;
        }

        // Pop pitches (in reverse order)
        for (int i = pitch_count - 1; i >= 0; i--) {
            pitches[i] = pop(stack);
        }

        // Pop the marker
        pop(stack);

        play_chord_notes(pitches, pitch_count, channel, velocity, duration);
    } else {
        // Single note mode
        if (count == 1) {
            // Defaults: just pitch on stack
            int pitch = pop(stack);
            play_single_note(default_channel, pitch, default_velocity, default_duration);
        } else if (count == 4) {
            // Explicit: ch pitch vel dur
            int duration = pop(stack);
            int velocity = pop(stack);
            int pitch = pop(stack);
            int channel = pop(stack);
            play_single_note(channel, pitch, velocity, duration);
        } else {
            printf("Invalid note: expected 1 (pitch) or 4 (ch pitch vel dur) items, got %d\n", count);
        }
    }
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

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    int pitch = note_pitch(n);
    int vel = note_vel(n);
    int ch = note_ch(n);
    int dur = note_dur(n);

    // Convert ticks to ms: (ticks / PPQ) * (60000 / BPM)
    int ms = (dur * 60000) / (TICKS_PER_QUARTER * global_bpm);

    unsigned char msg[3];

    // Note on
    msg[0] = 0x90 | (ch & 0x0F);
    msg[1] = pitch & 0x7F;
    msg[2] = vel & 0x7F;
    libremidi_midi_out_send_message(midi_out, msg, 3);

    // Wait
    if (ms > 0) usleep(ms * 1000);

    // Note off
    msg[0] = 0x80 | (ch & 0x0F);
    msg[2] = 0;
    libremidi_midi_out_send_message(midi_out, msg, 3);
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
                break;
            case EVT_NOTE_OFF:
                msg[0] = 0x80 | (e->channel & 0x0F);
                msg[1] = e->data1 & 0x7F;
                msg[2] = 0;
                break;
            case EVT_CC:
                msg[0] = 0xB0 | (e->channel & 0x0F);
                msg[1] = e->data1 & 0x7F;
                msg[2] = e->data2 & 0x7F;
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
    }

    if (ms > 0) usleep(ms * 1000);

    // All notes off
    for (int i = 0; i < n; i++) {
        msg[0] = 0x80;
        msg[1] = pitches[i] & 0x7F;
        msg[2] = 0;
        libremidi_midi_out_send_message(midi_out, msg, 3);
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

// Note name helpers: c d e f g a b push MIDI note numbers (octave 4 = middle)
void op_note_c(Stack* stack) { push(stack, 60); }
void op_note_d(Stack* stack) { push(stack, 62); }
void op_note_e(Stack* stack) { push(stack, 64); }
void op_note_f(Stack* stack) { push(stack, 65); }
void op_note_g(Stack* stack) { push(stack, 67); }
void op_note_a(Stack* stack) { push(stack, 69); }
void op_note_b(Stack* stack) { push(stack, 71); }

// octave ( note oct -- note ) set octave (0-9, middle C = octave 4)
void op_octave(Stack* stack) {
    int32_t oct = pop(stack);
    int32_t note = pop(stack);
    int pc = note % 12;  // pitch class
    push(stack, pc + (oct + 1) * 12);
}

// Initialize the dictionary with primitive words
void init_dictionary(void) {
    // Arithmetic
    add_word("+", op_plus, 1);
    add_word("-", op_minus, 1);
    add_word("*", op_multiply, 1);
    add_word("/", op_divide, 1);

    // Stack manipulation
    add_word("swap", op_swap, 1);
    add_word("dup", op_dup, 1);
    add_word("drop", op_drop, 1);
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
    add_word("note-on", op_note_on, 1);
    add_word("cc", op_cc, 1);
    add_word("panic", op_all_notes_off, 1);

    // Concise notation
    add_word(",", op_comma, 1);
    add_word("(", op_chord_open, 1);
    add_word(")", op_chord_close, 1);
    add_word("ch!", op_ch_store, 1);
    add_word("vel!", op_vel_store, 1);
    add_word("dur!", op_dur_store, 1);

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

    // Note names (octave 4)
    add_word("C", op_note_c, 1);
    add_word("D", op_note_d, 1);
    add_word("E", op_note_e, 1);
    add_word("F", op_note_f, 1);
    add_word("G", op_note_g, 1);
    add_word("A", op_note_a, 1);
    add_word("B", op_note_b, 1);
    add_word("octave", op_octave, 1);
}

// Parse pitch name like c4, C#4, Db5, etc.
// Returns MIDI note number (0-127) or -1 if not a valid pitch name
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

    // Execute the function for this word
    dictionary[index].function(&stack);
}

// Check if character is a special single-char token
static int is_special_char(char c) {
    return c == ',' || c == '(' || c == ')';
}

// Parse and execute a command line
void execute_line(const char* input) {
    char word[MAX_WORD_LENGTH];
    int i = 0;

    while (input[i] != '\0') {
        // Skip whitespace
        while (isspace(input[i])) {
            i++;
        }

        if (input[i] == '\0') break;

        // Special single-character tokens: , ( )
        if (is_special_char(input[i])) {
            word[0] = input[i];
            word[1] = '\0';
            i++;
            execute_word(word);
            continue;
        }

        // Extract next word (until whitespace or special char)
        int start = i;
        while (!isspace(input[i]) && input[i] != '\0' && !is_special_char(input[i])) {
            i++;
        }

        int len = i - start;
        if (len >= MAX_WORD_LENGTH) {
            printf("Word too long\n");
            return;
        }

        strncpy(word, input + start, len);
        word[len] = '\0';

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
    printf("\n");
    printf("  ch!  ( n -- )           Set default channel (1-16)\n");
    printf("  vel! ( n -- )           Set default velocity (0-127)\n");
    printf("  dur! ( n -- )           Set default duration (ms)\n");

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
    printf("  100 vel! 250 dur! c4, d4, e4,\n");
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
}

int main(void) {
    // Initialize stack
    stack.top = -1;

    // Initialize dictionary with primitives
    init_dictionary();

    // Start interactive interpreter
    interpreter_loop();

    // Cleanup
    cleanup_midi();

    return 0;
}
