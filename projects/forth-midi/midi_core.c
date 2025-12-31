/* midi_core.c - MIDI I/O operations for MIDI Forth interpreter */

#include "forth_midi.h"

/* All globals now accessed via g_ctx macros defined in forth_midi.h */

/* Callback for port enumeration */
static void on_output_port_found(void* ctx, const libremidi_midi_out_port* port) {
    (void)ctx;
    if (out_port_count >= MAX_PORTS) return;
    libremidi_midi_out_port_clone(port, &out_ports[out_port_count]);
    out_port_count++;
}

/* Initialize MIDI observer */
void midi_init_observer(void) {
    int ret = 0;

    /* Free existing observer if any */
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
        return;
    }

    observer_conf.track_hardware = true;
    observer_conf.track_virtual = true;
    observer_conf.track_any = true;

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        printf("Failed to init api config: %d\n", ret);
        return;
    }

    api_conf.configuration_type = Observer;
    api_conf.api = UNSPECIFIED;

    ret = libremidi_midi_observer_new(&observer_conf, &api_conf, &midi_observer);
    if (ret != 0) {
        printf("Failed to create MIDI observer: %d\n", ret);
        return;
    }

    /* Enumerate output ports */
    out_port_count = 0;
    ret = libremidi_midi_observer_enumerate_output_ports(midi_observer, NULL, on_output_port_found);
    if (ret != 0) {
        printf("Failed to enumerate ports: %d\n", ret);
    }
}

/* Cleanup MIDI observer */
void midi_cleanup_observer(void) {
    /* Cleanup output */
    if (midi_out != NULL) {
        op_all_notes_off(NULL);
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
    }

    for (int i = 0; i < out_port_count; i++) {
        libremidi_midi_out_port_free(out_ports[i]);
    }
    out_port_count = 0;

    /* Cleanup input */
    midi_input_cleanup();

    if (midi_observer != NULL) {
        libremidi_midi_observer_free(midi_observer);
        midi_observer = NULL;
    }
}

/* Low-level MIDI send functions */
void midi_send_note_on(int pitch, int velocity, int channel) {
    if (midi_out == NULL) return;
    unsigned char msg[3];
    msg[0] = 0x90 | ((channel - 1) & 0x0F);
    msg[1] = pitch & 0x7F;
    msg[2] = velocity & 0x7F;
    libremidi_midi_out_send_message(midi_out, msg, 3);
}

void midi_send_note_off(int pitch, int channel) {
    if (midi_out == NULL) return;
    unsigned char msg[3];
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[1] = pitch & 0x7F;
    msg[2] = 0;
    libremidi_midi_out_send_message(midi_out, msg, 3);
}

void midi_send_cc(int cc, int value, int channel) {
    if (midi_out == NULL) return;
    unsigned char msg[3];
    msg[0] = 0xB0 | ((channel - 1) & 0x0F);
    msg[1] = cc & 0x7F;
    msg[2] = value & 0x7F;
    libremidi_midi_out_send_message(midi_out, msg, 3);
}

void midi_send_program_change(int program, int channel) {
    if (midi_out == NULL) return;
    unsigned char msg[2];
    msg[0] = 0xC0 | ((channel - 1) & 0x0F);
    msg[1] = program & 0x7F;
    libremidi_midi_out_send_message(midi_out, msg, 2);
}

void midi_send_pitch_bend(int value, int channel) {
    if (midi_out == NULL) return;
    unsigned char msg[3];
    msg[0] = 0xE0 | ((channel - 1) & 0x0F);
    msg[1] = value & 0x7F;         /* LSB */
    msg[2] = (value >> 7) & 0x7F;  /* MSB */
    libremidi_midi_out_send_message(midi_out, msg, 3);
}

void midi_sleep_ms(int ms) {
    if (ms > 0 && !forth_no_sleep()) {
        usleep(ms * 1000);
    }
}

/* midi-output-list ( -- ) List available MIDI output ports */
void op_midi_output_list(Stack* s) {
    (void)stack;
    midi_init_observer();

    /* Clear and re-enumerate */
    for (int i = 0; i < out_port_count; i++) {
        libremidi_midi_out_port_free(out_ports[i]);
    }
    out_port_count = 0;
    libremidi_midi_observer_enumerate_output_ports(midi_observer, NULL, on_output_port_found);

    printf("Hardware MIDI outputs:\n");
    if (out_port_count == 0) {
        printf("  (none - use midi-output-virtual to create a virtual port)\n");
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

/* midi-output-open ( n -- ) Open MIDI output port by index */
void op_midi_output_open(Stack* s) {
    int32_t port_idx = pop(&stack);

    midi_init_observer();

    if (port_idx < 0 || port_idx >= out_port_count) {
        printf("Invalid port index: %d (have %d ports)\n", port_idx, out_port_count);
        return;
    }

    /* Close existing output if open */
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
    api_conf.api = UNSPECIFIED;

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

/* Helper to open port by name (searches hardware ports first, then creates virtual) */
int open_virtual_port(const char* name) {
    /* First, search hardware ports for substring match */
    midi_init_observer();

    /* Re-enumerate ports to get current list */
    for (int i = 0; i < out_port_count; i++) {
        libremidi_midi_out_port_free(out_ports[i]);
    }
    out_port_count = 0;
    libremidi_midi_observer_enumerate_output_ports(midi_observer, NULL, on_output_port_found);

    /* Search for substring match in hardware port names */
    for (int i = 0; i < out_port_count; i++) {
        const char* port_name = NULL;
        size_t len = 0;
        if (libremidi_midi_out_port_name(out_ports[i], &port_name, &len) == 0) {
            if (strstr(port_name, name) != NULL) {
                /* Found a match - open this hardware port */
                if (midi_out != NULL) {
                    libremidi_midi_out_free(midi_out);
                    midi_out = NULL;
                }

                libremidi_midi_configuration midi_conf;
                if (libremidi_midi_configuration_init(&midi_conf) != 0) {
                    printf("Failed to init MIDI config\n");
                    return -1;
                }
                midi_conf.version = MIDI1;
                midi_conf.out_port = out_ports[i];

                libremidi_api_configuration api_conf;
                if (libremidi_midi_api_configuration_init(&api_conf) != 0) {
                    printf("Failed to init API config\n");
                    return -1;
                }
                api_conf.configuration_type = Output;
                api_conf.api = UNSPECIFIED;

                int ret = libremidi_midi_out_new(&midi_conf, &api_conf, &midi_out);
                if (ret != 0) {
                    printf("Failed to open MIDI output: %d\n", ret);
                    return ret;
                }
                printf("Opened MIDI output: %s\n", port_name);
                return 0;
            }
        }
    }

    /* No hardware port matched - create virtual port */
    if (midi_out != NULL) {
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
    }

    int ret = 0;

    libremidi_midi_configuration midi_conf;
    ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        printf("Failed to init MIDI config\n");
        return ret;
    }

    midi_conf.version = MIDI1;
    midi_conf.virtual_port = true;
    midi_conf.port_name = name;

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        printf("Failed to init API config\n");
        return ret;
    }

    api_conf.configuration_type = Output;
    api_conf.api = UNSPECIFIED;

    ret = libremidi_midi_out_new(&midi_conf, &api_conf, &midi_out);
    if (ret != 0) {
        printf("Failed to create virtual MIDI output: %d\n", ret);
        return ret;
    }

    printf("Created virtual MIDI output: %s\n", name);
    return 0;
}

/* midi-output-virtual ( -- ) Create virtual MIDI output 'ForthMIDI' */
void op_midi_output_virtual(Stack* s) {
    (void)stack;
    open_virtual_port("ForthMIDI");
}

/* midi-output-close ( -- ) Close MIDI output */
void op_midi_output_close(Stack* s) {
    (void)stack;
    if (midi_out != NULL) {
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
        printf("MIDI output closed\n");
    }
}

/* panic ( -- ) All notes off on all channels */
void op_all_notes_off(Stack* s) {
    (void)stack;
    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    /* Send All Notes Off (CC 123) on all channels */
    for (int ch = 0; ch < 16; ch++) {
        unsigned char msg[3];
        msg[0] = 0xB0 | ch;
        msg[1] = 123;  /* All Notes Off */
        msg[2] = 0;
        libremidi_midi_out_send_message(midi_out, msg, 3);
    }
}

/* ms ( n -- ) Sleep for n milliseconds */
void op_sleep(Stack* s) {
    (void)s;
    int32_t ms = pop(&stack);
    midi_sleep_ms(ms);
}

/* ============================================================================
 * MIDI Input Implementation
 * ============================================================================ */

/* Callback for input port enumeration */
static void on_input_port_found(void* ctx, const libremidi_midi_in_port* port) {
    (void)ctx;
    if (in_port_count >= MAX_INPUT_PORTS) return;
    libremidi_midi_in_port_clone(port, &in_ports[in_port_count]);
    in_port_count++;
}

/* Ring buffer operations (lock-free for single producer, single consumer) */
static inline int queue_is_empty(void) {
    return input_queue.head == input_queue.tail;
}

static inline int queue_is_full(void) {
    return ((input_queue.head + 1) % MIDI_INPUT_QUEUE_SIZE) == input_queue.tail;
}

static void queue_push(const MidiInputMessage* msg) {
    if (queue_is_full()) {
        /* Drop oldest message to make room */
        input_queue.tail = (input_queue.tail + 1) % MIDI_INPUT_QUEUE_SIZE;
    }
    input_queue.messages[input_queue.head] = *msg;
    input_queue.head = (input_queue.head + 1) % MIDI_INPUT_QUEUE_SIZE;
}

static int queue_pop(MidiInputMessage* msg) {
    if (queue_is_empty()) return 0;
    *msg = input_queue.messages[input_queue.tail];
    input_queue.tail = (input_queue.tail + 1) % MIDI_INPUT_QUEUE_SIZE;
    return 1;
}

/* MIDI input callback - called from libremidi's thread */
static void midi_input_callback(void* ctx, libremidi_timestamp timestamp,
                                 const libremidi_midi1_symbol* data, size_t len) {
    (void)ctx;
    (void)timestamp;

    if (len == 0 || len > 3) return;  /* Ignore empty or sysex messages */

    MidiInputMessage msg;
    msg.status = data[0];
    msg.data1 = (len > 1) ? data[1] : 0;
    msg.data2 = (len > 2) ? data[2] : 0;
    msg.length = (uint8_t)len;

    queue_push(&msg);
}

/* Initialize MIDI input subsystem */
void midi_input_init(void) {
    midi_init_observer();  /* Ensure observer exists */

    /* Clear input port list */
    for (int i = 0; i < in_port_count; i++) {
        libremidi_midi_in_port_free(in_ports[i]);
    }
    in_port_count = 0;

    /* Enumerate input ports */
    libremidi_midi_observer_enumerate_input_ports(midi_observer, NULL, on_input_port_found);
}

/* Cleanup MIDI input */
void midi_input_cleanup(void) {
    if (midi_in != NULL) {
        libremidi_midi_in_free(midi_in);
        midi_in = NULL;
    }

    for (int i = 0; i < in_port_count; i++) {
        libremidi_midi_in_port_free(in_ports[i]);
    }
    in_port_count = 0;

    /* Clear queue */
    input_queue.head = 0;
    input_queue.tail = 0;
}

/* Check if input messages are pending */
int midi_input_pending(void) {
    return !queue_is_empty();
}

/* Dequeue a message (returns 0 if empty) */
int midi_input_dequeue(MidiInputMessage* msg) {
    return queue_pop(msg);
}

/* midi-input-list ( -- ) List available MIDI input ports */
void op_midi_input_list(Stack* s) {
    (void)s;
    midi_input_init();

    printf("MIDI inputs:\n");
    if (in_port_count == 0) {
        printf("  (none available)\n");
    } else {
        for (int i = 0; i < in_port_count; i++) {
            const char* name = NULL;
            size_t len = 0;
            if (libremidi_midi_in_port_name(in_ports[i], &name, &len) == 0) {
                printf("  %d: %s\n", i, name);
            }
        }
    }
}

/* midi-input-open ( n -- ) Open MIDI input port by index */
void op_midi_input_open(Stack* s) {
    (void)s;
    int32_t port_idx = pop(&stack);

    midi_input_init();

    if (port_idx < 0 || port_idx >= in_port_count) {
        printf("Invalid input port index: %d (have %d ports)\n", port_idx, in_port_count);
        return;
    }

    /* Close existing input if open */
    if (midi_in != NULL) {
        libremidi_midi_in_free(midi_in);
        midi_in = NULL;
    }

    /* Clear queue */
    input_queue.head = 0;
    input_queue.tail = 0;

    int ret = 0;

    libremidi_midi_configuration midi_conf;
    ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        printf("Failed to init MIDI config\n");
        return;
    }

    midi_conf.version = MIDI1;
    midi_conf.in_port = in_ports[port_idx];
    midi_conf.on_midi1_message.context = NULL;
    midi_conf.on_midi1_message.callback = midi_input_callback;
    midi_conf.ignore_sysex = true;
    midi_conf.ignore_timing = true;
    midi_conf.ignore_sensing = true;

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        printf("Failed to init API config\n");
        return;
    }

    api_conf.configuration_type = Input;
    api_conf.api = UNSPECIFIED;

    ret = libremidi_midi_in_new(&midi_conf, &api_conf, &midi_in);
    if (ret != 0) {
        printf("Failed to open MIDI input: %d\n", ret);
        return;
    }

    const char* name = NULL;
    size_t len = 0;
    libremidi_midi_in_port_name(in_ports[port_idx], &name, &len);
    printf("Opened MIDI input: %s\n", name);
}

/* midi-input-virtual ( -- ) Create virtual MIDI input */
void op_midi_input_open_virtual(Stack* s) {
    (void)s;

    midi_input_init();

    /* Close existing input if open */
    if (midi_in != NULL) {
        libremidi_midi_in_free(midi_in);
        midi_in = NULL;
    }

    /* Clear queue */
    input_queue.head = 0;
    input_queue.tail = 0;

    int ret = 0;

    libremidi_midi_configuration midi_conf;
    ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        printf("Failed to init MIDI config\n");
        return;
    }

    midi_conf.version = MIDI1;
    midi_conf.virtual_port = true;
    midi_conf.port_name = "ForthMIDI Input";
    midi_conf.on_midi1_message.context = NULL;
    midi_conf.on_midi1_message.callback = midi_input_callback;
    midi_conf.ignore_sysex = true;
    midi_conf.ignore_timing = true;
    midi_conf.ignore_sensing = true;

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        printf("Failed to init API config\n");
        return;
    }

    api_conf.configuration_type = Input;
    api_conf.api = UNSPECIFIED;

    ret = libremidi_midi_in_new(&midi_conf, &api_conf, &midi_in);
    if (ret != 0) {
        printf("Failed to create virtual MIDI input: %d\n", ret);
        return;
    }

    printf("Created virtual MIDI input: ForthMIDI Input\n");
}

/* midi-input-close ( -- ) Close MIDI input */
void op_midi_input_close(Stack* s) {
    (void)s;
    if (midi_in != NULL) {
        libremidi_midi_in_free(midi_in);
        midi_in = NULL;
        printf("MIDI input closed\n");
    }
}

/* midi-input? ( -- flag ) Check if input messages are pending */
void op_midi_input_pending(Stack* s) {
    (void)s;
    push(&stack, midi_input_pending() ? -1 : 0);
}

/* midi-input@ ( -- status data1 data2 flag ) Read next input message */
void op_midi_input_read(Stack* s) {
    (void)s;
    MidiInputMessage msg;
    if (midi_input_dequeue(&msg)) {
        push(&stack, msg.status);
        push(&stack, msg.data1);
        push(&stack, msg.data2);
        push(&stack, -1);  /* true = success */
    } else {
        push(&stack, 0);
        push(&stack, 0);
        push(&stack, 0);
        push(&stack, 0);   /* false = no message */
    }
}

/* midi-input-flush ( -- ) Clear all pending input messages */
void op_midi_input_flush(Stack* s) {
    (void)s;
    input_queue.head = 0;
    input_queue.tail = 0;
}
