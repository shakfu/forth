/* midi_core.c - MIDI I/O operations for MIDI Forth interpreter */

#include "forth_midi.h"

/* MIDI globals - defined here */
libremidi_midi_observer_handle* midi_observer = NULL;
libremidi_midi_out_handle* midi_out = NULL;
libremidi_midi_out_port* out_ports[MAX_PORTS];
int out_port_count = 0;

/* Context defaults for concise notation */
int default_channel = 1;      /* 1-16 */
int default_velocity = 80;    /* 0-127 */
int default_duration = 500;   /* milliseconds */
int current_pitch = 60;       /* Track last played pitch for relative intervals */

/* Articulation flags (set by pitch parser, consumed by play) */
int articulation_staccato = 0;   /* Reduce duration to 50% */
int articulation_accent = 0;     /* Increase velocity by 20 */
int articulation_tenuto = 0;     /* Full duration */

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
    if (midi_out != NULL) {
        op_all_notes_off(NULL);
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
    if (ms > 0) {
        usleep(ms * 1000);
    }
}

/* midi-list ( -- ) List available MIDI output ports */
void op_midi_list(Stack* stack) {
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
        printf("  (none - use midi-open to create a virtual port)\n");
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

/* midi-open-port ( n -- ) Open MIDI output port by index */
void op_midi_open(Stack* stack) {
    int32_t port_idx = pop(stack);

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

/* Helper to open virtual port with a given name */
int open_virtual_port(const char* name) {
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

/* midi-open ( -- ) Create virtual MIDI output 'ForthMIDI' */
void op_midi_open_port(Stack* stack) {
    (void)stack;
    open_virtual_port("ForthMIDI");
}

/* midi-close ( -- ) Close MIDI output */
void op_midi_close(Stack* stack) {
    (void)stack;
    if (midi_out != NULL) {
        libremidi_midi_out_free(midi_out);
        midi_out = NULL;
        printf("MIDI output closed\n");
    }
}

/* panic ( -- ) All notes off on all channels */
void op_all_notes_off(Stack* stack) {
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
void op_sleep(Stack* stack) {
    int32_t ms = pop(stack);
    if (ms > 0) {
        usleep(ms * 1000);
    }
}
