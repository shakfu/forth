/* midi_ffi.c - C FFI implementation for MicroHs MIDI library */
#include "midi_ffi.h"
#include <libremidi/libremidi-c.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define MAX_PORTS 64
#define MAX_PORT_NAME 256

/* Global state */
static libremidi_midi_observer_handle* g_observer = NULL;
static libremidi_midi_out_handle* g_midi_out = NULL;
static libremidi_midi_out_port* g_ports[MAX_PORTS];
static char g_port_names[MAX_PORTS][MAX_PORT_NAME];
static int g_port_count = 0;
static int g_initialized = 0;

/* Callback for port enumeration */
static void port_callback(void* ctx, const libremidi_midi_out_port* port) {
    (void)ctx;
    if (g_port_count >= MAX_PORTS) return;

    /* Clone the port */
    if (libremidi_midi_out_port_clone(port, &g_ports[g_port_count]) != 0) {
        return;
    }

    /* Get the port name */
    const char* name = NULL;
    size_t len = 0;
    if (libremidi_midi_out_port_name(port, &name, &len) == 0 && name) {
        size_t copy_len = len < MAX_PORT_NAME - 1 ? len : MAX_PORT_NAME - 1;
        memcpy(g_port_names[g_port_count], name, copy_len);
        g_port_names[g_port_count][copy_len] = '\0';
    } else {
        snprintf(g_port_names[g_port_count], MAX_PORT_NAME, "Port %d", g_port_count);
    }

    g_port_count++;
}

int midi_init(void) {
    if (g_initialized) return 0;

    /* Initialize observer configuration */
    libremidi_observer_configuration obs_conf;
    libremidi_midi_observer_configuration_init(&obs_conf);

    /* Initialize API configuration */
    libremidi_api_configuration api_conf;
    libremidi_midi_api_configuration_init(&api_conf);
    api_conf.configuration_type = Observer;
    api_conf.api = UNSPECIFIED;

    /* Create observer */
    if (libremidi_midi_observer_new(&obs_conf, &api_conf, &g_observer) != 0) {
        return -1;
    }

    g_initialized = 1;
    return 0;
}

void midi_cleanup(void) {
    midi_close();

    /* Free ports */
    for (int i = 0; i < g_port_count; i++) {
        if (g_ports[i]) {
            libremidi_midi_out_port_free(g_ports[i]);
            g_ports[i] = NULL;
        }
    }
    g_port_count = 0;

    /* Free observer */
    if (g_observer) {
        libremidi_midi_observer_free(g_observer);
        g_observer = NULL;
    }

    g_initialized = 0;
}

int midi_list_ports(void) {
    if (!g_initialized) {
        if (midi_init() != 0) return 0;
    }

    /* Free any existing ports */
    for (int i = 0; i < g_port_count; i++) {
        if (g_ports[i]) {
            libremidi_midi_out_port_free(g_ports[i]);
            g_ports[i] = NULL;
        }
    }
    g_port_count = 0;

    /* Enumerate ports */
    libremidi_midi_observer_enumerate_output_ports(g_observer, NULL, port_callback);

    return g_port_count;
}

const char* midi_port_name(int index) {
    if (index < 0 || index >= g_port_count) {
        return "";
    }
    return g_port_names[index];
}

int midi_open(int port_index) {
    if (!g_initialized) {
        if (midi_init() != 0) return -1;
    }

    if (port_index < 0 || port_index >= g_port_count) {
        return -1;
    }

    /* Close any existing connection */
    midi_close();

    /* Initialize MIDI configuration */
    libremidi_midi_configuration midi_conf;
    libremidi_midi_configuration_init(&midi_conf);
    midi_conf.version = MIDI1;
    midi_conf.out_port = g_ports[port_index];

    /* Initialize API configuration */
    libremidi_api_configuration api_conf;
    libremidi_midi_api_configuration_init(&api_conf);
    api_conf.configuration_type = Output;
    api_conf.api = UNSPECIFIED;

    /* Create MIDI output */
    if (libremidi_midi_out_new(&midi_conf, &api_conf, &g_midi_out) != 0) {
        g_midi_out = NULL;
        return -1;
    }

    return 0;
}

int midi_open_virtual(const char* name) {
    if (!g_initialized) {
        if (midi_init() != 0) return -1;
    }

    /* Close any existing connection */
    midi_close();

    /* Initialize MIDI configuration for virtual port */
    libremidi_midi_configuration midi_conf;
    libremidi_midi_configuration_init(&midi_conf);
    midi_conf.version = MIDI1;
    midi_conf.virtual_port = true;
    midi_conf.port_name = name ? name : "MhsMidi";

    /* Initialize API configuration */
    libremidi_api_configuration api_conf;
    libremidi_midi_api_configuration_init(&api_conf);
    api_conf.configuration_type = Output;
    api_conf.api = UNSPECIFIED;

    /* Create MIDI output */
    if (libremidi_midi_out_new(&midi_conf, &api_conf, &g_midi_out) != 0) {
        g_midi_out = NULL;
        return -1;
    }

    return 0;
}

void midi_close(void) {
    if (g_midi_out) {
        midi_panic();
        libremidi_midi_out_free(g_midi_out);
        g_midi_out = NULL;
    }
}

int midi_is_open(void) {
    return g_midi_out != NULL;
}

int midi_send(uint8_t status, uint8_t data1, uint8_t data2) {
    if (!g_midi_out) return -1;

    unsigned char msg[3] = { status, data1 & 0x7F, data2 & 0x7F };
    return libremidi_midi_out_send_message(g_midi_out, msg, 3);
}

int midi_note_on(int channel, int pitch, int velocity) {
    if (channel < 1 || channel > 16) return -1;
    uint8_t status = 0x90 | ((channel - 1) & 0x0F);
    return midi_send(status, (uint8_t)pitch, (uint8_t)velocity);
}

int midi_note_off(int channel, int pitch) {
    if (channel < 1 || channel > 16) return -1;
    uint8_t status = 0x80 | ((channel - 1) & 0x0F);
    return midi_send(status, (uint8_t)pitch, 0);
}

int midi_cc(int channel, int controller, int value) {
    if (channel < 1 || channel > 16) return -1;
    uint8_t status = 0xB0 | ((channel - 1) & 0x0F);
    return midi_send(status, (uint8_t)controller, (uint8_t)value);
}

int midi_program(int channel, int program) {
    if (!g_midi_out) return -1;
    if (channel < 1 || channel > 16) return -1;

    unsigned char msg[2];
    msg[0] = 0xC0 | ((channel - 1) & 0x0F);
    msg[1] = (uint8_t)(program & 0x7F);
    return libremidi_midi_out_send_message(g_midi_out, msg, 2);
}

int midi_pitch_bend(int channel, int value) {
    if (!g_midi_out) return -1;
    if (channel < 1 || channel > 16) return -1;

    /* Convert from -8192..8191 to 0..16383 */
    int bent = value + 8192;
    if (bent < 0) bent = 0;
    if (bent > 16383) bent = 16383;

    unsigned char msg[3];
    msg[0] = 0xE0 | ((channel - 1) & 0x0F);
    msg[1] = (uint8_t)(bent & 0x7F);        /* LSB */
    msg[2] = (uint8_t)((bent >> 7) & 0x7F); /* MSB */
    return libremidi_midi_out_send_message(g_midi_out, msg, 3);
}

void midi_sleep(int ms) {
    if (ms > 0) {
        usleep((useconds_t)(ms * 1000));
    }
}

void midi_panic(void) {
    if (!g_midi_out) return;

    /* Send all notes off on all 16 channels */
    for (int ch = 1; ch <= 16; ch++) {
        midi_cc(ch, 123, 0);  /* All Notes Off */
    }
}
