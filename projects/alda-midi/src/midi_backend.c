/**
 * @file midi_backend.c
 * @brief MIDI I/O backend using libremidi.
 *
 * Adapted from forth-midi/midi_core.c
 */

#include "alda/midi_backend.h"
#include "alda/tsf_backend.h"
#include <stdio.h>
#include <string.h>

/* Cross-platform sleep */
#ifdef _WIN32
#include <windows.h>
#define usleep(us) Sleep((us) / 1000)
#else
#include <unistd.h>
#endif

/* ============================================================================
 * Port Enumeration Callback
 * ============================================================================ */

typedef struct {
    AldaContext* ctx;
} EnumContext;

static void on_output_port_found(void* user_ctx, const libremidi_midi_out_port* port) {
    EnumContext* ec = (EnumContext*)user_ctx;
    AldaContext* ctx = ec->ctx;

    if (ctx->out_port_count >= ALDA_MAX_PORTS) return;

    libremidi_midi_out_port_clone(port, &ctx->out_ports[ctx->out_port_count]);
    ctx->out_port_count++;
}

/* ============================================================================
 * Initialization and Cleanup
 * ============================================================================ */

void alda_midi_init_observer(AldaContext* ctx) {
    if (!ctx) return;

    int ret = 0;

    /* Free existing observer if any */
    if (ctx->midi_observer != NULL) {
        for (int i = 0; i < ctx->out_port_count; i++) {
            if (ctx->out_ports[i]) {
                libremidi_midi_out_port_free(ctx->out_ports[i]);
                ctx->out_ports[i] = NULL;
            }
        }
        ctx->out_port_count = 0;
        libremidi_midi_observer_free(ctx->midi_observer);
        ctx->midi_observer = NULL;
    }

    /* Create observer configuration */
    libremidi_observer_configuration observer_conf;
    ret = libremidi_midi_observer_configuration_init(&observer_conf);
    if (ret != 0) {
        fprintf(stderr, "Failed to init observer config: %d\n", ret);
        return;
    }

    observer_conf.track_hardware = true;
    observer_conf.track_virtual = true;
    observer_conf.track_any = true;

    /* Create API configuration */
    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        fprintf(stderr, "Failed to init API config: %d\n", ret);
        return;
    }

    api_conf.configuration_type = Observer;
    api_conf.api = UNSPECIFIED;

    /* Create observer */
    ret = libremidi_midi_observer_new(&observer_conf, &api_conf, &ctx->midi_observer);
    if (ret != 0) {
        fprintf(stderr, "Failed to create MIDI observer: %d\n", ret);
        return;
    }

    /* Enumerate output ports */
    ctx->out_port_count = 0;
    EnumContext ec = { .ctx = ctx };
    ret = libremidi_midi_observer_enumerate_output_ports(ctx->midi_observer, &ec, on_output_port_found);
    if (ret != 0) {
        fprintf(stderr, "Failed to enumerate ports: %d\n", ret);
    }
}

void alda_midi_cleanup(AldaContext* ctx) {
    if (!ctx) return;

    /* Close output */
    if (ctx->midi_out != NULL) {
        alda_midi_all_notes_off(ctx);
        libremidi_midi_out_free(ctx->midi_out);
        ctx->midi_out = NULL;
    }

    /* Free ports */
    for (int i = 0; i < ctx->out_port_count; i++) {
        if (ctx->out_ports[i]) {
            libremidi_midi_out_port_free(ctx->out_ports[i]);
            ctx->out_ports[i] = NULL;
        }
    }
    ctx->out_port_count = 0;

    /* Free observer */
    if (ctx->midi_observer != NULL) {
        libremidi_midi_observer_free(ctx->midi_observer);
        ctx->midi_observer = NULL;
    }
}

/* ============================================================================
 * Port Management
 * ============================================================================ */

void alda_midi_list_ports(AldaContext* ctx) {
    if (!ctx) return;

    alda_midi_init_observer(ctx);

    printf("MIDI outputs:\n");
    if (ctx->out_port_count == 0) {
        printf("  (none - use virtual port)\n");
    } else {
        for (int i = 0; i < ctx->out_port_count; i++) {
            const char* name = NULL;
            size_t len = 0;
            if (libremidi_midi_out_port_name(ctx->out_ports[i], &name, &len) == 0) {
                printf("  %d: %s\n", i, name);
            }
        }
    }
}

int alda_midi_open_port(AldaContext* ctx, int port_idx) {
    if (!ctx) return -1;

    alda_midi_init_observer(ctx);

    if (port_idx < 0 || port_idx >= ctx->out_port_count) {
        fprintf(stderr, "Invalid port index: %d (have %d ports)\n",
                port_idx, ctx->out_port_count);
        return -1;
    }

    /* Close existing output */
    if (ctx->midi_out != NULL) {
        libremidi_midi_out_free(ctx->midi_out);
        ctx->midi_out = NULL;
    }

    int ret = 0;

    /* Create MIDI configuration */
    libremidi_midi_configuration midi_conf;
    ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        fprintf(stderr, "Failed to init MIDI config\n");
        return -1;
    }

    midi_conf.version = MIDI1;
    midi_conf.out_port = ctx->out_ports[port_idx];

    /* Create API configuration */
    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        fprintf(stderr, "Failed to init API config\n");
        return -1;
    }

    api_conf.configuration_type = Output;
    api_conf.api = UNSPECIFIED;

    /* Open output */
    ret = libremidi_midi_out_new(&midi_conf, &api_conf, &ctx->midi_out);
    if (ret != 0) {
        fprintf(stderr, "Failed to open MIDI output: %d\n", ret);
        return -1;
    }

    const char* name = NULL;
    size_t len = 0;
    libremidi_midi_out_port_name(ctx->out_ports[port_idx], &name, &len);
    if (ctx->verbose_mode) {
        printf("Opened MIDI output: %s\n", name);
    }

    return 0;
}

int alda_midi_open_virtual(AldaContext* ctx, const char* name) {
    if (!ctx || !name) return -1;

    alda_midi_init_observer(ctx);

    /* Close existing output */
    if (ctx->midi_out != NULL) {
        libremidi_midi_out_free(ctx->midi_out);
        ctx->midi_out = NULL;
    }

    int ret = 0;

    /* Create MIDI configuration */
    libremidi_midi_configuration midi_conf;
    ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        fprintf(stderr, "Failed to init MIDI config\n");
        return -1;
    }

    midi_conf.version = MIDI1;
    midi_conf.virtual_port = true;
    midi_conf.port_name = name;

    /* Create API configuration */
    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        fprintf(stderr, "Failed to init API config\n");
        return -1;
    }

    api_conf.configuration_type = Output;
    api_conf.api = UNSPECIFIED;

    /* Create virtual output */
    ret = libremidi_midi_out_new(&midi_conf, &api_conf, &ctx->midi_out);
    if (ret != 0) {
        fprintf(stderr, "Failed to create virtual MIDI output: %d\n", ret);
        return -1;
    }

    if (ctx->verbose_mode) {
        printf("Created virtual MIDI output: %s\n", name);
    }

    return 0;
}

int alda_midi_open_by_name(AldaContext* ctx, const char* name) {
    if (!ctx || !name) return -1;

    alda_midi_init_observer(ctx);

    /* Search for substring match in hardware port names */
    for (int i = 0; i < ctx->out_port_count; i++) {
        const char* port_name = NULL;
        size_t len = 0;
        if (libremidi_midi_out_port_name(ctx->out_ports[i], &port_name, &len) == 0) {
            if (strstr(port_name, name) != NULL) {
                /* Found a match */
                return alda_midi_open_port(ctx, i);
            }
        }
    }

    /* No hardware port matched - create virtual port */
    return alda_midi_open_virtual(ctx, name);
}

int alda_midi_open_auto(AldaContext* ctx, const char* virtual_name) {
    if (!ctx) return -1;

    alda_midi_init_observer(ctx);

    /* If hardware ports available, open the first one */
    if (ctx->out_port_count > 0) {
        return alda_midi_open_port(ctx, 0);
    }

    /* No hardware ports - create virtual port */
    return alda_midi_open_virtual(ctx, virtual_name);
}

void alda_midi_close(AldaContext* ctx) {
    if (!ctx) return;

    if (ctx->midi_out != NULL) {
        alda_midi_all_notes_off(ctx);
        libremidi_midi_out_free(ctx->midi_out);
        ctx->midi_out = NULL;
        if (ctx->verbose_mode) {
            printf("MIDI output closed\n");
        }
    }
}

int alda_midi_is_open(AldaContext* ctx) {
    return ctx && ctx->midi_out != NULL;
}

/* ============================================================================
 * MIDI Message Sending
 * ============================================================================ */

void alda_midi_send_note_on(AldaContext* ctx, int channel, int pitch, int velocity) {
    if (!ctx) return;

    /* Built-in synth takes priority when enabled */
    if (ctx->tsf_enabled && alda_tsf_is_enabled()) {
        alda_tsf_send_note_on(channel, pitch, velocity);
        return;
    }

    /* Otherwise send to libremidi if MIDI output is open */
    if (ctx->midi_out) {
        unsigned char msg[3];
        msg[0] = 0x90 | ((channel - 1) & 0x0F);
        msg[1] = pitch & 0x7F;
        msg[2] = velocity & 0x7F;
        libremidi_midi_out_send_message(ctx->midi_out, msg, 3);
    }
}

void alda_midi_send_note_off(AldaContext* ctx, int channel, int pitch) {
    if (!ctx) return;

    /* Built-in synth takes priority when enabled */
    if (ctx->tsf_enabled && alda_tsf_is_enabled()) {
        alda_tsf_send_note_off(channel, pitch);
        return;
    }

    /* Otherwise send to libremidi if MIDI output is open */
    if (ctx->midi_out) {
        unsigned char msg[3];
        msg[0] = 0x80 | ((channel - 1) & 0x0F);
        msg[1] = pitch & 0x7F;
        msg[2] = 0;
        libremidi_midi_out_send_message(ctx->midi_out, msg, 3);
    }
}

void alda_midi_send_program(AldaContext* ctx, int channel, int program) {
    if (!ctx) return;

    /* Built-in synth takes priority when enabled */
    if (ctx->tsf_enabled && alda_tsf_is_enabled()) {
        alda_tsf_send_program(channel, program);
        return;
    }

    /* Otherwise send to libremidi if MIDI output is open */
    if (ctx->midi_out) {
        unsigned char msg[2];
        msg[0] = 0xC0 | ((channel - 1) & 0x0F);
        msg[1] = program & 0x7F;
        libremidi_midi_out_send_message(ctx->midi_out, msg, 2);
    }
}

void alda_midi_send_cc(AldaContext* ctx, int channel, int cc, int value) {
    if (!ctx) return;

    /* Built-in synth takes priority when enabled */
    if (ctx->tsf_enabled && alda_tsf_is_enabled()) {
        alda_tsf_send_cc(channel, cc, value);
        return;
    }

    /* Otherwise send to libremidi if MIDI output is open */
    if (ctx->midi_out) {
        unsigned char msg[3];
        msg[0] = 0xB0 | ((channel - 1) & 0x0F);
        msg[1] = cc & 0x7F;
        msg[2] = value & 0x7F;
        libremidi_midi_out_send_message(ctx->midi_out, msg, 3);
    }
}

void alda_midi_all_notes_off(AldaContext* ctx) {
    if (!ctx) return;

    /* Built-in synth takes priority when enabled */
    if (ctx->tsf_enabled && alda_tsf_is_enabled()) {
        alda_tsf_all_notes_off();
        return;
    }

    /* Otherwise send All Notes Off (CC 123) on all channels via libremidi */
    if (ctx->midi_out) {
        for (int ch = 0; ch < 16; ch++) {
            unsigned char msg[3];
            msg[0] = 0xB0 | ch;
            msg[1] = 123;  /* All Notes Off */
            msg[2] = 0;
            libremidi_midi_out_send_message(ctx->midi_out, msg, 3);
        }
    }
}

/* ============================================================================
 * Timing
 * ============================================================================ */

void alda_midi_sleep_ms(AldaContext* ctx, int ms) {
    if (ms > 0 && !alda_no_sleep(ctx)) {
        usleep(ms * 1000);
    }
}
