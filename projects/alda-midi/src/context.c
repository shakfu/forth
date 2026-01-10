/**
 * @file context.c
 * @brief Alda interpreter context implementation.
 */

#include "alda/context.h"
#include "alda/instruments.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ============================================================================
 * Context Management
 * ============================================================================ */

void alda_context_init(AldaContext* ctx) {
    if (!ctx) return;

    /* MIDI output handles */
    ctx->midi_observer = NULL;
    ctx->midi_out = NULL;
    for (int i = 0; i < ALDA_MAX_PORTS; i++) {
        ctx->out_ports[i] = NULL;
    }
    ctx->out_port_count = 0;

    /* Parts management */
    ctx->part_count = 0;
    ctx->current_part_count = 0;
    ctx->next_channel = 1;  /* Start assigning from channel 1 */

    for (int i = 0; i < ALDA_MAX_PARTS; i++) {
        ctx->current_part_indices[i] = -1;
        memset(&ctx->parts[i], 0, sizeof(AldaPartState));
    }

    /* Global defaults */
    ctx->global_tempo = ALDA_DEFAULT_TEMPO;
    ctx->global_volume = ALDA_DEFAULT_VOLUME;
    ctx->global_quant = ALDA_DEFAULT_QUANT;
    ctx->global_pan = ALDA_DEFAULT_PAN;

    /* Markers (deferred) */
    ctx->marker_count = 0;

    /* Variables (deferred) */
    ctx->variable_count = 0;

    /* Event queue */
    ctx->events = NULL;
    ctx->event_count = 0;
    ctx->event_capacity = 0;

    /* Runtime flags */
    ctx->no_sleep_mode = 0;
    ctx->verbose_mode = 0;

    /* Repeat context */
    ctx->current_repetition = 0;

    /* File context */
    ctx->current_file = NULL;
    ctx->current_line = 0;
}

void alda_context_cleanup(AldaContext* ctx) {
    if (!ctx) return;

    /* Free event queue */
    if (ctx->events) {
        free(ctx->events);
        ctx->events = NULL;
    }
    ctx->event_count = 0;
    ctx->event_capacity = 0;

    /* Note: MIDI cleanup is handled separately by midi_backend */
}

void alda_context_reset(AldaContext* ctx) {
    if (!ctx) return;

    /* Reset parts but keep them defined */
    for (int i = 0; i < ctx->part_count; i++) {
        AldaPartState* part = &ctx->parts[i];
        part->current_tick = 0;
        part->voice_count = 0;
        part->current_voice = -1;
        part->in_voice_group = 0;
    }

    /* Clear current part selection */
    ctx->current_part_count = 0;
    for (int i = 0; i < ALDA_MAX_PARTS; i++) {
        ctx->current_part_indices[i] = -1;
    }

    /* Clear events */
    ctx->event_count = 0;

    /* Clear markers and variables */
    ctx->marker_count = 0;
    ctx->variable_count = 0;
}

/* ============================================================================
 * Part Management
 * ============================================================================ */

void alda_part_init(AldaPartState* part, const char* name, int channel, int program) {
    if (!part) return;

    memset(part, 0, sizeof(AldaPartState));

    if (name) {
        strncpy(part->name, name, sizeof(part->name) - 1);
        part->name[sizeof(part->name) - 1] = '\0';
    }

    part->program = program;
    part->channel = channel;

    /* Musical state defaults */
    part->octave = ALDA_DEFAULT_OCTAVE;
    part->volume = -1;  /* -1 = use global */
    part->velocity_override = 69;  /* Default mf velocity (matches aldakit) */
    part->tempo = 0;   /* 0 = use global */
    part->quant = 0;   /* 0 = use global */
    part->pan = ALDA_DEFAULT_PAN;

    /* Duration defaults */
    part->default_duration = ALDA_DEFAULT_DURATION;
    part->default_dots = 0;

    /* Timing */
    part->current_tick = 0;

    /* Voices */
    part->voice_count = 0;
    part->current_voice = -1;  /* Merged mode */
    part->in_voice_group = 0;

    /* Key signature (all natural) */
    for (int i = 0; i < 7; i++) {
        part->key_signature[i] = 0;
    }

    /* Transposition (none) */
    part->transpose = 0;
}

AldaPartState* alda_find_part(AldaContext* ctx, const char* name) {
    if (!ctx || !name) return NULL;

    for (int i = 0; i < ctx->part_count; i++) {
        /* Check name */
        if (strcmp(ctx->parts[i].name, name) == 0) {
            return &ctx->parts[i];
        }
        /* Check alias */
        if (ctx->parts[i].alias[0] != '\0' &&
            strcmp(ctx->parts[i].alias, name) == 0) {
            return &ctx->parts[i];
        }
    }

    return NULL;
}

AldaPartState* alda_get_or_create_part(AldaContext* ctx, const char* name) {
    if (!ctx || !name) return NULL;

    /* First, try to find existing part */
    AldaPartState* existing = alda_find_part(ctx, name);
    if (existing) {
        return existing;
    }

    /* Check capacity */
    if (ctx->part_count >= ALDA_MAX_PARTS) {
        fprintf(stderr, "Error: Maximum number of parts (%d) exceeded\n",
                ALDA_MAX_PARTS);
        return NULL;
    }

    /* Look up instrument program number */
    int program = alda_instrument_program(name);
    if (program < 0) {
        /* Unknown instrument - default to piano */
        if (ctx->verbose_mode) {
            fprintf(stderr, "Warning: Unknown instrument '%s', using piano\n", name);
        }
        program = 0;
    }

    /* Allocate channel (skip 10 for non-percussion) */
    int channel = ctx->next_channel;

    /* Check if this is a percussion instrument */
    int is_percussion = alda_instrument_is_percussion(name);

    if (is_percussion) {
        channel = 10;  /* Drums always on channel 10 */
    } else {
        /* Skip channel 10 for non-percussion */
        if (channel == 10) {
            channel = 11;
        }
        ctx->next_channel = channel + 1;
        if (ctx->next_channel > 16) {
            ctx->next_channel = 1;  /* Wrap around */
        }
        if (ctx->next_channel == 10) {
            ctx->next_channel = 11;  /* Skip 10 again */
        }
    }

    /* Create new part */
    AldaPartState* part = &ctx->parts[ctx->part_count];
    alda_part_init(part, name, channel, program);
    ctx->part_count++;

    if (ctx->verbose_mode) {
        printf("Created part: %s (program=%d, channel=%d)\n",
               name, program, channel);
    }

    return part;
}

/* Helper to create a new part (always creates, never reuses) */
static AldaPartState* alda_create_new_part(AldaContext* ctx, const char* name) {
    if (!ctx || !name) return NULL;

    /* Check capacity */
    if (ctx->part_count >= ALDA_MAX_PARTS) {
        fprintf(stderr, "Error: Maximum number of parts (%d) exceeded\n",
                ALDA_MAX_PARTS);
        return NULL;
    }

    /* Look up instrument program number */
    int program = alda_instrument_program(name);
    if (program < 0) {
        if (ctx->verbose_mode) {
            fprintf(stderr, "Warning: Unknown instrument '%s', using piano\n", name);
        }
        program = 0;
    }

    /* Allocate channel (skip 10 for non-percussion) */
    int channel = ctx->next_channel;
    int is_percussion = alda_instrument_is_percussion(name);

    if (is_percussion) {
        channel = 10;
    } else {
        if (channel == 10) {
            channel = 11;
        }
        ctx->next_channel = channel + 1;
        if (ctx->next_channel > 16) {
            ctx->next_channel = 1;
        }
        if (ctx->next_channel == 10) {
            ctx->next_channel = 11;
        }
    }

    /* Create new part */
    AldaPartState* part = &ctx->parts[ctx->part_count];
    alda_part_init(part, name, channel, program);
    ctx->part_count++;

    if (ctx->verbose_mode) {
        printf("Created part: %s (program=%d, channel=%d)\n",
               name, program, channel);
    }

    return part;
}

int alda_set_current_parts(AldaContext* ctx, char** names, int count) {
    if (!ctx) return -1;

    /* Clear current selection */
    ctx->current_part_count = 0;

    if (!names || count <= 0) {
        return 0;
    }

    /* For multi-instrument groups (count > 1), always create new parts.
     * For single instruments, reuse existing or create new. */
    int is_group = (count > 1);

    for (int i = 0; i < count && i < ALDA_MAX_PARTS; i++) {
        AldaPartState* part;

        if (is_group) {
            /* Groups always create new parts for each instrument */
            part = alda_create_new_part(ctx, names[i]);
        } else {
            /* Single instrument: reuse existing or create new */
            part = alda_get_or_create_part(ctx, names[i]);
        }

        if (!part) {
            return -1;
        }

        /* Find part index */
        int idx = (int)(part - ctx->parts);
        ctx->current_part_indices[ctx->current_part_count] = idx;
        ctx->current_part_count++;
    }

    return 0;
}

AldaPartState* alda_current_part(AldaContext* ctx) {
    if (!ctx || ctx->current_part_count == 0) {
        return NULL;
    }
    int idx = ctx->current_part_indices[0];
    if (idx < 0 || idx >= ctx->part_count) {
        return NULL;
    }
    return &ctx->parts[idx];
}

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

int alda_effective_tempo(AldaContext* ctx, AldaPartState* part) {
    if (!ctx) return ALDA_DEFAULT_TEMPO;
    if (part && part->tempo > 0) {
        return part->tempo;
    }
    return ctx->global_tempo;
}

int alda_effective_velocity(AldaContext* ctx, AldaPartState* part) {
    /* Check for direct velocity override (set by dynamics) */
    if (part && part->velocity_override >= 0) {
        return part->velocity_override;
    }

    int volume = ALDA_DEFAULT_VOLUME;

    if (part && part->volume >= 0) {
        volume = part->volume;
    } else if (ctx) {
        volume = ctx->global_volume;
    }

    /* Map 0-100 to 0-127 (truncation matches aldakit) */
    int velocity = (volume * 127) / 100;
    if (velocity < 0) velocity = 0;
    if (velocity > 127) velocity = 127;
    return velocity;
}

int alda_effective_quant(AldaContext* ctx, AldaPartState* part) {
    if (part && part->quant > 0) {
        return part->quant;
    }
    if (ctx) {
        return ctx->global_quant;
    }
    return ALDA_DEFAULT_QUANT;
}

int alda_no_sleep(AldaContext* ctx) {
    return ctx ? ctx->no_sleep_mode : 0;
}

void alda_set_no_sleep(AldaContext* ctx, int value) {
    if (ctx) {
        ctx->no_sleep_mode = value ? 1 : 0;
    }
}
