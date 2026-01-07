/**
 * @file interpreter.c
 * @brief Alda AST interpreter - walks AST and generates MIDI events.
 */

#include "alda/interpreter.h"
#include "alda/scheduler.h"
#include "alda/midi_backend.h"
#include "alda/instruments.h"
#include "alda/parser.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

/* Forward declarations for visitor functions */
static int visit_node(AldaContext* ctx, AldaNode* node);
static int visit_root(AldaContext* ctx, AldaNode* node);
static int visit_part_decl(AldaContext* ctx, AldaNode* node);
static int visit_event_seq(AldaContext* ctx, AldaNode* node);
static int visit_note(AldaContext* ctx, AldaNode* node);
static int visit_rest(AldaContext* ctx, AldaNode* node);
static int visit_chord(AldaContext* ctx, AldaNode* node);
static int visit_octave_set(AldaContext* ctx, AldaNode* node);
static int visit_octave_up(AldaContext* ctx, AldaNode* node);
static int visit_octave_down(AldaContext* ctx, AldaNode* node);
static int visit_lisp_list(AldaContext* ctx, AldaNode* node);
static int visit_repeat(AldaContext* ctx, AldaNode* node);
static int visit_bracket_seq(AldaContext* ctx, AldaNode* node);
static int visit_voice_group(AldaContext* ctx, AldaNode* node);
static int visit_voice(AldaContext* ctx, AldaNode* node);

/* ============================================================================
 * Pitch Calculation
 * ============================================================================ */

/* Note letter to semitone offset (C=0, D=2, E=4, F=5, G=7, A=9, B=11) */
static const int NOTE_OFFSETS[] = {
    9,  /* A */
    11, /* B */
    0,  /* C */
    2,  /* D */
    4,  /* E */
    5,  /* F */
    7   /* G */
};

int alda_calculate_pitch(char letter, const char* accidentals, int octave) {
    /* Validate letter */
    int c = tolower((unsigned char)letter);
    if (c < 'a' || c > 'g') {
        return -1;
    }

    /* Get base semitone offset */
    int semitone = NOTE_OFFSETS[c - 'a'];

    /* Apply accidentals */
    if (accidentals) {
        for (const char* p = accidentals; *p; p++) {
            if (*p == '+') {
                semitone++;  /* Sharp */
            } else if (*p == '-') {
                semitone--;  /* Flat */
            }
            /* _ (natural) has no effect in equal temperament */
        }
    }

    /* Calculate MIDI pitch: C4 = 60 */
    /* octave 4, C = 0 -> 60 */
    /* Formula: (octave + 1) * 12 + semitone */
    int pitch = (octave + 1) * 12 + semitone;

    /* Clamp to valid MIDI range */
    if (pitch < 0) pitch = 0;
    if (pitch > 127) pitch = 127;

    return pitch;
}

/* ============================================================================
 * Duration Calculation from AST
 * ============================================================================ */

int alda_ast_duration_to_ticks(AldaContext* ctx, AldaPartState* part, AldaNode* duration) {
    int tempo = alda_effective_tempo(ctx, part);

    /* If no duration node, use part's default */
    if (!duration) {
        return alda_duration_to_ticks(part->default_duration, part->default_dots);
    }

    /* Handle different duration node types */
    switch (duration->type) {
        case ALDA_NODE_DURATION: {
            /* Duration with potentially multiple components (tied) */
            int total = 0;
            AldaNode* comp = duration->data.duration.components;
            while (comp) {
                total += alda_ast_duration_to_ticks(ctx, part, comp);
                comp = comp->next;
            }
            return total;
        }

        case ALDA_NODE_NOTE_LENGTH: {
            /* Standard note length (e.g., 4 for quarter, 8 for eighth) */
            int denom = duration->data.note_length.denominator;
            int dots = duration->data.note_length.dots;
            return alda_duration_to_ticks(denom, dots);
        }

        case ALDA_NODE_NOTE_LENGTH_MS: {
            /* Duration in milliseconds */
            int ms = duration->data.note_length_ms.ms;
            return alda_ms_to_ticks(ms, tempo);
        }

        case ALDA_NODE_NOTE_LENGTH_S: {
            /* Duration in seconds */
            double seconds = duration->data.note_length_s.seconds;
            return alda_seconds_to_ticks(seconds, tempo);
        }

        default:
            /* Unknown duration type - use default */
            return alda_duration_to_ticks(part->default_duration, part->default_dots);
    }
}

/* ============================================================================
 * AST Visitor Functions
 * ============================================================================ */

static int visit_node(AldaContext* ctx, AldaNode* node) {
    if (!node) return 0;

    switch (node->type) {
        case ALDA_NODE_ROOT:
            return visit_root(ctx, node);

        case ALDA_NODE_PART_DECL:
            return visit_part_decl(ctx, node);

        case ALDA_NODE_EVENT_SEQ:
            return visit_event_seq(ctx, node);

        case ALDA_NODE_NOTE:
            return visit_note(ctx, node);

        case ALDA_NODE_REST:
            return visit_rest(ctx, node);

        case ALDA_NODE_CHORD:
            return visit_chord(ctx, node);

        case ALDA_NODE_OCTAVE_SET:
            return visit_octave_set(ctx, node);

        case ALDA_NODE_OCTAVE_UP:
            return visit_octave_up(ctx, node);

        case ALDA_NODE_OCTAVE_DOWN:
            return visit_octave_down(ctx, node);

        case ALDA_NODE_LISP_LIST:
            return visit_lisp_list(ctx, node);

        case ALDA_NODE_REPEAT:
            return visit_repeat(ctx, node);

        case ALDA_NODE_BRACKET_SEQ:
            return visit_bracket_seq(ctx, node);

        case ALDA_NODE_VOICE_GROUP:
            return visit_voice_group(ctx, node);

        case ALDA_NODE_VOICE:
            return visit_voice(ctx, node);

        case ALDA_NODE_BARLINE:
            /* Barlines are visual-only, no action needed */
            return 0;

        /* Deferred features */
        case ALDA_NODE_VAR_DEF:
        case ALDA_NODE_VAR_REF:
        case ALDA_NODE_MARKER:
        case ALDA_NODE_AT_MARKER:
        case ALDA_NODE_CRAM:
        case ALDA_NODE_ON_REPS:
            if (ctx->verbose_mode) {
                fprintf(stderr, "Warning: %s not yet implemented\n",
                        alda_node_type_name(node->type));
            }
            return 0;

        default:
            return 0;
    }
}

static int visit_root(AldaContext* ctx, AldaNode* node) {
    /* Process all children of root */
    AldaNode* child = node->data.root.children;
    while (child) {
        if (visit_node(ctx, child) < 0) {
            return -1;
        }
        child = child->next;
    }
    return 0;
}

static int visit_part_decl(AldaContext* ctx, AldaNode* node) {
    /* Set current parts from declaration */
    char** names = node->data.part_decl.names;
    int count = (int)node->data.part_decl.name_count;

    if (alda_set_current_parts(ctx, names, count) < 0) {
        return -1;
    }

    /* Apply alias if present */
    if (node->data.part_decl.alias && ctx->current_part_count > 0) {
        AldaPartState* part = alda_current_part(ctx);
        if (part) {
            strncpy(part->alias, node->data.part_decl.alias,
                    sizeof(part->alias) - 1);
            part->alias[sizeof(part->alias) - 1] = '\0';
        }
    }

    /* Schedule program changes for all active parts */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        AldaPartState* part = &ctx->parts[idx];
        alda_schedule_program_change(ctx, part, part->current_tick);
    }

    return 0;
}

static int visit_event_seq(AldaContext* ctx, AldaNode* node) {
    /* Process all events in sequence */
    AldaNode* event = node->data.event_seq.events;
    while (event) {
        if (visit_node(ctx, event) < 0) {
            return -1;
        }
        event = event->next;
    }
    return 0;
}

static int visit_note(AldaContext* ctx, AldaNode* node) {
    AldaPartState* part = alda_current_part(ctx);
    if (!part) {
        fprintf(stderr, "Error: No current part for note\n");
        return -1;
    }

    /* Calculate pitch */
    int pitch = alda_calculate_pitch(
        node->data.note.letter,
        node->data.note.accidentals,
        part->octave
    );

    if (pitch < 0) {
        fprintf(stderr, "Error: Invalid note\n");
        return -1;
    }

    /* Calculate duration */
    int duration_ticks = alda_ast_duration_to_ticks(ctx, part, node->data.note.duration);

    /* Update part's default duration if note specified one */
    if (node->data.note.duration &&
        node->data.note.duration->type == ALDA_NODE_NOTE_LENGTH) {
        part->default_duration = node->data.note.duration->data.note_length.denominator;
        part->default_dots = node->data.note.duration->data.note_length.dots;
    }

    /* Get velocity */
    int velocity = alda_effective_velocity(ctx, part);

    /* Schedule note for all active parts */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        AldaPartState* p = &ctx->parts[idx];

        int* tick = (p->current_voice >= 0 && p->in_voice_group)
                    ? &p->voices[p->current_voice].current_tick
                    : &p->current_tick;

        alda_schedule_note(ctx, p, *tick, pitch, velocity, duration_ticks);

        /* Advance tick position */
        *tick += duration_ticks;
    }

    return 0;
}

static int visit_rest(AldaContext* ctx, AldaNode* node) {
    AldaPartState* part = alda_current_part(ctx);
    if (!part) {
        fprintf(stderr, "Error: No current part for rest\n");
        return -1;
    }

    /* Calculate duration */
    int duration_ticks = alda_ast_duration_to_ticks(ctx, part, node->data.rest.duration);

    /* Advance tick position for all active parts (no note scheduled) */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        AldaPartState* p = &ctx->parts[idx];

        int* tick = (p->current_voice >= 0 && p->in_voice_group)
                    ? &p->voices[p->current_voice].current_tick
                    : &p->current_tick;

        *tick += duration_ticks;
    }

    return 0;
}

static int visit_chord(AldaContext* ctx, AldaNode* node) {
    AldaPartState* part = alda_current_part(ctx);
    if (!part) {
        fprintf(stderr, "Error: No current part for chord\n");
        return -1;
    }

    /* Collect all notes in the chord */
    int pitches[16];
    int durations[16];
    int count = 0;
    int max_duration = 0;

    AldaNode* note = node->data.chord.notes;
    while (note && count < 16) {
        if (note->type == ALDA_NODE_NOTE) {
            pitches[count] = alda_calculate_pitch(
                note->data.note.letter,
                note->data.note.accidentals,
                part->octave
            );

            durations[count] = alda_ast_duration_to_ticks(ctx, part, note->data.note.duration);

            if (durations[count] > max_duration) {
                max_duration = durations[count];
            }

            count++;
        }
        note = note->next;
    }

    if (count == 0) {
        return 0;  /* Empty chord */
    }

    /* Get velocity */
    int velocity = alda_effective_velocity(ctx, part);

    /* Schedule all chord notes for all active parts */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        AldaPartState* p = &ctx->parts[idx];

        int* tick = (p->current_voice >= 0 && p->in_voice_group)
                    ? &p->voices[p->current_voice].current_tick
                    : &p->current_tick;

        for (int n = 0; n < count; n++) {
            alda_schedule_note(ctx, p, *tick, pitches[n], velocity, durations[n]);
        }

        /* Advance by max duration (all notes start at same time) */
        *tick += max_duration;
    }

    return 0;
}

static int visit_octave_set(AldaContext* ctx, AldaNode* node) {
    int octave = node->data.octave_set.octave;

    /* Set octave for all active parts */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        ctx->parts[idx].octave = octave;
    }

    return 0;
}

static int visit_octave_up(AldaContext* ctx, AldaNode* node) {
    (void)node;

    /* Increment octave for all active parts */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        if (ctx->parts[idx].octave < 9) {
            ctx->parts[idx].octave++;
        }
    }

    return 0;
}

static int visit_octave_down(AldaContext* ctx, AldaNode* node) {
    (void)node;

    /* Decrement octave for all active parts */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        if (ctx->parts[idx].octave > 0) {
            ctx->parts[idx].octave--;
        }
    }

    return 0;
}

/* Forward declaration */
int alda_eval_attribute(AldaContext* ctx, AldaPartState* part, AldaNode* lisp_list);

static int visit_lisp_list(AldaContext* ctx, AldaNode* node) {
    /* Evaluate attribute for all active parts */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        AldaPartState* part = &ctx->parts[idx];
        if (alda_eval_attribute(ctx, part, node) < 0) {
            return -1;
        }
    }
    return 0;
}

static int visit_repeat(AldaContext* ctx, AldaNode* node) {
    int count = node->data.repeat.count;
    AldaNode* event = node->data.repeat.event;

    for (int i = 0; i < count; i++) {
        if (visit_node(ctx, event) < 0) {
            return -1;
        }
    }

    return 0;
}

static int visit_bracket_seq(AldaContext* ctx, AldaNode* node) {
    /* Bracket sequences are just event sequences */
    AldaNode* event = node->data.bracket_seq.events;
    while (event) {
        if (visit_node(ctx, event) < 0) {
            return -1;
        }
        event = event->next;
    }
    return 0;
}

static int visit_voice_group(AldaContext* ctx, AldaNode* node) {
    /* Process all voices in the group */
    AldaNode* voice = node->data.voice_group.voices;

    /* Mark all active parts as being in a voice group */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        AldaPartState* part = &ctx->parts[idx];
        part->in_voice_group = 1;
        part->voice_count = 0;

        /* Initialize all voice start ticks to current part tick */
        for (int v = 0; v < ALDA_MAX_VOICES; v++) {
            part->voices[v].start_tick = part->current_tick;
            part->voices[v].current_tick = part->current_tick;
        }
    }

    /* Process each voice */
    while (voice) {
        if (visit_node(ctx, voice) < 0) {
            return -1;
        }
        voice = voice->next;
    }

    /* Merge voices: set part tick to max of all voice ticks */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        AldaPartState* part = &ctx->parts[idx];

        int max_tick = part->current_tick;
        for (int v = 0; v < part->voice_count; v++) {
            if (part->voices[v].current_tick > max_tick) {
                max_tick = part->voices[v].current_tick;
            }
        }
        part->current_tick = max_tick;

        /* Exit voice group mode */
        part->in_voice_group = 0;
        part->current_voice = -1;
    }

    return 0;
}

static int visit_voice(AldaContext* ctx, AldaNode* node) {
    int voice_num = node->data.voice.number;

    /* Set up voice for all active parts */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        AldaPartState* part = &ctx->parts[idx];

        if (voice_num == 0) {
            /* V0: means exit voice mode and merge */
            part->current_voice = -1;
        } else {
            /* Find or create voice slot */
            int voice_idx = -1;
            for (int v = 0; v < part->voice_count; v++) {
                if (part->voices[v].number == voice_num) {
                    voice_idx = v;
                    break;
                }
            }

            if (voice_idx < 0 && part->voice_count < ALDA_MAX_VOICES) {
                /* Create new voice */
                voice_idx = part->voice_count++;
                part->voices[voice_idx].number = voice_num;
                part->voices[voice_idx].start_tick = part->current_tick;
                part->voices[voice_idx].current_tick = part->current_tick;
            }

            part->current_voice = voice_idx;
        }
    }

    /* Process voice events */
    AldaNode* event = node->data.voice.events;
    while (event) {
        if (visit_node(ctx, event) < 0) {
            return -1;
        }
        event = event->next;
    }

    return 0;
}

/* ============================================================================
 * Main Interpreter Functions
 * ============================================================================ */

int alda_interpret_ast(AldaContext* ctx, AldaNode* root) {
    if (!ctx || !root) return -1;

    /* Clear any previous events */
    alda_events_clear(ctx);

    /* Reset part positions */
    for (int i = 0; i < ctx->part_count; i++) {
        ctx->parts[i].current_tick = 0;
        ctx->parts[i].voice_count = 0;
        ctx->parts[i].current_voice = -1;
        ctx->parts[i].in_voice_group = 0;
    }

    /* Visit the AST */
    return visit_node(ctx, root);
}

int alda_interpret_string(AldaContext* ctx, const char* source, const char* filename) {
    if (!ctx || !source) return -1;

    /* Parse the source */
    char* error = NULL;
    AldaNode* ast = alda_parse(source, filename, &error);

    if (error) {
        fprintf(stderr, "%s\n", error);
        free(error);
        return -1;
    }

    if (!ast) {
        fprintf(stderr, "Error: Failed to parse source\n");
        return -1;
    }

    /* Interpret the AST */
    int result = alda_interpret_ast(ctx, ast);

    /* Free the AST */
    alda_ast_free(ast);

    return result;
}

int alda_interpret_file(AldaContext* ctx, const char* filename) {
    if (!ctx || !filename) return -1;

    /* Read file contents */
    FILE* f = fopen(filename, "r");
    if (!f) {
        fprintf(stderr, "Error: Cannot open file '%s'\n", filename);
        return -1;
    }

    /* Get file size */
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    if (size < 0) {
        fclose(f);
        fprintf(stderr, "Error: Cannot determine file size for '%s'\n", filename);
        return -1;
    }
    fseek(f, 0, SEEK_SET);

    /* Allocate buffer */
    char* source = malloc((size_t)size + 1);
    if (!source) {
        fclose(f);
        fprintf(stderr, "Error: Out of memory\n");
        return -1;
    }

    /* Read file */
    size_t bytes_requested = (size_t)size;
    size_t bytes_read = fread(source, 1, bytes_requested, f);
    /* Defensive bounds check: fread returns at most bytes_requested */
    size_t null_pos = (bytes_read < bytes_requested) ? bytes_read : bytes_requested;
    source[null_pos] = '\0';
    fclose(f);

    /* Interpret */
    ctx->current_file = filename;
    int result = alda_interpret_string(ctx, source, filename);
    ctx->current_file = NULL;

    free(source);
    return result;
}
