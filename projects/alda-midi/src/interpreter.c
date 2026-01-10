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
static int visit_var_def(AldaContext* ctx, AldaNode* node);
static int visit_var_ref(AldaContext* ctx, AldaNode* node);
static int visit_cram(AldaContext* ctx, AldaNode* node);
static int visit_marker(AldaContext* ctx, AldaNode* node);
static int visit_at_marker(AldaContext* ctx, AldaNode* node);
static int visit_on_reps(AldaContext* ctx, AldaNode* node);

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

int alda_calculate_pitch(char letter, const char* accidentals, int octave, const int* key_sig) {
    /* Validate letter */
    int c = tolower((unsigned char)letter);
    if (c < 'a' || c > 'g') {
        return -1;
    }

    /* Get base semitone offset */
    int semitone = NOTE_OFFSETS[c - 'a'];

    /* Check for explicit accidentals or natural sign */
    int has_explicit_accidental = 0;
    int has_natural = 0;
    int accidental_offset = 0;

    if (accidentals) {
        for (const char* p = accidentals; *p; p++) {
            if (*p == '+') {
                accidental_offset++;
                has_explicit_accidental = 1;
            } else if (*p == '-') {
                accidental_offset--;
                has_explicit_accidental = 1;
            } else if (*p == '_') {
                has_natural = 1;  /* Natural sign overrides key signature */
            }
        }
    }

    /* Apply accidentals: explicit > natural > key signature */
    if (has_explicit_accidental) {
        semitone += accidental_offset;
    } else if (!has_natural && key_sig) {
        /* Apply key signature (C=0, D=1, E=2, F=3, G=4, A=5, B=6) */
        /* Map letter to key_sig index */
        static const int LETTER_TO_INDEX[] = {
            5,  /* a -> index 5 (A) */
            6,  /* b -> index 6 (B) */
            0,  /* c -> index 0 (C) */
            1,  /* d -> index 1 (D) */
            2,  /* e -> index 2 (E) */
            3,  /* f -> index 3 (F) */
            4   /* g -> index 4 (G) */
        };
        int idx = LETTER_TO_INDEX[c - 'a'];
        semitone += key_sig[idx];
    }
    /* If has_natural and no explicit accidental, keep semitone as-is (natural) */

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

        case ALDA_NODE_VAR_DEF:
            return visit_var_def(ctx, node);

        case ALDA_NODE_VAR_REF:
            return visit_var_ref(ctx, node);

        case ALDA_NODE_CRAM:
            return visit_cram(ctx, node);

        case ALDA_NODE_MARKER:
            return visit_marker(ctx, node);

        case ALDA_NODE_AT_MARKER:
            return visit_at_marker(ctx, node);

        case ALDA_NODE_ON_REPS:
            return visit_on_reps(ctx, node);

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

    /* Calculate pitch (with key signature and transposition) */
    int pitch = alda_calculate_pitch(
        node->data.note.letter,
        node->data.note.accidentals,
        part->octave,
        part->key_signature
    );

    if (pitch < 0) {
        fprintf(stderr, "Error: Invalid note\n");
        return -1;
    }

    /* Apply transposition */
    pitch += part->transpose;
    if (pitch < 0) pitch = 0;
    if (pitch > 127) pitch = 127;

    /* Calculate duration */
    int duration_ticks = alda_ast_duration_to_ticks(ctx, part, node->data.note.duration);

    /* Update part's default duration if note specified one.
     * Note: duration node is ALDA_NODE_DURATION containing component nodes. */
    if (node->data.note.duration &&
        node->data.note.duration->type == ALDA_NODE_DURATION) {
        AldaNode* first_comp = node->data.note.duration->data.duration.components;
        if (first_comp && first_comp->type == ALDA_NODE_NOTE_LENGTH) {
            part->default_duration = first_comp->data.note_length.denominator;
            part->default_dots = first_comp->data.note_length.dots;
        }
    }

    /* Get velocity */
    int velocity = alda_effective_velocity(ctx, part);

    /* Check if this note is slurred (should skip quantization) */
    int slurred = node->data.note.slurred;

    /* Schedule note for all active parts */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        AldaPartState* p = &ctx->parts[idx];

        int* tick = (p->current_voice >= 0 && p->in_voice_group)
                    ? &p->voices[p->current_voice].current_tick
                    : &p->current_tick;

        alda_schedule_note_slurred(ctx, p, *tick, pitch, velocity, duration_ticks, slurred);

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

    /* First pass: check if first note has an explicit duration, and update default.
     * Note: duration node is ALDA_NODE_DURATION containing component nodes. */
    AldaNode* first_note = node->data.chord.notes;
    if (first_note && first_note->type == ALDA_NODE_NOTE &&
        first_note->data.note.duration &&
        first_note->data.note.duration->type == ALDA_NODE_DURATION) {
        /* Get the first component of the duration */
        AldaNode* first_comp = first_note->data.note.duration->data.duration.components;
        if (first_comp && first_comp->type == ALDA_NODE_NOTE_LENGTH) {
            /* Update default duration from first note in chord */
            part->default_duration = first_comp->data.note_length.denominator;
            part->default_dots = first_comp->data.note_length.dots;
        }
    }

    /* Collect all notes in the chord, handling octave changes */
    int pitches[16];
    int durations[16];
    int count = 0;
    int max_duration = 0;

    AldaNode* note = node->data.chord.notes;
    while (note && count < 16) {
        if (note->type == ALDA_NODE_OCTAVE_UP) {
            /* Apply octave change for subsequent notes in chord */
            if (part->octave < 9) {
                part->octave++;
            }
        } else if (note->type == ALDA_NODE_OCTAVE_DOWN) {
            if (part->octave > 0) {
                part->octave--;
            }
        } else if (note->type == ALDA_NODE_NOTE) {
            int p = alda_calculate_pitch(
                note->data.note.letter,
                note->data.note.accidentals,
                part->octave,
                part->key_signature
            );

            /* Apply transposition */
            p += part->transpose;
            if (p < 0) p = 0;
            if (p > 127) p = 127;

            pitches[count] = p;

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
            /* All chord notes use the same duration (max of all notes) */
            alda_schedule_note(ctx, p, *tick, pitches[n], velocity, max_duration);
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

    /* Save outer repetition context (for nested repeats) */
    int saved_rep = ctx->current_repetition;

    for (int i = 0; i < count; i++) {
        /* Set current repetition (1-indexed) */
        ctx->current_repetition = i + 1;

        if (visit_node(ctx, event) < 0) {
            ctx->current_repetition = saved_rep;
            return -1;
        }
    }

    /* Restore outer repetition context */
    ctx->current_repetition = saved_rep;

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
 * Variable Handling
 * ============================================================================ */

static AldaVariable* find_variable(AldaContext* ctx, const char* name) {
    for (int i = 0; i < ctx->variable_count; i++) {
        if (strcmp(ctx->variables[i].name, name) == 0) {
            return &ctx->variables[i];
        }
    }
    return NULL;
}

static int store_variable(AldaContext* ctx, const char* name, AldaNode* events) {
    /* Check if variable already exists */
    AldaVariable* existing = find_variable(ctx, name);
    if (existing) {
        /* Update existing variable */
        existing->events = events;
        return 0;
    }

    /* Add new variable */
    if (ctx->variable_count >= ALDA_MAX_VARIABLES) {
        fprintf(stderr, "Error: Too many variables (max %d)\n", ALDA_MAX_VARIABLES);
        return -1;
    }

    AldaVariable* var = &ctx->variables[ctx->variable_count++];
    strncpy(var->name, name, sizeof(var->name) - 1);
    var->name[sizeof(var->name) - 1] = '\0';
    var->events = events;

    return 0;
}

static int visit_var_def(AldaContext* ctx, AldaNode* node) {
    const char* name = node->data.var_def.name;
    AldaNode* events = node->data.var_def.events;

    if (ctx->verbose_mode) {
        fprintf(stderr, "Defining variable: %s\n", name);
    }

    /* Store the variable (we store the AST node, not a copy) */
    return store_variable(ctx, name, events);
}

static int visit_var_ref(AldaContext* ctx, AldaNode* node) {
    const char* name = node->data.var_ref.name;

    AldaVariable* var = find_variable(ctx, name);
    if (!var) {
        fprintf(stderr, "Error: Undefined variable '%s'\n", name);
        return -1;
    }

    if (ctx->verbose_mode) {
        fprintf(stderr, "Expanding variable: %s\n", name);
    }

    /* Visit all stored events (linked list) */
    AldaNode* event = var->events;
    while (event) {
        if (visit_node(ctx, event) < 0) {
            return -1;
        }
        event = event->next;
    }

    return 0;
}

/* ============================================================================
 * Marker Handling
 * ============================================================================ */

static AldaMarker* find_marker(AldaContext* ctx, const char* name) {
    for (int i = 0; i < ctx->marker_count; i++) {
        if (strcmp(ctx->markers[i].name, name) == 0) {
            return &ctx->markers[i];
        }
    }
    return NULL;
}

static int store_marker(AldaContext* ctx, const char* name, int tick) {
    /* Check if marker already exists */
    AldaMarker* existing = find_marker(ctx, name);
    if (existing) {
        /* Update existing marker */
        existing->tick = tick;
        return 0;
    }

    /* Add new marker */
    if (ctx->marker_count >= ALDA_MAX_MARKERS) {
        fprintf(stderr, "Error: Too many markers (max %d)\n", ALDA_MAX_MARKERS);
        return -1;
    }

    AldaMarker* marker = &ctx->markers[ctx->marker_count++];
    strncpy(marker->name, name, sizeof(marker->name) - 1);
    marker->name[sizeof(marker->name) - 1] = '\0';
    marker->tick = tick;

    return 0;
}

static int visit_marker(AldaContext* ctx, AldaNode* node) {
    const char* name = node->data.marker.name;

    /* Get current tick from the first active part */
    AldaPartState* part = alda_current_part(ctx);
    if (!part) {
        fprintf(stderr, "Error: No current part for marker\n");
        return -1;
    }

    int tick = part->current_tick;

    if (ctx->verbose_mode) {
        fprintf(stderr, "Setting marker '%s' at tick %d\n", name, tick);
    }

    return store_marker(ctx, name, tick);
}

static int visit_at_marker(AldaContext* ctx, AldaNode* node) {
    const char* name = node->data.at_marker.name;

    AldaMarker* marker = find_marker(ctx, name);
    if (!marker) {
        fprintf(stderr, "Error: Undefined marker '%s'\n", name);
        return -1;
    }

    if (ctx->verbose_mode) {
        fprintf(stderr, "Jumping to marker '%s' at tick %d\n", name, marker->tick);
    }

    /* Set tick position for all active parts */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        AldaPartState* part = &ctx->parts[idx];
        part->current_tick = marker->tick;
    }

    return 0;
}

/* ============================================================================
 * On-Repetitions Handling
 * ============================================================================ */

static int visit_on_reps(AldaContext* ctx, AldaNode* node) {
    /* Check if current repetition is in the allowed list */
    int current_rep = ctx->current_repetition;

    /* If not in a repeat context, always play */
    if (current_rep == 0) {
        return visit_node(ctx, node->data.on_reps.event);
    }

    /* Check if current repetition is in the list */
    int* reps = node->data.on_reps.reps;
    size_t rep_count = node->data.on_reps.rep_count;

    for (size_t i = 0; i < rep_count; i++) {
        if (reps[i] == current_rep) {
            /* Current repetition matches - play the event */
            return visit_node(ctx, node->data.on_reps.event);
        }
    }

    /* Current repetition not in list - skip the event */
    return 0;
}

/* ============================================================================
 * Cram Expression Handling
 * ============================================================================ */

/* Calculate the "weight" of a duration node (relative to quarter note = 1.0) */
static double duration_weight(AldaContext* ctx, AldaPartState* part, AldaNode* dur) {
    if (!dur) {
        /* Default: quarter note = 1.0 */
        return 1.0;
    }

    /* Handle DURATION container nodes by extracting the first component */
    if (dur->type == ALDA_NODE_DURATION) {
        AldaNode* first_comp = dur->data.duration.components;
        if (first_comp) {
            return duration_weight(ctx, part, first_comp);
        }
        return 1.0;
    }

    if (dur->type == ALDA_NODE_NOTE_LENGTH) {
        int denom = dur->data.note_length.denominator;
        int dots = dur->data.note_length.dots;

        /* Base weight: 4/denom (quarter=1, half=2, whole=4, eighth=0.5) */
        double weight = 4.0 / (double)denom;

        /* Apply dots */
        double dot_add = weight / 2.0;
        for (int i = 0; i < dots; i++) {
            weight += dot_add;
            dot_add /= 2.0;
        }

        return weight;
    }

    /* For other duration types, use default */
    return 1.0;
}

/* Calculate cram note duration with scaling */
static int calculate_cram_duration(AldaContext* ctx, AldaPartState* part,
                                   AldaNode* note_dur, double scale_factor) {
    /* Get the weight of this note's duration */
    double weight = duration_weight(ctx, part, note_dur);

    /* Apply scale factor to get actual ticks */
    int ticks = (int)(weight * scale_factor + 0.5);

    return ticks > 0 ? ticks : 1;
}

/* Forward declaration for nested cram handling */
static int process_cram_with_duration(AldaContext* ctx, AldaNode* node, int cram_duration_ticks);

static int visit_cram(AldaContext* ctx, AldaNode* node) {
    AldaPartState* part = alda_current_part(ctx);
    if (!part) {
        fprintf(stderr, "Error: No current part for cram\n");
        return -1;
    }

    /* Get the cram's total duration in ticks */
    int cram_duration_ticks = alda_ast_duration_to_ticks(ctx, part, node->data.cram.duration);

    return process_cram_with_duration(ctx, node, cram_duration_ticks);
}

/* Process a cram expression with a specified duration (used for nested crams) */
static int process_cram_with_duration(AldaContext* ctx, AldaNode* node, int cram_duration_ticks) {
    AldaPartState* part = alda_current_part(ctx);
    if (!part) {
        fprintf(stderr, "Error: No current part for cram\n");
        return -1;
    }

    /* Calculate total weight of all children */
    double total_weight = 0.0;
    AldaNode* child = node->data.cram.events;
    while (child) {
        if (child->type == ALDA_NODE_NOTE) {
            total_weight += duration_weight(ctx, part, child->data.note.duration);
        } else if (child->type == ALDA_NODE_REST) {
            total_weight += duration_weight(ctx, part, child->data.rest.duration);
        } else if (child->type == ALDA_NODE_CHORD) {
            /* Chord uses duration of first note or default */
            AldaNode* first = child->data.chord.notes;
            if (first && first->type == ALDA_NODE_NOTE) {
                total_weight += duration_weight(ctx, part, first->data.note.duration);
            } else {
                total_weight += 1.0;
            }
        } else if (child->type == ALDA_NODE_CRAM) {
            /* Nested cram: its weight is its duration */
            total_weight += duration_weight(ctx, part, child->data.cram.duration);
        } else {
            /* Other events (octave changes, etc.) don't have duration weight */
        }
        child = child->next;
    }

    if (total_weight <= 0.0) {
        total_weight = 1.0;  /* Avoid division by zero */
    }

    /* Calculate scale factor: ticks per unit of weight */
    double scale_factor = (double)cram_duration_ticks / total_weight;

    if (ctx->verbose_mode) {
        fprintf(stderr, "Cram: total_weight=%.2f, duration=%d ticks, scale=%.2f\n",
                total_weight, cram_duration_ticks, scale_factor);
    }

    /* Process each child with scaled duration */
    child = node->data.cram.events;
    while (child) {
        if (child->type == ALDA_NODE_NOTE) {
            /* Calculate scaled duration for this note */
            int note_ticks = calculate_cram_duration(ctx, part, child->data.note.duration, scale_factor);

            /* Calculate pitch (with key signature and transposition) */
            int pitch = alda_calculate_pitch(
                child->data.note.letter,
                child->data.note.accidentals,
                part->octave,
                part->key_signature
            );

            /* Apply transposition */
            if (pitch >= 0) {
                pitch += part->transpose;
                if (pitch < 0) pitch = 0;
                if (pitch > 127) pitch = 127;
            }

            if (pitch >= 0) {
                int velocity = alda_effective_velocity(ctx, part);

                /* Schedule note for all active parts */
                for (int i = 0; i < ctx->current_part_count; i++) {
                    int idx = ctx->current_part_indices[i];
                    AldaPartState* p = &ctx->parts[idx];

                    int* tick = (p->current_voice >= 0 && p->in_voice_group)
                                ? &p->voices[p->current_voice].current_tick
                                : &p->current_tick;

                    alda_schedule_note(ctx, p, *tick, pitch, velocity, note_ticks);
                    *tick += note_ticks;
                }
            }
        } else if (child->type == ALDA_NODE_REST) {
            int rest_ticks = calculate_cram_duration(ctx, part, child->data.rest.duration, scale_factor);

            /* Advance tick for all active parts */
            for (int i = 0; i < ctx->current_part_count; i++) {
                int idx = ctx->current_part_indices[i];
                AldaPartState* p = &ctx->parts[idx];

                int* tick = (p->current_voice >= 0 && p->in_voice_group)
                            ? &p->voices[p->current_voice].current_tick
                            : &p->current_tick;

                *tick += rest_ticks;
            }
        } else if (child->type == ALDA_NODE_CRAM) {
            /* Nested cram: calculate its allocated duration from parent's scale factor */
            int nested_ticks = calculate_cram_duration(ctx, part, child->data.cram.duration, scale_factor);

            /* Process nested cram with the allocated duration (not its own) */
            if (process_cram_with_duration(ctx, child, nested_ticks) < 0) {
                return -1;
            }
        } else if (child->type == ALDA_NODE_CHORD) {
            /* Handle chord within cram */
            AldaNode* first = child->data.chord.notes;
            int chord_ticks = 1;
            if (first && first->type == ALDA_NODE_NOTE) {
                chord_ticks = calculate_cram_duration(ctx, part, first->data.note.duration, scale_factor);
            } else {
                chord_ticks = calculate_cram_duration(ctx, part, NULL, scale_factor);
            }

            int velocity = alda_effective_velocity(ctx, part);

            /* Collect pitches (with key signature and transposition) */
            int pitches[16];
            int count = 0;
            AldaNode* note = child->data.chord.notes;
            while (note && count < 16) {
                if (note->type == ALDA_NODE_NOTE) {
                    int p = alda_calculate_pitch(
                        note->data.note.letter,
                        note->data.note.accidentals,
                        part->octave,
                        part->key_signature
                    );
                    if (p >= 0) {
                        /* Apply transposition */
                        p += part->transpose;
                        if (p < 0) p = 0;
                        if (p > 127) p = 127;
                        pitches[count] = p;
                        count++;
                    }
                }
                note = note->next;
            }

            /* Schedule chord for all active parts */
            for (int i = 0; i < ctx->current_part_count; i++) {
                int idx = ctx->current_part_indices[i];
                AldaPartState* p = &ctx->parts[idx];

                int* tick = (p->current_voice >= 0 && p->in_voice_group)
                            ? &p->voices[p->current_voice].current_tick
                            : &p->current_tick;

                for (int j = 0; j < count; j++) {
                    alda_schedule_note(ctx, p, *tick, pitches[j], velocity, chord_ticks);
                }
                *tick += chord_ticks;
            }
        } else {
            /* Other node types (octave changes, etc.) - visit normally */
            if (visit_node(ctx, child) < 0) {
                return -1;
            }
        }
        child = child->next;
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
