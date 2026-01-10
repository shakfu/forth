/**
 * @file context.h
 * @brief Alda interpreter context and state management.
 */

#ifndef ALDA_CONTEXT_H
#define ALDA_CONTEXT_H

#include <libremidi/libremidi-c.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define ALDA_MAX_PARTS          64
#define ALDA_MAX_VOICES         16
#define ALDA_MAX_PORTS          32
#define ALDA_MAX_EVENTS         16384
#define ALDA_MAX_MARKERS        256
#define ALDA_MAX_VARIABLES      256
#define ALDA_TICKS_PER_QUARTER  480

/* Default values */
#define ALDA_DEFAULT_TEMPO      120
#define ALDA_DEFAULT_OCTAVE     4
#define ALDA_DEFAULT_VOLUME     54    /* 0-100, maps to velocity (mf) */
#define ALDA_DEFAULT_QUANT      90    /* Quantization percentage */
#define ALDA_DEFAULT_PAN        64    /* Center (MIDI 0-127, corresponds to Alda 50) */
#define ALDA_DEFAULT_DURATION   4     /* Quarter note */

/* ============================================================================
 * Event Types
 * ============================================================================ */

typedef enum {
    ALDA_EVT_NOTE_ON,
    ALDA_EVT_NOTE_OFF,
    ALDA_EVT_PROGRAM,
    ALDA_EVT_CC,
    ALDA_EVT_PAN,
    ALDA_EVT_TEMPO
} AldaEventType;

/* ============================================================================
 * Voice State (for polyphonic parts)
 * ============================================================================ */

typedef struct {
    int number;          /* Voice number (1-based, 0 = merged) */
    int current_tick;    /* Voice-specific tick position */
    int start_tick;      /* Tick where voice started */
} AldaVoiceState;

/* ============================================================================
 * Part State (per-instrument)
 * ============================================================================ */

typedef struct {
    char name[64];           /* Instrument name (e.g., "piano") */
    char alias[64];          /* Optional alias */
    int program;             /* GM program number (0-127) */
    int channel;             /* MIDI channel (1-16) */

    /* Musical state */
    int octave;              /* Current octave (0-9), default 4 */
    int volume;              /* 0-100, maps to velocity */
    int velocity_override;   /* 0-127, -1 = use volume, set by dynamics */
    int tempo;               /* BPM, 0 = use global */
    int quant;               /* Quantization percentage (0-100) */
    int pan;                 /* Pan position (0-127, 64=center) */

    /* Duration state */
    int default_duration;    /* Note value denominator (4=quarter) */
    int default_dots;        /* Dotted duration count */

    /* Timing */
    int current_tick;        /* Position in ticks from start */

    /* Voice state (for polyphonic parts) */
    AldaVoiceState voices[ALDA_MAX_VOICES];
    int voice_count;
    int current_voice;       /* -1 = merged (default), 0+ = specific voice index */
    int in_voice_group;      /* Non-zero if inside V1:, V2:, etc. */

    /* Key signature (sharps/flats for each scale degree C-B) */
    /* +1 = sharp, -1 = flat, 0 = natural */
    int key_signature[7];

    /* Transposition (semitones, positive = up, negative = down) */
    int transpose;
} AldaPartState;

/* ============================================================================
 * Scheduled Event
 * ============================================================================ */

typedef struct {
    int tick;            /* Absolute tick position */
    AldaEventType type;  /* Event type */
    int channel;         /* MIDI channel (0-15) */
    int data1;           /* Pitch or CC number or program */
    int data2;           /* Velocity or CC value */
    int part_index;      /* Source part index (for debugging) */
} AldaScheduledEvent;

/* ============================================================================
 * Marker (for @marker jumps - deferred feature)
 * ============================================================================ */

typedef struct {
    char name[64];       /* Marker name */
    int tick;            /* Tick position where marker was placed */
    int part_index;      /* Part context */
} AldaMarker;

/* ============================================================================
 * Variable (for Alda variables - deferred feature)
 * ============================================================================ */

struct AldaNode;  /* Forward declaration */

typedef struct {
    char name[64];           /* Variable name */
    struct AldaNode* events; /* Stored AST subtree */
} AldaVariable;

/* ============================================================================
 * Main Context
 * ============================================================================ */

typedef struct AldaContext {
    /* MIDI output */
    libremidi_midi_observer_handle* midi_observer;
    libremidi_midi_out_handle* midi_out;
    libremidi_midi_out_port* out_ports[ALDA_MAX_PORTS];
    int out_port_count;

    /* Parts management */
    AldaPartState parts[ALDA_MAX_PARTS];
    int part_count;
    int current_part_indices[ALDA_MAX_PARTS]; /* Currently active parts */
    int current_part_count;
    int next_channel;    /* Next channel to auto-assign (1-16) */

    /* Global defaults */
    int global_tempo;    /* BPM, default 120 */
    int global_volume;   /* 0-100, default 100 */
    int global_quant;    /* 0-100, default 90 */
    int global_pan;      /* 0-127, default 64 */

    /* Markers (deferred feature) */
    AldaMarker markers[ALDA_MAX_MARKERS];
    int marker_count;

    /* Variables (deferred feature) */
    AldaVariable variables[ALDA_MAX_VARIABLES];
    int variable_count;

    /* Event queue for scheduled playback */
    AldaScheduledEvent* events;
    int event_count;
    int event_capacity;

    /* Runtime flags */
    int no_sleep_mode;   /* Disable timing (for tests) */
    int verbose_mode;    /* Debug output */
    int tsf_enabled;     /* Built-in synth enabled */

    /* Repeat context for on-repetitions */
    int current_repetition;  /* 1-indexed, 0 means not in a repeat */

    /* Current file context for error reporting */
    const char* current_file;
    int current_line;
} AldaContext;

/* ============================================================================
 * Context Management Functions
 * ============================================================================ */

/**
 * @brief Initialize an AldaContext with default values.
 * @param ctx Context to initialize.
 */
void alda_context_init(AldaContext* ctx);

/**
 * @brief Cleanup an AldaContext and free resources.
 * @param ctx Context to cleanup.
 */
void alda_context_cleanup(AldaContext* ctx);

/**
 * @brief Reset runtime state (keeps MIDI connection).
 * @param ctx Context to reset.
 */
void alda_context_reset(AldaContext* ctx);

/* ============================================================================
 * Part Management Functions
 * ============================================================================ */

/**
 * @brief Get or create a part by name.
 * @param ctx Context.
 * @param name Instrument name.
 * @return Pointer to part state, or NULL if max parts exceeded.
 */
AldaPartState* alda_get_or_create_part(AldaContext* ctx, const char* name);

/**
 * @brief Find a part by name.
 * @param ctx Context.
 * @param name Instrument name or alias.
 * @return Pointer to part state, or NULL if not found.
 */
AldaPartState* alda_find_part(AldaContext* ctx, const char* name);

/**
 * @brief Set the currently active parts.
 * @param ctx Context.
 * @param names Array of part names.
 * @param count Number of names.
 * @return 0 on success, -1 on error.
 */
int alda_set_current_parts(AldaContext* ctx, char** names, int count);

/**
 * @brief Get the current active part (first in list).
 * @param ctx Context.
 * @return Pointer to current part, or NULL if none active.
 */
AldaPartState* alda_current_part(AldaContext* ctx);

/**
 * @brief Initialize a part with default values.
 * @param part Part to initialize.
 * @param name Instrument name.
 * @param channel MIDI channel (1-16).
 * @param program GM program number (0-127).
 */
void alda_part_init(AldaPartState* part, const char* name, int channel, int program);

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

/**
 * @brief Get effective tempo for a part.
 * @param ctx Context.
 * @param part Part state.
 * @return Tempo in BPM.
 */
int alda_effective_tempo(AldaContext* ctx, AldaPartState* part);

/**
 * @brief Get effective volume for a part (as velocity 0-127).
 * @param ctx Context.
 * @param part Part state.
 * @return Velocity value.
 */
int alda_effective_velocity(AldaContext* ctx, AldaPartState* part);

/**
 * @brief Get effective quantization for a part.
 * @param ctx Context.
 * @param part Part state.
 * @return Quantization percentage.
 */
int alda_effective_quant(AldaContext* ctx, AldaPartState* part);

/**
 * @brief Check if no-sleep mode is enabled.
 * @param ctx Context.
 * @return Non-zero if no-sleep mode is active.
 */
int alda_no_sleep(AldaContext* ctx);

/**
 * @brief Set no-sleep mode.
 * @param ctx Context.
 * @param value Non-zero to enable.
 */
void alda_set_no_sleep(AldaContext* ctx, int value);

#ifdef __cplusplus
}
#endif

#endif /* ALDA_CONTEXT_H */
