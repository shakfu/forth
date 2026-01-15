/*
 * music_notation.c - Alda-like musical notation parser for Joy-MIDI
 *
 * Notes are converted to MIDI integers at PARSE TIME, not execution time.
 * This means [c d e] directly becomes [60 62 64], enabling Joy combinators.
 *
 * Examples:
 *   c d e              - pushes 60, 62, 64 onto stack
 *   [c d e]            - list [60 62 64] (no 'notes' needed!)
 *   [c d e] [7 +] map  - transpose: [67 69 71]
 *   [c e g] play       - plays C major arpeggio
 *   [c e g] chord      - plays C major chord
 *
 * Notation:
 *   c d e f g a b      - notes in octave 4 (c=60)
 *   c5 d5 e5           - notes in octave 5 (c5=72)
 *   c+ c-              - sharp/flat (c+=61, c-=59)
 *   c+5                - C#5 (73)
 */

#include "music_notation.h"
#include "music_context.h"
#include "midi_primitives.h"
#include "joy_parser.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

/* Special value for rest */
#define REST_MARKER (-1)

/* Default octave for notes without explicit octave */
#define DEFAULT_OCTAVE 4

/* ============================================================================
 * Note Pattern Parser (used at parse time)
 * ============================================================================ */

/* Note offset from C within an octave */
static int note_offset(char note) {
    switch (tolower(note)) {
        case 'c': return 0;
        case 'd': return 2;
        case 'e': return 4;
        case 'f': return 5;
        case 'g': return 7;
        case 'a': return 9;
        case 'b': return 11;
        default: return -1;
    }
}

/*
 * Parse a note pattern and return MIDI pitch.
 * Patterns: c, c4, c5, c+, c-, c+5, c-4
 * Returns -1 if not a valid note pattern.
 */
static int parse_note_pattern(const char* name) {
    const char* p = name;

    /* Must start with note letter */
    int offset = note_offset(*p);
    if (offset < 0) return -1;
    p++;

    /* Optional accidentals: + (sharp), - (flat) */
    int accidental = 0;
    while (*p == '+' || *p == '-') {
        if (*p == '+') accidental++;
        else if (*p == '-') accidental--;
        p++;
    }

    /* Optional octave number (0-9) */
    int octave = DEFAULT_OCTAVE;
    if (*p >= '0' && *p <= '9') {
        octave = *p - '0';
        p++;
    }

    /* Must have consumed entire string */
    if (*p != '\0') return -1;

    /* Calculate MIDI pitch */
    int pitch = (octave + 1) * 12 + offset + accidental;
    if (pitch < 0) pitch = 0;
    if (pitch > 127) pitch = 127;

    return pitch;
}

/*
 * Symbol transformer for the parser.
 * Converts note patterns to integers at parse time.
 */
static bool note_transformer(const char* symbol, JoyValue* out_value) {
    /* Try to parse as note pattern */
    int pitch = parse_note_pattern(symbol);
    if (pitch >= 0) {
        *out_value = joy_integer(pitch);
        return true;
    }

    /* Check for rest */
    if (symbol[0] == 'r' && (symbol[1] == '\0' ||
        (symbol[1] >= '0' && symbol[1] <= '9' && symbol[2] == '\0'))) {
        *out_value = joy_integer(REST_MARKER);
        return true;
    }

    return false;  /* Not a note pattern */
}

/* ============================================================================
 * Runtime State Handlers (dynamics, etc.)
 * ============================================================================ */

/* Parse dynamics: ppp, pp, p, mp, mf, f, ff, fff - sets velocity state */
static bool parse_dynamic(MusicContext* mctx, const char* name) {
    int velocity = -1;

    if (strcmp(name, "ppp") == 0) velocity = 16;
    else if (strcmp(name, "pp") == 0) velocity = 33;
    else if (strcmp(name, "p") == 0) velocity = 49;
    else if (strcmp(name, "mp") == 0) velocity = 64;
    else if (strcmp(name, "mf") == 0) velocity = 80;
    else if (strcmp(name, "f") == 0) velocity = 96;
    else if (strcmp(name, "ff") == 0) velocity = 112;
    else if (strcmp(name, "fff") == 0) velocity = 127;

    if (velocity >= 0) {
        mctx->velocity = velocity;
        return true;
    }
    return false;
}

/* ============================================================================
 * Play Helpers
 * ============================================================================ */

/* Helper: play a single note with current settings */
static void play_single_note(MusicContext* mctx, int pitch) {
    /* Check if we're in scheduling mode */
    if (is_scheduling()) {
        /* Add event to schedule instead of playing */
        MidiSchedule* sched = get_current_schedule();
        if (sched) {
            int play_dur = mctx->duration_ms * mctx->quantization / 100;
            if (pitch != REST_MARKER) {
                schedule_add_event(sched, get_schedule_time(),
                                  get_schedule_channel(), pitch,
                                  mctx->velocity, play_dur);
            }
            /* Advance time by note duration (including rest part) */
            advance_schedule_time(mctx->duration_ms);
        }
        return;
    }

    /* Immediate playback mode */
    if (pitch == REST_MARKER) {
        /* Rest: just wait */
        usleep(mctx->duration_ms * 1000);
        return;
    }

    /* Apply quantization */
    int play_dur = mctx->duration_ms * mctx->quantization / 100;
    int rest_dur = mctx->duration_ms - play_dur;

    send_note_on(pitch, mctx->velocity);
    if (play_dur > 0) {
        usleep(play_dur * 1000);
    }
    send_note_off(pitch);

    if (rest_dur > 0) {
        usleep(rest_dur * 1000);
    }
}

/* ============================================================================
 * Play and Chord Primitives
 * ============================================================================ */

/* play - pop and play notes sequentially */
void music_play_(JoyContext* ctx) {
    if (ctx->stack->depth < 1) {
        joy_error_underflow("play", 1, ctx->stack->depth);
        return;
    }

    MusicContext* mctx = (MusicContext*)ctx->user_data;
    if (!mctx) {
        joy_error("play: no music context");
        return;
    }

    JoyValue val = joy_stack_pop(ctx->stack);

    if (val.type == JOY_INTEGER) {
        /* Single note */
        play_single_note(mctx, (int)val.data.integer);
    } else if (val.type == JOY_LIST) {
        /* List of notes - play sequentially */
        JoyList* list = val.data.list;
        for (size_t i = 0; i < list->length; i++) {
            if (list->items[i].type == JOY_INTEGER) {
                play_single_note(mctx, (int)list->items[i].data.integer);
            }
        }
        joy_value_free(&val);
    } else {
        joy_value_free(&val);
        joy_error("play: expected integer or list");
    }
}

/* chord - pop and play notes simultaneously */
void music_chord_(JoyContext* ctx) {
    if (ctx->stack->depth < 1) {
        joy_error_underflow("chord", 1, ctx->stack->depth);
        return;
    }

    MusicContext* mctx = (MusicContext*)ctx->user_data;
    if (!mctx) {
        joy_error("chord: no music context");
        return;
    }

    JoyValue val = joy_stack_pop(ctx->stack);

    if (val.type == JOY_INTEGER) {
        /* Single note - just play it */
        play_single_note(mctx, (int)val.data.integer);
    } else if (val.type == JOY_LIST) {
        /* List of notes - play simultaneously */
        JoyList* list = val.data.list;
        int pitches[16];
        int count = 0;

        /* Collect pitches */
        for (size_t i = 0; i < list->length && count < 16; i++) {
            if (list->items[i].type == JOY_INTEGER) {
                int p = (int)list->items[i].data.integer;
                if (p != REST_MARKER) {
                    pitches[count++] = p;
                }
            }
        }

        /* Check if we're in scheduling mode */
        if (is_scheduling()) {
            MidiSchedule* sched = get_current_schedule();
            if (sched && count > 0) {
                int play_dur = mctx->duration_ms * mctx->quantization / 100;
                int current_time = get_schedule_time();
                int channel = get_schedule_channel();

                /* Add all notes at the same time */
                for (int i = 0; i < count; i++) {
                    schedule_add_event(sched, current_time, channel,
                                      pitches[i], mctx->velocity, play_dur);
                }

                /* Advance time by one note duration (chord = 1 beat) */
                advance_schedule_time(mctx->duration_ms);
            }
            joy_value_free(&val);
            return;
        }

        /* Immediate playback mode */
        /* Note on for all */
        for (int i = 0; i < count; i++) {
            send_note_on(pitches[i], mctx->velocity);
        }

        /* Wait for duration */
        int play_dur = mctx->duration_ms * mctx->quantization / 100;
        if (play_dur > 0) {
            usleep(play_dur * 1000);
        }

        /* Note off for all */
        for (int i = 0; i < count; i++) {
            send_note_off(pitches[i]);
        }

        /* Rest gap */
        int rest_dur = mctx->duration_ms - play_dur;
        if (rest_dur > 0) {
            usleep(rest_dur * 1000);
        }

        joy_value_free(&val);
    } else {
        joy_value_free(&val);
        joy_error("chord: expected integer or list");
    }
}

/* ============================================================================
 * Undefined Symbol Handler (for dynamics only now)
 * ============================================================================ */

bool music_handle_symbol(JoyContext* ctx, const char* name) {
    MusicContext* mctx = (MusicContext*)ctx->user_data;
    if (!mctx) return false;

    /* Dynamic: ppp, pp, p, mp, mf, f, ff, fff (modify state, push nothing) */
    if (parse_dynamic(mctx, name)) return true;

    return false;  /* Not handled */
}

/* ============================================================================
 * Public API
 * ============================================================================ */

void music_notation_init(JoyContext* ctx) {
    /* Create music context */
    MusicContext* mctx = music_context_new();
    if (!mctx) {
        fprintf(stderr, "Failed to create music context\n");
        return;
    }

    /* Store in Joy context */
    ctx->user_data = mctx;
    ctx->undef_handler = music_handle_symbol;

    /* Set up parse-time note conversion */
    joy_set_symbol_transformer(note_transformer);
}

void music_notation_cleanup(JoyContext* ctx) {
    if (ctx->user_data) {
        music_context_free((MusicContext*)ctx->user_data);
        ctx->user_data = NULL;
    }
    ctx->undef_handler = NULL;
    joy_set_symbol_transformer(NULL);
}

MusicContext* music_get_context(JoyContext* ctx) {
    return (MusicContext*)ctx->user_data;
}
