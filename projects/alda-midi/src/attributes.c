/**
 * @file attributes.c
 * @brief Alda S-expression (lisp-list) attribute evaluation.
 *
 * Handles attributes like (tempo 120), (volume 80), (quant 90), dynamics, etc.
 */

#include "alda/context.h"
#include "alda/ast.h"
#include "alda/scheduler.h"
#include <string.h>
#include <stdio.h>
#include <ctype.h>

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

/* Case-insensitive string compare */
static int strcasecmp_local(const char* s1, const char* s2) {
    while (*s1 && *s2) {
        int c1 = tolower((unsigned char)*s1);
        int c2 = tolower((unsigned char)*s2);
        if (c1 != c2) return c1 - c2;
        s1++;
        s2++;
    }
    return tolower((unsigned char)*s1) - tolower((unsigned char)*s2);
}

/* Get the first symbol name from a lisp list */
static const char* get_symbol_name(AldaNode* lisp_list) {
    if (!lisp_list || lisp_list->type != ALDA_NODE_LISP_LIST) {
        return NULL;
    }

    AldaNode* first = lisp_list->data.lisp_list.elements;
    if (!first || first->type != ALDA_NODE_LISP_SYMBOL) {
        return NULL;
    }

    return first->data.lisp_symbol.name;
}

/* Get the first numeric argument from a lisp list */
static int get_number_arg(AldaNode* lisp_list, double* out) {
    if (!lisp_list || lisp_list->type != ALDA_NODE_LISP_LIST) {
        return -1;
    }

    AldaNode* first = lisp_list->data.lisp_list.elements;
    if (!first) return -1;

    AldaNode* second = first->next;
    if (!second || second->type != ALDA_NODE_LISP_NUMBER) {
        return -1;
    }

    *out = second->data.lisp_number.value;
    return 0;
}

/* ============================================================================
 * Key Signature Lookup
 * ============================================================================ */

/* Key signature data: array of 7 values for C,D,E,F,G,A,B
 * +1 = sharp, -1 = flat, 0 = natural */

typedef struct {
    const char* tonic;
    const char* mode;
    int accidentals[7];  /* C, D, E, F, G, A, B */
} KeySigEntry;

/* Standard key signatures */
static const KeySigEntry KEY_SIGNATURES[] = {
    /* Major keys */
    {"c",  "major", {0, 0, 0, 0, 0, 0, 0}},       /* C major: no accidentals */
    {"g",  "major", {0, 0, 0, 1, 0, 0, 0}},       /* G major: F# */
    {"d",  "major", {1, 0, 0, 1, 0, 0, 0}},       /* D major: F#, C# */
    {"a",  "major", {1, 0, 0, 1, 1, 0, 0}},       /* A major: F#, C#, G# */
    {"e",  "major", {1, 1, 0, 1, 1, 0, 0}},       /* E major: F#, C#, G#, D# */
    {"b",  "major", {1, 1, 0, 1, 1, 1, 0}},       /* B major: F#, C#, G#, D#, A# */
    {"f#", "major", {1, 1, 1, 1, 1, 1, 0}},       /* F# major: all sharps except B */
    {"gb", "major", {-1, -1, -1, 0, -1, -1, -1}}, /* Gb major: all flats except F */
    {"f",  "major", {0, 0, 0, 0, 0, 0, -1}},      /* F major: Bb */
    {"bb", "major", {0, 0, -1, 0, 0, 0, -1}},     /* Bb major: Bb, Eb */
    {"eb", "major", {0, 0, -1, 0, 0, -1, -1}},    /* Eb major: Bb, Eb, Ab */
    {"ab", "major", {0, -1, -1, 0, 0, -1, -1}},   /* Ab major: Bb, Eb, Ab, Db */
    {"db", "major", {0, -1, -1, 0, -1, -1, -1}},  /* Db major: Bb, Eb, Ab, Db, Gb */
    {"cb", "major", {-1, -1, -1, -1, -1, -1, -1}},/* Cb major: all flats */

    /* Minor keys (natural minor / aeolian) */
    {"a",  "minor", {0, 0, 0, 0, 0, 0, 0}},       /* A minor: no accidentals */
    {"e",  "minor", {0, 0, 0, 1, 0, 0, 0}},       /* E minor: F# */
    {"b",  "minor", {1, 0, 0, 1, 0, 0, 0}},       /* B minor: F#, C# */
    {"f#", "minor", {1, 0, 0, 1, 1, 0, 0}},       /* F# minor: F#, C#, G# */
    {"c#", "minor", {1, 1, 0, 1, 1, 0, 0}},       /* C# minor: F#, C#, G#, D# */
    {"g#", "minor", {1, 1, 0, 1, 1, 1, 0}},       /* G# minor: F#, C#, G#, D#, A# */
    {"d",  "minor", {0, 0, 0, 0, 0, 0, -1}},      /* D minor: Bb */
    {"g",  "minor", {0, 0, -1, 0, 0, 0, -1}},     /* G minor: Bb, Eb */
    {"c",  "minor", {0, 0, -1, 0, 0, -1, -1}},    /* C minor: Bb, Eb, Ab */
    {"f",  "minor", {0, -1, -1, 0, 0, -1, -1}},   /* F minor: Bb, Eb, Ab, Db */
    {"bb", "minor", {0, -1, -1, 0, -1, -1, -1}},  /* Bb minor: Bb, Eb, Ab, Db, Gb */

    /* Modes - calculated from relative major */
    {"d",  "dorian",     {0, 0, 0, 0, 0, 0, 0}},  /* D dorian: same as C major */
    {"e",  "phrygian",   {0, 0, 0, 0, 0, 0, 0}},  /* E phrygian: same as C major */
    {"f",  "lydian",     {0, 0, 0, 0, 0, 0, 0}},  /* F lydian: same as C major */
    {"g",  "mixolydian", {0, 0, 0, 0, 0, 0, 0}},  /* G mixolydian: same as C major */
    {"a",  "aeolian",    {0, 0, 0, 0, 0, 0, 0}},  /* A aeolian: same as C major */
    {"b",  "locrian",    {0, 0, 0, 0, 0, 0, 0}},  /* B locrian: same as C major */

    /* Other common modes */
    {"c",  "dorian",     {0, 0, -1, 0, 0, 0, -1}}, /* C dorian: Bb, Eb */
    {"c",  "phrygian",   {0, -1, -1, 0, 0, -1, -1}},/* C phrygian: Bb, Eb, Ab, Db */
    {"c",  "lydian",     {0, 0, 0, 1, 0, 0, 0}},   /* C lydian: F# */
    {"c",  "mixolydian", {0, 0, 0, 0, 0, 0, -1}},  /* C mixolydian: Bb */

    {NULL, NULL, {0, 0, 0, 0, 0, 0, 0}}
};

/* Look up a key signature by tonic and mode */
static int lookup_key_signature(const char* tonic, const char* mode, int* out_sig) {
    if (!tonic || !out_sig) return -1;

    /* Default mode is major */
    const char* mode_to_use = mode ? mode : "major";

    for (int i = 0; KEY_SIGNATURES[i].tonic != NULL; i++) {
        if (strcasecmp_local(KEY_SIGNATURES[i].tonic, tonic) == 0 &&
            strcasecmp_local(KEY_SIGNATURES[i].mode, mode_to_use) == 0) {
            for (int j = 0; j < 7; j++) {
                out_sig[j] = KEY_SIGNATURES[i].accidentals[j];
            }
            return 0;
        }
    }
    return -1;  /* Not found */
}

/* Parse key signature from string format like "f+ c+" or "b- e-" */
static int parse_key_sig_string(const char* str, int* out_sig) {
    if (!str || !out_sig) return -1;

    /* Reset to natural */
    for (int i = 0; i < 7; i++) {
        out_sig[i] = 0;
    }

    /* Map note letters to index: C=0, D=1, E=2, F=3, G=4, A=5, B=6 */
    static const int LETTER_TO_INDEX[] = {
        5,  /* a -> 5 */
        6,  /* b -> 6 */
        0,  /* c -> 0 */
        1,  /* d -> 1 */
        2,  /* e -> 2 */
        3,  /* f -> 3 */
        4   /* g -> 4 */
    };

    const char* p = str;
    while (*p) {
        /* Skip whitespace */
        while (*p && isspace((unsigned char)*p)) p++;
        if (!*p) break;

        /* Parse note letter */
        char letter = tolower((unsigned char)*p);
        if (letter < 'a' || letter > 'g') {
            p++;
            continue;
        }
        int idx = LETTER_TO_INDEX[letter - 'a'];
        p++;

        /* Parse accidental */
        if (*p == '+' || *p == '#') {
            out_sig[idx] = 1;  /* Sharp */
            p++;
        } else if (*p == '-' || *p == 'b') {
            out_sig[idx] = -1;  /* Flat */
            p++;
        }
    }

    return 0;
}

/* Parse key signature from a lisp list argument (second element of key-sig sexp) */
static int parse_key_sig_arg(AldaNode* arg, int* out_sig) {
    if (!arg || !out_sig) return -1;

    /* Handle quoted list: '(g major) -> LISP_LIST with symbols */
    if (arg->type == ALDA_NODE_LISP_LIST) {
        AldaNode* first = arg->data.lisp_list.elements;
        if (!first || first->type != ALDA_NODE_LISP_SYMBOL) {
            return -1;
        }

        const char* tonic = first->data.lisp_symbol.name;
        const char* mode = NULL;

        /* Check for mode (second element) */
        if (first->next && first->next->type == ALDA_NODE_LISP_SYMBOL) {
            mode = first->next->data.lisp_symbol.name;
        }

        return lookup_key_signature(tonic, mode, out_sig);
    }

    /* Handle string format: "f+ c+" */
    if (arg->type == ALDA_NODE_LISP_STRING) {
        return parse_key_sig_string(arg->data.lisp_string.value, out_sig);
    }

    return -1;
}

/* ============================================================================
 * Dynamics Lookup
 * ============================================================================ */

typedef struct {
    const char* name;
    int velocity;  /* 0-127, matches aldakit */
} DynamicEntry;

/* Dynamics mapping: velocity values matching aldakit */
static const DynamicEntry DYNAMICS[] = {
    {"pppppp", 1},
    {"ppppp", 10},
    {"pppp", 20},
    {"ppp", 30},
    {"pp", 39},
    {"p", 50},
    {"mp", 58},
    {"mf", 69},
    {"f", 79},
    {"ff", 88},
    {"fff", 98},
    {"ffff", 108},
    {"fffff", 117},
    {"ffffff", 127},
    {NULL, 0}
};

static int lookup_dynamic(const char* name) {
    for (int i = 0; DYNAMICS[i].name != NULL; i++) {
        if (strcasecmp_local(DYNAMICS[i].name, name) == 0) {
            return DYNAMICS[i].velocity;
        }
    }
    return -1;  /* Not a dynamic marking */
}

/* ============================================================================
 * Main Attribute Evaluation
 * ============================================================================ */

/**
 * @brief Evaluate an attribute S-expression.
 * @param ctx Alda context.
 * @param part Part state to modify (or NULL for global).
 * @param lisp_list The ALDA_NODE_LISP_LIST node.
 * @return 0 on success, -1 on error.
 */
int alda_eval_attribute(AldaContext* ctx, AldaPartState* part, AldaNode* lisp_list) {
    if (!ctx || !lisp_list) return -1;

    const char* name = get_symbol_name(lisp_list);
    if (!name) {
        return 0;  /* Empty or invalid - ignore */
    }

    double num_val;
    int has_num = (get_number_arg(lisp_list, &num_val) == 0);

    /* Check for dynamics first (pp, mf, ff, etc.) */
    /* Dynamics set velocity directly (0-127) to match aldakit */
    int dynamic_vel = lookup_dynamic(name);
    if (dynamic_vel >= 0) {
        if (part) {
            part->velocity_override = dynamic_vel;
        }
        /* Note: global dynamics not supported, use per-part */
        return 0;
    }

    /* Handle named attributes */

    /* tempo / tempo! */
    if (strcasecmp_local(name, "tempo") == 0 ||
        strcasecmp_local(name, "tempo!") == 0) {
        if (has_num) {
            int tempo = (int)num_val;
            if (tempo > 0 && tempo <= 1000) {
                /* Global if ends with !, otherwise per-part */
                if (name[strlen(name) - 1] == '!' || !part) {
                    ctx->global_tempo = tempo;
                    /* Schedule tempo change at current position */
                    int tick = part ? part->current_tick : 0;
                    alda_schedule_tempo(ctx, tick, tempo);
                } else if (part) {
                    part->tempo = tempo;
                    /* Schedule tempo change at part's current position */
                    alda_schedule_tempo(ctx, part->current_tick, tempo);
                }
            }
        }
        return 0;
    }

    /* volume / vol / volume! / vol! */
    if (strcasecmp_local(name, "volume") == 0 ||
        strcasecmp_local(name, "vol") == 0 ||
        strcasecmp_local(name, "volume!") == 0 ||
        strcasecmp_local(name, "vol!") == 0) {
        if (has_num) {
            int vol = (int)num_val;
            /* Clamp to valid range */
            if (vol < 0) vol = 0;
            if (vol > 100) vol = 100;
            if (name[strlen(name) - 1] == '!' || !part) {
                ctx->global_volume = vol;
            } else if (part) {
                part->volume = vol;
                part->velocity_override = -1;  /* Clear dynamics override */
            }
        }
        return 0;
    }

    /* quant / quantization / quant! / quantization! */
    if (strcasecmp_local(name, "quant") == 0 ||
        strcasecmp_local(name, "quantization") == 0 ||
        strcasecmp_local(name, "quant!") == 0 ||
        strcasecmp_local(name, "quantization!") == 0) {
        if (has_num) {
            int quant = (int)num_val;
            if (quant >= 0 && quant <= 100) {
                if (name[strlen(name) - 1] == '!' || !part) {
                    ctx->global_quant = quant;
                } else if (part) {
                    part->quant = quant;
                }
            }
        }
        return 0;
    }

    /* pan / panning / pan! / panning! */
    /* Official Alda uses 0-100, convert to MIDI 0-127 */
    if (strcasecmp_local(name, "pan") == 0 ||
        strcasecmp_local(name, "panning") == 0 ||
        strcasecmp_local(name, "pan!") == 0 ||
        strcasecmp_local(name, "panning!") == 0) {
        if (has_num) {
            int pan_100 = (int)num_val;
            if (pan_100 >= 0 && pan_100 <= 100) {
                int pan_127 = pan_100 * 127 / 100;
                if (name[strlen(name) - 1] == '!' || !part) {
                    ctx->global_pan = pan_127;
                } else if (part) {
                    part->pan = pan_127;
                    /* Schedule pan change at current position */
                    alda_schedule_pan(ctx, part, part->current_tick, pan_127);
                }
            }
        }
        return 0;
    }

    /* octave / octave! */
    if (strcasecmp_local(name, "octave") == 0 ||
        strcasecmp_local(name, "octave!") == 0) {
        if (has_num) {
            int octave = (int)num_val;
            if (octave >= 0 && octave <= 9) {
                if (part) {
                    part->octave = octave;
                }
            }
        }
        return 0;
    }

    /* set-duration / duration */
    if (strcasecmp_local(name, "set-duration") == 0 ||
        strcasecmp_local(name, "duration") == 0) {
        if (has_num && part) {
            /* Value is in beats (1.0 = quarter note) */
            /* Convert to denominator: 1.0 -> 4, 0.5 -> 8, 2.0 -> 2 */
            if (num_val > 0) {
                int denom = (int)(4.0 / num_val);
                if (denom > 0) {
                    part->default_duration = denom;
                    part->default_dots = 0;
                }
            }
        }
        return 0;
    }

    /* set-note-length / note-length */
    if (strcasecmp_local(name, "set-note-length") == 0 ||
        strcasecmp_local(name, "note-length") == 0) {
        if (has_num && part) {
            int denom = (int)num_val;
            if (denom > 0) {
                part->default_duration = denom;
                part->default_dots = 0;
            }
        }
        return 0;
    }

    /* key-sig / key-signature */
    if (strcasecmp_local(name, "key-sig") == 0 ||
        strcasecmp_local(name, "key-signature") == 0 ||
        strcasecmp_local(name, "key-sig!") == 0 ||
        strcasecmp_local(name, "key-signature!") == 0) {
        if (!part) return 0;  /* Need a part to set key signature */

        /* Get the argument (second element of the lisp list) */
        AldaNode* first = lisp_list->data.lisp_list.elements;
        if (!first) return 0;

        AldaNode* arg = first->next;
        if (!arg) return 0;

        int new_sig[7];
        if (parse_key_sig_arg(arg, new_sig) == 0) {
            /* Apply key signature to part */
            for (int i = 0; i < 7; i++) {
                part->key_signature[i] = new_sig[i];
            }
        }
        return 0;
    }

    /* transpose / transposition */
    if (strcasecmp_local(name, "transpose") == 0 ||
        strcasecmp_local(name, "transposition") == 0 ||
        strcasecmp_local(name, "transpose!") == 0 ||
        strcasecmp_local(name, "transposition!") == 0) {
        if (has_num && part) {
            /* Value is semitones (positive = up, negative = down) */
            part->transpose = (int)num_val;
        }
        return 0;
    }

    /* Unrecognized attribute - just ignore */
    if (ctx->verbose_mode) {
        fprintf(stderr, "Warning: Unknown attribute (%s)\n", name);
    }

    return 0;
}
