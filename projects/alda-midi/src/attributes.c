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
 * Dynamics Lookup
 * ============================================================================ */

typedef struct {
    const char* name;
    int volume;  /* 0-100 */
} DynamicEntry;

static const DynamicEntry DYNAMICS[] = {
    {"pppppp", 7},
    {"ppppp", 14},
    {"pppp", 21},
    {"ppp", 28},
    {"pp", 36},
    {"p", 43},
    {"mp", 50},
    {"mf", 64},
    {"f", 71},
    {"ff", 79},
    {"fff", 86},
    {"ffff", 93},
    {"fffff", 98},
    {"ffffff", 100},
    {NULL, 0}
};

static int lookup_dynamic(const char* name) {
    for (int i = 0; DYNAMICS[i].name != NULL; i++) {
        if (strcasecmp_local(DYNAMICS[i].name, name) == 0) {
            return DYNAMICS[i].volume;
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
    int dynamic_vol = lookup_dynamic(name);
    if (dynamic_vol >= 0) {
        if (part) {
            part->volume = dynamic_vol;
        } else {
            ctx->global_volume = dynamic_vol;
        }
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
            if (vol >= 0 && vol <= 100) {
                if (name[strlen(name) - 1] == '!' || !part) {
                    ctx->global_volume = vol;
                } else if (part) {
                    part->volume = vol;
                }
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
    if (strcasecmp_local(name, "pan") == 0 ||
        strcasecmp_local(name, "panning") == 0 ||
        strcasecmp_local(name, "pan!") == 0 ||
        strcasecmp_local(name, "panning!") == 0) {
        if (has_num) {
            int pan = (int)num_val;
            if (pan >= 0 && pan <= 127) {
                if (name[strlen(name) - 1] == '!' || !part) {
                    ctx->global_pan = pan;
                } else if (part) {
                    part->pan = pan;
                    /* Schedule pan change at current position */
                    alda_schedule_pan(ctx, part, part->current_tick, pan);
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

    /* key-sig / key-signature - deferred, just ignore */
    if (strcasecmp_local(name, "key-sig") == 0 ||
        strcasecmp_local(name, "key-signature") == 0 ||
        strcasecmp_local(name, "key-sig!") == 0 ||
        strcasecmp_local(name, "key-signature!") == 0) {
        /* TODO: Parse key signature */
        return 0;
    }

    /* transpose / transposition - deferred */
    if (strcasecmp_local(name, "transpose") == 0 ||
        strcasecmp_local(name, "transposition") == 0) {
        /* TODO: Implement transposition */
        return 0;
    }

    /* Unrecognized attribute - just ignore */
    if (ctx->verbose_mode) {
        fprintf(stderr, "Warning: Unknown attribute (%s)\n", name);
    }

    return 0;
}
