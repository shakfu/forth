/*
 * music_theory.c - Common music theory utilities implementation
 */

#include "music_theory.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Cross-platform case-insensitive string compare */
#ifdef _WIN32
#define strcasecmp _stricmp
#else
#include <strings.h>
#endif

/* ============================================================================
 * Chord Interval Definitions
 * ============================================================================ */

const int CHORD_MAJOR[]     = {0, 4, 7};
const int CHORD_MINOR[]     = {0, 3, 7};
const int CHORD_DIM[]       = {0, 3, 6};
const int CHORD_AUG[]       = {0, 4, 8};
const int CHORD_DOM7[]      = {0, 4, 7, 10};
const int CHORD_MAJ7[]      = {0, 4, 7, 11};
const int CHORD_MIN7[]      = {0, 3, 7, 10};
const int CHORD_DIM7[]      = {0, 3, 6, 9};
const int CHORD_HALF_DIM7[] = {0, 3, 6, 10};
const int CHORD_SUS2[]      = {0, 2, 7};
const int CHORD_SUS4[]      = {0, 5, 7};

/* ============================================================================
 * Pitch Parsing
 * ============================================================================ */

int music_parse_pitch(const char* name) {
    if (name == NULL || name[0] == '\0') return -1;

    /* Parse note letter (case insensitive) */
    int note;
    char c = toupper((unsigned char)name[0]);
    switch (c) {
        case 'C': note = 0;  break;
        case 'D': note = 2;  break;
        case 'E': note = 4;  break;
        case 'F': note = 5;  break;
        case 'G': note = 7;  break;
        case 'A': note = 9;  break;
        case 'B': note = 11; break;
        default: return -1;
    }

    const char* p = name + 1;

    /* Parse accidental (optional) */
    if (*p == '#' || *p == 's' || *p == 'S') {
        note++;
        p++;
    } else if (*p == 'b' || *p == 'B') {
        note--;
        p++;
    }

    /* Parse octave number (required) */
    if (*p == '\0') return -1;

    /* Handle negative octave (-1) */
    int negative = 0;
    if (*p == '-') {
        negative = 1;
        p++;
    }

    if (*p == '\0' || !isdigit((unsigned char)*p)) return -1;

    int octave = atoi(p);
    if (negative) octave = -octave;

    /* Validate octave range: -1 to 9 */
    if (octave < -1 || octave > 9) return -1;

    /* Calculate MIDI note number */
    int midi_note = (octave + 1) * 12 + note;

    /* Validate final MIDI range */
    if (midi_note < 0 || midi_note > 127) return -1;

    return midi_note;
}

char* music_pitch_to_name(int pitch, char* buf, int buflen, int use_sharps) {
    if (buf == NULL || buflen < 5) return NULL;
    if (pitch < 0 || pitch > 127) return NULL;

    static const char* sharp_names[] = {
        "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"
    };
    static const char* flat_names[] = {
        "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"
    };

    int octave = (pitch / 12) - 1;
    int note_index = pitch % 12;

    const char* note_name = use_sharps ? sharp_names[note_index] : flat_names[note_index];

    snprintf(buf, buflen, "%s%d", note_name, octave);
    return buf;
}

/* ============================================================================
 * Chord Building
 * ============================================================================ */

int music_build_chord(int root, const int* intervals, int num_notes, int* out_pitches) {
    if (intervals == NULL || out_pitches == NULL || num_notes <= 0) return 0;
    if (root < 0 || root > 127) return 0;

    int valid_count = 0;
    for (int i = 0; i < num_notes; i++) {
        int pitch = root + intervals[i];
        if (pitch >= 0 && pitch <= 127) {
            out_pitches[valid_count++] = pitch;
        }
    }

    return valid_count;
}

/* ============================================================================
 * Scale Interval Definitions
 * ============================================================================ */

/* Diatonic modes */
const int SCALE_MAJOR[]      = {0, 2, 4, 5, 7, 9, 11};
const int SCALE_DORIAN[]     = {0, 2, 3, 5, 7, 9, 10};
const int SCALE_PHRYGIAN[]   = {0, 1, 3, 5, 7, 8, 10};
const int SCALE_LYDIAN[]     = {0, 2, 4, 6, 7, 9, 11};
const int SCALE_MIXOLYDIAN[] = {0, 2, 4, 5, 7, 9, 10};
const int SCALE_MINOR[]      = {0, 2, 3, 5, 7, 8, 10};
const int SCALE_LOCRIAN[]    = {0, 1, 3, 5, 6, 8, 10};

/* Other minor scales */
const int SCALE_HARMONIC_MINOR[] = {0, 2, 3, 5, 7, 8, 11};
const int SCALE_MELODIC_MINOR[]  = {0, 2, 3, 5, 7, 9, 11};

/* Pentatonic scales */
const int SCALE_PENTATONIC_MAJOR[] = {0, 2, 4, 7, 9};
const int SCALE_PENTATONIC_MINOR[] = {0, 3, 5, 7, 10};

/* Blues scale */
const int SCALE_BLUES[] = {0, 3, 5, 6, 7, 10};

/* Symmetric scales */
const int SCALE_WHOLE_TONE[]    = {0, 2, 4, 6, 8, 10};
const int SCALE_CHROMATIC[]     = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
const int SCALE_DIMINISHED_HW[] = {0, 1, 3, 4, 6, 7, 9, 10};
const int SCALE_DIMINISHED_WH[] = {0, 2, 3, 5, 6, 8, 9, 11};
const int SCALE_AUGMENTED[]     = {0, 3, 4, 7, 8, 11};

/* Bebop scales (8 notes) */
const int SCALE_BEBOP_DOMINANT[] = {0, 2, 4, 5, 7, 9, 10, 11};
const int SCALE_BEBOP_MAJOR[]    = {0, 2, 4, 5, 7, 8, 9, 11};
const int SCALE_BEBOP_MINOR[]    = {0, 2, 3, 5, 7, 8, 9, 10};

/* Exotic/World scales */
const int SCALE_HUNGARIAN_MINOR[]   = {0, 2, 3, 6, 7, 8, 11};
const int SCALE_DOUBLE_HARMONIC[]   = {0, 1, 4, 5, 7, 8, 11};
const int SCALE_NEAPOLITAN_MAJOR[]  = {0, 1, 3, 5, 7, 9, 11};
const int SCALE_NEAPOLITAN_MINOR[]  = {0, 1, 3, 5, 7, 8, 11};
const int SCALE_PHRYGIAN_DOMINANT[] = {0, 1, 4, 5, 7, 8, 10};
const int SCALE_PERSIAN[]           = {0, 1, 4, 5, 6, 8, 11};
const int SCALE_ALTERED[]           = {0, 1, 3, 4, 6, 8, 10};

/* Japanese scales */
const int SCALE_HIRAJOSHI[] = {0, 2, 3, 7, 8};
const int SCALE_IN_SEN[]    = {0, 1, 5, 7, 10};
const int SCALE_IWATO[]     = {0, 1, 5, 6, 10};
const int SCALE_KUMOI[]     = {0, 2, 3, 7, 9};

/* Other world scales */
const int SCALE_EGYPTIAN[]       = {0, 2, 5, 7, 10};
const int SCALE_ROMANIAN_MINOR[] = {0, 2, 3, 6, 7, 9, 10};
const int SCALE_SPANISH_8_TONE[] = {0, 1, 3, 4, 5, 6, 8, 10};
const int SCALE_ENIGMATIC[]      = {0, 1, 4, 6, 8, 10, 11};

/* Arabic Maqamat (12-TET approximations) */
const int SCALE_MAQAM_HIJAZ[]      = {0, 1, 4, 5, 7, 8, 10};
const int SCALE_MAQAM_NAHAWAND[]   = {0, 2, 3, 5, 7, 8, 11};
const int SCALE_MAQAM_NIKRIZ[]     = {0, 2, 3, 6, 7, 9, 10};
const int SCALE_MAQAM_ATHAR_KURD[] = {0, 1, 3, 5, 6, 8, 10};
const int SCALE_MAQAM_SHAWQ_AFZA[] = {0, 2, 3, 6, 7, 9, 11};
const int SCALE_MAQAM_JIHARKAH[]   = {0, 2, 4, 5, 7, 9, 10};

/* Indian Ragas (12-TET approximations) */
const int SCALE_RAGA_BHAIRAV[]     = {0, 1, 4, 5, 7, 8, 11};
const int SCALE_RAGA_TODI[]        = {0, 1, 3, 6, 7, 8, 11};
const int SCALE_RAGA_MARWA[]       = {0, 1, 4, 6, 7, 9, 11};
const int SCALE_RAGA_PURVI[]       = {0, 1, 4, 6, 7, 8, 11};
const int SCALE_RAGA_CHARUKESHI[]  = {0, 2, 4, 5, 7, 8, 10};
const int SCALE_RAGA_ASAVARI[]     = {0, 2, 3, 5, 7, 8, 10};
const int SCALE_RAGA_BILAWAL[]     = {0, 2, 4, 5, 7, 9, 11};
const int SCALE_RAGA_KHAMAJ[]      = {0, 2, 4, 5, 7, 9, 10};
const int SCALE_RAGA_KALYAN[]      = {0, 2, 4, 6, 7, 9, 11};
const int SCALE_RAGA_BHIMPALASI[]  = {0, 3, 5, 7, 10};
const int SCALE_RAGA_DARBARI[]     = {0, 2, 3, 5, 7, 8, 9};

/* ============================================================================
 * Microtonal Scales (Cents-Based)
 * ============================================================================ */

/* Arabic Maqamat with quarter tones (cents) */
const int SCALE_MAQAM_BAYATI_CENTS[]     = {0, 150, 300, 500, 700, 800, 1000};
const int SCALE_MAQAM_RAST_CENTS[]       = {0, 200, 350, 500, 700, 900, 1050};
const int SCALE_MAQAM_SABA_CENTS[]       = {0, 150, 300, 400, 500, 700, 800};
const int SCALE_MAQAM_SIKAH_CENTS[]      = {0, 150, 350, 500, 650, 850, 1000};
const int SCALE_MAQAM_HUZAM_CENTS[]      = {0, 150, 350, 500, 700, 850, 1050};
const int SCALE_MAQAM_IRAQ_CENTS[]       = {0, 150, 350, 500, 700, 850, 1000};
const int SCALE_MAQAM_BASTANIKAR_CENTS[] = {0, 150, 350, 500, 700, 800, 1000};

/* Turkish Makamlar with commas (cents) */
const int SCALE_MAKAM_USSAK_CENTS[]   = {0, 150, 300, 500, 700, 800, 1000};
const int SCALE_MAKAM_HUSEYNI_CENTS[] = {0, 150, 300, 500, 700, 900, 1000};

/* Indian 22-Shruti scale (cents from Sa)
 * Traditional Indian music divides the octave into 22 shrutis
 */
const int SCALE_SHRUTI_CENTS[] = {
    0,    /* Sa  - tonic */
    90,   /* ri  - minor second (low) */
    112,  /* Ri  - minor second (high) */
    182,  /* ri  - major second (low) */
    204,  /* Ri  - major second (high) */
    294,  /* ga  - minor third (low) */
    316,  /* Ga  - minor third (high) */
    386,  /* ga  - major third (low) */
    408,  /* Ga  - major third (high) */
    498,  /* Ma  - perfect fourth */
    520,  /* ma  - fourth (high) */
    590,  /* Ma# - augmented fourth (low) */
    612,  /* Ma# - augmented fourth (high) */
    702,  /* Pa  - perfect fifth */
    792,  /* dha - minor sixth (low) */
    814,  /* Dha - minor sixth (high) */
    884,  /* dha - major sixth (low) */
    906,  /* Dha - major sixth (high) */
    996,  /* ni  - minor seventh (low) */
    1018, /* Ni  - minor seventh (high) */
    1088, /* ni  - major seventh (low) */
    1110  /* Ni  - major seventh (high) */
};

/* ============================================================================
 * Scale Functions
 * ============================================================================ */

int music_build_scale(int root, const int* intervals, int num_notes, int* out_pitches) {
    if (intervals == NULL || out_pitches == NULL || num_notes <= 0) return 0;
    if (root < 0 || root > 127) return 0;

    int valid_count = 0;
    for (int i = 0; i < num_notes; i++) {
        int pitch = root + intervals[i];
        if (pitch >= 0 && pitch <= 127) {
            out_pitches[valid_count++] = pitch;
        }
    }

    return valid_count;
}

int music_scale_degree(int root, const int* intervals, int num_notes, int degree) {
    if (intervals == NULL || num_notes <= 0 || degree < 1) return -1;
    if (root < 0 || root > 127) return -1;

    /* Convert 1-based degree to 0-based index */
    int index = degree - 1;

    /* Calculate octave offset and scale index */
    int octaves = index / num_notes;
    int scale_index = index % num_notes;

    /* Calculate pitch */
    int pitch = root + intervals[scale_index] + (octaves * 12);

    if (pitch < 0 || pitch > 127) return -1;
    return pitch;
}

int music_in_scale(int pitch, int root, const int* intervals, int num_notes) {
    if (intervals == NULL || num_notes <= 0) return 0;
    if (pitch < 0 || pitch > 127) return 0;
    if (root < 0 || root > 127) return 0;

    /* Get the pitch class (0-11) relative to the root */
    int root_class = root % 12;
    int pitch_class = pitch % 12;

    /* Calculate interval from root (in pitch class space) */
    int interval = (pitch_class - root_class + 12) % 12;

    /* Check if this interval exists in the scale */
    for (int i = 0; i < num_notes; i++) {
        if (intervals[i] == interval) {
            return 1;
        }
    }

    return 0;
}

int music_quantize_to_scale(int pitch, int root, const int* intervals, int num_notes) {
    if (intervals == NULL || num_notes <= 0) return pitch;
    if (pitch < 0 || pitch > 127) return pitch;
    if (root < 0 || root > 127) return pitch;

    /* If already in scale, return as-is */
    if (music_in_scale(pitch, root, intervals, num_notes)) {
        return pitch;
    }

    /* Find the nearest scale tone */
    int best_pitch = pitch;
    int best_distance = 128;

    /* Check pitches within one octave up and down */
    for (int offset = -12; offset <= 12; offset++) {
        int candidate = pitch + offset;
        if (candidate < 0 || candidate > 127) continue;

        if (music_in_scale(candidate, root, intervals, num_notes)) {
            int distance = abs(offset);
            if (distance < best_distance) {
                best_distance = distance;
                best_pitch = candidate;
            }
        }
    }

    return best_pitch;
}

/* ============================================================================
 * Microtonal Functions
 * ============================================================================ */

MicrotonalNote music_cents_to_note(int root_midi, int cents) {
    MicrotonalNote note;

    /* Handle negative cents (going down from root) */
    int total_cents = cents;
    if (total_cents < 0) {
        /* Normalize to positive by subtracting octaves */
        int octaves_down = (-total_cents / 1200) + 1;
        total_cents += octaves_down * 1200;
        root_midi -= octaves_down * 12;
    }

    /* Calculate semitones and remaining cents */
    int semitones = total_cents / 100;
    int remainder = total_cents % 100;

    /* Round to nearest semitone if remainder > 50 cents */
    if (remainder > 50) {
        semitones++;
        remainder -= 100;  /* negative bend */
    }

    note.midi_note = root_midi + semitones;
    note.bend_cents = remainder;

    /* Clamp MIDI note to valid range */
    if (note.midi_note < 0) {
        note.midi_note = 0;
        note.bend_cents = 0;
    } else if (note.midi_note > 127) {
        note.midi_note = 127;
        note.bend_cents = 0;
    }

    return note;
}

int music_cents_to_bend(int cents) {
    /*
     * Standard MIDI pitch bend range is +/- 2 semitones (200 cents).
     * Pitch bend is 14-bit: 0-16383, with 8192 as center (no bend).
     *
     * For +/- 2 semitone range:
     *   bend = 8192 + (cents * 8192 / 200)
     *   bend = 8192 + (cents * 40.96)
     */
    int bend = 8192 + (cents * 8192) / 200;

    /* Clamp to valid range */
    if (bend < 0) bend = 0;
    if (bend > 16383) bend = 16383;

    return bend;
}

int music_build_microtonal_scale(int root_midi, const int* cents, int num_notes,
                                  MicrotonalNote* out_notes) {
    if (cents == NULL || out_notes == NULL || num_notes <= 0) return 0;
    if (root_midi < 0 || root_midi > 127) return 0;

    int valid_count = 0;
    for (int i = 0; i < num_notes; i++) {
        MicrotonalNote note = music_cents_to_note(root_midi, cents[i]);
        if (note.midi_note >= 0 && note.midi_note <= 127) {
            out_notes[valid_count++] = note;
        }
    }

    return valid_count;
}

MicrotonalNote music_microtonal_degree(int root_midi, const int* cents,
                                        int num_notes, int degree) {
    MicrotonalNote note = {-1, 0};

    if (cents == NULL || num_notes <= 0 || degree < 1) return note;
    if (root_midi < 0 || root_midi > 127) return note;

    /* Convert 1-based degree to 0-based index */
    int index = degree - 1;

    /* Calculate octave offset and scale index */
    int octaves = index / num_notes;
    int scale_index = index % num_notes;

    /* Get cents for this degree, adding octaves */
    int total_cents = cents[scale_index] + (octaves * 1200);

    return music_cents_to_note(root_midi, total_cents);
}

/* ============================================================================
 * Dynamics Parsing
 * ============================================================================ */

int music_parse_dynamics(const char* name) {
    if (name == NULL) return -1;

    /* Compare case-insensitively */
    if (strcasecmp(name, "ppp") == 0) return DYN_PPP;
    if (strcasecmp(name, "pp")  == 0) return DYN_PP;
    if (strcasecmp(name, "p")   == 0) return DYN_P;
    if (strcasecmp(name, "mp")  == 0) return DYN_MP;
    if (strcasecmp(name, "mf")  == 0) return DYN_MF;
    if (strcasecmp(name, "f")   == 0) return DYN_F;
    if (strcasecmp(name, "ff")  == 0) return DYN_FF;
    if (strcasecmp(name, "fff") == 0) return DYN_FFF;

    return -1;
}

/* ============================================================================
 * Duration Calculation
 * ============================================================================ */

int music_duration_ms(double beats, int bpm) {
    if (bpm <= 0) return 0;

    /* Duration = (beats / bpm) * 60000 ms */
    return (int)((beats * 60000.0) / bpm);
}
