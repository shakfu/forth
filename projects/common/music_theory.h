/*
 * music_theory.h - Common music theory utilities for MIDI languages
 *
 * Provides:
 * - Pitch parsing (note name to MIDI number)
 * - Chord interval definitions
 * - Dynamics constants (velocity values)
 * - Duration constants
 * - MIDI note constants
 */

#ifndef MUSIC_THEORY_H
#define MUSIC_THEORY_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Pitch Parsing
 * ============================================================================ */

/*
 * Parse a note name string to a MIDI pitch number.
 *
 * Accepts formats:
 *   "C4"   -> 60   (middle C)
 *   "c4"   -> 60   (case insensitive)
 *   "C#4"  -> 61   (sharp)
 *   "Cs4"  -> 61   (sharp alternate)
 *   "Db4"  -> 61   (flat)
 *   "C-1"  -> 0    (lowest MIDI note)
 *   "G9"   -> 127  (highest MIDI note)
 *
 * Returns: MIDI pitch (0-127) on success, -1 on error
 */
int music_parse_pitch(const char* name);

/*
 * Convert a MIDI pitch number to a note name.
 * Writes the result to the provided buffer.
 *
 * Parameters:
 *   pitch  - MIDI pitch number (0-127)
 *   buf    - Output buffer (must be at least 5 bytes)
 *   buflen - Size of output buffer
 *   use_sharps - If true, use sharps (C#); if false, use flats (Db)
 *
 * Returns: Pointer to buf on success, NULL on error
 */
char* music_pitch_to_name(int pitch, char* buf, int buflen, int use_sharps);

/* ============================================================================
 * Chord Intervals
 * ============================================================================
 *
 * Chord intervals are stored as arrays of semitone offsets from the root.
 * For example, a major triad is [0, 4, 7].
 */

/* Chord interval arrays */
extern const int CHORD_MAJOR[];      /* Major triad: [0, 4, 7] */
extern const int CHORD_MINOR[];      /* Minor triad: [0, 3, 7] */
extern const int CHORD_DIM[];        /* Diminished triad: [0, 3, 6] */
extern const int CHORD_AUG[];        /* Augmented triad: [0, 4, 8] */
extern const int CHORD_DOM7[];       /* Dominant 7th: [0, 4, 7, 10] */
extern const int CHORD_MAJ7[];       /* Major 7th: [0, 4, 7, 11] */
extern const int CHORD_MIN7[];       /* Minor 7th: [0, 3, 7, 10] */
extern const int CHORD_DIM7[];       /* Diminished 7th: [0, 3, 6, 9] */
extern const int CHORD_HALF_DIM7[];  /* Half-diminished 7th: [0, 3, 6, 10] */
extern const int CHORD_SUS2[];       /* Suspended 2nd: [0, 2, 7] */
extern const int CHORD_SUS4[];       /* Suspended 4th: [0, 5, 7] */

/* Chord sizes */
#define CHORD_TRIAD_SIZE 3
#define CHORD_7TH_SIZE   4

/*
 * Build a chord from a root pitch and interval array.
 *
 * Parameters:
 *   root       - Root pitch (MIDI number 0-127)
 *   intervals  - Array of semitone intervals
 *   num_notes  - Number of notes in the chord
 *   out_pitches - Output array for chord pitches (must be at least num_notes)
 *
 * Returns: Number of valid pitches written (may be less than num_notes
 *          if some would exceed MIDI range)
 */
int music_build_chord(int root, const int* intervals, int num_notes, int* out_pitches);

/* ============================================================================
 * Scale Intervals
 * ============================================================================
 *
 * Scale intervals are stored as arrays of semitone offsets from the root.
 * For example, a major scale is [0, 2, 4, 5, 7, 9, 11].
 */

/* Diatonic modes */
extern const int SCALE_MAJOR[];          /* Ionian: [0, 2, 4, 5, 7, 9, 11] */
extern const int SCALE_DORIAN[];         /* [0, 2, 3, 5, 7, 9, 10] */
extern const int SCALE_PHRYGIAN[];       /* [0, 1, 3, 5, 7, 8, 10] */
extern const int SCALE_LYDIAN[];         /* [0, 2, 4, 6, 7, 9, 11] */
extern const int SCALE_MIXOLYDIAN[];     /* [0, 2, 4, 5, 7, 9, 10] */
extern const int SCALE_MINOR[];          /* Aeolian/Natural minor: [0, 2, 3, 5, 7, 8, 10] */
extern const int SCALE_LOCRIAN[];        /* [0, 1, 3, 5, 6, 8, 10] */

/* Other minor scales */
extern const int SCALE_HARMONIC_MINOR[]; /* [0, 2, 3, 5, 7, 8, 11] */
extern const int SCALE_MELODIC_MINOR[];  /* [0, 2, 3, 5, 7, 9, 11] (ascending) */

/* Pentatonic scales */
extern const int SCALE_PENTATONIC_MAJOR[]; /* [0, 2, 4, 7, 9] */
extern const int SCALE_PENTATONIC_MINOR[]; /* [0, 3, 5, 7, 10] */

/* Blues scale */
extern const int SCALE_BLUES[];          /* [0, 3, 5, 6, 7, 10] */

/* Symmetric scales */
extern const int SCALE_WHOLE_TONE[];     /* [0, 2, 4, 6, 8, 10] */
extern const int SCALE_CHROMATIC[];      /* [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] */
extern const int SCALE_DIMINISHED_HW[];  /* Half-whole: [0, 1, 3, 4, 6, 7, 9, 10] */
extern const int SCALE_DIMINISHED_WH[];  /* Whole-half: [0, 2, 3, 5, 6, 8, 9, 11] */
extern const int SCALE_AUGMENTED[];      /* [0, 3, 4, 7, 8, 11] */

/* Bebop scales (8 notes) */
extern const int SCALE_BEBOP_DOMINANT[]; /* [0, 2, 4, 5, 7, 9, 10, 11] */
extern const int SCALE_BEBOP_MAJOR[];    /* [0, 2, 4, 5, 7, 8, 9, 11] */
extern const int SCALE_BEBOP_MINOR[];    /* [0, 2, 3, 5, 7, 8, 9, 10] */

/* Exotic/World scales */
extern const int SCALE_HUNGARIAN_MINOR[];   /* [0, 2, 3, 6, 7, 8, 11] */
extern const int SCALE_DOUBLE_HARMONIC[];   /* Byzantine: [0, 1, 4, 5, 7, 8, 11] */
extern const int SCALE_NEAPOLITAN_MAJOR[];  /* [0, 1, 3, 5, 7, 9, 11] */
extern const int SCALE_NEAPOLITAN_MINOR[];  /* [0, 1, 3, 5, 7, 8, 11] */
extern const int SCALE_PHRYGIAN_DOMINANT[]; /* Spanish/Jewish: [0, 1, 4, 5, 7, 8, 10] */
extern const int SCALE_PERSIAN[];           /* [0, 1, 4, 5, 6, 8, 11] */
extern const int SCALE_ALTERED[];           /* Super Locrian: [0, 1, 3, 4, 6, 8, 10] */

/* Japanese scales */
extern const int SCALE_HIRAJOSHI[];      /* [0, 2, 3, 7, 8] */
extern const int SCALE_IN_SEN[];         /* [0, 1, 5, 7, 10] */
extern const int SCALE_IWATO[];          /* [0, 1, 5, 6, 10] */
extern const int SCALE_KUMOI[];          /* [0, 2, 3, 7, 9] */

/* Other world scales */
extern const int SCALE_EGYPTIAN[];       /* [0, 2, 5, 7, 10] */
extern const int SCALE_ROMANIAN_MINOR[]; /* [0, 2, 3, 6, 7, 9, 10] */
extern const int SCALE_SPANISH_8_TONE[]; /* [0, 1, 3, 4, 5, 6, 8, 10] */
extern const int SCALE_ENIGMATIC[];      /* [0, 1, 4, 6, 8, 10, 11] */

/* Arabic Maqamat (12-TET approximations) */
extern const int SCALE_MAQAM_HIJAZ[];      /* [0, 1, 4, 5, 7, 8, 10] */
extern const int SCALE_MAQAM_NAHAWAND[];   /* [0, 2, 3, 5, 7, 8, 11] */
extern const int SCALE_MAQAM_NIKRIZ[];     /* [0, 2, 3, 6, 7, 9, 10] */
extern const int SCALE_MAQAM_ATHAR_KURD[]; /* [0, 1, 3, 5, 6, 8, 10] */
extern const int SCALE_MAQAM_SHAWQ_AFZA[]; /* [0, 2, 3, 6, 7, 9, 11] */
extern const int SCALE_MAQAM_JIHARKAH[];   /* [0, 2, 4, 5, 7, 9, 10] - approx */

/* Indian Ragas (12-TET approximations) */
extern const int SCALE_RAGA_BHAIRAV[];     /* [0, 1, 4, 5, 7, 8, 11] */
extern const int SCALE_RAGA_TODI[];        /* [0, 1, 3, 6, 7, 8, 11] */
extern const int SCALE_RAGA_MARWA[];       /* [0, 1, 4, 6, 7, 9, 11] */
extern const int SCALE_RAGA_PURVI[];       /* [0, 1, 4, 6, 7, 8, 11] */
extern const int SCALE_RAGA_CHARUKESHI[];  /* [0, 2, 4, 5, 7, 8, 10] */
extern const int SCALE_RAGA_ASAVARI[];     /* [0, 2, 3, 5, 7, 8, 10] - same as minor */
extern const int SCALE_RAGA_BILAWAL[];     /* [0, 2, 4, 5, 7, 9, 11] - same as major */
extern const int SCALE_RAGA_KHAMAJ[];      /* [0, 2, 4, 5, 7, 9, 10] */
extern const int SCALE_RAGA_KALYAN[];      /* [0, 2, 4, 6, 7, 9, 11] - same as lydian */
extern const int SCALE_RAGA_BHIMPALASI[];  /* [0, 3, 5, 7, 10] - pentatonic */
extern const int SCALE_RAGA_DARBARI[];     /* [0, 2, 3, 5, 7, 8, 9] */

/* Scale sizes */
#define SCALE_DIATONIC_SIZE    7
#define SCALE_PENTATONIC_SIZE  5
#define SCALE_BLUES_SIZE       6
#define SCALE_WHOLE_TONE_SIZE  6
#define SCALE_CHROMATIC_SIZE  12
#define SCALE_DIMINISHED_SIZE  8
#define SCALE_AUGMENTED_SIZE   6
#define SCALE_BEBOP_SIZE       8

/*
 * Build a scale from a root pitch and interval array (one octave).
 *
 * Parameters:
 *   root        - Root pitch (MIDI number 0-127)
 *   intervals   - Array of semitone intervals
 *   num_notes   - Number of notes in the scale
 *   out_pitches - Output array for scale pitches (must be at least num_notes)
 *
 * Returns: Number of valid pitches written (may be less than num_notes
 *          if some would exceed MIDI range)
 */
int music_build_scale(int root, const int* intervals, int num_notes, int* out_pitches);

/*
 * Get a specific scale degree.
 *
 * Parameters:
 *   root      - Root pitch (MIDI number 0-127)
 *   intervals - Array of semitone intervals
 *   num_notes - Number of notes in the scale
 *   degree    - Scale degree (1-based: 1=root, 2=second, etc.)
 *               Supports degrees beyond the octave (e.g., 9 = 2nd + octave)
 *
 * Returns: MIDI pitch for the degree, or -1 if out of range
 */
int music_scale_degree(int root, const int* intervals, int num_notes, int degree);

/*
 * Check if a pitch belongs to a scale (in any octave).
 *
 * Parameters:
 *   pitch     - MIDI pitch to check (0-127)
 *   root      - Root pitch of the scale
 *   intervals - Array of semitone intervals
 *   num_notes - Number of notes in the scale
 *
 * Returns: 1 if pitch is in scale, 0 otherwise
 */
int music_in_scale(int pitch, int root, const int* intervals, int num_notes);

/*
 * Quantize a pitch to the nearest scale tone.
 *
 * Parameters:
 *   pitch     - MIDI pitch to quantize (0-127)
 *   root      - Root pitch of the scale
 *   intervals - Array of semitone intervals
 *   num_notes - Number of notes in the scale
 *
 * Returns: Nearest pitch that belongs to the scale
 */
int music_quantize_to_scale(int pitch, int root, const int* intervals, int num_notes);

/* ============================================================================
 * Microtonal Scales (Cents-Based)
 * ============================================================================
 *
 * For scales with quarter tones or other microtonal intervals, we use cents.
 * 100 cents = 1 semitone, 50 cents = quarter tone.
 *
 * These scales require pitch bend for accurate playback:
 *   - Calculate base MIDI note (cents / 100)
 *   - Apply pitch bend for the fractional part
 */

/* Arabic Maqamat with quarter tones (cents) */
extern const int SCALE_MAQAM_BAYATI_CENTS[];    /* 0, 150, 300, 500, 700, 800, 1000 */
extern const int SCALE_MAQAM_RAST_CENTS[];      /* 0, 200, 350, 500, 700, 900, 1050 */
extern const int SCALE_MAQAM_SABA_CENTS[];      /* 0, 150, 300, 400, 500, 700, 800 */
extern const int SCALE_MAQAM_SIKAH_CENTS[];     /* 0, 150, 350, 500, 650, 850, 1000 */
extern const int SCALE_MAQAM_HUZAM_CENTS[];     /* 0, 150, 350, 500, 700, 850, 1050 */
extern const int SCALE_MAQAM_IRAQ_CENTS[];      /* 0, 150, 350, 500, 700, 850, 1000 */
extern const int SCALE_MAQAM_BASTANIKAR_CENTS[];/* 0, 150, 350, 500, 700, 800, 1000 */

/* Turkish Makamlar with commas (cents) */
extern const int SCALE_MAKAM_USSAK_CENTS[];     /* 0, 150, 300, 500, 700, 800, 1000 */
extern const int SCALE_MAKAM_HUSEYNI_CENTS[];   /* 0, 150, 300, 500, 700, 900, 1000 */

/* Indian Shrutis - 22-shruti scale (cents, subset shown) */
extern const int SCALE_SHRUTI_CENTS[];          /* 22 intervals in cents */
#define SCALE_SHRUTI_SIZE 22

/* Microtonal helper constants */
#define CENTS_PER_SEMITONE   100
#define CENTS_PER_QUARTERTONE 50
#define CENTS_PER_OCTAVE    1200

/*
 * Microtonal note representation.
 * Combines MIDI note with pitch bend offset for sub-semitone accuracy.
 */
typedef struct {
    int midi_note;     /* Base MIDI note (0-127) */
    int bend_cents;    /* Pitch bend in cents (-100 to +100) */
} MicrotonalNote;

/*
 * Convert cents interval to a microtonal note.
 *
 * Parameters:
 *   root_midi - Root MIDI note (0-127)
 *   cents     - Interval in cents from root
 *
 * Returns: MicrotonalNote with base MIDI note and bend offset
 */
MicrotonalNote music_cents_to_note(int root_midi, int cents);

/*
 * Calculate pitch bend value for a cents offset.
 * Assumes standard +/- 2 semitone bend range.
 *
 * Parameters:
 *   cents - Cents offset (-200 to +200 for standard bend range)
 *
 * Returns: MIDI pitch bend value (0-16383, center = 8192)
 */
int music_cents_to_bend(int cents);

/*
 * Build a microtonal scale with pitch bend values.
 *
 * Parameters:
 *   root_midi   - Root MIDI note (0-127)
 *   cents       - Array of intervals in cents
 *   num_notes   - Number of notes in scale
 *   out_notes   - Output array of MicrotonalNote (must be at least num_notes)
 *
 * Returns: Number of valid notes written
 */
int music_build_microtonal_scale(int root_midi, const int* cents, int num_notes,
                                  MicrotonalNote* out_notes);

/*
 * Get a specific degree from a microtonal scale.
 *
 * Parameters:
 *   root_midi - Root MIDI note
 *   cents     - Array of intervals in cents
 *   num_notes - Number of notes in scale
 *   degree    - Scale degree (1-based)
 *
 * Returns: MicrotonalNote for the degree
 */
MicrotonalNote music_microtonal_degree(int root_midi, const int* cents,
                                        int num_notes, int degree);

/* ============================================================================
 * Dynamics (Velocity Constants)
 * ============================================================================ */

#define DYN_PPP  16   /* Pianississimo */
#define DYN_PP   33   /* Pianissimo */
#define DYN_P    49   /* Piano */
#define DYN_MP   64   /* Mezzo-piano */
#define DYN_MF   80   /* Mezzo-forte */
#define DYN_F    96   /* Forte */
#define DYN_FF  112   /* Fortissimo */
#define DYN_FFF 127   /* Fortississimo */

/*
 * Parse a dynamics name to a velocity value.
 *
 * Accepts: "ppp", "pp", "p", "mp", "mf", "f", "ff", "fff"
 *
 * Returns: Velocity (0-127) on success, -1 on error
 */
int music_parse_dynamics(const char* name);

/* ============================================================================
 * Duration Constants (milliseconds at 120 BPM)
 * ============================================================================ */

#define DUR_WHOLE      2000  /* Whole note */
#define DUR_HALF       1000  /* Half note */
#define DUR_QUARTER     500  /* Quarter note */
#define DUR_EIGHTH      250  /* Eighth note */
#define DUR_SIXTEENTH   125  /* Sixteenth note */

/*
 * Calculate duration in milliseconds for a given BPM.
 *
 * Parameters:
 *   beats - Number of beats (1.0 = quarter note)
 *   bpm   - Tempo in beats per minute
 *
 * Returns: Duration in milliseconds
 */
int music_duration_ms(double beats, int bpm);

/*
 * Calculate a dotted duration (1.5x the original).
 */
static inline int music_dotted(int duration) {
    return duration + duration / 2;
}

/* ============================================================================
 * MIDI Note Constants
 * ============================================================================
 *
 * MIDI note numbers for common pitches.
 * Format: NOTE_<name><octave>
 *
 * Middle C (C4) = 60
 */

/* Octave -1 (MIDI 0-11) */
#define NOTE_C_1   0
#define NOTE_CS_1  1
#define NOTE_DB_1  1
#define NOTE_D_1   2
#define NOTE_DS_1  3
#define NOTE_EB_1  3
#define NOTE_E_1   4
#define NOTE_F_1   5
#define NOTE_FS_1  6
#define NOTE_GB_1  6
#define NOTE_G_1   7
#define NOTE_GS_1  8
#define NOTE_AB_1  8
#define NOTE_A_1   9
#define NOTE_AS_1 10
#define NOTE_BB_1 10
#define NOTE_B_1  11

/* Octave 0 (MIDI 12-23) */
#define NOTE_C0  12
#define NOTE_CS0 13
#define NOTE_DB0 13
#define NOTE_D0  14
#define NOTE_DS0 15
#define NOTE_EB0 15
#define NOTE_E0  16
#define NOTE_F0  17
#define NOTE_FS0 18
#define NOTE_GB0 18
#define NOTE_G0  19
#define NOTE_GS0 20
#define NOTE_AB0 20
#define NOTE_A0  21
#define NOTE_AS0 22
#define NOTE_BB0 22
#define NOTE_B0  23

/* Octave 1 (MIDI 24-35) */
#define NOTE_C1  24
#define NOTE_CS1 25
#define NOTE_DB1 25
#define NOTE_D1  26
#define NOTE_DS1 27
#define NOTE_EB1 27
#define NOTE_E1  28
#define NOTE_F1  29
#define NOTE_FS1 30
#define NOTE_GB1 30
#define NOTE_G1  31
#define NOTE_GS1 32
#define NOTE_AB1 32
#define NOTE_A1  33
#define NOTE_AS1 34
#define NOTE_BB1 34
#define NOTE_B1  35

/* Octave 2 (MIDI 36-47) */
#define NOTE_C2  36
#define NOTE_CS2 37
#define NOTE_DB2 37
#define NOTE_D2  38
#define NOTE_DS2 39
#define NOTE_EB2 39
#define NOTE_E2  40
#define NOTE_F2  41
#define NOTE_FS2 42
#define NOTE_GB2 42
#define NOTE_G2  43
#define NOTE_GS2 44
#define NOTE_AB2 44
#define NOTE_A2  45
#define NOTE_AS2 46
#define NOTE_BB2 46
#define NOTE_B2  47

/* Octave 3 (MIDI 48-59) */
#define NOTE_C3  48
#define NOTE_CS3 49
#define NOTE_DB3 49
#define NOTE_D3  50
#define NOTE_DS3 51
#define NOTE_EB3 51
#define NOTE_E3  52
#define NOTE_F3  53
#define NOTE_FS3 54
#define NOTE_GB3 54
#define NOTE_G3  55
#define NOTE_GS3 56
#define NOTE_AB3 56
#define NOTE_A3  57
#define NOTE_AS3 58
#define NOTE_BB3 58
#define NOTE_B3  59

/* Octave 4 - Middle C (MIDI 60-71) */
#define NOTE_C4  60
#define NOTE_CS4 61
#define NOTE_DB4 61
#define NOTE_D4  62
#define NOTE_DS4 63
#define NOTE_EB4 63
#define NOTE_E4  64
#define NOTE_F4  65
#define NOTE_FS4 66
#define NOTE_GB4 66
#define NOTE_G4  67
#define NOTE_GS4 68
#define NOTE_AB4 68
#define NOTE_A4  69
#define NOTE_AS4 70
#define NOTE_BB4 70
#define NOTE_B4  71

/* Octave 5 (MIDI 72-83) */
#define NOTE_C5  72
#define NOTE_CS5 73
#define NOTE_DB5 73
#define NOTE_D5  74
#define NOTE_DS5 75
#define NOTE_EB5 75
#define NOTE_E5  76
#define NOTE_F5  77
#define NOTE_FS5 78
#define NOTE_GB5 78
#define NOTE_G5  79
#define NOTE_GS5 80
#define NOTE_AB5 80
#define NOTE_A5  81
#define NOTE_AS5 82
#define NOTE_BB5 82
#define NOTE_B5  83

/* Octave 6 (MIDI 84-95) */
#define NOTE_C6  84
#define NOTE_CS6 85
#define NOTE_DB6 85
#define NOTE_D6  86
#define NOTE_DS6 87
#define NOTE_EB6 87
#define NOTE_E6  88
#define NOTE_F6  89
#define NOTE_FS6 90
#define NOTE_GB6 90
#define NOTE_G6  91
#define NOTE_GS6 92
#define NOTE_AB6 92
#define NOTE_A6  93
#define NOTE_AS6 94
#define NOTE_BB6 94
#define NOTE_B6  95

/* Octave 7 (MIDI 96-107) */
#define NOTE_C7  96
#define NOTE_CS7 97
#define NOTE_DB7 97
#define NOTE_D7  98
#define NOTE_DS7 99
#define NOTE_EB7 99
#define NOTE_E7 100
#define NOTE_F7 101
#define NOTE_FS7 102
#define NOTE_GB7 102
#define NOTE_G7 103
#define NOTE_GS7 104
#define NOTE_AB7 104
#define NOTE_A7 105
#define NOTE_AS7 106
#define NOTE_BB7 106
#define NOTE_B7 107

/* Octave 8 (MIDI 108-119) */
#define NOTE_C8 108
#define NOTE_CS8 109
#define NOTE_DB8 109
#define NOTE_D8 110
#define NOTE_DS8 111
#define NOTE_EB8 111
#define NOTE_E8 112
#define NOTE_F8 113
#define NOTE_FS8 114
#define NOTE_GB8 114
#define NOTE_G8 115
#define NOTE_GS8 116
#define NOTE_AB8 116
#define NOTE_A8 117
#define NOTE_AS8 118
#define NOTE_BB8 118
#define NOTE_B8 119

/* Octave 9 (partial, MIDI 120-127) */
#define NOTE_C9 120
#define NOTE_CS9 121
#define NOTE_DB9 121
#define NOTE_D9 122
#define NOTE_DS9 123
#define NOTE_EB9 123
#define NOTE_E9 124
#define NOTE_F9 125
#define NOTE_FS9 126
#define NOTE_GB9 126
#define NOTE_G9 127

/* ============================================================================
 * Common CC Numbers
 * ============================================================================ */

#define CC_MODULATION   1
#define CC_BREATH       2
#define CC_VOLUME       7
#define CC_PAN         10
#define CC_EXPRESSION  11
#define CC_SUSTAIN     64
#define CC_REVERB      91
#define CC_CHORUS      93
#define CC_ALL_NOTES_OFF 123

#ifdef __cplusplus
}
#endif

#endif /* MUSIC_THEORY_H */
