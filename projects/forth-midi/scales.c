/* scales.c - Scale operations for MIDI Forth interpreter */

#include "forth_midi.h"

/* Scale lookup table entry */
typedef struct {
    const char* name;
    const int* intervals;
    int size;
} ScaleInfo;

/* Scale IDs - must match order in scale_table */
enum {
    SCALE_ID_MAJOR = 0,
    SCALE_ID_DORIAN,
    SCALE_ID_PHRYGIAN,
    SCALE_ID_LYDIAN,
    SCALE_ID_MIXOLYDIAN,
    SCALE_ID_MINOR,
    SCALE_ID_LOCRIAN,
    SCALE_ID_HARMONIC_MINOR,
    SCALE_ID_MELODIC_MINOR,
    SCALE_ID_PENTATONIC_MAJOR,
    SCALE_ID_PENTATONIC_MINOR,
    SCALE_ID_BLUES,
    SCALE_ID_WHOLE_TONE,
    SCALE_ID_CHROMATIC,
    SCALE_ID_DIMINISHED_HW,
    SCALE_ID_DIMINISHED_WH,
    SCALE_ID_AUGMENTED,
    SCALE_ID_BEBOP_DOMINANT,
    SCALE_ID_BEBOP_MAJOR,
    SCALE_ID_BEBOP_MINOR,
    SCALE_ID_HUNGARIAN_MINOR,
    SCALE_ID_DOUBLE_HARMONIC,
    SCALE_ID_NEAPOLITAN_MAJOR,
    SCALE_ID_NEAPOLITAN_MINOR,
    SCALE_ID_COUNT
};

/* Scale lookup table */
static const ScaleInfo scale_table[] = {
    { "major",            SCALE_MAJOR,            SCALE_DIATONIC_SIZE },
    { "dorian",           SCALE_DORIAN,           SCALE_DIATONIC_SIZE },
    { "phrygian",         SCALE_PHRYGIAN,         SCALE_DIATONIC_SIZE },
    { "lydian",           SCALE_LYDIAN,           SCALE_DIATONIC_SIZE },
    { "mixolydian",       SCALE_MIXOLYDIAN,       SCALE_DIATONIC_SIZE },
    { "minor",            SCALE_MINOR,            SCALE_DIATONIC_SIZE },
    { "locrian",          SCALE_LOCRIAN,          SCALE_DIATONIC_SIZE },
    { "harmonic-minor",   SCALE_HARMONIC_MINOR,   SCALE_DIATONIC_SIZE },
    { "melodic-minor",    SCALE_MELODIC_MINOR,    SCALE_DIATONIC_SIZE },
    { "pentatonic",       SCALE_PENTATONIC_MAJOR, SCALE_PENTATONIC_SIZE },
    { "pentatonic-minor", SCALE_PENTATONIC_MINOR, SCALE_PENTATONIC_SIZE },
    { "blues",            SCALE_BLUES,            SCALE_BLUES_SIZE },
    { "whole-tone",       SCALE_WHOLE_TONE,       SCALE_WHOLE_TONE_SIZE },
    { "chromatic",        SCALE_CHROMATIC,        SCALE_CHROMATIC_SIZE },
    { "diminished-hw",    SCALE_DIMINISHED_HW,    SCALE_DIMINISHED_SIZE },
    { "diminished-wh",    SCALE_DIMINISHED_WH,    SCALE_DIMINISHED_SIZE },
    { "augmented",        SCALE_AUGMENTED,        SCALE_AUGMENTED_SIZE },
    { "bebop-dominant",   SCALE_BEBOP_DOMINANT,   SCALE_BEBOP_SIZE },
    { "bebop-major",      SCALE_BEBOP_MAJOR,      SCALE_BEBOP_SIZE },
    { "bebop-minor",      SCALE_BEBOP_MINOR,      SCALE_BEBOP_SIZE },
    { "hungarian-minor",  SCALE_HUNGARIAN_MINOR,  SCALE_DIATONIC_SIZE },
    { "double-harmonic",  SCALE_DOUBLE_HARMONIC,  SCALE_DIATONIC_SIZE },
    { "neapolitan-major", SCALE_NEAPOLITAN_MAJOR, SCALE_DIATONIC_SIZE },
    { "neapolitan-minor", SCALE_NEAPOLITAN_MINOR, SCALE_DIATONIC_SIZE },
};

/* Scale constant words - push scale ID */
void op_scale_major(Stack* stack) { push(stack, SCALE_ID_MAJOR); }
void op_scale_dorian(Stack* stack) { push(stack, SCALE_ID_DORIAN); }
void op_scale_phrygian(Stack* stack) { push(stack, SCALE_ID_PHRYGIAN); }
void op_scale_lydian(Stack* stack) { push(stack, SCALE_ID_LYDIAN); }
void op_scale_mixolydian(Stack* stack) { push(stack, SCALE_ID_MIXOLYDIAN); }
void op_scale_minor(Stack* stack) { push(stack, SCALE_ID_MINOR); }
void op_scale_locrian(Stack* stack) { push(stack, SCALE_ID_LOCRIAN); }
void op_scale_harmonic_minor(Stack* stack) { push(stack, SCALE_ID_HARMONIC_MINOR); }
void op_scale_melodic_minor(Stack* stack) { push(stack, SCALE_ID_MELODIC_MINOR); }
void op_scale_pentatonic(Stack* stack) { push(stack, SCALE_ID_PENTATONIC_MAJOR); }
void op_scale_pentatonic_minor(Stack* stack) { push(stack, SCALE_ID_PENTATONIC_MINOR); }
void op_scale_blues(Stack* stack) { push(stack, SCALE_ID_BLUES); }
void op_scale_whole_tone(Stack* stack) { push(stack, SCALE_ID_WHOLE_TONE); }
void op_scale_chromatic(Stack* stack) { push(stack, SCALE_ID_CHROMATIC); }
void op_scale_diminished_hw(Stack* stack) { push(stack, SCALE_ID_DIMINISHED_HW); }
void op_scale_diminished_wh(Stack* stack) { push(stack, SCALE_ID_DIMINISHED_WH); }
void op_scale_augmented_scale(Stack* stack) { push(stack, SCALE_ID_AUGMENTED); }
void op_scale_bebop_dominant(Stack* stack) { push(stack, SCALE_ID_BEBOP_DOMINANT); }
void op_scale_bebop_major(Stack* stack) { push(stack, SCALE_ID_BEBOP_MAJOR); }
void op_scale_bebop_minor(Stack* stack) { push(stack, SCALE_ID_BEBOP_MINOR); }
void op_scale_hungarian_minor(Stack* stack) { push(stack, SCALE_ID_HUNGARIAN_MINOR); }
void op_scale_double_harmonic(Stack* stack) { push(stack, SCALE_ID_DOUBLE_HARMONIC); }
void op_scale_neapolitan_major(Stack* stack) { push(stack, SCALE_ID_NEAPOLITAN_MAJOR); }
void op_scale_neapolitan_minor(Stack* stack) { push(stack, SCALE_ID_NEAPOLITAN_MINOR); }

/* scale ( root scale-id -- p1 p2 ... pN N ) build scale and push all pitches + count */
static void op_scale(Stack* stack) {
    int32_t scale_id = pop(stack);
    int32_t root = pop(stack);

    if (scale_id < 0 || scale_id >= SCALE_ID_COUNT) {
        printf("Invalid scale ID: %d\n", scale_id);
        push(stack, 0);
        return;
    }

    const ScaleInfo* info = &scale_table[scale_id];
    int pitches[16];
    int count = music_build_scale(root, info->intervals, info->size, pitches);

    for (int i = 0; i < count; i++) {
        push(stack, pitches[i]);
    }
    push(stack, count);
}

/* degree ( root scale-id degree -- pitch ) get nth degree of scale (1-based) */
static void op_degree(Stack* stack) {
    int32_t deg = pop(stack);
    int32_t scale_id = pop(stack);
    int32_t root = pop(stack);

    if (scale_id < 0 || scale_id >= SCALE_ID_COUNT) {
        printf("Invalid scale ID: %d\n", scale_id);
        push(stack, root);
        return;
    }

    const ScaleInfo* info = &scale_table[scale_id];
    int pitch = music_scale_degree(root, info->intervals, info->size, deg);
    push(stack, pitch >= 0 ? pitch : root);
}

/* in-scale? ( pitch root scale-id -- flag ) check if pitch is in scale */
static void op_in_scale(Stack* stack) {
    int32_t scale_id = pop(stack);
    int32_t root = pop(stack);
    int32_t pitch = pop(stack);

    if (scale_id < 0 || scale_id >= SCALE_ID_COUNT) {
        printf("Invalid scale ID: %d\n", scale_id);
        push(stack, 0);
        return;
    }

    const ScaleInfo* info = &scale_table[scale_id];
    int result = music_in_scale(pitch, root, info->intervals, info->size);
    push(stack, result ? -1 : 0);  /* Forth true = -1 */
}

/* quantize ( pitch root scale-id -- quantized-pitch ) */
static void op_quantize(Stack* stack) {
    int32_t scale_id = pop(stack);
    int32_t root = pop(stack);
    int32_t pitch = pop(stack);

    if (scale_id < 0 || scale_id >= SCALE_ID_COUNT) {
        printf("Invalid scale ID: %d\n", scale_id);
        push(stack, pitch);
        return;
    }

    const ScaleInfo* info = &scale_table[scale_id];
    int result = music_quantize_to_scale(pitch, root, info->intervals, info->size);
    push(stack, result);
}

/* scales ( -- ) list all available scales */
static void op_scales(Stack* stack) {
    (void)stack;
    printf("Available scales (%d total):\n", SCALE_ID_COUNT);
    for (int i = 0; i < SCALE_ID_COUNT; i++) {
        printf("  %2d: scale-%s\n", i, scale_table[i].name);
    }
}

/* Register all scale words */
void register_scale_words(void) {
    /* Scale constants */
    add_word("scale-major", op_scale_major, 1);
    add_word("scale-dorian", op_scale_dorian, 1);
    add_word("scale-phrygian", op_scale_phrygian, 1);
    add_word("scale-lydian", op_scale_lydian, 1);
    add_word("scale-mixolydian", op_scale_mixolydian, 1);
    add_word("scale-minor", op_scale_minor, 1);
    add_word("scale-locrian", op_scale_locrian, 1);
    add_word("scale-harmonic-minor", op_scale_harmonic_minor, 1);
    add_word("scale-melodic-minor", op_scale_melodic_minor, 1);
    add_word("scale-pentatonic", op_scale_pentatonic, 1);
    add_word("scale-pentatonic-minor", op_scale_pentatonic_minor, 1);
    add_word("scale-blues", op_scale_blues, 1);
    add_word("scale-whole-tone", op_scale_whole_tone, 1);
    add_word("scale-chromatic", op_scale_chromatic, 1);
    add_word("scale-diminished-hw", op_scale_diminished_hw, 1);
    add_word("scale-diminished-wh", op_scale_diminished_wh, 1);
    add_word("scale-augmented", op_scale_augmented_scale, 1);
    add_word("scale-bebop-dominant", op_scale_bebop_dominant, 1);
    add_word("scale-bebop-major", op_scale_bebop_major, 1);
    add_word("scale-bebop-minor", op_scale_bebop_minor, 1);
    add_word("scale-hungarian-minor", op_scale_hungarian_minor, 1);
    add_word("scale-double-harmonic", op_scale_double_harmonic, 1);
    add_word("scale-neapolitan-major", op_scale_neapolitan_major, 1);
    add_word("scale-neapolitan-minor", op_scale_neapolitan_minor, 1);

    /* Scale operations */
    add_word("scale", op_scale, 1);
    add_word("degree", op_degree, 1);
    add_word("in-scale?", op_in_scale, 1);
    add_word("quantize", op_quantize, 1);
    add_word("scales", op_scales, 1);
}
