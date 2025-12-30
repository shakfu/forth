/* packed_notes.c - Packed note operations for MIDI Forth interpreter */

#include "forth_midi.h"

/* note ( pitch vel ch dur -- packed-note ) */
void op_note(Stack* stack) {
    int32_t dur = pop(stack);
    int32_t ch = pop(stack);
    int32_t vel = pop(stack);
    int32_t pitch = pop(stack);
    push(stack, pack_note(pitch, vel, ch - 1, dur));  /* ch 1-16 -> 0-15 */
}

/* pitch@ ( packed-note -- pitch ) */
void op_pitch_fetch(Stack* stack) {
    int32_t n = pop(stack);
    push(stack, note_pitch(n));
}

/* vel@ ( packed-note -- velocity ) */
void op_vel_fetch(Stack* stack) {
    int32_t n = pop(stack);
    push(stack, note_vel(n));
}

/* ch@ ( packed-note -- channel ) returns 1-16 */
void op_ch_fetch(Stack* stack) {
    int32_t n = pop(stack);
    push(stack, note_ch(n) + 1);
}

/* dur@ ( packed-note -- duration ) */
void op_dur_fetch(Stack* stack) {
    int32_t n = pop(stack);
    push(stack, note_dur(n));
}

/* note. ( packed-note -- ) print note info */
void op_note_print(Stack* stack) {
    int32_t n = pop(stack);
    printf("note: pitch=%d vel=%d ch=%d dur=%d",
           note_pitch(n), note_vel(n), note_ch(n) + 1, note_dur(n));
}

/* transpose ( value semitones -- value )
 * Polymorphic: works on packed notes or bracket sequences */
void op_transpose(Stack* stack) {
    if (stack->top < 1) {
        printf("transpose needs a value and semitones\n");
        return;
    }

    int32_t semi = pop(stack);
    int32_t val = pop(stack);

    /* Check if it's a bracket sequence */
    if ((val & 0xFF000000) == SEQ_MARKER) {
        int idx = val & 0x00FFFFFF;
        if (idx < 0 || idx >= bracket_seq_count || !bracket_seq_storage[idx]) {
            printf("Invalid sequence\n");
            push(stack, 0);
            return;
        }

        BracketSequence* seq = bracket_seq_storage[idx];

        /* Transpose pitches in place */
        for (int i = 0; i < seq->count; i++) {
            SeqElement* elem = &seq->elements[i];
            if (elem->type == SEQ_ELEM_PITCH) {
                int new_val = elem->value + semi;
                if (new_val < 0) new_val = 0;
                if (new_val > 127) new_val = 127;
                elem->value = new_val;
            }
            if (elem->type == SEQ_ELEM_CHORD) {
                for (int j = 0; j < elem->chord_count; j++) {
                    int new_val = elem->chord_pitches[j] + semi;
                    if (new_val < 0) new_val = 0;
                    if (new_val > 127) new_val = 127;
                    elem->chord_pitches[j] = new_val;
                }
            }
        }

        push(stack, val);
        return;
    }

    /* Otherwise treat as packed note */
    int new_pitch = note_pitch(val) + semi;
    if (new_pitch < 0) new_pitch = 0;
    if (new_pitch > 127) new_pitch = 127;
    push(stack, pack_note(new_pitch, note_vel(val), note_ch(val), note_dur(val)));
}

/* note! ( packed-note -- ) play the note immediately (blocking) */
void op_note_play(Stack* stack) {
    int32_t n = pop(stack);

    int pitch = note_pitch(n);
    int vel = note_vel(n);
    int ch = note_ch(n);
    int dur = note_dur(n);

    /* Update current pitch for relative intervals */
    current_pitch = pitch;

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    /* Convert ticks to ms: (ticks / PPQ) * (60000 / BPM) */
    int ms = (dur * 60000) / (TICKS_PER_QUARTER * global_bpm);

    /* Note on */
    midi_send_note_on(pitch, vel, ch + 1);
    capture_add_event(0, ch, pitch, vel);

    /* Wait */
    midi_sleep_ms(ms);

    /* Note off */
    midi_send_note_off(pitch, ch + 1);
    capture_add_event(1, ch, pitch, 0);
}
