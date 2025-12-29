/* patterns.c - Pattern and chord builders for MIDI Forth interpreter */

#include "forth_midi.h"

/* Duration helpers - push tick values */
void op_quarter(Stack* stack) { push(stack, TICKS_PER_QUARTER); }
void op_half(Stack* stack) { push(stack, TICKS_PER_QUARTER * 2); }
void op_whole(Stack* stack) { push(stack, TICKS_PER_QUARTER * 4); }
void op_eighth(Stack* stack) { push(stack, TICKS_PER_QUARTER / 2); }
void op_sixteenth(Stack* stack) { push(stack, TICKS_PER_QUARTER / 4); }

/* Chord builders - push pitches relative to root */

/* major ( root -- root root+4 root+7 ) */
void op_chord_major(Stack* stack) {
    int32_t root = pop(stack);
    push(stack, root);
    push(stack, root + 4);
    push(stack, root + 7);
}

/* minor ( root -- root root+3 root+7 ) */
void op_chord_minor(Stack* stack) {
    int32_t root = pop(stack);
    push(stack, root);
    push(stack, root + 3);
    push(stack, root + 7);
}

/* dim ( root -- root root+3 root+6 ) */
void op_chord_dim(Stack* stack) {
    int32_t root = pop(stack);
    push(stack, root);
    push(stack, root + 3);
    push(stack, root + 6);
}

/* aug ( root -- root root+4 root+8 ) */
void op_chord_aug(Stack* stack) {
    int32_t root = pop(stack);
    push(stack, root);
    push(stack, root + 4);
    push(stack, root + 8);
}

/* dom7 ( root -- root root+4 root+7 root+10 ) */
void op_chord_7(Stack* stack) {
    int32_t root = pop(stack);
    push(stack, root);
    push(stack, root + 4);
    push(stack, root + 7);
    push(stack, root + 10);
}

/* maj7 ( root -- root root+4 root+7 root+11 ) */
void op_chord_maj7(Stack* stack) {
    int32_t root = pop(stack);
    push(stack, root);
    push(stack, root + 4);
    push(stack, root + 7);
    push(stack, root + 11);
}

/* min7 ( root -- root root+3 root+7 root+10 ) */
void op_chord_min7(Stack* stack) {
    int32_t root = pop(stack);
    push(stack, root);
    push(stack, root + 3);
    push(stack, root + 7);
    push(stack, root + 10);
}

/* play-chord ( p1 p2 p3 vel dur n -- ) play n notes as chord */
void op_play_chord(Stack* stack) {
    int32_t n = pop(stack);
    int32_t dur = pop(stack);
    int32_t vel = pop(stack);

    if (n < 1 || n > 8) {
        printf("Chord size must be 1-8\n");
        return;
    }

    int pitches[8];
    for (int i = n - 1; i >= 0; i--) {
        pitches[i] = pop(stack);
    }

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    int ms = (dur * 60000) / (TICKS_PER_QUARTER * global_bpm);

    /* All notes on */
    for (int i = 0; i < n; i++) {
        midi_send_note_on(pitches[i], vel, 1);
        capture_add_event(0, 0, pitches[i], vel);
    }

    midi_sleep_ms(ms);

    /* All notes off */
    for (int i = 0; i < n; i++) {
        midi_send_note_off(pitches[i], 1);
        capture_add_event(1, 0, pitches[i], 0);
    }
}

/* chord>seq ( p1 p2 ... pn vel dur time n -- ) add chord to sequence */
void op_chord_to_seq(Stack* stack) {
    int32_t n = pop(stack);
    int32_t time = pop(stack);
    int32_t dur = pop(stack);
    int32_t vel = pop(stack);

    if (n < 1 || n > 8) {
        printf("Chord size must be 1-8\n");
        return;
    }
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }

    int pitches[8];
    for (int i = n - 1; i >= 0; i--) {
        pitches[i] = pop(stack);
    }

    Sequence* seq = &sequences[current_seq];
    for (int i = 0; i < n; i++) {
        if (seq->length >= MAX_SEQ_EVENTS - 2) {
            printf("Sequence full\n");
            return;
        }
        MidiEvent* e_on = &seq->events[seq->length++];
        e_on->time = time;
        e_on->type = EVT_NOTE_ON;
        e_on->channel = 0;
        e_on->data1 = pitches[i];
        e_on->data2 = vel;

        MidiEvent* e_off = &seq->events[seq->length++];
        e_off->time = time + dur;
        e_off->type = EVT_NOTE_OFF;
        e_off->channel = 0;
        e_off->data1 = pitches[i];
        e_off->data2 = 0;
    }
}

/* arp>seq ( p1 p2 ... pn vel notedur spacing time n -- ) add arpeggio to sequence */
void op_arp_to_seq(Stack* stack) {
    int32_t n = pop(stack);
    int32_t time = pop(stack);
    int32_t spacing = pop(stack);
    int32_t notedur = pop(stack);
    int32_t vel = pop(stack);

    if (n < 1 || n > 8) {
        printf("Arp size must be 1-8\n");
        return;
    }
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }

    int pitches[8];
    for (int i = n - 1; i >= 0; i--) {
        pitches[i] = pop(stack);
    }

    Sequence* seq = &sequences[current_seq];
    for (int i = 0; i < n; i++) {
        if (seq->length >= MAX_SEQ_EVENTS - 2) {
            printf("Sequence full\n");
            return;
        }
        int t = time + (i * spacing);

        MidiEvent* e_on = &seq->events[seq->length++];
        e_on->time = t;
        e_on->type = EVT_NOTE_ON;
        e_on->channel = 0;
        e_on->data1 = pitches[i];
        e_on->data2 = vel;

        MidiEvent* e_off = &seq->events[seq->length++];
        e_off->time = t + notedur;
        e_off->type = EVT_NOTE_OFF;
        e_off->channel = 0;
        e_off->data1 = pitches[i];
        e_off->data2 = 0;
    }
}
