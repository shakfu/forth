/* sequences.c - Sequence operations for MIDI Forth interpreter */

#include "forth_midi.h"

/* Sequence globals - defined here */
Sequence sequences[MAX_SEQUENCES];
int sequence_count = 0;
int current_seq = -1;
int global_bpm = 120;

/* Helper to sort events by time (simple insertion sort) */
static void seq_sort(Sequence* seq) {
    for (int i = 1; i < seq->length; i++) {
        MidiEvent tmp = seq->events[i];
        int j = i - 1;
        while (j >= 0 && seq->events[j].time > tmp.time) {
            seq->events[j + 1] = seq->events[j];
            j--;
        }
        seq->events[j + 1] = tmp;
    }
}

/* Helper to add event to current sequence */
static void add_event(int time, int type, int ch, int d1, int d2) {
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }
    Sequence* seq = &sequences[current_seq];
    if (seq->length >= MAX_SEQ_EVENTS) {
        printf("Sequence full\n");
        return;
    }
    MidiEvent* e = &seq->events[seq->length++];
    e->time = time;
    e->type = type;
    e->channel = ch;
    e->data1 = d1;
    e->data2 = d2;
}

/* seq-new ( -- seq-id ) Create new sequence */
void op_seq_new(Stack* stack) {
    if (sequence_count >= MAX_SEQUENCES) {
        printf("Max sequences reached\n");
        push(stack, -1);
        return;
    }
    int id = sequence_count++;
    sequences[id].length = 0;
    sequences[id].bpm = global_bpm;
    current_seq = id;
    push(stack, id);
}

/* seq ( id -- ) Select sequence as current */
void op_seq_select(Stack* stack) {
    int32_t id = pop(stack);
    if (id < 0 || id >= sequence_count) {
        printf("Invalid sequence id: %d\n", id);
        return;
    }
    current_seq = id;
}

/* seq@ ( -- id ) Get current sequence id */
void op_seq_current(Stack* stack) {
    push(stack, current_seq);
}

/* seq-note ( time pitch vel dur -- ) Add note to current sequence */
void op_seq_note(Stack* stack) {
    int32_t dur = pop(stack);
    int32_t vel = pop(stack);
    int32_t pitch = pop(stack);
    int32_t time = pop(stack);

    if (current_seq < 0) {
        printf("No sequence selected (use seq-new first)\n");
        return;
    }

    int ch = default_channel;  /* use current default channel */
    add_event(time, EVT_NOTE_ON, ch, pitch, vel);
    add_event(time + dur, EVT_NOTE_OFF, ch, pitch, 0);
}

/* seq-note-ch ( time ch pitch vel dur -- ) Add note with channel */
void op_seq_note_ch(Stack* stack) {
    int32_t dur = pop(stack);
    int32_t vel = pop(stack);
    int32_t pitch = pop(stack);
    int32_t ch = pop(stack) - 1;  /* 1-16 -> 0-15 */
    int32_t time = pop(stack);

    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }

    add_event(time, EVT_NOTE_ON, ch, pitch, vel);
    add_event(time + dur, EVT_NOTE_OFF, ch, pitch, 0);
}

/* seq-add ( packed-note time -- ) Add packed note at time */
void op_seq_add(Stack* stack) {
    int32_t time = pop(stack);
    int32_t n = pop(stack);

    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }

    int ch = note_ch(n);
    int pitch = note_pitch(n);
    int vel = note_vel(n);
    int dur = note_dur(n);

    add_event(time, EVT_NOTE_ON, ch, pitch, vel);
    add_event(time + dur, EVT_NOTE_OFF, ch, pitch, 0);
}

/* seq-length ( -- n ) Get length of current sequence */
void op_seq_length(Stack* stack) {
    if (current_seq < 0) {
        push(stack, 0);
        return;
    }
    push(stack, sequences[current_seq].length);
}

/* seq-clear ( -- ) Clear current sequence */
void op_seq_clear(Stack* stack) {
    (void)stack;
    if (current_seq < 0) return;
    sequences[current_seq].length = 0;
}

/* seq-play ( -- ) Play current sequence */
void op_seq_play(Stack* stack) {
    (void)stack;
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }
    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    Sequence* seq = &sequences[current_seq];
    if (seq->length == 0) {
        printf("Sequence empty\n");
        return;
    }

    /* Sort events by time */
    seq_sort(seq);

    int bpm = seq->bpm > 0 ? seq->bpm : global_bpm;
    int last_time = 0;

    for (int i = 0; i < seq->length; i++) {
        MidiEvent* e = &seq->events[i];

        /* Wait until this event's time */
        if (e->time > last_time) {
            int ticks = e->time - last_time;
            int ms = (ticks * 60000) / (TICKS_PER_QUARTER * bpm);
            midi_sleep_ms(ms);
            last_time = e->time;
        }

        /* Send the event */
        unsigned char msg[3];
        switch (e->type) {
            case EVT_NOTE_ON:
                msg[0] = 0x90 | (e->channel & 0x0F);
                msg[1] = e->data1 & 0x7F;
                msg[2] = e->data2 & 0x7F;
                capture_add_event(0, e->channel, e->data1, e->data2);
                break;
            case EVT_NOTE_OFF:
                msg[0] = 0x80 | (e->channel & 0x0F);
                msg[1] = e->data1 & 0x7F;
                msg[2] = 0;
                capture_add_event(1, e->channel, e->data1, 0);
                break;
            case EVT_CC:
                msg[0] = 0xB0 | (e->channel & 0x0F);
                msg[1] = e->data1 & 0x7F;
                msg[2] = e->data2 & 0x7F;
                capture_add_event(2, e->channel, e->data1, e->data2);
                break;
            default:
                continue;
        }
        libremidi_midi_out_send_message(midi_out, msg, 3);
    }
}

/* seq-transpose ( semitones -- ) Transpose all notes in current sequence */
void op_seq_transpose(Stack* stack) {
    int32_t semi = pop(stack);
    if (current_seq < 0) return;

    Sequence* seq = &sequences[current_seq];
    for (int i = 0; i < seq->length; i++) {
        if (seq->events[i].type == EVT_NOTE_ON || seq->events[i].type == EVT_NOTE_OFF) {
            int p = seq->events[i].data1 + semi;
            if (p < 0) p = 0;
            if (p > 127) p = 127;
            seq->events[i].data1 = p;
        }
    }
}

/* seq-show ( -- ) Show current sequence events */
void op_seq_show(Stack* stack) {
    (void)stack;
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }
    Sequence* seq = &sequences[current_seq];
    printf("Sequence %d: %d events, bpm=%d\n", current_seq, seq->length, seq->bpm);
    for (int i = 0; i < seq->length; i++) {
        MidiEvent* e = &seq->events[i];
        const char* type_str = e->type == EVT_NOTE_ON ? "ON " :
                               e->type == EVT_NOTE_OFF ? "OFF" : "CC ";
        printf("  t=%4d %s ch=%d d1=%3d d2=%3d\n",
               e->time, type_str, e->channel + 1, e->data1, e->data2);
    }
}

/* seq-reverse ( -- ) Reverse timing of current sequence */
void op_seq_reverse(Stack* stack) {
    (void)stack;
    if (current_seq < 0) return;
    Sequence* seq = &sequences[current_seq];
    if (seq->length == 0) return;

    /* Find max time */
    int max_time = 0;
    for (int i = 0; i < seq->length; i++) {
        if (seq->events[i].time > max_time)
            max_time = seq->events[i].time;
    }

    /* Reverse times and swap note-on/off */
    for (int i = 0; i < seq->length; i++) {
        seq->events[i].time = max_time - seq->events[i].time;
        if (seq->events[i].type == EVT_NOTE_ON)
            seq->events[i].type = EVT_NOTE_OFF;
        else if (seq->events[i].type == EVT_NOTE_OFF)
            seq->events[i].type = EVT_NOTE_ON;
    }
    seq_sort(seq);
}

/* seq-stretch ( factor -- ) Multiply all times by factor/100 */
void op_seq_stretch(Stack* stack) {
    int32_t factor = pop(stack);
    if (current_seq < 0) return;

    Sequence* seq = &sequences[current_seq];
    for (int i = 0; i < seq->length; i++) {
        seq->events[i].time = (seq->events[i].time * factor) / 100;
    }
}

/* bpm! ( n -- ) Set tempo */
void op_bpm_store(Stack* stack) {
    int32_t bpm = pop(stack);
    if (bpm < 20) bpm = 20;
    if (bpm > 300) bpm = 300;
    global_bpm = bpm;
}

/* bpm@ ( -- n ) Get tempo */
void op_bpm_fetch(Stack* stack) {
    push(stack, global_bpm);
}
