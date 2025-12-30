/* notation.c - Concise notation system for MIDI Forth interpreter */

#include "forth_midi.h"

/* Parse pitch name like c4, C#4, Db5, etc.
 * Also handles articulation suffixes: c4. (staccato), c4> (accent), c4- (tenuto)
 * Returns MIDI note number (0-127) or -1 if not a valid pitch name
 * Sets global articulation flags as side effect */
int parse_pitch(const char* token) {
    if (token == NULL || token[0] == '\0') return -1;

    int len = strlen(token);
    char suffix = token[len - 1];

    /* Check for articulation suffix */
    if (suffix == '.' || suffix == '>' || suffix == '-') {
        /* Strip suffix and parse base pitch */
        char base[16];
        if (len - 1 >= (int)sizeof(base)) return -1;
        strncpy(base, token, len - 1);
        base[len - 1] = '\0';

        int pitch = music_parse_pitch(base);
        if (pitch < 0) return -1;

        /* Set articulation flags */
        if (suffix == '.') {
            articulation_staccato = 1;
        } else if (suffix == '>') {
            articulation_accent = 1;
        } else if (suffix == '-') {
            /* Tenuto: full duration (clear staccato if set) */
            articulation_staccato = 0;
        }

        return pitch;
    }

    /* No articulation suffix - use common library directly */
    return music_parse_pitch(token);
}

/* Helper: play a single note with given params
 * Applies and resets articulation flags */
void play_single_note(Stack* stack, int pitch) {
    (void)stack;

    /* Always track current pitch for relative intervals */
    current_pitch = pitch;

    int velocity = default_velocity;
    int duration = default_duration;
    int channel = default_channel;

    /* Apply articulation */
    if (articulation_staccato) {
        duration = duration / 2;  /* 50% duration */
        articulation_staccato = 0;
    }
    if (articulation_accent) {
        velocity = velocity + 20;
        if (velocity > 127) velocity = 127;
        articulation_accent = 0;
    }

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    if (channel < 1 || channel > 16) {
        printf("Channel must be 1-16\n");
        return;
    }
    if (pitch < 0 || pitch > 127) {
        printf("Pitch must be 0-127\n");
        return;
    }
    if (velocity < 0 || velocity > 127) {
        printf("Velocity must be 0-127\n");
        return;
    }

    /* Note on */
    midi_send_note_on(pitch, velocity, channel);
    capture_add_event(0, channel - 1, pitch, velocity);

    /* Wait */
    midi_sleep_ms(duration);

    /* Note off */
    midi_send_note_off(pitch, channel);
    capture_add_event(1, channel - 1, pitch, 0);
}

/* Helper: play chord notes
 * Applies and resets articulation flags */
void play_chord_notes(Stack* stack) {
    /* Find chord marker */
    int marker_pos = -1;
    for (int i = stack->top; i >= 0; i--) {
        if (stack->data[i] == CHORD_MARKER) {
            marker_pos = i;
            break;
        }
    }

    if (marker_pos < 0) {
        printf("No chord marker found\n");
        return;
    }

    int count = stack->top - marker_pos;
    if (count < 1 || count > 16) {
        printf("Chord must have 1-16 notes\n");
        while (stack->top >= marker_pos) pop(stack);
        return;
    }

    int pitches[16];
    int velocity = default_velocity;
    int duration = default_duration;
    int channel = default_channel;

    /* Apply articulation */
    if (articulation_staccato) {
        duration = duration / 2;
        articulation_staccato = 0;
    }
    if (articulation_accent) {
        velocity = velocity + 20;
        if (velocity > 127) velocity = 127;
        articulation_accent = 0;
    }

    /* Pop pitches (in reverse order) */
    for (int i = count - 1; i >= 0; i--) {
        pitches[i] = pop(stack);
    }

    /* Pop the marker */
    pop(stack);

    /* Track current pitch (use highest note in chord) */
    if (count > 0) {
        current_pitch = pitches[count - 1];
    }

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    /* All notes on */
    for (int i = 0; i < count; i++) {
        midi_send_note_on(pitches[i], velocity, channel);
        capture_add_event(0, channel - 1, pitches[i], velocity);
    }

    /* Wait */
    midi_sleep_ms(duration);

    /* All notes off */
    for (int i = 0; i < count; i++) {
        midi_send_note_off(pitches[i], channel);
        capture_add_event(1, channel - 1, pitches[i], 0);
    }
}

/* , ( stack contents -- ) The comma - universal play trigger */
void op_comma(Stack* stack) {
    if (stack->top < 0) {
        printf("Stack empty\n");
        return;
    }

    /* Check if top of stack is a bracket sequence */
    int32_t top_val = peek(stack);
    if ((top_val & 0xFF000000) == SEQ_MARKER) {
        int idx = top_val & 0x00FFFFFF;
        pop(stack);
        if (idx >= 0 && idx < bracket_seq_count && bracket_seq_storage[idx]) {
            execute_bracket_sequence(bracket_seq_storage[idx]);
        }
        return;
    }

    /* First, check for alternatives (ALT_MARKER) */
    int alt_pos = -1;
    for (int i = stack->top; i >= 0; i--) {
        if (stack->data[i] == ALT_MARKER) {
            alt_pos = i;
            break;
        }
        if (stack->data[i] == CHORD_MARKER || stack->data[i] == EXPLICIT_MARKER) {
            break;
        }
    }

    if (alt_pos >= 0) {
        /* Alternatives mode: pick one randomly */
        int alt_count = stack->top - alt_pos;
        if (alt_count < 1) {
            printf("Empty alternatives\n");
            pop(stack);
            return;
        }

        int pick = rand() % alt_count;
        int32_t picked = stack->data[alt_pos + 1 + pick];
        stack->top = alt_pos - 1;
        push(stack, picked);
        /* Continue to process the single picked value */
    }

    /* Count items and find markers */
    int count = 0;
    int chord_pos = -1;
    int explicit_pos = -1;
    for (int i = stack->top; i >= 0; i--) {
        if (stack->data[i] == CHORD_MARKER) {
            chord_pos = i;
            break;
        }
        if (stack->data[i] == EXPLICIT_MARKER) {
            explicit_pos = i;
            break;
        }
        count++;
    }

    if (chord_pos >= 0) {
        /* Chord mode */
        play_chord_notes(stack);
    } else if (explicit_pos >= 0) {
        /* Explicit mode with [ ] brackets: ch pitch vel dur */
        if (count != 4) {
            printf("Explicit mode [ch pitch vel dur] requires exactly 4 values, got %d\n", count);
            while (stack->top >= explicit_pos) pop(stack);
            return;
        }
        int duration = pop(stack);
        int velocity = pop(stack);
        int pitch = pop(stack);
        int channel = pop(stack);
        pop(stack);  /* Remove EXPLICIT_MARKER */

        current_pitch = pitch;

        if (midi_out == NULL) {
            printf("No MIDI output open\n");
            return;
        }

        midi_send_note_on(pitch, velocity, channel);
        capture_add_event(0, channel - 1, pitch, velocity);
        midi_sleep_ms(duration);
        midi_send_note_off(pitch, channel);
        capture_add_event(1, channel - 1, pitch, 0);
    } else {
        /* Single note mode */
        if (count == 1) {
            int pitch = pop(stack);
            if (pitch == REST_MARKER) {
                midi_sleep_ms(default_duration);
            } else {
                play_single_note(stack, pitch);
            }
        } else if (count == 2) {
            /* Could be: r dur, (rest with explicit duration) */
            int dur_or_pitch = pop(stack);
            int first = pop(stack);
            if (first == REST_MARKER) {
                midi_sleep_ms(dur_or_pitch);
            } else {
                printf("Invalid note: expected 1 (pitch) or 4 (ch pitch vel dur) items, got 2\n");
            }
        } else if (count == 4) {
            /* Explicit: ch pitch vel dur */
            int duration = pop(stack);
            int velocity = pop(stack);
            int pitch = pop(stack);
            int channel = pop(stack);

            current_pitch = pitch;

            if (midi_out == NULL) {
                printf("No MIDI output open\n");
                return;
            }

            midi_send_note_on(pitch, velocity, channel);
            capture_add_event(0, channel - 1, pitch, velocity);
            midi_sleep_ms(duration);
            midi_send_note_off(pitch, channel);
            capture_add_event(1, channel - 1, pitch, 0);
        } else {
            printf("Invalid note: expected 1 (pitch) or 4 (ch pitch vel dur) items, got %d\n", count);
            for (int i = 0; i < count; i++) pop(stack);
        }
    }
}

/* ( ( -- ) Chord grouping open */
void op_chord_open(Stack* stack) {
    push(stack, CHORD_MARKER);
}

/* ) ( -- ) Chord grouping close */
void op_chord_close(Stack* stack) {
    (void)stack;
    /* Just a no-op marker - the comma will handle playback */
}

/* | ( val -- ALT_MARKER val ) Alternative grouping */
void op_alt_open(Stack* stack) {
    if (stack->top < 0) {
        printf("Stack empty for |\n");
        return;
    }

    /* Check if we already have an ALT_MARKER in the stack */
    int has_alt_marker = 0;
    for (int i = stack->top; i >= 0; i--) {
        if (stack->data[i] == ALT_MARKER) {
            has_alt_marker = 1;
            break;
        }
        if (stack->data[i] == CHORD_MARKER || stack->data[i] == EXPLICIT_MARKER) {
            break;
        }
    }

    if (!has_alt_marker) {
        /* First | - insert ALT_MARKER before the top value */
        int32_t top_val = pop(stack);
        push(stack, ALT_MARKER);
        push(stack, top_val);
    }
    /* Otherwise, just continue - next value will be pushed by following word */
}

/* % ( val probability -- val | REST_MARKER ) Probability gate */
void op_percent(Stack* stack) {
    if (stack->top < 1) {
        printf("Stack needs value and probability for %%\n");
        return;
    }

    int32_t probability = pop(stack);
    if (probability < 0) probability = 0;
    if (probability > 100) probability = 100;

    int32_t roll = rand() % 100;

    if (roll >= probability) {
        /* Failed probability check - drop the value */
        pop(stack);
        push(stack, REST_MARKER);
    }
    /* Otherwise, leave the value on stack to be played */
}

/* pb ( ch val -- ) Pitch bend */
void op_pitch_bend(Stack* stack) {
    if (stack->top < 1) {
        printf("pb needs channel and value (0-16383)\n");
        return;
    }

    int32_t value = pop(stack);
    int32_t channel = pop(stack);

    if (channel < 1 || channel > 16) {
        printf("Channel must be 1-16\n");
        return;
    }
    if (value < 0) value = 0;
    if (value > 16383) value = 16383;

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    midi_send_pitch_bend(value, channel);
}

/* ch! ( n -- ) Set default channel */
void op_ch_store(Stack* stack) {
    int32_t ch = pop(stack);
    if (ch < 1 || ch > 16) {
        printf("Channel must be 1-16\n");
        return;
    }
    default_channel = ch;
}

/* ch@ ( -- n ) Get default channel */
void op_ch_fetch_default(Stack* stack) {
    push(stack, default_channel);
}

/* vel! ( n -- ) Set default velocity */
void op_vel_store(Stack* stack) {
    int32_t vel = pop(stack);
    if (vel < 0 || vel > 127) {
        printf("Velocity must be 0-127\n");
        return;
    }
    default_velocity = vel;
}

/* vel@ ( -- n ) Get default velocity */
void op_vel_fetch_default(Stack* stack) {
    push(stack, default_velocity);
}

/* dur! ( n -- ) Set default duration */
void op_dur_store(Stack* stack) {
    int32_t dur = pop(stack);
    if (dur < 1) {
        printf("Duration must be positive\n");
        return;
    }
    default_duration = dur;
}

/* dur@ ( -- n ) Get default duration */
void op_dur_fetch_default(Stack* stack) {
    push(stack, default_duration);
}

/* ^ ( -- pitch ) Octave up from current pitch */
void op_octave_up(Stack* stack) {
    int new_pitch = current_pitch + 12;
    if (new_pitch > 127) new_pitch = 127;
    push(stack, new_pitch);
}

/* v ( -- pitch ) Octave down from current pitch */
void op_octave_down(Stack* stack) {
    int new_pitch = current_pitch - 12;
    if (new_pitch < 0) new_pitch = 0;
    push(stack, new_pitch);
}

/* pc ( ch prog -- ) Program change */
void op_program_change(Stack* stack) {
    if (stack->top < 1) {
        printf("pc needs channel and program\n");
        return;
    }

    int program = pop(stack);
    int channel = pop(stack);

    if (channel < 1 || channel > 16) {
        printf("Channel must be 1-16\n");
        return;
    }
    if (program < 0 || program > 127) {
        printf("Program must be 0-127\n");
        return;
    }

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    midi_send_program_change(program, channel);
}
