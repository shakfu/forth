/* generative.c - Generative music functions for MIDI Forth interpreter */

#include "forth_midi.h"

/* Pure PRNG state (Linear Congruential Generator, same constants as glibc) */
int32_t prng_seed = 42;

/* Helper: advance PRNG and return non-negative value */
static int32_t prng_next(void) {
    prng_seed = (prng_seed * 1103515245 + 12345) & 0x7FFFFFFF;
    return prng_seed / 65536;
}

/* seed! ( n -- ) Set the PRNG seed */
void op_seed_store(Stack* stack) {
    prng_seed = pop(stack);
}

/* seed@ ( -- n ) Get the current PRNG seed */
void op_seed_fetch(Stack* stack) {
    push(stack, prng_seed);
}

/* next-random ( -- n ) Generate random number using seed, advances seed */
void op_next_random(Stack* stack) {
    push(stack, prng_next());
}

/* srand-range ( lo hi -- n ) Random int in range [lo, hi] using seed */
void op_srand_range(Stack* stack) {
    int32_t hi = pop(stack);
    int32_t lo = pop(stack);
    if (lo >= hi) {
        push(stack, lo);
        return;
    }
    int32_t r = prng_next();
    push(stack, lo + (r % (hi - lo + 1)));
}

/* chance ( probability -- flag ) Probability gate (0-100) using seed */
void op_chance(Stack* stack) {
    int32_t probability = pop(stack);
    if (probability < 0) probability = 0;
    if (probability > 100) probability = 100;
    int32_t r = prng_next() % 100;
    push(stack, r < probability ? 1 : 0);
}

/* random ( -- n ) Push random number 0-99 (uses system rand) */
void op_random(Stack* stack) {
    push(stack, rand() % 100);
}

/* Helper: find ALT_MARKER position for alternatives syntax (c4|e4|g4) */
/* Returns -1 if not found */
static int find_list_marker(Stack* stack) {
    for (int i = stack->top; i >= 0; i--) {
        if (stack->data[i] == ALT_MARKER) {
            return i;
        }
    }
    return -1;
}

/* euclidean ( hits steps -- b1 b2 ... bn ) Bjorklund's algorithm */
void op_euclidean(Stack* stack) {
    int32_t steps = pop(stack);
    int32_t hits = pop(stack);

    if (steps <= 0) return;
    if (steps > 64) steps = 64;

    /* Edge cases */
    if (hits <= 0) {
        for (int i = 0; i < steps; i++) push(stack, 0);
        return;
    }
    if (hits >= steps) {
        for (int i = 0; i < steps; i++) push(stack, 1);
        return;
    }

    /* Iterative Bjorklund's algorithm */
    int pattern[64];

    /* Initialize: hits 1s followed by (steps-hits) 0s */
    for (int i = 0; i < hits; i++) pattern[i] = 1;
    for (int i = hits; i < steps; i++) pattern[i] = 0;

    int left = hits;
    int right = steps - hits;

    while (right > 1) {
        int min_val = left < right ? left : right;

        /* Interleave: move elements from end to positions after left */
        int new_pattern[64];
        int src_left = 0;
        int src_right = steps - right;
        int dst = 0;

        for (int i = 0; i < min_val; i++) {
            new_pattern[dst++] = pattern[src_left++];
            new_pattern[dst++] = pattern[src_right++];
        }

        /* Copy remaining elements */
        while (src_left < steps - right) {
            new_pattern[dst++] = pattern[src_left++];
        }
        while (src_right < steps) {
            new_pattern[dst++] = pattern[src_right++];
        }

        for (int i = 0; i < steps; i++) pattern[i] = new_pattern[i];

        /* Update counts for next iteration */
        if (left < right) {
            right = right - left;
        } else {
            int old_left = left;
            left = right;
            right = old_left - right;
        }
    }

    /* Push pattern to stack */
    for (int i = 0; i < steps; i++) {
        push(stack, pattern[i]);
    }
}

/* reverse ( seq -- seq ) Reverse sequence elements in place */
void op_reverse(Stack* stack) {
    if (stack->top < 0) {
        printf("reverse needs a sequence\n");
        return;
    }

    int32_t top_val = peek(stack);

    /* Check if it's a sequence */
    if ((top_val & 0xFF000000) == SEQ_MARKER) {
        int idx = top_val & 0x00FFFFFF;
        if (idx < 0 || idx >= bracket_seq_count || !bracket_seq_storage[idx]) {
            printf("Invalid sequence\n");
            return;
        }

        BracketSequence* seq = bracket_seq_storage[idx];
        int start = 0;
        int end = seq->count - 1;

        while (start < end) {
            SeqElement tmp = seq->elements[start];
            seq->elements[start] = seq->elements[end];
            seq->elements[end] = tmp;
            start++;
            end--;
        }
        /* Leave sequence on stack */
        return;
    }

    printf("reverse needs a sequence (use [ ... ])\n");
}

/* arp-up ( seq -- seq ) No change, ascending order */
void op_arp_up(Stack* stack) {
    (void)stack;
}

/* arp-down ( seq -- seq ) Reverse to descending order */
void op_arp_down(Stack* stack) {
    op_reverse(stack);
}

/* arp-up-down ( seq -- seq ) Duplicate sequence with middle reversed appended */
void op_arp_up_down(Stack* stack) {
    if (stack->top < 0) {
        printf("arp-up-down needs a sequence\n");
        return;
    }

    int32_t top_val = peek(stack);

    /* Check if it's a sequence */
    if ((top_val & 0xFF000000) == SEQ_MARKER) {
        int idx = top_val & 0x00FFFFFF;
        if (idx < 0 || idx >= bracket_seq_count || !bracket_seq_storage[idx]) {
            printf("Invalid sequence\n");
            return;
        }

        BracketSequence* src = bracket_seq_storage[idx];
        if (src->count <= 1) return;

        /* Create new sequence: original + reversed middle */
        if (bracket_seq_count >= MAX_BRACKET_SEQS) {
            printf("Too many sequences\n");
            return;
        }

        BracketSequence* out = seq_alloc();
        if (!out) {
            printf("Out of memory\n");
            return;
        }

        /* Copy original */
        for (int i = 0; i < src->count && out->count < MAX_SEQ_ELEMENTS; i++) {
            out->elements[out->count++] = src->elements[i];
        }
        /* Add reversed middle (skip first and last) */
        for (int i = src->count - 2; i > 0 && out->count < MAX_SEQ_ELEMENTS; i--) {
            out->elements[out->count++] = src->elements[i];
        }

        pop(stack);
        int out_idx = bracket_seq_count++;
        bracket_seq_storage[out_idx] = out;
        push(stack, SEQ_MARKER | out_idx);
        return;
    }

    /* Check for ALT_MARKER */
    int marker_pos = find_list_marker(stack);
    if (marker_pos >= 0) {
        int count = stack->top - marker_pos;
        if (count <= 1) return;

        /* Add reversed middle (skip first and last) */
        for (int i = stack->top - 1; i > marker_pos + 1; i--) {
            push(stack, stack->data[i]);
        }
        return;
    }

    printf("arp-up-down needs a sequence (use [ ... ]) or alternatives (use |)\n");
}

/* retrograde - alias for reverse */
void op_retrograde(Stack* stack) {
    op_reverse(stack);
}

/* invert ( seq axis -- seq ) Invert pitches around axis */
void op_invert(Stack* stack) {
    int32_t axis = pop(stack);

    if (stack->top < 0) {
        printf("invert needs a sequence and axis\n");
        return;
    }

    int32_t top_val = peek(stack);

    /* Check if it's a sequence */
    if ((top_val & 0xFF000000) == SEQ_MARKER) {
        int idx = top_val & 0x00FFFFFF;
        if (idx < 0 || idx >= bracket_seq_count || !bracket_seq_storage[idx]) {
            printf("Invalid sequence\n");
            return;
        }

        BracketSequence* seq = bracket_seq_storage[idx];
        /* Invert pitch elements in place */
        for (int i = 0; i < seq->count; i++) {
            if (seq->elements[i].type == SEQ_ELEM_PITCH ||
                seq->elements[i].type == SEQ_ELEM_NUMBER) {
                seq->elements[i].value = 2 * axis - seq->elements[i].value;
            }
            /* Also invert chord pitches */
            if (seq->elements[i].type == SEQ_ELEM_CHORD) {
                for (int j = 0; j < seq->elements[i].chord_count; j++) {
                    seq->elements[i].chord_pitches[j] = 2 * axis - seq->elements[i].chord_pitches[j];
                }
            }
        }
        return;
    }

    /* Check for ALT_MARKER */
    int marker_pos = find_list_marker(stack);
    if (marker_pos >= 0) {
        for (int i = marker_pos + 1; i <= stack->top; i++) {
            stack->data[i] = 2 * axis - stack->data[i];
        }
        return;
    }

    printf("invert needs a sequence (use [ ... ]) or alternatives (use |)\n");
}

/* shuffle ( seq -- seq ) Shuffle sequence elements in place */
void op_shuffle(Stack* stack) {
    if (stack->top < 0) {
        printf("shuffle needs a sequence\n");
        return;
    }

    int32_t top_val = peek(stack);

    /* Check if it's a sequence */
    if ((top_val & 0xFF000000) == SEQ_MARKER) {
        int idx = top_val & 0x00FFFFFF;
        if (idx < 0 || idx >= bracket_seq_count || !bracket_seq_storage[idx]) {
            printf("Invalid sequence\n");
            return;
        }

        BracketSequence* seq = bracket_seq_storage[idx];
        int count = seq->count;

        /* Fisher-Yates shuffle */
        for (int i = count - 1; i > 0; i--) {
            int j = prng_next() % (i + 1);
            SeqElement tmp = seq->elements[i];
            seq->elements[i] = seq->elements[j];
            seq->elements[j] = tmp;
        }
        /* Leave sequence on stack */
        return;
    }

    printf("shuffle needs a sequence (use [ ... ])\n");
}

/* pick ( seq -- value ) Pick one random element from sequence or alternatives */
void op_pick_random(Stack* stack) {
    if (stack->top < 0) {
        printf("pick needs a sequence or alternatives\n");
        return;
    }

    int32_t top_val = peek(stack);

    /* Check if it's a sequence */
    if ((top_val & 0xFF000000) == SEQ_MARKER) {
        pop(stack);  /* Remove sequence from stack */
        int idx = top_val & 0x00FFFFFF;
        if (idx < 0 || idx >= bracket_seq_count || !bracket_seq_storage[idx]) {
            printf("Invalid sequence\n");
            push(stack, 0);
            return;
        }

        BracketSequence* seq = bracket_seq_storage[idx];
        if (seq->count < 1) {
            push(stack, 0);
            return;
        }

        int pick_idx = prng_next() % seq->count;
        SeqElement* elem = &seq->elements[pick_idx];

        /* Return the value based on element type */
        switch (elem->type) {
            case SEQ_ELEM_PITCH:
            case SEQ_ELEM_NUMBER:
            case SEQ_ELEM_INTERVAL:
            case SEQ_ELEM_DYNAMIC:
            case SEQ_ELEM_DURATION:
                push(stack, elem->value);
                break;
            case SEQ_ELEM_CHORD:
                /* Return first pitch of chord */
                push(stack, elem->chord_pitches[0]);
                break;
            case SEQ_ELEM_REST:
                push(stack, REST_MARKER);
                break;
            default:
                push(stack, 0);
        }
        return;
    }

    /* Check for ALT_MARKER (alternatives syntax: c4|e4|g4 pick) */
    int marker_pos = find_list_marker(stack);
    if (marker_pos >= 0) {
        int count = stack->top - marker_pos;
        if (count < 1) {
            stack->top = marker_pos - 1;
            push(stack, 0);
            return;
        }

        int idx = prng_next() % count;
        int32_t picked = stack->data[marker_pos + 1 + idx];
        stack->top = marker_pos - 1;
        push(stack, picked);
        return;
    }

    printf("pick needs a sequence (use [ ... ]) or alternatives (use |)\n");
    push(stack, 0);
}

/* pick-n ( seq n -- seq ) Pick n random elements from sequence (with replacement) */
void op_pick_n(Stack* stack) {
    int32_t n = pop(stack);
    int32_t top_val = pop(stack);

    /* Check if it's a sequence */
    if ((top_val & 0xFF000000) == SEQ_MARKER) {
        int idx = top_val & 0x00FFFFFF;
        if (idx < 0 || idx >= bracket_seq_count || !bracket_seq_storage[idx]) {
            printf("Invalid sequence\n");
            push(stack, 0);
            return;
        }

        BracketSequence* src = bracket_seq_storage[idx];
        if (src->count < 1 || n <= 0) {
            push(stack, top_val);
            return;
        }

        /* Create output sequence */
        if (bracket_seq_count >= MAX_BRACKET_SEQS) {
            printf("Too many sequences\n");
            push(stack, 0);
            return;
        }

        BracketSequence* out = seq_alloc();
        if (!out) {
            printf("Out of memory\n");
            push(stack, 0);
            return;
        }

        if (n > MAX_SEQ_ELEMENTS) n = MAX_SEQ_ELEMENTS;
        for (int i = 0; i < n; i++) {
            int pick_idx = prng_next() % src->count;
            out->elements[out->count] = src->elements[pick_idx];
            out->count++;
        }

        int out_idx = bracket_seq_count++;
        bracket_seq_storage[out_idx] = out;
        push(stack, SEQ_MARKER | out_idx);
        return;
    }

    /* Check for ALT_MARKER - put value back and check */
    push(stack, top_val);
    int marker_pos = find_list_marker(stack);
    if (marker_pos >= 0) {
        int count = stack->top - marker_pos;
        if (count < 1 || n <= 0) {
            stack->top = marker_pos;
            return;
        }

        int32_t values[64];
        int actual_count = count < 64 ? count : 64;
        for (int i = 0; i < actual_count; i++) {
            values[i] = stack->data[marker_pos + 1 + i];
        }

        stack->top = marker_pos;

        for (int i = 0; i < n && i < 64; i++) {
            int pick_idx = prng_next() % actual_count;
            push(stack, values[pick_idx]);
        }
        return;
    }

    printf("pick-n needs a sequence (use [ ... ]) or alternatives (use |)\n");
}

/* random-walk ( start max-step n -- seq ) Generate random walk as sequence */
void op_random_walk(Stack* stack) {
    int32_t n = pop(stack);
    int32_t max_step = pop(stack);
    int32_t pitch = pop(stack);

    if (n <= 0 || n > MAX_SEQ_ELEMENTS) {
        if (n > MAX_SEQ_ELEMENTS) n = MAX_SEQ_ELEMENTS;
        if (n <= 0) n = 1;
    }

    /* Create a new sequence */
    if (bracket_seq_count >= MAX_BRACKET_SEQS) {
        printf("Too many sequences\n");
        push(stack, 0);
        return;
    }

    BracketSequence* seq = seq_alloc();
    if (!seq) {
        printf("Out of memory\n");
        push(stack, 0);
        return;
    }

    for (int i = 0; i < n; i++) {
        seq->elements[seq->count].type = SEQ_ELEM_PITCH;
        seq->elements[seq->count].value = pitch;
        seq->elements[seq->count].chord_count = 0;
        seq->count++;

        int32_t r = prng_next();
        int32_t step = (r % (2 * max_step + 1)) - max_step;
        pitch = pitch + step;
        if (pitch < 0) pitch = 0;
        if (pitch > 127) pitch = 127;
    }

    int idx = bracket_seq_count++;
    bracket_seq_storage[idx] = seq;
    push(stack, SEQ_MARKER | idx);
}

/* drunk-walk ( scale-seq start max-degrees n -- seq ) Drunk walk within scale */
void op_drunk_walk(Stack* stack) {
    int32_t n = pop(stack);
    int32_t max_degrees = pop(stack);
    int32_t start = pop(stack);
    int32_t scale_val = pop(stack);

    /* Check if input is a sequence */
    if ((scale_val & 0xFF000000) != SEQ_MARKER) {
        printf("drunk-walk needs a scale sequence (use [ ... ])\n");
        push(stack, 0);
        return;
    }

    int scale_idx = scale_val & 0x00FFFFFF;
    if (scale_idx < 0 || scale_idx >= bracket_seq_count || !bracket_seq_storage[scale_idx]) {
        printf("Invalid scale sequence\n");
        push(stack, 0);
        return;
    }

    BracketSequence* scale_seq = bracket_seq_storage[scale_idx];
    if (scale_seq->count < 1 || n <= 0) {
        push(stack, 0);
        return;
    }

    /* Extract pitch values from scale sequence */
    int32_t scale[64];
    int actual_scale_count = 0;
    for (int i = 0; i < scale_seq->count && actual_scale_count < 64; i++) {
        if (scale_seq->elements[i].type == SEQ_ELEM_PITCH ||
            scale_seq->elements[i].type == SEQ_ELEM_NUMBER) {
            scale[actual_scale_count++] = scale_seq->elements[i].value;
        }
    }

    if (actual_scale_count < 1) {
        printf("Scale sequence has no pitches\n");
        push(stack, 0);
        return;
    }

    /* Find closest index to start */
    int idx = 0;
    int best_dist = abs(start - scale[0]);
    for (int i = 1; i < actual_scale_count; i++) {
        int dist = abs(start - scale[i]);
        if (dist < best_dist) {
            best_dist = dist;
            idx = i;
        }
    }

    /* Create output sequence */
    if (bracket_seq_count >= MAX_BRACKET_SEQS) {
        printf("Too many sequences\n");
        push(stack, 0);
        return;
    }

    BracketSequence* out_seq = seq_alloc();
    if (!out_seq) {
        printf("Out of memory\n");
        push(stack, 0);
        return;
    }

    if (n > MAX_SEQ_ELEMENTS) n = MAX_SEQ_ELEMENTS;
    for (int i = 0; i < n; i++) {
        out_seq->elements[out_seq->count].type = SEQ_ELEM_PITCH;
        out_seq->elements[out_seq->count].value = scale[idx];
        out_seq->elements[out_seq->count].chord_count = 0;
        out_seq->count++;

        int32_t r = prng_next();
        int32_t step = (r % (2 * max_degrees + 1)) - max_degrees;
        idx = idx + step;
        if (idx < 0) idx = 0;
        if (idx >= actual_scale_count) idx = actual_scale_count - 1;
    }

    int out_idx = bracket_seq_count++;
    bracket_seq_storage[out_idx] = out_seq;
    push(stack, SEQ_MARKER | out_idx);
}

/* weighted-pick ( seq -- v ) Pick from sequence with alternating value/weight pairs */
void op_weighted_pick(Stack* stack) {
    int32_t top_val = peek(stack);

    /* Check if it's a sequence */
    if ((top_val & 0xFF000000) == SEQ_MARKER) {
        pop(stack);
        int idx = top_val & 0x00FFFFFF;
        if (idx < 0 || idx >= bracket_seq_count || !bracket_seq_storage[idx]) {
            printf("Invalid sequence\n");
            push(stack, 0);
            return;
        }

        BracketSequence* seq = bracket_seq_storage[idx];
        if (seq->count < 2 || seq->count % 2 != 0) {
            printf("weighted-pick needs pairs of (value weight)\n");
            push(stack, 0);
            return;
        }

        int pairs = seq->count / 2;

        /* Calculate total weight */
        int32_t total = 0;
        for (int i = 0; i < pairs; i++) {
            SeqElement* wt = &seq->elements[i * 2 + 1];
            if (wt->type == SEQ_ELEM_NUMBER || wt->type == SEQ_ELEM_PITCH) {
                total += wt->value;
            }
        }

        if (total <= 0) {
            /* Return first value */
            SeqElement* first = &seq->elements[0];
            push(stack, first->value);
            return;
        }

        int32_t r = (prng_next() % total) + 1;
        int32_t cumulative = 0;
        int32_t result = seq->elements[0].value;

        for (int i = 0; i < pairs; i++) {
            SeqElement* wt = &seq->elements[i * 2 + 1];
            if (wt->type == SEQ_ELEM_NUMBER || wt->type == SEQ_ELEM_PITCH) {
                cumulative += wt->value;
            }
            if (r <= cumulative) {
                result = seq->elements[i * 2].value;
                break;
            }
        }

        push(stack, result);
        return;
    }

    /* Check for ALT_MARKER */
    int marker_pos = find_list_marker(stack);
    if (marker_pos >= 0) {
        int count = stack->top - marker_pos;
        if (count < 2 || count % 2 != 0) {
            printf("weighted-pick needs pairs of (value weight)\n");
            stack->top = marker_pos - 1;
            push(stack, 0);
            return;
        }

        int pairs = count / 2;

        int32_t total = 0;
        for (int i = 0; i < pairs; i++) {
            total += stack->data[marker_pos + 2 + i * 2];
        }

        if (total <= 0) {
            stack->top = marker_pos - 1;
            push(stack, stack->data[marker_pos + 1]);
            return;
        }

        int32_t r = (prng_next() % total) + 1;

        int32_t cumulative = 0;
        int32_t result = stack->data[marker_pos + 1];
        for (int i = 0; i < pairs; i++) {
            cumulative += stack->data[marker_pos + 2 + i * 2];
            if (r <= cumulative) {
                result = stack->data[marker_pos + 1 + i * 2];
                break;
            }
        }

        stack->top = marker_pos - 1;
        push(stack, result);
        return;
    }

    printf("weighted-pick needs a sequence (use [ ... ]) or alternatives (use |)\n");
    push(stack, 0);
}

/* concat ( seq1 seq2 -- seq ) Concatenate two sequences */
void op_concat(Stack* stack) {
    if (stack->top < 1) {
        printf("concat needs two sequences\n");
        return;
    }

    int32_t val2 = pop(stack);
    int32_t val1 = pop(stack);

    /* Both must be sequences */
    if ((val1 & 0xFF000000) != SEQ_MARKER || (val2 & 0xFF000000) != SEQ_MARKER) {
        printf("concat needs two sequences\n");
        push(stack, 0);
        return;
    }

    int idx1 = val1 & 0x00FFFFFF;
    int idx2 = val2 & 0x00FFFFFF;

    if (idx1 < 0 || idx1 >= bracket_seq_count || !bracket_seq_storage[idx1] ||
        idx2 < 0 || idx2 >= bracket_seq_count || !bracket_seq_storage[idx2]) {
        printf("Invalid sequence\n");
        push(stack, 0);
        return;
    }

    BracketSequence* seq1 = bracket_seq_storage[idx1];
    BracketSequence* seq2 = bracket_seq_storage[idx2];

    /* Create new sequence */
    if (bracket_seq_count >= MAX_BRACKET_SEQS) {
        printf("Too many sequences\n");
        push(stack, 0);
        return;
    }

    BracketSequence* out = seq_alloc();
    if (!out) {
        printf("Out of memory\n");
        push(stack, 0);
        return;
    }

    /* Copy seq1 */
    for (int i = 0; i < seq1->count && out->count < MAX_SEQ_ELEMENTS; i++) {
        out->elements[out->count++] = seq1->elements[i];
    }

    /* Copy seq2 */
    for (int i = 0; i < seq2->count && out->count < MAX_SEQ_ELEMENTS; i++) {
        out->elements[out->count++] = seq2->elements[i];
    }

    int out_idx = bracket_seq_count++;
    bracket_seq_storage[out_idx] = out;
    push(stack, SEQ_MARKER | out_idx);
}

