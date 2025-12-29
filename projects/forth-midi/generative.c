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

/* [ ( -- ) Push list marker */
void op_list_begin(Stack* stack) {
    push(stack, LIST_MARKER);
}

/* Helper: find list marker position, returns -1 if not found */
/* Also recognizes ALT_MARKER so pick/shuffle work with alternatives */
static int find_list_marker(Stack* stack) {
    for (int i = stack->top; i >= 0; i--) {
        if (stack->data[i] == LIST_MARKER || stack->data[i] == ALT_MARKER) {
            return i;
        }
    }
    return -1;
}

/* ] ( LIST_MARKER v1 v2 ... vn -- count ) End list, push count */
void op_list_end(Stack* stack) {
    int marker_pos = find_list_marker(stack);
    if (marker_pos < 0) {
        printf("No list marker found\n");
        push(stack, 0);
        return;
    }
    int count = stack->top - marker_pos;
    /* Remove marker but leave values, push count */
    for (int i = marker_pos; i < stack->top; i++) {
        stack->data[i] = stack->data[i + 1];
    }
    stack->top--;
    push(stack, count);
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

/* reverse ( LIST_MARKER v1 v2 ... vn -- LIST_MARKER vn ... v2 v1 ) */
void op_reverse(Stack* stack) {
    int marker_pos = find_list_marker(stack);
    if (marker_pos < 0) {
        printf("No list marker found for reverse\n");
        return;
    }

    int start = marker_pos + 1;
    int end = stack->top;

    while (start < end) {
        int32_t tmp = stack->data[start];
        stack->data[start] = stack->data[end];
        stack->data[end] = tmp;
        start++;
        end--;
    }
}

/* arp-up ( LIST_MARKER v1 ... vn -- LIST_MARKER v1 ... vn ) - no change */
void op_arp_up(Stack* stack) {
    (void)stack;
}

/* arp-down ( LIST_MARKER v1 ... vn -- LIST_MARKER vn ... v1 ) - reverse */
void op_arp_down(Stack* stack) {
    op_reverse(stack);
}

/* arp-up-down ( LIST_MARKER v1 v2 ... vn -- LIST_MARKER v1 ... vn ... v2 ) */
void op_arp_up_down(Stack* stack) {
    int marker_pos = find_list_marker(stack);
    if (marker_pos < 0) {
        printf("No list marker found for arp-up-down\n");
        return;
    }

    int count = stack->top - marker_pos;
    if (count <= 1) return;

    /* Add reversed middle (skip first and last) */
    for (int i = stack->top - 1; i > marker_pos + 1; i--) {
        push(stack, stack->data[i]);
    }
}

/* retrograde - alias for reverse */
void op_retrograde(Stack* stack) {
    op_reverse(stack);
}

/* invert ( LIST_MARKER v1 ... vn axis -- LIST_MARKER v1' ... vn' ) */
void op_invert(Stack* stack) {
    int32_t axis = pop(stack);

    int marker_pos = find_list_marker(stack);
    if (marker_pos < 0) {
        printf("No list marker found for invert\n");
        return;
    }

    for (int i = marker_pos + 1; i <= stack->top; i++) {
        stack->data[i] = 2 * axis - stack->data[i];
    }
}

/* shuffle ( LIST_MARKER v1 ... vn -- LIST_MARKER shuffled... ) */
void op_shuffle(Stack* stack) {
    int marker_pos = find_list_marker(stack);
    if (marker_pos < 0) {
        printf("No list marker found for shuffle\n");
        return;
    }

    int start = marker_pos + 1;
    int count = stack->top - marker_pos;

    for (int i = count - 1; i > 0; i--) {
        int j = prng_next() % (i + 1);

        int32_t tmp = stack->data[start + i];
        stack->data[start + i] = stack->data[start + j];
        stack->data[start + j] = tmp;
    }
}

/* pick ( LIST_MARKER v1 ... vn -- v ) Pick one random element using seed */
void op_pick_random(Stack* stack) {
    int marker_pos = find_list_marker(stack);
    if (marker_pos < 0) {
        printf("No list marker found for pick\n");
        return;
    }

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
}

/* pick-n ( LIST_MARKER v1 ... vn n -- LIST_MARKER picked1 ... pickedn ) */
void op_pick_n(Stack* stack) {
    int32_t n = pop(stack);

    int marker_pos = find_list_marker(stack);
    if (marker_pos < 0) {
        printf("No list marker found for pick-n\n");
        return;
    }

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
        int idx = prng_next() % actual_count;
        push(stack, values[idx]);
    }
}

/* random-walk ( start max-step n -- LIST_MARKER p1 p2 ... pn ) */
void op_random_walk(Stack* stack) {
    int32_t n = pop(stack);
    int32_t max_step = pop(stack);
    int32_t pitch = pop(stack);

    if (n <= 0) {
        push(stack, LIST_MARKER);
        return;
    }
    if (n > 64) n = 64;

    push(stack, LIST_MARKER);

    for (int i = 0; i < n; i++) {
        push(stack, pitch);
        int32_t r = prng_next();
        int32_t step = (r % (2 * max_step + 1)) - max_step;
        pitch = pitch + step;
        if (pitch < 0) pitch = 0;
        if (pitch > 127) pitch = 127;
    }
}

/* drunk-walk ( LIST_MARKER scale... start max-degrees n -- LIST_MARKER p1 ... pn ) */
void op_drunk_walk(Stack* stack) {
    int32_t n = pop(stack);
    int32_t max_degrees = pop(stack);
    int32_t start = pop(stack);

    int marker_pos = find_list_marker(stack);
    if (marker_pos < 0) {
        printf("No list marker found for drunk-walk (need scale)\n");
        push(stack, LIST_MARKER);
        return;
    }

    int scale_count = stack->top - marker_pos;
    if (scale_count < 1 || n <= 0) {
        stack->top = marker_pos;
        return;
    }

    int32_t scale[64];
    int actual_scale_count = scale_count < 64 ? scale_count : 64;
    for (int i = 0; i < actual_scale_count; i++) {
        scale[i] = stack->data[marker_pos + 1 + i];
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

    stack->top = marker_pos;

    if (n > 64) n = 64;
    for (int i = 0; i < n; i++) {
        push(stack, scale[idx]);
        int32_t r = prng_next();
        int32_t step = (r % (2 * max_degrees + 1)) - max_degrees;
        idx = idx + step;
        if (idx < 0) idx = 0;
        if (idx >= actual_scale_count) idx = actual_scale_count - 1;
    }
}

/* weighted-pick ( LIST_MARKER v1 w1 v2 w2 ... vn wn -- v ) */
void op_weighted_pick(Stack* stack) {
    int marker_pos = find_list_marker(stack);
    if (marker_pos < 0) {
        printf("No list marker found for weighted-pick\n");
        return;
    }

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
}

/* list. ( LIST_MARKER v1 ... vn -- ) Print list values */
void op_list_print(Stack* stack) {
    int marker_pos = find_list_marker(stack);
    if (marker_pos < 0) {
        printf("No list marker found\n");
        return;
    }

    printf("[ ");
    for (int i = marker_pos + 1; i <= stack->top; i++) {
        printf("%d ", stack->data[i]);
    }
    printf("]\n");
}

/* list-len ( LIST_MARKER v1 ... vn -- LIST_MARKER v1 ... vn n ) */
void op_list_len(Stack* stack) {
    int marker_pos = find_list_marker(stack);
    if (marker_pos < 0) {
        printf("No list marker found\n");
        push(stack, 0);
        return;
    }
    push(stack, stack->top - marker_pos);
}
