/* primitives.c - Arithmetic and basic operations for MIDI Forth interpreter */

#include "forth_midi.h"

/* Arithmetic operations */
void op_add(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a + b);
}

void op_sub(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a - b);
}

void op_mul(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a * b);
}

void op_div(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    if (b == 0) {
        printf("Division by zero!\n");
        return;
    }
    push(stack, a / b);
}

void op_mod(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    if (b == 0) {
        printf("Modulo by zero!\n");
        return;
    }
    push(stack, a % b);
}

void op_abs(Stack* stack) {
    int32_t a = pop(stack);
    push(stack, a < 0 ? -a : a);
}

void op_negate(Stack* stack) {
    int32_t a = pop(stack);
    push(stack, -a);
}

void op_min(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a < b ? a : b);
}

void op_max(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a > b ? a : b);
}

/* Bitwise operations */
void op_and(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a & b);
}

void op_or(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a | b);
}

void op_xor(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a ^ b);
}

void op_not(Stack* stack) {
    int32_t a = pop(stack);
    push(stack, ~a);
}

/* Comparison operations - use -1 for true (standard Forth convention) */
void op_eq(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a == b ? -1 : 0);
}

void op_lt(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a < b ? -1 : 0);
}

void op_gt(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a > b ? -1 : 0);
}

void op_le(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a <= b ? -1 : 0);
}

void op_ge(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a >= b ? -1 : 0);
}

void op_ne(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a != b ? -1 : 0);
}

/* Output operations */
void op_print(Stack* stack) {
    int32_t value = pop(stack);
    printf("%d ", value);
}

void op_cr(Stack* stack) {
    (void)stack;
    printf("\n");
}

void op_space(Stack* stack) {
    (void)stack;
    printf(" ");
}

void op_emit(Stack* stack) {
    int32_t c = pop(stack);
    printf("%c", (char)c);
}
