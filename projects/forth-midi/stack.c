/* stack.c - Stack operations for MIDI Forth interpreter */

#include "forth_midi.h"

void push(Stack* s, int32_t value) {
    if (s->top >= MAX_STACK_SIZE - 1) {
        printf("Stack overflow!\n");
        return;
    }
    s->data[++(s->top)] = value;
}

int32_t pop(Stack* s) {
    if (s->top < 0) {
        printf("Stack underflow!\n");
        return 0;
    }
    return s->data[(s->top)--];
}

int32_t peek(Stack* s) {
    if (s->top < 0) {
        printf("Stack empty!\n");
        return 0;
    }
    return s->data[s->top];
}

void op_dup(Stack* stack) {
    int32_t a = peek(stack);
    push(stack, a);
}

void op_drop(Stack* stack) {
    pop(stack);
}

void op_swap(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, b);
    push(stack, a);
}

void op_over(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = peek(stack);
    push(stack, b);
    push(stack, a);
}

void op_rot(Stack* stack) {
    int32_t c = pop(stack);
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, b);
    push(stack, c);
    push(stack, a);
}

void op_depth(Stack* stack) {
    push(stack, stack->top + 1);
}

void op_clear(Stack* stack) {
    stack->top = -1;
}

void op_print_stack(Stack* stack) {
    printf("<%d> ", stack->top + 1);
    for (int i = 0; i <= stack->top; i++) {
        printf("%d ", stack->data[i]);
    }
}
