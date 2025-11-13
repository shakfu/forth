#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>

#define MAX_STACK_SIZE 100
#define MAX_WORD_LENGTH 32
#define MAX_WORDS 100
#define MAX_INPUT_LENGTH 256

// Stack structure
typedef struct {
    int32_t data[MAX_STACK_SIZE];
    int top;
} Stack;

// Word structure for dictionary
typedef struct {
    char name[MAX_WORD_LENGTH];
    void (*function)(Stack* stack);
    int is_primitive;
} Word;

// Global variables
Stack stack;
Word dictionary[MAX_WORDS];
int dict_count = 0;

// Stack operations
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
        printf("Stack underflow!\n");
        return 0;
    }
    return s->data[s->top];
}

// Dictionary operations
int find_word(const char* name) {
    for (int i = 0; i < dict_count; i++) {
        if (strcmp(dictionary[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

void add_word(const char* name, void (*function)(Stack* stack), int is_primitive) {
    if (dict_count >= MAX_WORDS) {
        printf("Dictionary full!\n");
        return;
    }
    
    strcpy(dictionary[dict_count].name, name);
    dictionary[dict_count].function = function;
    dictionary[dict_count].is_primitive = is_primitive;
    dict_count++;
}

// Primitive operations
void op_plus(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a + b);
}

void op_minus(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a - b);
}

void op_multiply(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, a * b);
}

void op_divide(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    if (b == 0) {
        printf("Division by zero!\n");
        return;
    }
    push(stack, a / b);
}

void op_swap(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, b);
    push(stack, a);
}

void op_dup(Stack* stack) {
    int32_t a = peek(stack);
    push(stack, a);
}

void op_drop(Stack* stack) {
    pop(stack);
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

void op_print(Stack* stack) {
    printf("%d ", pop(stack));
}

void op_cr(Stack* stack) {
    printf("\n");
}

void op_dot(Stack* stack) {
    int32_t value = pop(stack);
    printf("%d ", value);
}

void op_space(Stack* stack) {
    printf(" ");
}

void op_lit(Stack* stack) {
    // This is a special case - literal values are handled differently
    // For simplicity, we'll just push the next value as a literal
    // In a real implementation, this would be part of parsing
}

// Define some additional operations
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

void op_eq(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, (a == b) ? 1 : 0);
}

void op_lt(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, (a < b) ? 1 : 0);
}

void op_gt(Stack* stack) {
    int32_t b = pop(stack);
    int32_t a = pop(stack);
    push(stack, (a > b) ? 1 : 0);
}

void op_if(Stack* stack) {
    // Simplified implementation - would require more complex parsing in real Forth
    printf("IF not implemented in this simple version\n");
}

// Initialize the dictionary with primitive words
void init_dictionary() {
    add_word("+", op_plus, 1);
    add_word("-", op_minus, 1);
    add_word("*", op_multiply, 1);
    add_word("/", op_divide, 1);
    add_word("swap", op_swap, 1);
    add_word("dup", op_dup, 1);
    add_word("drop", op_drop, 1);
    add_word("over", op_over, 1);
    add_word("rot", op_rot, 1);
    add_word(".", op_dot, 1);
    add_word("cr", op_cr, 1);
    add_word("space", op_space, 1);
    add_word("and", op_and, 1);
    add_word("or", op_or, 1);
    add_word("xor", op_xor, 1);
    add_word("not", op_not, 1);
    add_word("=", op_eq, 1);
    add_word("<", op_lt, 1);
    add_word(">", op_gt, 1);
    add_word("if", op_if, 1);
    
    // Special words for literals
    add_word("lit", op_lit, 1);
}

// Parse and execute a single word
void execute_word(const char* word) {
    int index = find_word(word);
    
    if (index == -1) {
        // Try to parse as a number
        char* endptr;
        long num = strtol(word, &endptr, 10);
        if (*endptr == '\0') {
            push(&stack, (int32_t)num);
        } else {
            printf("Unknown word: %s\n", word);
        }
        return;
    }
    
    // Execute the function for this word
    dictionary[index].function(&stack);
}

// Parse and execute a command line
void execute_line(const char* input) {
    char word[MAX_WORD_LENGTH];
    int i = 0;
    int start = 0;
    
    while (input[i] != '\0') {
        // Skip whitespace
        while (isspace(input[i])) {
            i++;
        }
        
        if (input[i] == '\0') break;
        
        // Extract next word
        start = i;
        while (!isspace(input[i]) && input[i] != '\0') {
            i++;
        }
        
        int len = i - start;
        if (len >= MAX_WORD_LENGTH) {
            printf("Word too long\n");
            return;
        }
        
        strncpy(word, input + start, len);
        word[len] = '\0';
        
        // Execute the word
        execute_word(word);
    }
}

// Interactive interpreter loop
void interpreter_loop() {
    char input[MAX_INPUT_LENGTH];
    
    printf("Forth-like interpreter (type 'quit' to exit)\n");
    printf("Example: 1 2 + . cr\n");
    
    while (1) {
        printf("> ");
        
        if (!fgets(input, sizeof(input), stdin)) {
            break;
        }
        
        // Remove newline
        input[strcspn(input, "\n")] = 0;
        
        // Check for quit command
        if (strcmp(input, "quit") == 0) {
            break;
        }
        
        execute_line(input);
        printf("\n");
    }
}

// Example usage function
void run_examples() {
    printf("Running examples:\n");
    
    // Simple arithmetic
    printf("1 2 + . cr\n");
    execute_line("1 2 + . cr");
    
    printf("5 3 - . cr\n");
    execute_line("5 3 - . cr");
    
    printf("4 6 * . cr\n");
    execute_line("4 6 * . cr");
    
    printf("10 2 / . cr\n");
    execute_line("10 2 / . cr");
    
    // Stack operations
    printf("\nStack operations:\n");
    printf("1 2 3 swap . . . cr\n");
    execute_line("1 2 3 swap . . . cr");
    
    printf("1 2 dup . . cr\n");
    execute_line("1 2 dup . . cr");
    
    // Bitwise operations
    printf("\nBitwise operations:\n");
    printf("5 3 and . cr\n");
    execute_line("5 3 and . cr");
    
    printf("5 3 or . cr\n");
    execute_line("5 3 or . cr");
    
    printf("5 3 xor . cr\n");
    execute_line("5 3 xor . cr");
}

int main() {
    // Initialize stack
    stack.top = -1;
    
    // Initialize dictionary with primitives
    init_dictionary();
    
    // Run examples first
    run_examples();
    
    // Start interactive interpreter
    interpreter_loop();
    
    return 0;
}
