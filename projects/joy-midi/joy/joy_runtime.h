/**
 * joy_runtime.h - Joy language runtime for C
 *
 * This header defines the core types and operations for executing
 * Joy programs compiled to C.
 */

#ifndef JOY_RUNTIME_H
#define JOY_RUNTIME_H

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <setjmp.h>

/* ---------- Joy Type System ---------- */

typedef enum {
    JOY_INTEGER,
    JOY_FLOAT,
    JOY_BOOLEAN,
    JOY_CHAR,
    JOY_STRING,
    JOY_LIST,
    JOY_SET,
    JOY_QUOTATION,
    JOY_SYMBOL,
    JOY_FILE
} JoyType;

/* Forward declarations */
typedef struct JoyValue JoyValue;
typedef struct JoyList JoyList;
typedef struct JoyQuotation JoyQuotation;
typedef struct JoyStack JoyStack;

/* Joy List - dynamic array of JoyValues */
struct JoyList {
    JoyValue* items;
    size_t length;
    size_t capacity;
};

/* Joy Quotation - executable code block */
struct JoyQuotation {
    JoyValue* terms;
    size_t length;
    size_t capacity;
};

/* Joy Value - tagged union for all Joy types */
struct JoyValue {
    JoyType type;
    union {
        int64_t integer;
        double floating;
        bool boolean;
        char character;
        char* string;       /* owned, null-terminated */
        JoyList* list;      /* owned */
        uint64_t set;       /* bitset for 0-63 */
        JoyQuotation* quotation;  /* owned */
        char* symbol;       /* owned, null-terminated */
        FILE* file;         /* NOT owned - external file handle */
    } data;
};

/* Joy Stack - the main data stack */
struct JoyStack {
    JoyValue* items;
    size_t depth;
    size_t capacity;
};

/* ---------- Value Constructors ---------- */

JoyValue joy_integer(int64_t value);
JoyValue joy_float(double value);
JoyValue joy_boolean(bool value);
JoyValue joy_char(char value);
JoyValue joy_string(const char* value);
JoyValue joy_string_owned(char* value);  /* takes ownership */
JoyValue joy_list_empty(void);
JoyValue joy_list_from(JoyValue* items, size_t count);
JoyValue joy_set_empty(void);
JoyValue joy_set_from(int* members, size_t count);
JoyValue joy_quotation_empty(void);
JoyValue joy_quotation_from(JoyValue* terms, size_t count);
JoyValue joy_symbol(const char* name);
JoyValue joy_file(FILE* file);

/* ---------- Value Operations ---------- */

JoyValue joy_value_copy(JoyValue value);
void joy_value_free(JoyValue* value);
bool joy_value_equal(JoyValue a, JoyValue b);
bool joy_numeric_value(JoyValue v, double* result);
bool joy_value_truthy(JoyValue value);
void joy_value_print(JoyValue value);

/* ---------- List Operations ---------- */

JoyList* joy_list_new(size_t initial_capacity);
void joy_list_free(JoyList* list);
void joy_list_push(JoyList* list, JoyValue value);
JoyValue joy_list_pop(JoyList* list);
JoyValue joy_list_first(JoyList* list);
JoyList* joy_list_rest(JoyList* list);
JoyList* joy_list_copy(JoyList* list);
size_t joy_list_length(JoyList* list);
bool joy_list_null(JoyList* list);
JoyList* joy_list_concat(JoyList* a, JoyList* b);
JoyList* joy_list_cons(JoyValue value, JoyList* list);

/* ---------- Quotation Operations ---------- */

JoyQuotation* joy_quotation_new(size_t initial_capacity);
void joy_quotation_free(JoyQuotation* quotation);
void joy_quotation_push(JoyQuotation* quotation, JoyValue term);
JoyQuotation* joy_quotation_copy(JoyQuotation* quotation);
JoyQuotation* joy_quotation_concat(JoyQuotation* a, JoyQuotation* b);

/* ---------- Set Operations ---------- */

bool joy_set_member(uint64_t set, int member);
uint64_t joy_set_insert(uint64_t set, int member);
uint64_t joy_set_remove(uint64_t set, int member);
uint64_t joy_set_union(uint64_t a, uint64_t b);
uint64_t joy_set_intersection(uint64_t a, uint64_t b);
uint64_t joy_set_difference(uint64_t a, uint64_t b);
size_t joy_set_size(uint64_t set);

/* ---------- Stack Operations ---------- */

JoyStack* joy_stack_new(size_t initial_capacity);
void joy_stack_free(JoyStack* stack);
void joy_stack_push(JoyStack* stack, JoyValue value);
JoyValue joy_stack_pop(JoyStack* stack);
JoyValue joy_stack_peek(JoyStack* stack);
JoyValue joy_stack_peek_n(JoyStack* stack, size_t n);
void joy_stack_dup(JoyStack* stack);
void joy_stack_swap(JoyStack* stack);
void joy_stack_pop_free(JoyStack* stack);
size_t joy_stack_depth(JoyStack* stack);
void joy_stack_clear(JoyStack* stack);
JoyStack* joy_stack_copy(JoyStack* stack);
void joy_stack_print(JoyStack* stack);

/* ---------- Execution Context ---------- */

typedef struct JoyContext JoyContext;
typedef void (*JoyPrimitive)(JoyContext* ctx);

/* Sequence part - a (channel, quotation) pair for parallel execution */
typedef struct {
    int channel;
    JoyQuotation* quotation;
} SeqPart;

/* Sequence definition - multiple parts that execute in parallel */
typedef struct {
    SeqPart* parts;
    size_t part_count;
    size_t part_capacity;
} SeqDefinition;

/* Word definition */
typedef struct {
    char* name;
    bool is_primitive;  /* true = body.primitive is a function ptr, false = body.quotation */
    bool is_user;       /* true = user-defined word (for 'user' primitive), false = builtin */
    bool is_seq;        /* true = body.seq is a sequence definition */
    union {
        JoyPrimitive primitive;
        JoyQuotation* quotation;
        SeqDefinition* seq;
    } body;
} JoyWord;

/* Dictionary entry */
typedef struct JoyDictEntry {
    char* key;
    JoyWord* word;
    struct JoyDictEntry* next;
} JoyDictEntry;

/* Dictionary for word definitions */
typedef struct {
    JoyDictEntry** buckets;
    size_t bucket_count;
    size_t count;
} JoyDict;

/* Callback for handling undefined symbols (returns true if handled) */
typedef bool (*JoyUndefHandler)(JoyContext* ctx, const char* name);

/* Execution context */
/* Post-eval hook - called after each line of input is evaluated */
typedef void (*JoyPostEvalHook)(void);

struct JoyContext {
    JoyStack* stack;
    JoyDict* dictionary;
    bool trace_enabled;
    int autoput;      /* 0=off, 1=on (auto-print stack after each line) */
    int undeferror;   /* 0=off (undefined symbols are errors), 1=on (allow undefined) */
    int echo;         /* 0=none, 1=echo input, 2=echo output, 3=echo both */
    JoyUndefHandler undef_handler;  /* custom handler for undefined symbols */
    void* user_data;                /* user data (e.g., MusicContext*) */
    jmp_buf* error_jmp;             /* error recovery point (NULL = exit on error) */
    JoyPostEvalHook post_eval_hook; /* hook called after each line evaluation */
};

/* ---------- Dictionary Operations ---------- */

JoyDict* joy_dict_new(void);
void joy_dict_free(JoyDict* dict);
void joy_dict_define_primitive(JoyDict* dict, const char* name, JoyPrimitive fn);
void joy_dict_define_user(JoyDict* dict, const char* name, JoyPrimitive fn);
void joy_dict_define_quotation(JoyDict* dict, const char* name, JoyQuotation* quot);
void joy_dict_define_value(JoyDict* dict, const char* name, JoyValue value);
void joy_dict_define_seq(JoyDict* dict, const char* name, SeqDefinition* seq);
JoyWord* joy_dict_lookup(JoyDict* dict, const char* name);
bool joy_dict_remove(JoyDict* dict, const char* name);

/* Sequence definition helpers */
SeqDefinition* seq_definition_new(void);
void seq_definition_free(SeqDefinition* seq);
void seq_definition_add_part(SeqDefinition* seq, int channel, JoyQuotation* quotation);

/* ---------- Execution ---------- */

JoyContext* joy_context_new(void);
void joy_context_free(JoyContext* ctx);
void joy_execute_value(JoyContext* ctx, JoyValue value);
void joy_execute_quotation(JoyContext* ctx, JoyQuotation* quotation);
void joy_execute_symbol(JoyContext* ctx, const char* name);

/* ---------- Error Handling ---------- */

void joy_error(const char* message);
void joy_error_type(const char* op, const char* expected, JoyType got);
void joy_error_underflow(const char* op, size_t required, size_t actual);
void joy_set_current_context(JoyContext* ctx);  /* Set context for error recovery */

/* ---------- Runtime Initialization ---------- */

void joy_runtime_init(JoyContext* ctx);
void joy_register_primitives(JoyContext* ctx);

/* Command line argument support */
void joy_set_argv(int argc, char** argv);

#endif /* JOY_RUNTIME_H */
