/**
 * joy_runtime.c - Joy language runtime implementation
 */

#include "joy_runtime.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---------- Memory Helpers ---------- */

static void* joy_alloc(size_t size) {
    void* ptr = malloc(size);
    if (!ptr) {
        joy_error("Out of memory");
    }
    return ptr;
}

static void* joy_realloc(void* ptr, size_t size) {
    void* new_ptr = realloc(ptr, size);
    if (!new_ptr) {
        joy_error("Out of memory");
    }
    return new_ptr;
}

static char* joy_strdup(const char* s) {
    if (!s) return NULL;
    size_t len = strlen(s) + 1;
    char* copy = joy_alloc(len);
    memcpy(copy, s, len);
    return copy;
}

/* ---------- Error Handling ---------- */

/* Current context for error recovery (thread-local would be better for MT) */
static JoyContext* g_current_ctx = NULL;

void joy_set_current_context(JoyContext* ctx) {
    g_current_ctx = ctx;
}

static void joy_do_error(void) {
    if (g_current_ctx && g_current_ctx->error_jmp) {
        longjmp(*g_current_ctx->error_jmp, 1);
    }
    exit(1);
}

void joy_error(const char* message) {
    fprintf(stderr, "Joy error: %s\n", message);
    joy_do_error();
}

void joy_error_type(const char* op, const char* expected, JoyType got) {
    const char* type_names[] = {
        "INTEGER", "FLOAT", "BOOLEAN", "CHAR", "STRING",
        "LIST", "SET", "QUOTATION", "SYMBOL", "FILE"
    };
    fprintf(stderr, "Joy type error in '%s': expected %s, got %s\n",
            op, expected, type_names[got]);
    joy_do_error();
}

void joy_error_underflow(const char* op, size_t required, size_t actual) {
    fprintf(stderr, "Joy stack underflow in '%s': need %zu, have %zu\n",
            op, required, actual);
    joy_do_error();
}

/* ---------- Value Constructors ---------- */

JoyValue joy_integer(int64_t value) {
    JoyValue v = {.type = JOY_INTEGER};
    v.data.integer = value;
    return v;
}

JoyValue joy_float(double value) {
    JoyValue v = {.type = JOY_FLOAT};
    v.data.floating = value;
    return v;
}

JoyValue joy_boolean(bool value) {
    JoyValue v = {.type = JOY_BOOLEAN};
    v.data.boolean = value;
    return v;
}

JoyValue joy_char(char value) {
    JoyValue v = {.type = JOY_CHAR};
    v.data.character = value;
    return v;
}

JoyValue joy_string(const char* value) {
    JoyValue v = {.type = JOY_STRING};
    v.data.string = joy_strdup(value);
    return v;
}

JoyValue joy_string_owned(char* value) {
    JoyValue v = {.type = JOY_STRING};
    v.data.string = value;
    return v;
}

JoyValue joy_list_empty(void) {
    JoyValue v = {.type = JOY_LIST};
    v.data.list = joy_list_new(8);
    return v;
}

JoyValue joy_list_from(JoyValue* items, size_t count) {
    JoyValue v = {.type = JOY_LIST};
    v.data.list = joy_list_new(count > 8 ? count : 8);
    for (size_t i = 0; i < count; i++) {
        joy_list_push(v.data.list, joy_value_copy(items[i]));
    }
    return v;
}

JoyValue joy_set_empty(void) {
    JoyValue v = {.type = JOY_SET};
    v.data.set = 0;
    return v;
}

JoyValue joy_set_from(int* members, size_t count) {
    JoyValue v = {.type = JOY_SET};
    v.data.set = 0;
    for (size_t i = 0; i < count; i++) {
        if (members[i] >= 0 && members[i] < 64) {
            v.data.set |= (1ULL << members[i]);
        }
    }
    return v;
}

JoyValue joy_quotation_empty(void) {
    JoyValue v = {.type = JOY_QUOTATION};
    v.data.quotation = joy_quotation_new(8);
    return v;
}

JoyValue joy_quotation_from(JoyValue* terms, size_t count) {
    JoyValue v = {.type = JOY_QUOTATION};
    v.data.quotation = joy_quotation_new(count > 8 ? count : 8);
    for (size_t i = 0; i < count; i++) {
        joy_quotation_push(v.data.quotation, joy_value_copy(terms[i]));
    }
    return v;
}

JoyValue joy_symbol(const char* name) {
    JoyValue v = {.type = JOY_SYMBOL};
    v.data.symbol = joy_strdup(name);
    return v;
}

JoyValue joy_file(FILE* file) {
    JoyValue v = {.type = JOY_FILE};
    v.data.file = file;
    return v;
}

/* ---------- Value Operations ---------- */

JoyValue joy_value_copy(JoyValue value) {
    JoyValue copy = value;
    switch (value.type) {
        case JOY_STRING:
            copy.data.string = joy_strdup(value.data.string);
            break;
        case JOY_SYMBOL:
            copy.data.symbol = joy_strdup(value.data.symbol);
            break;
        case JOY_LIST:
            copy.data.list = joy_list_copy(value.data.list);
            break;
        case JOY_QUOTATION:
            copy.data.quotation = joy_quotation_copy(value.data.quotation);
            break;
        default:
            break;  /* primitives are copied by value */
    }
    return copy;
}

void joy_value_free(JoyValue* value) {
    switch (value->type) {
        case JOY_STRING:
            free(value->data.string);
            value->data.string = NULL;
            break;
        case JOY_SYMBOL:
            free(value->data.symbol);
            value->data.symbol = NULL;
            break;
        case JOY_LIST:
            joy_list_free(value->data.list);
            value->data.list = NULL;
            break;
        case JOY_QUOTATION:
            joy_quotation_free(value->data.quotation);
            value->data.quotation = NULL;
            break;
        default:
            break;
    }
}

/* Get numeric value for Joy comparison. Returns true if value has numeric interpretation.
 * Joy treats many types as having numeric interpretations:
 * - INTEGER/FLOAT: direct value
 * - CHAR: ordinal value
 * - BOOLEAN: 1 for true, 0 for false
 * - SET: bitset integer (the set bits directly)
 * - LIST/QUOTATION: 0 if empty, otherwise not numeric
 * - STRING: 0 if empty, otherwise not numeric
 * - FILE: 0 if NULL (failed open), otherwise not numeric
 */
bool joy_numeric_value(JoyValue v, double* result) {
    switch (v.type) {
        case JOY_INTEGER:
            *result = (double)v.data.integer;
            return true;
        case JOY_FLOAT:
            *result = v.data.floating;
            return true;
        case JOY_CHAR:
            *result = (double)(unsigned char)v.data.character;
            return true;
        case JOY_BOOLEAN:
            *result = v.data.boolean ? 1.0 : 0.0;
            return true;
        case JOY_SET:
            *result = (double)v.data.set;
            return true;
        case JOY_LIST:
            if (v.data.list->length == 0) {
                *result = 0.0;
                return true;
            }
            return false;
        case JOY_QUOTATION:
            if (v.data.quotation->length == 0) {
                *result = 0.0;
                return true;
            }
            return false;
        case JOY_STRING:
            if (v.data.string[0] == '\0') {
                *result = 0.0;
                return true;
            }
            return false;
        case JOY_FILE:
            if (v.data.file == NULL) {
                *result = 0.0;
                return true;
            }
            return false;
        default:
            return false;
    }
}

/* Joy's = equality semantics:
 * - Non-empty lists/quotations are NEVER equal with =
 * - Symbols compare by name (with each other and with strings)
 * - Numeric types compare by value (int, float, char, bool, set, empty containers)
 */
bool joy_value_equal(JoyValue a, JoyValue b) {
    /* Non-empty lists/quotations are never equal with = */
    if (a.type == JOY_LIST && a.data.list->length > 0) return false;
    if (a.type == JOY_QUOTATION && a.data.quotation->length > 0) return false;
    if (b.type == JOY_LIST && b.data.list->length > 0) return false;
    if (b.type == JOY_QUOTATION && b.data.quotation->length > 0) return false;

    /* Symbol comparison */
    if (a.type == JOY_SYMBOL && b.type == JOY_SYMBOL) {
        return strcmp(a.data.symbol, b.data.symbol) == 0;
    }
    /* Symbol vs String */
    if (a.type == JOY_SYMBOL && b.type == JOY_STRING) {
        return strcmp(a.data.symbol, b.data.string) == 0;
    }
    if (a.type == JOY_STRING && b.type == JOY_SYMBOL) {
        return strcmp(a.data.string, b.data.symbol) == 0;
    }
    /* String vs String */
    if (a.type == JOY_STRING && b.type == JOY_STRING) {
        return strcmp(a.data.string, b.data.string) == 0;
    }

    /* FLOAT vs SET: compare IEEE 754 bit representation */
    if (a.type == JOY_FLOAT && b.type == JOY_SET) {
        uint64_t float_bits;
        memcpy(&float_bits, &a.data.floating, sizeof(double));
        return float_bits == b.data.set;
    }
    if (a.type == JOY_SET && b.type == JOY_FLOAT) {
        uint64_t float_bits;
        memcpy(&float_bits, &b.data.floating, sizeof(double));
        return a.data.set == float_bits;
    }

    /* FILE comparison: equal if same file pointer */
    if (a.type == JOY_FILE && b.type == JOY_FILE) {
        return a.data.file == b.data.file;
    }

    /* Try numeric comparison */
    double av, bv;
    if (joy_numeric_value(a, &av) && joy_numeric_value(b, &bv)) {
        return av == bv;
    }

    return false;
}

bool joy_value_truthy(JoyValue value) {
    switch (value.type) {
        case JOY_BOOLEAN:
            return value.data.boolean;
        case JOY_INTEGER:
            return value.data.integer != 0;
        case JOY_FLOAT:
            return value.data.floating != 0.0;
        case JOY_STRING:
            return value.data.string[0] != '\0';
        case JOY_LIST:
            return value.data.list->length > 0;
        case JOY_SET:
            return value.data.set != 0;
        case JOY_QUOTATION:
            return value.data.quotation->length > 0;
        default:
            return true;
    }
}

void joy_value_print(JoyValue value) {
    switch (value.type) {
        case JOY_INTEGER:
            printf("%lld", (long long)value.data.integer);
            break;
        case JOY_FLOAT:
            printf("%g", value.data.floating);
            break;
        case JOY_BOOLEAN:
            printf("%s", value.data.boolean ? "true" : "false");
            break;
        case JOY_CHAR:
            printf("'%c'", value.data.character);
            break;
        case JOY_STRING:
            printf("\"%s\"", value.data.string);
            break;
        case JOY_LIST:
            printf("[");
            for (size_t i = 0; i < value.data.list->length; i++) {
                if (i > 0) printf(" ");
                joy_value_print(value.data.list->items[i]);
            }
            printf("]");
            break;
        case JOY_SET:
            printf("{");
            bool first = true;
            for (int i = 0; i < 64; i++) {
                if (value.data.set & (1ULL << i)) {
                    if (!first) printf(" ");
                    printf("%d", i);
                    first = false;
                }
            }
            printf("}");
            break;
        case JOY_QUOTATION:
            printf("[");
            for (size_t i = 0; i < value.data.quotation->length; i++) {
                if (i > 0) printf(" ");
                joy_value_print(value.data.quotation->terms[i]);
            }
            printf("]");
            break;
        case JOY_SYMBOL:
            printf("%s", value.data.symbol);
            break;
        case JOY_FILE:
            if (value.data.file == stdin)
                printf("<stdin>");
            else if (value.data.file == stdout)
                printf("<stdout>");
            else if (value.data.file == stderr)
                printf("<stderr>");
            else
                printf("<file:%p>", (void*)value.data.file);
            break;
    }
}

/* ---------- List Operations ---------- */

JoyList* joy_list_new(size_t initial_capacity) {
    JoyList* list = joy_alloc(sizeof(JoyList));
    list->capacity = initial_capacity > 0 ? initial_capacity : 8;
    list->items = joy_alloc(list->capacity * sizeof(JoyValue));
    list->length = 0;
    return list;
}

void joy_list_free(JoyList* list) {
    if (!list) return;
    for (size_t i = 0; i < list->length; i++) {
        joy_value_free(&list->items[i]);
    }
    free(list->items);
    free(list);
}

void joy_list_push(JoyList* list, JoyValue value) {
    if (list->length >= list->capacity) {
        list->capacity *= 2;
        list->items = joy_realloc(list->items, list->capacity * sizeof(JoyValue));
    }
    list->items[list->length++] = value;
}

JoyValue joy_list_pop(JoyList* list) {
    if (list->length == 0) {
        joy_error("Cannot pop from empty list");
    }
    return list->items[--list->length];
}

JoyValue joy_list_first(JoyList* list) {
    if (list->length == 0) {
        joy_error("Cannot get first of empty list");
    }
    return joy_value_copy(list->items[0]);
}

JoyList* joy_list_rest(JoyList* list) {
    JoyList* rest = joy_list_new(list->length > 1 ? list->length - 1 : 8);
    for (size_t i = 1; i < list->length; i++) {
        joy_list_push(rest, joy_value_copy(list->items[i]));
    }
    return rest;
}

JoyList* joy_list_copy(JoyList* list) {
    JoyList* copy = joy_list_new(list->capacity);
    for (size_t i = 0; i < list->length; i++) {
        joy_list_push(copy, joy_value_copy(list->items[i]));
    }
    return copy;
}

size_t joy_list_length(JoyList* list) {
    return list->length;
}

bool joy_list_null(JoyList* list) {
    return list->length == 0;
}

JoyList* joy_list_concat(JoyList* a, JoyList* b) {
    JoyList* result = joy_list_new(a->length + b->length);
    for (size_t i = 0; i < a->length; i++) {
        joy_list_push(result, joy_value_copy(a->items[i]));
    }
    for (size_t i = 0; i < b->length; i++) {
        joy_list_push(result, joy_value_copy(b->items[i]));
    }
    return result;
}

JoyList* joy_list_cons(JoyValue value, JoyList* list) {
    JoyList* result = joy_list_new(list->length + 1);
    joy_list_push(result, joy_value_copy(value));
    for (size_t i = 0; i < list->length; i++) {
        joy_list_push(result, joy_value_copy(list->items[i]));
    }
    return result;
}

/* ---------- Quotation Operations ---------- */

JoyQuotation* joy_quotation_new(size_t initial_capacity) {
    JoyQuotation* quot = joy_alloc(sizeof(JoyQuotation));
    quot->capacity = initial_capacity > 0 ? initial_capacity : 8;
    quot->terms = joy_alloc(quot->capacity * sizeof(JoyValue));
    quot->length = 0;
    return quot;
}

void joy_quotation_free(JoyQuotation* quotation) {
    if (!quotation) return;
    for (size_t i = 0; i < quotation->length; i++) {
        joy_value_free(&quotation->terms[i]);
    }
    free(quotation->terms);
    free(quotation);
}

void joy_quotation_push(JoyQuotation* quotation, JoyValue term) {
    if (quotation->length >= quotation->capacity) {
        quotation->capacity *= 2;
        quotation->terms = joy_realloc(quotation->terms, quotation->capacity * sizeof(JoyValue));
    }
    quotation->terms[quotation->length++] = term;
}

JoyQuotation* joy_quotation_copy(JoyQuotation* quotation) {
    JoyQuotation* copy = joy_quotation_new(quotation->capacity);
    for (size_t i = 0; i < quotation->length; i++) {
        joy_quotation_push(copy, joy_value_copy(quotation->terms[i]));
    }
    return copy;
}

JoyQuotation* joy_quotation_concat(JoyQuotation* a, JoyQuotation* b) {
    JoyQuotation* result = joy_quotation_new(a->length + b->length);
    for (size_t i = 0; i < a->length; i++) {
        joy_quotation_push(result, joy_value_copy(a->terms[i]));
    }
    for (size_t i = 0; i < b->length; i++) {
        joy_quotation_push(result, joy_value_copy(b->terms[i]));
    }
    return result;
}

/* ---------- Set Operations ---------- */

bool joy_set_member(uint64_t set, int member) {
    if (member < 0 || member >= 64) return false;
    return (set & (1ULL << member)) != 0;
}

uint64_t joy_set_insert(uint64_t set, int member) {
    if (member < 0 || member >= 64) return set;
    return set | (1ULL << member);
}

uint64_t joy_set_remove(uint64_t set, int member) {
    if (member < 0 || member >= 64) return set;
    return set & ~(1ULL << member);
}

uint64_t joy_set_union(uint64_t a, uint64_t b) {
    return a | b;
}

uint64_t joy_set_intersection(uint64_t a, uint64_t b) {
    return a & b;
}

uint64_t joy_set_difference(uint64_t a, uint64_t b) {
    return a & ~b;
}

size_t joy_set_size(uint64_t set) {
    size_t count = 0;
    while (set) {
        count += set & 1;
        set >>= 1;
    }
    return count;
}

/* ---------- Stack Operations ---------- */

JoyStack* joy_stack_new(size_t initial_capacity) {
    JoyStack* stack = joy_alloc(sizeof(JoyStack));
    stack->capacity = initial_capacity > 0 ? initial_capacity : 64;
    stack->items = joy_alloc(stack->capacity * sizeof(JoyValue));
    stack->depth = 0;
    return stack;
}

void joy_stack_free(JoyStack* stack) {
    if (!stack) return;
    for (size_t i = 0; i < stack->depth; i++) {
        joy_value_free(&stack->items[i]);
    }
    free(stack->items);
    free(stack);
}

void joy_stack_push(JoyStack* stack, JoyValue value) {
    if (stack->depth >= stack->capacity) {
        stack->capacity *= 2;
        stack->items = joy_realloc(stack->items, stack->capacity * sizeof(JoyValue));
    }
    stack->items[stack->depth++] = value;
}

JoyValue joy_stack_pop(JoyStack* stack) {
    if (stack->depth == 0) {
        joy_error("Stack underflow");
    }
    return stack->items[--stack->depth];
}

JoyValue joy_stack_peek(JoyStack* stack) {
    if (stack->depth == 0) {
        joy_error("Stack underflow");
    }
    return stack->items[stack->depth - 1];
}

JoyValue joy_stack_peek_n(JoyStack* stack, size_t n) {
    if (n >= stack->depth) {
        joy_error("Stack underflow");
    }
    return stack->items[stack->depth - 1 - n];
}

void joy_stack_dup(JoyStack* stack) {
    if (stack->depth == 0) {
        joy_error_underflow("dup", 1, 0);
    }
    joy_stack_push(stack, joy_value_copy(stack->items[stack->depth - 1]));
}

void joy_stack_swap(JoyStack* stack) {
    if (stack->depth < 2) {
        joy_error_underflow("swap", 2, stack->depth);
    }
    JoyValue tmp = stack->items[stack->depth - 1];
    stack->items[stack->depth - 1] = stack->items[stack->depth - 2];
    stack->items[stack->depth - 2] = tmp;
}

void joy_stack_pop_free(JoyStack* stack) {
    if (stack->depth == 0) {
        joy_error("Stack underflow");
    }
    joy_value_free(&stack->items[--stack->depth]);
}

size_t joy_stack_depth(JoyStack* stack) {
    return stack->depth;
}

void joy_stack_clear(JoyStack* stack) {
    while (stack->depth > 0) {
        joy_value_free(&stack->items[--stack->depth]);
    }
}

JoyStack* joy_stack_copy(JoyStack* stack) {
    JoyStack* copy = joy_stack_new(stack->capacity);
    for (size_t i = 0; i < stack->depth; i++) {
        joy_stack_push(copy, joy_value_copy(stack->items[i]));
    }
    return copy;
}

void joy_stack_print(JoyStack* stack) {
    printf("Stack(%zu): ", stack->depth);
    for (size_t i = 0; i < stack->depth; i++) {
        if (i > 0) printf(" ");
        joy_value_print(stack->items[i]);
    }
    printf("\n");
}

/* ---------- Dictionary Operations ---------- */

static size_t hash_string(const char* s) {
    size_t hash = 5381;
    int c;
    while ((c = *s++)) {
        hash = ((hash << 5) + hash) + c;
    }
    return hash;
}

JoyDict* joy_dict_new(void) {
    JoyDict* dict = joy_alloc(sizeof(JoyDict));
    dict->bucket_count = 256;
    dict->buckets = joy_alloc(dict->bucket_count * sizeof(JoyDictEntry*));
    memset(dict->buckets, 0, dict->bucket_count * sizeof(JoyDictEntry*));
    dict->count = 0;
    return dict;
}

void joy_dict_free(JoyDict* dict) {
    if (!dict) return;
    for (size_t i = 0; i < dict->bucket_count; i++) {
        JoyDictEntry* entry = dict->buckets[i];
        while (entry) {
            JoyDictEntry* next = entry->next;
            free(entry->key);
            if (!entry->word->is_primitive && entry->word->body.quotation) {
                joy_quotation_free(entry->word->body.quotation);
            }
            free(entry->word->name);
            free(entry->word);
            free(entry);
            entry = next;
        }
    }
    free(dict->buckets);
    free(dict);
}

static void joy_dict_set(JoyDict* dict, const char* name, JoyWord* word) {
    size_t bucket = hash_string(name) % dict->bucket_count;

    /* Check if exists */
    JoyDictEntry* entry = dict->buckets[bucket];
    while (entry) {
        if (strcmp(entry->key, name) == 0) {
            /* Replace existing */
            if (!entry->word->is_primitive && entry->word->body.quotation) {
                joy_quotation_free(entry->word->body.quotation);
            }
            free(entry->word->name);
            free(entry->word);
            entry->word = word;
            return;
        }
        entry = entry->next;
    }

    /* Add new */
    entry = joy_alloc(sizeof(JoyDictEntry));
    entry->key = joy_strdup(name);
    entry->word = word;
    entry->next = dict->buckets[bucket];
    dict->buckets[bucket] = entry;
    dict->count++;
}

void joy_dict_define_primitive(JoyDict* dict, const char* name, JoyPrimitive fn) {
    JoyWord* word = joy_alloc(sizeof(JoyWord));
    word->name = joy_strdup(name);
    word->is_primitive = true;
    word->is_user = false;  /* Built-in primitive */
    word->is_seq = false;
    word->body.primitive = fn;
    joy_dict_set(dict, name, word);
}

void joy_dict_define_user(JoyDict* dict, const char* name, JoyPrimitive fn) {
    /* Like joy_dict_define_primitive, but marks as user-defined.
     * Used for compiled user definitions that execute as C functions but should
     * be recognized as user-defined words by the 'user' predicate. */
    JoyWord* word = joy_alloc(sizeof(JoyWord));
    word->name = joy_strdup(name);
    word->is_primitive = true;   /* Body is a function pointer */
    word->is_user = true;        /* Mark as user-defined for 'user' predicate */
    word->is_seq = false;
    word->body.primitive = fn;
    joy_dict_set(dict, name, word);
}

void joy_dict_define_quotation(JoyDict* dict, const char* name, JoyQuotation* quot) {
    JoyWord* word = joy_alloc(sizeof(JoyWord));
    word->name = joy_strdup(name);
    word->is_primitive = false;  /* Body is a quotation */
    word->is_user = true;        /* User-defined via quotation */
    word->is_seq = false;
    word->body.quotation = quot;
    joy_dict_set(dict, name, word);
}

void joy_dict_define_value(JoyDict* dict, const char* name, JoyValue value) {
    /* Define a word that pushes a single value when executed */
    JoyQuotation* quot = joy_quotation_new(1);
    joy_quotation_push(quot, joy_value_copy(value));
    joy_dict_define_quotation(dict, name, quot);
}

bool joy_dict_remove(JoyDict* dict, const char* name) {
    size_t bucket = hash_string(name) % dict->bucket_count;
    JoyDictEntry* entry = dict->buckets[bucket];
    JoyDictEntry* prev = NULL;

    while (entry) {
        if (strcmp(entry->key, name) == 0) {
            /* Found - unlink and free */
            if (prev) {
                prev->next = entry->next;
            } else {
                dict->buckets[bucket] = entry->next;
            }
            /* Free the word */
            if (entry->word->is_seq && entry->word->body.seq) {
                seq_definition_free(entry->word->body.seq);
            } else if (!entry->word->is_primitive && entry->word->body.quotation) {
                joy_quotation_free(entry->word->body.quotation);
            }
            free(entry->word->name);
            free(entry->word);
            free(entry->key);
            free(entry);
            dict->count--;
            return true;
        }
        prev = entry;
        entry = entry->next;
    }
    return false;  /* Not found */
}

void joy_dict_define_seq(JoyDict* dict, const char* name, SeqDefinition* seq) {
    JoyWord* word = joy_alloc(sizeof(JoyWord));
    word->name = joy_strdup(name);
    word->is_primitive = false;
    word->is_user = true;
    word->is_seq = true;
    word->body.seq = seq;
    joy_dict_set(dict, name, word);
}

/* ============================================================================
 * Sequence Definition Helpers
 * ============================================================================ */

SeqDefinition* seq_definition_new(void) {
    SeqDefinition* seq = joy_alloc(sizeof(SeqDefinition));
    seq->parts = NULL;
    seq->part_count = 0;
    seq->part_capacity = 0;
    return seq;
}

void seq_definition_free(SeqDefinition* seq) {
    if (!seq) return;
    for (size_t i = 0; i < seq->part_count; i++) {
        if (seq->parts[i].quotation) {
            joy_quotation_free(seq->parts[i].quotation);
        }
    }
    free(seq->parts);
    free(seq);
}

void seq_definition_add_part(SeqDefinition* seq, int channel, JoyQuotation* quotation) {
    if (!seq) return;

    /* Grow capacity if needed */
    if (seq->part_count >= seq->part_capacity) {
        size_t new_cap = seq->part_capacity == 0 ? 4 : seq->part_capacity * 2;
        seq->parts = realloc(seq->parts, new_cap * sizeof(SeqPart));
        seq->part_capacity = new_cap;
    }

    seq->parts[seq->part_count].channel = channel;
    seq->parts[seq->part_count].quotation = quotation;
    seq->part_count++;
}

JoyWord* joy_dict_lookup(JoyDict* dict, const char* name) {
    size_t bucket = hash_string(name) % dict->bucket_count;
    JoyDictEntry* entry = dict->buckets[bucket];
    while (entry) {
        if (strcmp(entry->key, name) == 0) {
            return entry->word;
        }
        entry = entry->next;
    }
    return NULL;
}

/* ---------- Execution ---------- */

JoyContext* joy_context_new(void) {
    JoyContext* ctx = joy_alloc(sizeof(JoyContext));
    ctx->stack = joy_stack_new(64);
    ctx->dictionary = joy_dict_new();
    ctx->trace_enabled = false;
    ctx->autoput = 1;      /* on by default (matches Joy42) */
    ctx->undeferror = 0;   /* undefined symbols are errors by default */
    ctx->echo = 0;         /* no echo by default */
    ctx->undef_handler = NULL;
    ctx->user_data = NULL;
    ctx->error_jmp = NULL; /* NULL = exit on error, set for REPL recovery */
    ctx->post_eval_hook = NULL; /* called after each line evaluation */
    return ctx;
}

void joy_context_free(JoyContext* ctx) {
    if (!ctx) return;
    joy_stack_free(ctx->stack);
    joy_dict_free(ctx->dictionary);
    free(ctx);
}

void joy_execute_value(JoyContext* ctx, JoyValue value) {
    if (ctx->trace_enabled) {
        printf("  exec: ");
        joy_value_print(value);
        printf("\n");
    }

    switch (value.type) {
        case JOY_SYMBOL:
            joy_execute_symbol(ctx, value.data.symbol);
            break;
        default:
            /* Push literals onto the stack */
            joy_stack_push(ctx->stack, joy_value_copy(value));
            break;
    }
}

void joy_execute_quotation(JoyContext* ctx, JoyQuotation* quotation) {
    for (size_t i = 0; i < quotation->length; i++) {
        joy_execute_value(ctx, quotation->terms[i]);
    }
}

/* Forward declaration for seq execution handler (implemented in midi_primitives.c) */
extern void joy_execute_seq(JoyContext* ctx, SeqDefinition* seq);

void joy_execute_symbol(JoyContext* ctx, const char* name) {
    JoyWord* word = joy_dict_lookup(ctx->dictionary, name);
    if (!word) {
        /* Try custom handler first */
        if (ctx->undef_handler && ctx->undef_handler(ctx, name)) {
            return;  /* Handler processed it */
        }
        fprintf(stderr, "Undefined word: %s\n", name);
        joy_error("Undefined word");
    }

    if (word->is_seq) {
        /* Execute sequence definition */
        joy_execute_seq(ctx, word->body.seq);
    } else if (word->is_primitive) {
        word->body.primitive(ctx);
    } else {
        joy_execute_quotation(ctx, word->body.quotation);
    }
}

void joy_runtime_init(JoyContext* ctx) {
    joy_register_primitives(ctx);
}
