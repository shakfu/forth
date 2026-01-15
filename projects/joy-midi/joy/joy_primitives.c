/**
 * joy_primitives.c - Joy primitive operations implemented in C
 */

/* Enable POSIX functions like strdup */
#define _POSIX_C_SOURCE 200809L

#include "joy_runtime.h"
#include "joy_parser.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <inttypes.h>
#include <sys/stat.h>

/* Global storage for command line arguments */
static int joy_argc = 0;
static char** joy_argv = NULL;

/* Function to initialize argc/argv - call from main() */
void joy_set_argv(int argc, char** argv) {
    joy_argc = argc;
    joy_argv = argv;
}

/* Helper macros */
#define REQUIRE(n, op) \
    if (ctx->stack->depth < (n)) joy_error_underflow(op, n, ctx->stack->depth)

#define POP() joy_stack_pop(ctx->stack)
#define PUSH(v) joy_stack_push(ctx->stack, v)
#define PEEK() joy_stack_peek(ctx->stack)
#define PEEK_N(n) joy_stack_peek_n(ctx->stack, n)

#define EXPECT_TYPE(v, t, op) \
    if ((v).type != (t)) joy_error_type(op, #t, (v).type)

/* ---------- Stack Operations ---------- */

static void prim_dup(JoyContext* ctx) {
    REQUIRE(1, "dup");
    joy_stack_dup(ctx->stack);
}

static void prim_pop(JoyContext* ctx) {
    REQUIRE(1, "pop");
    JoyValue v = POP();
    joy_value_free(&v);
}

static void prim_swap(JoyContext* ctx) {
    REQUIRE(2, "swap");
    joy_stack_swap(ctx->stack);
}

static void prim_rollup(JoyContext* ctx) {
    /* X Y Z -> Z X Y (Y ends on top) */
    REQUIRE(3, "rollup");
    JoyValue z = POP();
    JoyValue y = POP();
    JoyValue x = POP();
    PUSH(z);
    PUSH(x);
    PUSH(y);
}

static void prim_rolldown(JoyContext* ctx) {
    /* X Y Z -> Y Z X (X ends on top) */
    REQUIRE(3, "rolldown");
    JoyValue z = POP();
    JoyValue y = POP();
    JoyValue x = POP();
    PUSH(y);
    PUSH(z);
    PUSH(x);
}

static void prim_rotate(JoyContext* ctx) {
    REQUIRE(3, "rotate");
    JoyValue z = POP();
    JoyValue y = POP();
    JoyValue x = POP();
    PUSH(z);
    PUSH(y);
    PUSH(x);
}

static void prim_over(JoyContext* ctx) {
    /* X Y -> X Y X (copy second to top) */
    REQUIRE(2, "over");
    JoyValue y = POP();
    JoyValue x = PEEK();
    PUSH(y);
    PUSH(joy_value_copy(x));
}

static void prim_dup2(JoyContext* ctx) {
    /* X Y -> X Y X Y (duplicate top two) */
    REQUIRE(2, "dup2");
    JoyValue y = PEEK();
    JoyValue x = PEEK_N(1);
    PUSH(joy_value_copy(x));
    PUSH(joy_value_copy(y));
}

static void prim_dupd(JoyContext* ctx) {
    REQUIRE(2, "dupd");
    JoyValue y = POP();
    joy_stack_dup(ctx->stack);
    PUSH(y);
}

static void prim_swapd(JoyContext* ctx) {
    REQUIRE(3, "swapd");
    JoyValue z = POP();
    joy_stack_swap(ctx->stack);
    PUSH(z);
}

static void prim_popd(JoyContext* ctx) {
    REQUIRE(2, "popd");
    JoyValue y = POP();
    JoyValue x = POP();
    joy_value_free(&x);
    PUSH(y);
}

static void prim_rollupd(JoyContext* ctx) {
    /* X Y Z W -> Z X Y W : rollup under top */
    REQUIRE(4, "rollupd");
    JoyValue w = POP();
    JoyValue z = POP();
    JoyValue y = POP();
    JoyValue x = POP();
    PUSH(z);
    PUSH(x);
    PUSH(y);
    PUSH(w);
}

static void prim_rolldownd(JoyContext* ctx) {
    /* X Y Z W -> Y Z X W : rolldown under top */
    REQUIRE(4, "rolldownd");
    JoyValue w = POP();
    JoyValue z = POP();
    JoyValue y = POP();
    JoyValue x = POP();
    PUSH(y);
    PUSH(z);
    PUSH(x);
    PUSH(w);
}

static void prim_rotated(JoyContext* ctx) {
    /* X Y Z W -> Z Y X W : rotate under top */
    REQUIRE(4, "rotated");
    JoyValue w = POP();
    JoyValue z = POP();
    JoyValue y = POP();
    JoyValue x = POP();
    PUSH(z);
    PUSH(y);
    PUSH(x);
    PUSH(w);
}

static void prim_id(JoyContext* ctx) {
    /* Identity function - does nothing */
    (void)ctx;
}

static void prim_stack(JoyContext* ctx) {
    JoyValue list = joy_list_empty();
    for (size_t i = ctx->stack->depth; i > 0; i--) {
        joy_list_push(list.data.list, joy_value_copy(ctx->stack->items[i-1]));
    }
    PUSH(list);
}

static void prim_unstack(JoyContext* ctx) {
    REQUIRE(1, "unstack");
    JoyValue v = POP();
    joy_stack_clear(ctx->stack);
    if (v.type == JOY_LIST) {
        for (size_t i = v.data.list->length; i > 0; i--) {
            PUSH(joy_value_copy(v.data.list->items[i-1]));
        }
    } else if (v.type == JOY_QUOTATION) {
        for (size_t i = v.data.quotation->length; i > 0; i--) {
            PUSH(joy_value_copy(v.data.quotation->terms[i-1]));
        }
    } else {
        joy_error_type("unstack", "list or quotation", v.type);
    }
    joy_value_free(&v);
}

static void prim_pick(JoyContext* ctx) {
    /* X Y Z N -> X Y Z X : copy Nth item to top (0=dup, 1=over) */
    REQUIRE(2, "pick");
    JoyValue n = POP();
    EXPECT_TYPE(n, JOY_INTEGER, "pick");

    int64_t index = n.data.integer;
    joy_value_free(&n);

    if (ctx->stack->depth == 0) {
        joy_error_underflow("pick", 1, 0);
        return;
    }

    /* Clamp index to valid range */
    if (index < 0) index = 0;
    if ((size_t)index >= ctx->stack->depth) {
        index = ctx->stack->depth - 1;
    }

    /* Index 0 = top of stack, 1 = second, etc. */
    size_t pos = ctx->stack->depth - 1 - (size_t)index;
    PUSH(joy_value_copy(ctx->stack->items[pos]));
}

/* ---------- Arithmetic Operations ---------- */

static void prim_add(JoyContext* ctx) {
    REQUIRE(2, "+");
    JoyValue b = POP();
    JoyValue a = POP();

    if (a.type == JOY_INTEGER && b.type == JOY_INTEGER) {
        PUSH(joy_integer(a.data.integer + b.data.integer));
    } else if (a.type == JOY_FLOAT || b.type == JOY_FLOAT) {
        double av = a.type == JOY_FLOAT ? a.data.floating : (double)a.data.integer;
        double bv = b.type == JOY_FLOAT ? b.data.floating : (double)b.data.integer;
        PUSH(joy_float(av + bv));
    } else if (a.type == JOY_CHAR && b.type == JOY_INTEGER) {
        /* char + int -> char */
        PUSH(joy_char((char)(a.data.character + b.data.integer)));
    } else if (a.type == JOY_INTEGER && b.type == JOY_CHAR) {
        /* int + char -> char */
        PUSH(joy_char((char)(a.data.integer + b.data.character)));
    } else {
        joy_error_type("+", "number", a.type);
    }
}

static void prim_sub(JoyContext* ctx) {
    REQUIRE(2, "-");
    JoyValue b = POP();
    JoyValue a = POP();

    if (a.type == JOY_INTEGER && b.type == JOY_INTEGER) {
        PUSH(joy_integer(a.data.integer - b.data.integer));
    } else if (a.type == JOY_FLOAT || b.type == JOY_FLOAT) {
        double av = a.type == JOY_FLOAT ? a.data.floating : (double)a.data.integer;
        double bv = b.type == JOY_FLOAT ? b.data.floating : (double)b.data.integer;
        PUSH(joy_float(av - bv));
    } else if (a.type == JOY_CHAR && b.type == JOY_INTEGER) {
        /* char - int -> char */
        PUSH(joy_char((char)(a.data.character - b.data.integer)));
    } else if (a.type == JOY_CHAR && b.type == JOY_CHAR) {
        /* char - char -> int (distance) */
        PUSH(joy_integer(a.data.character - b.data.character));
    } else {
        joy_error_type("-", "number", a.type);
    }
}

static void prim_mul(JoyContext* ctx) {
    REQUIRE(2, "*");
    JoyValue b = POP();
    JoyValue a = POP();

    if (a.type == JOY_INTEGER && b.type == JOY_INTEGER) {
        PUSH(joy_integer(a.data.integer * b.data.integer));
    } else if (a.type == JOY_FLOAT || b.type == JOY_FLOAT) {
        double av = a.type == JOY_FLOAT ? a.data.floating : (double)a.data.integer;
        double bv = b.type == JOY_FLOAT ? b.data.floating : (double)b.data.integer;
        PUSH(joy_float(av * bv));
    } else {
        joy_error_type("*", "number", a.type);
    }
}

static void prim_div(JoyContext* ctx) {
    REQUIRE(2, "/");
    JoyValue b = POP();
    JoyValue a = POP();

    if (a.type == JOY_INTEGER && b.type == JOY_INTEGER) {
        if (b.data.integer == 0) joy_error("Division by zero");
        PUSH(joy_integer(a.data.integer / b.data.integer));
    } else if (a.type == JOY_FLOAT || b.type == JOY_FLOAT) {
        double av = a.type == JOY_FLOAT ? a.data.floating : (double)a.data.integer;
        double bv = b.type == JOY_FLOAT ? b.data.floating : (double)b.data.integer;
        if (bv == 0.0) joy_error("Division by zero");
        PUSH(joy_float(av / bv));
    } else {
        joy_error_type("/", "number", a.type);
    }
}

static void prim_rem(JoyContext* ctx) {
    REQUIRE(2, "rem");
    JoyValue b = POP();
    JoyValue a = POP();
    EXPECT_TYPE(a, JOY_INTEGER, "rem");
    EXPECT_TYPE(b, JOY_INTEGER, "rem");
    if (b.data.integer == 0) joy_error("Division by zero");
    PUSH(joy_integer(a.data.integer % b.data.integer));
}

static void prim_divmod(JoyContext* ctx) {
    /* N1 N2 -> Q R : integer division with remainder (quotient then remainder) */
    REQUIRE(2, "div");
    JoyValue b = POP();
    JoyValue a = POP();
    EXPECT_TYPE(a, JOY_INTEGER, "div");
    EXPECT_TYPE(b, JOY_INTEGER, "div");
    if (b.data.integer == 0) joy_error("Division by zero");
    PUSH(joy_integer(a.data.integer / b.data.integer));  /* quotient */
    PUSH(joy_integer(a.data.integer % b.data.integer));  /* remainder */
}

static void prim_succ(JoyContext* ctx) {
    REQUIRE(1, "succ");
    JoyValue v = POP();
    if (v.type == JOY_INTEGER) {
        PUSH(joy_integer(v.data.integer + 1));
    } else if (v.type == JOY_CHAR) {
        PUSH(joy_char((char)(v.data.character + 1)));
    } else {
        joy_error_type("succ", "integer or char", v.type);
    }
}

static void prim_pred(JoyContext* ctx) {
    REQUIRE(1, "pred");
    JoyValue v = POP();
    if (v.type == JOY_INTEGER) {
        PUSH(joy_integer(v.data.integer - 1));
    } else if (v.type == JOY_CHAR) {
        PUSH(joy_char((char)(v.data.character - 1)));
    } else {
        joy_error_type("pred", "integer or char", v.type);
    }
}

static void prim_abs(JoyContext* ctx) {
    REQUIRE(1, "abs");
    JoyValue v = POP();
    if (v.type == JOY_INTEGER) {
        PUSH(joy_integer(v.data.integer < 0 ? -v.data.integer : v.data.integer));
    } else if (v.type == JOY_FLOAT) {
        PUSH(joy_float(fabs(v.data.floating)));
    } else {
        joy_error_type("abs", "number", v.type);
    }
}

static void prim_neg(JoyContext* ctx) {
    REQUIRE(1, "neg");
    JoyValue v = POP();
    if (v.type == JOY_INTEGER) {
        PUSH(joy_integer(-v.data.integer));
    } else if (v.type == JOY_FLOAT) {
        PUSH(joy_float(-v.data.floating));
    } else {
        joy_error_type("neg", "number", v.type);
    }
}

static void prim_sign(JoyContext* ctx) {
    REQUIRE(1, "sign");
    JoyValue v = POP();
    if (v.type == JOY_INTEGER) {
        int64_t s = v.data.integer > 0 ? 1 : (v.data.integer < 0 ? -1 : 0);
        PUSH(joy_integer(s));
    } else if (v.type == JOY_FLOAT) {
        int s = v.data.floating > 0 ? 1 : (v.data.floating < 0 ? -1 : 0);
        PUSH(joy_integer(s));
    } else {
        joy_error_type("sign", "number", v.type);
    }
}

static void prim_max(JoyContext* ctx) {
    REQUIRE(2, "max");
    JoyValue b = POP();
    JoyValue a = POP();
    if (a.type == JOY_INTEGER && b.type == JOY_INTEGER) {
        PUSH(joy_integer(a.data.integer > b.data.integer ? a.data.integer : b.data.integer));
    } else if (a.type == JOY_FLOAT || b.type == JOY_FLOAT) {
        double av = a.type == JOY_FLOAT ? a.data.floating : (double)a.data.integer;
        double bv = b.type == JOY_FLOAT ? b.data.floating : (double)b.data.integer;
        PUSH(joy_float(av > bv ? av : bv));
    } else if (a.type == JOY_CHAR && b.type == JOY_CHAR) {
        PUSH(joy_char(a.data.character > b.data.character ? a.data.character : b.data.character));
    } else {
        joy_error_type("max", "number", a.type);
    }
}

static void prim_min(JoyContext* ctx) {
    REQUIRE(2, "min");
    JoyValue b = POP();
    JoyValue a = POP();
    if (a.type == JOY_INTEGER && b.type == JOY_INTEGER) {
        PUSH(joy_integer(a.data.integer < b.data.integer ? a.data.integer : b.data.integer));
    } else if (a.type == JOY_FLOAT || b.type == JOY_FLOAT) {
        double av = a.type == JOY_FLOAT ? a.data.floating : (double)a.data.integer;
        double bv = b.type == JOY_FLOAT ? b.data.floating : (double)b.data.integer;
        PUSH(joy_float(av < bv ? av : bv));
    } else if (a.type == JOY_CHAR && b.type == JOY_CHAR) {
        PUSH(joy_char(a.data.character < b.data.character ? a.data.character : b.data.character));
    } else {
        joy_error_type("min", "number", a.type);
    }
}

/* ---------- Math Functions ---------- */

static void prim_sin(JoyContext* ctx) {
    REQUIRE(1, "sin");
    JoyValue v = POP();
    double x = v.type == JOY_FLOAT ? v.data.floating : (double)v.data.integer;
    PUSH(joy_float(sin(x)));
}

static void prim_cos(JoyContext* ctx) {
    REQUIRE(1, "cos");
    JoyValue v = POP();
    double x = v.type == JOY_FLOAT ? v.data.floating : (double)v.data.integer;
    PUSH(joy_float(cos(x)));
}

static void prim_tan(JoyContext* ctx) {
    REQUIRE(1, "tan");
    JoyValue v = POP();
    double x = v.type == JOY_FLOAT ? v.data.floating : (double)v.data.integer;
    PUSH(joy_float(tan(x)));
}

static void prim_sqrt(JoyContext* ctx) {
    REQUIRE(1, "sqrt");
    JoyValue v = POP();
    double x = v.type == JOY_FLOAT ? v.data.floating : (double)v.data.integer;
    PUSH(joy_float(sqrt(x)));
}

static void prim_exp(JoyContext* ctx) {
    REQUIRE(1, "exp");
    JoyValue v = POP();
    double x = v.type == JOY_FLOAT ? v.data.floating : (double)v.data.integer;
    PUSH(joy_float(exp(x)));
}

static void prim_log(JoyContext* ctx) {
    REQUIRE(1, "log");
    JoyValue v = POP();
    double x = v.type == JOY_FLOAT ? v.data.floating : (double)v.data.integer;
    PUSH(joy_float(log(x)));
}

static void prim_pow(JoyContext* ctx) {
    REQUIRE(2, "pow");
    JoyValue b = POP();
    JoyValue a = POP();
    double av = a.type == JOY_FLOAT ? a.data.floating : (double)a.data.integer;
    double bv = b.type == JOY_FLOAT ? b.data.floating : (double)b.data.integer;
    PUSH(joy_float(pow(av, bv)));
}

static void prim_floor(JoyContext* ctx) {
    REQUIRE(1, "floor");
    JoyValue v = POP();
    double x = v.type == JOY_FLOAT ? v.data.floating : (double)v.data.integer;
    PUSH(joy_integer((int64_t)floor(x)));
}

static void prim_ceil(JoyContext* ctx) {
    REQUIRE(1, "ceil");
    JoyValue v = POP();
    double x = v.type == JOY_FLOAT ? v.data.floating : (double)v.data.integer;
    PUSH(joy_integer((int64_t)ceil(x)));
}

static void prim_trunc(JoyContext* ctx) {
    REQUIRE(1, "trunc");
    JoyValue v = POP();
    double x = v.type == JOY_FLOAT ? v.data.floating : (double)v.data.integer;
    PUSH(joy_integer((int64_t)trunc(x)));
}

static void prim_round(JoyContext* ctx) {
    REQUIRE(1, "round");
    JoyValue v = POP();
    double x = v.type == JOY_FLOAT ? v.data.floating : (double)v.data.integer;
    PUSH(joy_integer((int64_t)round(x)));
}

/*
 * typeof - return Joy type code for a value
 * Type codes (matching original Joy):
 *   2 = user-defined (quotation that is a defined word)
 *   3 = primitive/builtin (symbol)
 *   4 = boolean (logical)
 *   5 = char
 *   6 = integer
 *   7 = set
 *   8 = string
 *   9 = list
 *  10 = float
 *  11 = file
 */
static void prim_typeof(JoyContext* ctx) {
    REQUIRE(1, "typeof");
    JoyValue v = POP();
    int type_code;
    switch (v.type) {
        case JOY_BOOLEAN:   type_code = 4; break;
        case JOY_CHAR:      type_code = 5; break;
        case JOY_INTEGER:   type_code = 6; break;
        case JOY_SET:       type_code = 7; break;
        case JOY_STRING:    type_code = 8; break;
        case JOY_LIST:      type_code = 9; break;
        case JOY_FLOAT:     type_code = 10; break;
        case JOY_FILE:      type_code = 11; break;
        case JOY_QUOTATION: type_code = 2; break;  /* user-defined quotation */
        case JOY_SYMBOL: {
            /* Look up symbol to distinguish user-defined (2) from builtin (3) */
            JoyWord* word = joy_dict_lookup(ctx->dictionary, v.data.symbol);
            if (word && word->is_primitive) {
                type_code = 3;  /* built-in primitive */
            } else {
                type_code = 2;  /* user-defined word */
            }
            break;
        }
        default:            type_code = 0; break;
    }
    joy_value_free(&v);
    PUSH(joy_integer(type_code));
}

/* sametype - check if two values have the same type */
static void prim_sametype(JoyContext* ctx) {
    REQUIRE(2, "sametype");
    JoyValue b = POP();
    JoyValue a = POP();
    bool same;

    /* For symbols referring to primitives, compare the actual function pointers */
    if (a.type == JOY_SYMBOL && b.type == JOY_SYMBOL) {
        JoyWord* word_a = joy_dict_lookup(ctx->dictionary, a.data.symbol);
        JoyWord* word_b = joy_dict_lookup(ctx->dictionary, b.data.symbol);
        if (word_a && word_b && word_a->is_primitive && word_b->is_primitive) {
            /* Both are primitives - compare function pointers */
            same = (word_a->body.primitive == word_b->body.primitive);
        } else if (word_a && word_b && !word_a->is_primitive && !word_b->is_primitive) {
            /* Both are user-defined - same type */
            same = true;
        } else {
            /* One primitive, one user-defined */
            same = (word_a && word_b) ? (word_a->is_primitive == word_b->is_primitive) : false;
        }
    } else {
        same = (a.type == b.type);
    }

    joy_value_free(&a);
    joy_value_free(&b);
    PUSH(joy_boolean(same));
}

/* ---------- Comparison Operations ---------- */

static void prim_eq(JoyContext* ctx) {
    REQUIRE(2, "=");
    JoyValue b = POP();
    JoyValue a = POP();
    PUSH(joy_boolean(joy_value_equal(a, b)));
    joy_value_free(&a);
    joy_value_free(&b);
}

static void prim_neq(JoyContext* ctx) {
    REQUIRE(2, "!=");
    JoyValue b = POP();
    JoyValue a = POP();
    PUSH(joy_boolean(!joy_value_equal(a, b)));
    joy_value_free(&a);
    joy_value_free(&b);
}

/* Helper to get comparable value (string for string/symbol, numeric otherwise) */
static bool joy_can_compare(JoyValue a, JoyValue b, double* av, double* bv,
                            const char** as, const char** bs) {
    *as = NULL; *bs = NULL;
    /* Try numeric comparison first */
    if (joy_numeric_value(a, av) && joy_numeric_value(b, bv)) {
        return true;
    }
    /* String/symbol comparison */
    if (a.type == JOY_STRING) *as = a.data.string;
    else if (a.type == JOY_SYMBOL) *as = a.data.symbol;
    if (b.type == JOY_STRING) *bs = b.data.string;
    else if (b.type == JOY_SYMBOL) *bs = b.data.symbol;
    if (*as && *bs) return true;
    /* File comparison by pointer */
    if (a.type == JOY_FILE && b.type == JOY_FILE) {
        *av = (double)(uintptr_t)a.data.file;
        *bv = (double)(uintptr_t)b.data.file;
        return true;
    }
    return false;
}

static void prim_lt(JoyContext* ctx) {
    REQUIRE(2, "<");
    JoyValue b = POP();
    JoyValue a = POP();
    bool result = false;
    double av, bv;
    const char *as, *bs;
    if (joy_can_compare(a, b, &av, &bv, &as, &bs)) {
        if (as && bs) {
            result = strcmp(as, bs) < 0;
        } else {
            result = av < bv;
        }
    }
    PUSH(joy_boolean(result));
    joy_value_free(&a);
    joy_value_free(&b);
}

static void prim_gt(JoyContext* ctx) {
    REQUIRE(2, ">");
    JoyValue b = POP();
    JoyValue a = POP();
    bool result = false;
    double av, bv;
    const char *as, *bs;
    if (joy_can_compare(a, b, &av, &bv, &as, &bs)) {
        if (as && bs) {
            result = strcmp(as, bs) > 0;
        } else {
            result = av > bv;
        }
    }
    PUSH(joy_boolean(result));
    joy_value_free(&a);
    joy_value_free(&b);
}

static void prim_le(JoyContext* ctx) {
    REQUIRE(2, "<=");
    JoyValue b = POP();
    JoyValue a = POP();
    bool result = false;
    double av, bv;
    const char *as, *bs;
    if (joy_can_compare(a, b, &av, &bv, &as, &bs)) {
        if (as && bs) {
            result = strcmp(as, bs) <= 0;
        } else {
            result = av <= bv;
        }
    }
    PUSH(joy_boolean(result));
    joy_value_free(&a);
    joy_value_free(&b);
}

static void prim_ge(JoyContext* ctx) {
    REQUIRE(2, ">=");
    JoyValue b = POP();
    JoyValue a = POP();
    bool result = false;
    double av, bv;
    const char *as, *bs;
    if (joy_can_compare(a, b, &av, &bv, &as, &bs)) {
        if (as && bs) {
            result = strcmp(as, bs) >= 0;
        } else {
            result = av >= bv;
        }
    }
    PUSH(joy_boolean(result));
    joy_value_free(&a);
    joy_value_free(&b);
}

/* ---------- Logical Operations ---------- */

static void prim_and(JoyContext* ctx) {
    REQUIRE(2, "and");
    JoyValue b = POP();
    JoyValue a = POP();
    if (a.type == JOY_SET && b.type == JOY_SET) {
        /* Set intersection */
        JoyValue result = {.type = JOY_SET, .data.set = a.data.set & b.data.set};
        PUSH(result);
    } else {
        /* Logical conjunction */
        PUSH(joy_boolean(joy_value_truthy(a) && joy_value_truthy(b)));
    }
    joy_value_free(&a);
    joy_value_free(&b);
}

static void prim_or(JoyContext* ctx) {
    REQUIRE(2, "or");
    JoyValue b = POP();
    JoyValue a = POP();
    if (a.type == JOY_SET && b.type == JOY_SET) {
        /* Set union */
        JoyValue result = {.type = JOY_SET, .data.set = a.data.set | b.data.set};
        PUSH(result);
    } else {
        /* Logical disjunction */
        PUSH(joy_boolean(joy_value_truthy(a) || joy_value_truthy(b)));
    }
    joy_value_free(&a);
    joy_value_free(&b);
}

static void prim_not(JoyContext* ctx) {
    REQUIRE(1, "not");
    JoyValue v = POP();
    if (v.type == JOY_SET) {
        /* Bitwise complement of set */
        JoyValue result = {.type = JOY_SET, .data.set = ~v.data.set};
        PUSH(result);
    } else {
        /* Logical negation */
        PUSH(joy_boolean(!joy_value_truthy(v)));
    }
    joy_value_free(&v);
}

static void prim_xor(JoyContext* ctx) {
    REQUIRE(2, "xor");
    JoyValue b = POP();
    JoyValue a = POP();
    if (a.type == JOY_SET && b.type == JOY_SET) {
        /* Set symmetric difference */
        JoyValue result = {.type = JOY_SET, .data.set = a.data.set ^ b.data.set};
        PUSH(result);
    } else {
        /* Logical XOR (a != b for booleans) */
        PUSH(joy_boolean(joy_value_truthy(a) != joy_value_truthy(b)));
    }
    joy_value_free(&a);
    joy_value_free(&b);
}

static void prim_choice(JoyContext* ctx) {
    /* B T F -> X : if B then X=T else X=F */
    REQUIRE(3, "choice");
    JoyValue f = POP();
    JoyValue t = POP();
    JoyValue b = POP();
    bool cond = joy_value_truthy(b);
    joy_value_free(&b);
    if (cond) {
        PUSH(t);
        joy_value_free(&f);
    } else {
        PUSH(f);
        joy_value_free(&t);
    }
}

/* ---------- List/Aggregate Operations ---------- */

static void prim_first(JoyContext* ctx) {
    REQUIRE(1, "first");
    JoyValue v = POP();
    if (v.type == JOY_LIST) {
        if (v.data.list->length == 0) joy_error("first of empty list");
        PUSH(joy_value_copy(v.data.list->items[0]));
    } else if (v.type == JOY_QUOTATION) {
        if (v.data.quotation->length == 0) joy_error("first of empty quotation");
        PUSH(joy_value_copy(v.data.quotation->terms[0]));
    } else if (v.type == JOY_STRING) {
        if (v.data.string[0] == '\0') joy_error("first of empty string");
        PUSH(joy_char(v.data.string[0]));
    } else if (v.type == JOY_SET) {
        if (v.data.set == 0) joy_error("first of empty set");
        /* Find lowest set bit */
        int first_elem = 0;
        uint64_t s = v.data.set;
        while ((s & 1) == 0) { s >>= 1; first_elem++; }
        PUSH(joy_integer(first_elem));
    } else {
        joy_error_type("first", "aggregate", v.type);
    }
    joy_value_free(&v);
}

static void prim_rest(JoyContext* ctx) {
    REQUIRE(1, "rest");
    JoyValue v = POP();
    if (v.type == JOY_LIST) {
        JoyList* rest = joy_list_rest(v.data.list);
        JoyValue result = {.type = JOY_LIST, .data.list = rest};
        PUSH(result);
    } else if (v.type == JOY_QUOTATION) {
        JoyQuotation* rest = joy_quotation_new(v.data.quotation->length);
        for (size_t i = 1; i < v.data.quotation->length; i++) {
            joy_quotation_push(rest, joy_value_copy(v.data.quotation->terms[i]));
        }
        JoyValue result = {.type = JOY_QUOTATION, .data.quotation = rest};
        PUSH(result);
    } else if (v.type == JOY_STRING) {
        PUSH(joy_string(v.data.string + 1));
    } else if (v.type == JOY_SET) {
        /* Remove lowest set bit */
        uint64_t s = v.data.set;
        s &= (s - 1);  /* Clear lowest set bit */
        JoyValue result = {.type = JOY_SET, .data.set = s};
        PUSH(result);
    } else {
        joy_error_type("rest", "aggregate", v.type);
    }
    joy_value_free(&v);
}

static void prim_cons(JoyContext* ctx) {
    REQUIRE(2, "cons");
    JoyValue agg = POP();
    JoyValue item = POP();
    if (agg.type == JOY_LIST) {
        JoyList* result = joy_list_cons(item, agg.data.list);
        joy_value_free(&item);
        joy_value_free(&agg);
        JoyValue v = {.type = JOY_LIST, .data.list = result};
        PUSH(v);
    } else if (agg.type == JOY_QUOTATION) {
        JoyQuotation* result = joy_quotation_new(agg.data.quotation->length + 1);
        joy_quotation_push(result, joy_value_copy(item));
        for (size_t i = 0; i < agg.data.quotation->length; i++) {
            joy_quotation_push(result, joy_value_copy(agg.data.quotation->terms[i]));
        }
        joy_value_free(&item);
        joy_value_free(&agg);
        JoyValue v = {.type = JOY_QUOTATION, .data.quotation = result};
        PUSH(v);
    } else if (agg.type == JOY_SET) {
        /* cons on set: add element (must be integer 0-63) */
        if (item.type != JOY_INTEGER) {
            joy_value_free(&item);
            joy_value_free(&agg);
            joy_error_type("cons", "INTEGER for set element", item.type);
        }
        int64_t n = item.data.integer;
        if (n < 0 || n > 63) {
            joy_value_free(&item);
            joy_value_free(&agg);
            joy_error("cons: set element must be 0-63");
        }
        uint64_t new_set = agg.data.set | (1ULL << n);
        joy_value_free(&item);
        joy_value_free(&agg);
        JoyValue v = {.type = JOY_SET, .data.set = new_set};
        PUSH(v);
    } else if (agg.type == JOY_STRING) {
        /* cons on string: prepend char */
        if (item.type != JOY_CHAR) {
            joy_value_free(&item);
            joy_value_free(&agg);
            joy_error_type("cons", "CHAR for string element", item.type);
        }
        size_t len = strlen(agg.data.string);
        char* result = malloc(len + 2);
        result[0] = item.data.character;
        strcpy(result + 1, agg.data.string);
        joy_value_free(&item);
        joy_value_free(&agg);
        PUSH(joy_string(result));
        free(result);
    } else {
        joy_error_type("cons", "aggregate", agg.type);
    }
}

static void prim_swons(JoyContext* ctx) {
    REQUIRE(2, "swons");
    joy_stack_swap(ctx->stack);
    prim_cons(ctx);
}

static void prim_uncons(JoyContext* ctx) {
    REQUIRE(1, "uncons");
    JoyValue v = POP();
    if (v.type == JOY_LIST) {
        if (v.data.list->length == 0) joy_error("uncons of empty list");
        JoyValue first = joy_value_copy(v.data.list->items[0]);
        JoyList* rest = joy_list_rest(v.data.list);
        joy_value_free(&v);
        PUSH(first);
        JoyValue rv = {.type = JOY_LIST, .data.list = rest};
        PUSH(rv);
    } else if (v.type == JOY_QUOTATION) {
        if (v.data.quotation->length == 0) joy_error("uncons of empty quotation");
        JoyValue first = joy_value_copy(v.data.quotation->terms[0]);
        JoyQuotation* rest = joy_quotation_new(v.data.quotation->length);
        for (size_t i = 1; i < v.data.quotation->length; i++) {
            joy_quotation_push(rest, joy_value_copy(v.data.quotation->terms[i]));
        }
        joy_value_free(&v);
        PUSH(first);
        JoyValue rv = {.type = JOY_QUOTATION, .data.quotation = rest};
        PUSH(rv);
    } else if (v.type == JOY_STRING) {
        size_t len = strlen(v.data.string);
        if (len == 0) joy_error("uncons of empty string");
        JoyValue first = joy_char(v.data.string[0]);
        char* rest = strdup(v.data.string + 1);
        joy_value_free(&v);
        PUSH(first);
        PUSH(joy_string(rest));
        free(rest);
    } else if (v.type == JOY_SET) {
        if (v.data.set == 0) joy_error("uncons of empty set");
        /* Find lowest set bit */
        int first_elem = 0;
        uint64_t s = v.data.set;
        while ((s & 1) == 0) { s >>= 1; first_elem++; }
        /* Clear lowest set bit for rest */
        uint64_t rest_set = v.data.set & (v.data.set - 1);
        joy_value_free(&v);
        PUSH(joy_integer(first_elem));
        JoyValue rv = {.type = JOY_SET, .data.set = rest_set};
        PUSH(rv);
    } else {
        joy_error_type("uncons", "aggregate", v.type);
    }
}

static void prim_concat(JoyContext* ctx);

static void prim_swoncat(JoyContext* ctx) {
    /* swap concat */
    REQUIRE(2, "swoncat");
    joy_stack_swap(ctx->stack);
    prim_concat(ctx);
}

static void prim_concat(JoyContext* ctx) {
    REQUIRE(2, "concat");
    JoyValue b = POP();
    JoyValue a = POP();
    if (a.type == JOY_LIST && b.type == JOY_LIST) {
        JoyList* result = joy_list_concat(a.data.list, b.data.list);
        joy_value_free(&a);
        joy_value_free(&b);
        JoyValue v = {.type = JOY_LIST, .data.list = result};
        PUSH(v);
    } else if (a.type == JOY_QUOTATION && b.type == JOY_QUOTATION) {
        JoyQuotation* result = joy_quotation_concat(a.data.quotation, b.data.quotation);
        joy_value_free(&a);
        joy_value_free(&b);
        JoyValue v = {.type = JOY_QUOTATION, .data.quotation = result};
        PUSH(v);
    } else if (a.type == JOY_STRING && b.type == JOY_STRING) {
        size_t len = strlen(a.data.string) + strlen(b.data.string) + 1;
        char* result = malloc(len);
        strcpy(result, a.data.string);
        strcat(result, b.data.string);
        joy_value_free(&a);
        joy_value_free(&b);
        PUSH(joy_string_owned(result));
    } else if (a.type == JOY_SET && b.type == JOY_SET) {
        /* Set concat is union */
        JoyValue result = {.type = JOY_SET, .data.set = a.data.set | b.data.set};
        PUSH(result);
    } else {
        joy_error_type("concat", "aggregate", a.type);
    }
}

static void prim_size(JoyContext* ctx) {
    REQUIRE(1, "size");
    JoyValue v = POP();
    size_t sz = 0;
    switch (v.type) {
        case JOY_LIST: sz = v.data.list->length; break;
        case JOY_QUOTATION: sz = v.data.quotation->length; break;
        case JOY_STRING: sz = strlen(v.data.string); break;
        case JOY_SET: sz = joy_set_size(v.data.set); break;
        default: joy_error_type("size", "aggregate", v.type);
    }
    joy_value_free(&v);
    PUSH(joy_integer((int64_t)sz));
}

static void prim_at(JoyContext* ctx) {
    /* A I -> X : get element at index I from aggregate A */
    REQUIRE(2, "at");
    JoyValue idx = POP();
    JoyValue agg = POP();
    EXPECT_TYPE(idx, JOY_INTEGER, "at");
    int64_t i = idx.data.integer;
    joy_value_free(&idx);

    if (i < 0) {
        joy_value_free(&agg);
        joy_error("at: negative index");
    }

    switch (agg.type) {
        case JOY_LIST:
            if ((size_t)i >= agg.data.list->length) {
                joy_value_free(&agg);
                joy_error("at: index out of bounds");
            }
            PUSH(joy_value_copy(agg.data.list->items[i]));
            break;
        case JOY_QUOTATION:
            if ((size_t)i >= agg.data.quotation->length) {
                joy_value_free(&agg);
                joy_error("at: index out of bounds");
            }
            PUSH(joy_value_copy(agg.data.quotation->terms[i]));
            break;
        case JOY_STRING:
            if ((size_t)i >= strlen(agg.data.string)) {
                joy_value_free(&agg);
                joy_error("at: index out of bounds");
            }
            PUSH(joy_char(agg.data.string[i]));
            break;
        default:
            joy_value_free(&agg);
            joy_error_type("at", "aggregate", agg.type);
    }
    joy_value_free(&agg);
}

static void prim_drop(JoyContext* ctx) {
    /* A N -> B : drop first N elements from aggregate A */
    REQUIRE(2, "drop");
    JoyValue nv = POP();
    JoyValue agg = POP();
    EXPECT_TYPE(nv, JOY_INTEGER, "drop");
    int64_t n = nv.data.integer;
    joy_value_free(&nv);

    if (n < 0) {
        joy_value_free(&agg);
        joy_error("drop: negative count");
    }

    switch (agg.type) {
        case JOY_LIST: {
            size_t start = (size_t)n < agg.data.list->length ? (size_t)n : agg.data.list->length;
            JoyList* result = joy_list_new(agg.data.list->length - start);
            for (size_t i = start; i < agg.data.list->length; i++) {
                joy_list_push(result, joy_value_copy(agg.data.list->items[i]));
            }
            joy_value_free(&agg);
            JoyValue v = {.type = JOY_LIST, .data.list = result};
            PUSH(v);
            break;
        }
        case JOY_QUOTATION: {
            size_t start = (size_t)n < agg.data.quotation->length ? (size_t)n : agg.data.quotation->length;
            JoyQuotation* result = joy_quotation_new(agg.data.quotation->length - start);
            for (size_t i = start; i < agg.data.quotation->length; i++) {
                joy_quotation_push(result, joy_value_copy(agg.data.quotation->terms[i]));
            }
            joy_value_free(&agg);
            JoyValue v = {.type = JOY_QUOTATION, .data.quotation = result};
            PUSH(v);
            break;
        }
        case JOY_STRING: {
            size_t len = strlen(agg.data.string);
            size_t start = (size_t)n < len ? (size_t)n : len;
            char* result = strdup(agg.data.string + start);
            joy_value_free(&agg);
            PUSH(joy_string(result));
            free(result);
            break;
        }
        case JOY_SET: {
            /* Drop first N elements from set (in bit order) */
            uint64_t set = agg.data.set;
            uint64_t result = 0;
            int count = 0;
            for (int i = 0; i < 64; i++) {
                if (set & (1ULL << i)) {
                    if (count >= n) {
                        result |= (1ULL << i);
                    }
                    count++;
                }
            }
            joy_value_free(&agg);
            JoyValue v = {.type = JOY_SET, .data.set = result};
            PUSH(v);
            break;
        }
        default:
            joy_value_free(&agg);
            joy_error_type("drop", "aggregate", agg.type);
    }
}

static void prim_take(JoyContext* ctx) {
    /* A N -> B : take first N elements from aggregate A */
    REQUIRE(2, "take");
    JoyValue nv = POP();
    JoyValue agg = POP();
    EXPECT_TYPE(nv, JOY_INTEGER, "take");
    int64_t n = nv.data.integer;
    joy_value_free(&nv);

    if (n < 0) {
        joy_value_free(&agg);
        joy_error("take: negative count");
    }

    switch (agg.type) {
        case JOY_LIST: {
            size_t count = (size_t)n < agg.data.list->length ? (size_t)n : agg.data.list->length;
            JoyList* result = joy_list_new(count);
            for (size_t i = 0; i < count; i++) {
                joy_list_push(result, joy_value_copy(agg.data.list->items[i]));
            }
            joy_value_free(&agg);
            JoyValue v = {.type = JOY_LIST, .data.list = result};
            PUSH(v);
            break;
        }
        case JOY_QUOTATION: {
            size_t count = (size_t)n < agg.data.quotation->length ? (size_t)n : agg.data.quotation->length;
            JoyQuotation* result = joy_quotation_new(count);
            for (size_t i = 0; i < count; i++) {
                joy_quotation_push(result, joy_value_copy(agg.data.quotation->terms[i]));
            }
            joy_value_free(&agg);
            JoyValue v = {.type = JOY_QUOTATION, .data.quotation = result};
            PUSH(v);
            break;
        }
        case JOY_STRING: {
            size_t len = strlen(agg.data.string);
            size_t count = (size_t)n < len ? (size_t)n : len;
            char* result = malloc(count + 1);
            strncpy(result, agg.data.string, count);
            result[count] = '\0';
            joy_value_free(&agg);
            PUSH(joy_string(result));
            free(result);
            break;
        }
        case JOY_SET: {
            /* Take first N elements from set (in bit order) */
            uint64_t set = agg.data.set;
            uint64_t result = 0;
            int count = 0;
            for (int i = 0; i < 64 && count < n; i++) {
                if (set & (1ULL << i)) {
                    result |= (1ULL << i);
                    count++;
                }
            }
            joy_value_free(&agg);
            JoyValue v = {.type = JOY_SET, .data.set = result};
            PUSH(v);
            break;
        }
        default:
            joy_value_free(&agg);
            joy_error_type("take", "aggregate", agg.type);
    }
}

static void prim_null(JoyContext* ctx) {
    REQUIRE(1, "null");
    JoyValue v = POP();
    bool is_null = false;
    switch (v.type) {
        case JOY_INTEGER: is_null = v.data.integer == 0; break;
        case JOY_FLOAT: is_null = v.data.floating == 0.0; break;
        case JOY_BOOLEAN: is_null = !v.data.boolean; break;
        case JOY_LIST: is_null = v.data.list->length == 0; break;
        case JOY_QUOTATION: is_null = v.data.quotation->length == 0; break;
        case JOY_STRING: is_null = v.data.string[0] == '\0'; break;
        case JOY_SET: is_null = v.data.set == 0; break;
        default: is_null = false;
    }
    joy_value_free(&v);
    PUSH(joy_boolean(is_null));
}

static void prim_small(JoyContext* ctx) {
    REQUIRE(1, "small");
    JoyValue v = POP();
    bool is_small = false;
    switch (v.type) {
        case JOY_INTEGER: is_small = v.data.integer <= 1 && v.data.integer >= -1; break;
        case JOY_LIST: is_small = v.data.list->length <= 1; break;
        case JOY_QUOTATION: is_small = v.data.quotation->length <= 1; break;
        case JOY_STRING: is_small = strlen(v.data.string) <= 1; break;
        case JOY_SET: is_small = joy_set_size(v.data.set) <= 1; break;
        default: is_small = false;
    }
    joy_value_free(&v);
    PUSH(joy_boolean(is_small));
}

/* ---------- Quotation Combinators ---------- */

static void prim_i(JoyContext* ctx) {
    REQUIRE(1, "i");
    JoyValue v = POP();
    if (v.type == JOY_QUOTATION) {
        joy_execute_quotation(ctx, v.data.quotation);
    } else if (v.type == JOY_LIST) {
        /* Treat list as quotation */
        for (size_t i = 0; i < v.data.list->length; i++) {
            joy_execute_value(ctx, v.data.list->items[i]);
        }
    } else {
        joy_error_type("i", "QUOTATION", v.type);
    }
    joy_value_free(&v);
}

static void prim_x(JoyContext* ctx) {
    /* x == dup i : duplicate quotation, then execute */
    REQUIRE(1, "x");
    JoyValue v = PEEK();
    if (v.type != JOY_QUOTATION && v.type != JOY_LIST) {
        joy_error_type("x", "QUOTATION", v.type);
    }
    /* Push a copy (dup) */
    PUSH(joy_value_copy(v));
    /* Pop and execute (i) */
    JoyValue q = POP();
    if (q.type == JOY_QUOTATION) {
        joy_execute_quotation(ctx, q.data.quotation);
    } else if (q.type == JOY_LIST) {
        for (size_t i = 0; i < q.data.list->length; i++) {
            joy_execute_value(ctx, q.data.list->items[i]);
        }
    }
    joy_value_free(&q);
}

static void prim_dip(JoyContext* ctx) {
    REQUIRE(2, "dip");
    JoyValue quot = POP();
    JoyValue saved = POP();
    if (quot.type == JOY_QUOTATION) {
        joy_execute_quotation(ctx, quot.data.quotation);
    } else if (quot.type == JOY_LIST) {
        for (size_t i = 0; i < quot.data.list->length; i++) {
            joy_execute_value(ctx, quot.data.list->items[i]);
        }
    } else {
        joy_error_type("dip", "QUOTATION", quot.type);
    }
    PUSH(saved);
    joy_value_free(&quot);
}

static void prim_ifte(JoyContext* ctx) {
    REQUIRE(3, "ifte");
    JoyValue falseBranch = POP();
    JoyValue trueBranch = POP();
    JoyValue condition = POP();

    /* Save stack state */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Execute condition */
    if (condition.type == JOY_QUOTATION) {
        joy_execute_quotation(ctx, condition.data.quotation);
    } else if (condition.type == JOY_LIST) {
        for (size_t i = 0; i < condition.data.list->length; i++) {
            joy_execute_value(ctx, condition.data.list->items[i]);
        }
    }

    /* Get result and restore stack */
    JoyValue result = POP();
    bool cond_result = joy_value_truthy(result);
    joy_value_free(&result);

    /* Restore stack */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;

    /* Execute appropriate branch */
    JoyValue branch = cond_result ? trueBranch : falseBranch;
    if (branch.type == JOY_QUOTATION) {
        joy_execute_quotation(ctx, branch.data.quotation);
    } else if (branch.type == JOY_LIST) {
        for (size_t i = 0; i < branch.data.list->length; i++) {
            joy_execute_value(ctx, branch.data.list->items[i]);
        }
    }

    joy_value_free(&condition);
    joy_value_free(&trueBranch);
    joy_value_free(&falseBranch);
}

static void prim_branch(JoyContext* ctx) {
    REQUIRE(3, "branch");
    JoyValue falseBranch = POP();
    JoyValue trueBranch = POP();
    JoyValue cond = POP();
    bool b = joy_value_truthy(cond);
    joy_value_free(&cond);
    JoyValue branch = b ? trueBranch : falseBranch;
    if (branch.type == JOY_QUOTATION) {
        joy_execute_quotation(ctx, branch.data.quotation);
    } else if (branch.type == JOY_LIST) {
        for (size_t i = 0; i < branch.data.list->length; i++) {
            joy_execute_value(ctx, branch.data.list->items[i]);
        }
    }
    joy_value_free(&trueBranch);
    joy_value_free(&falseBranch);
}

static void prim_times(JoyContext* ctx) {
    REQUIRE(2, "times");
    JoyValue quot = POP();
    JoyValue count = POP();
    EXPECT_TYPE(count, JOY_INTEGER, "times");
    for (int64_t i = 0; i < count.data.integer; i++) {
        if (quot.type == JOY_QUOTATION) {
            joy_execute_quotation(ctx, quot.data.quotation);
        } else if (quot.type == JOY_LIST) {
            for (size_t j = 0; j < quot.data.list->length; j++) {
                joy_execute_value(ctx, quot.data.list->items[j]);
            }
        }
    }
    joy_value_free(&quot);
}

static void prim_while(JoyContext* ctx) {
    REQUIRE(2, "while");
    JoyValue body = POP();
    JoyValue cond = POP();

    while (true) {
        /* Save stack for condition test */
        JoyStack* saved = joy_stack_copy(ctx->stack);

        /* Execute condition */
        if (cond.type == JOY_QUOTATION) {
            joy_execute_quotation(ctx, cond.data.quotation);
        } else if (cond.type == JOY_LIST) {
            for (size_t i = 0; i < cond.data.list->length; i++) {
                joy_execute_value(ctx, cond.data.list->items[i]);
            }
        }

        JoyValue result = POP();
        bool cont = joy_value_truthy(result);
        joy_value_free(&result);

        /* Restore stack */
        joy_stack_free(ctx->stack);
        ctx->stack = saved;

        if (!cont) break;

        /* Execute body */
        if (body.type == JOY_QUOTATION) {
            joy_execute_quotation(ctx, body.data.quotation);
        } else if (body.type == JOY_LIST) {
            for (size_t i = 0; i < body.data.list->length; i++) {
                joy_execute_value(ctx, body.data.list->items[i]);
            }
        }
    }

    joy_value_free(&cond);
    joy_value_free(&body);
}

static void prim_map(JoyContext* ctx) {
    REQUIRE(2, "map");
    JoyValue quot = POP();
    JoyValue agg = POP();

    if (agg.type != JOY_LIST && agg.type != JOY_QUOTATION) {
        joy_error_type("map", "aggregate", agg.type);
    }

    size_t len = agg.type == JOY_LIST ? agg.data.list->length : agg.data.quotation->length;
    JoyValue* items = agg.type == JOY_LIST ? agg.data.list->items : agg.data.quotation->terms;

    JoyList* result = joy_list_new(len);

    for (size_t i = 0; i < len; i++) {
        PUSH(joy_value_copy(items[i]));
        if (quot.type == JOY_QUOTATION) {
            joy_execute_quotation(ctx, quot.data.quotation);
        } else if (quot.type == JOY_LIST) {
            for (size_t j = 0; j < quot.data.list->length; j++) {
                joy_execute_value(ctx, quot.data.list->items[j]);
            }
        }
        JoyValue mapped = POP();
        joy_list_push(result, mapped);
    }

    joy_value_free(&agg);
    joy_value_free(&quot);

    JoyValue rv = {.type = JOY_LIST, .data.list = result};
    PUSH(rv);
}

static void prim_step(JoyContext* ctx) {
    REQUIRE(2, "step");
    JoyValue quot = POP();
    JoyValue agg = POP();

    size_t len = 0;
    JoyValue* items = NULL;

    if (agg.type == JOY_LIST) {
        len = agg.data.list->length;
        items = agg.data.list->items;
    } else if (agg.type == JOY_QUOTATION) {
        len = agg.data.quotation->length;
        items = agg.data.quotation->terms;
    } else {
        joy_error_type("step", "aggregate", agg.type);
    }

    for (size_t i = 0; i < len; i++) {
        PUSH(joy_value_copy(items[i]));
        if (quot.type == JOY_QUOTATION) {
            joy_execute_quotation(ctx, quot.data.quotation);
        } else if (quot.type == JOY_LIST) {
            for (size_t j = 0; j < quot.data.list->length; j++) {
                joy_execute_value(ctx, quot.data.list->items[j]);
            }
        }
    }

    joy_value_free(&agg);
    joy_value_free(&quot);
}

static void prim_fold(JoyContext* ctx) {
    REQUIRE(3, "fold");
    JoyValue quot = POP();
    JoyValue init = POP();
    JoyValue agg = POP();

    PUSH(init);

    size_t len = 0;
    JoyValue* items = NULL;

    if (agg.type == JOY_LIST) {
        len = agg.data.list->length;
        items = agg.data.list->items;
    } else if (agg.type == JOY_QUOTATION) {
        len = agg.data.quotation->length;
        items = agg.data.quotation->terms;
    } else {
        joy_error_type("fold", "aggregate", agg.type);
    }

    for (size_t i = 0; i < len; i++) {
        PUSH(joy_value_copy(items[i]));
        if (quot.type == JOY_QUOTATION) {
            joy_execute_quotation(ctx, quot.data.quotation);
        } else if (quot.type == JOY_LIST) {
            for (size_t j = 0; j < quot.data.list->length; j++) {
                joy_execute_value(ctx, quot.data.list->items[j]);
            }
        }
    }

    joy_value_free(&agg);
    joy_value_free(&quot);
}

static void prim_filter(JoyContext* ctx) {
    REQUIRE(2, "filter");
    JoyValue quot = POP();
    JoyValue agg = POP();

    if (agg.type != JOY_LIST && agg.type != JOY_QUOTATION) {
        joy_error_type("filter", "aggregate", agg.type);
    }

    size_t len = agg.type == JOY_LIST ? agg.data.list->length : agg.data.quotation->length;
    JoyValue* items = agg.type == JOY_LIST ? agg.data.list->items : agg.data.quotation->terms;

    JoyList* result = joy_list_new(len);

    for (size_t i = 0; i < len; i++) {
        JoyValue item = joy_value_copy(items[i]);
        PUSH(joy_value_copy(item));
        if (quot.type == JOY_QUOTATION) {
            joy_execute_quotation(ctx, quot.data.quotation);
        } else if (quot.type == JOY_LIST) {
            for (size_t j = 0; j < quot.data.list->length; j++) {
                joy_execute_value(ctx, quot.data.list->items[j]);
            }
        }
        JoyValue pred = POP();
        if (joy_value_truthy(pred)) {
            joy_list_push(result, item);
        } else {
            joy_value_free(&item);
        }
        joy_value_free(&pred);
    }

    joy_value_free(&agg);
    joy_value_free(&quot);

    JoyValue rv = {.type = JOY_LIST, .data.list = result};
    PUSH(rv);
}

static void prim_split(JoyContext* ctx) {
    /* A [B] -> A1 A2 : split aggregate A into two based on test B */
    REQUIRE(2, "split");
    JoyValue quot = POP();
    JoyValue agg = POP();

    if (agg.type != JOY_LIST && agg.type != JOY_QUOTATION) {
        joy_error_type("split", "aggregate", agg.type);
    }

    size_t len = agg.type == JOY_LIST ? agg.data.list->length : agg.data.quotation->length;
    JoyValue* items = agg.type == JOY_LIST ? agg.data.list->items : agg.data.quotation->terms;

    JoyList* pass = joy_list_new(len);
    JoyList* fail = joy_list_new(len);

    for (size_t i = 0; i < len; i++) {
        JoyValue item = joy_value_copy(items[i]);
        PUSH(joy_value_copy(item));
        if (quot.type == JOY_QUOTATION) {
            joy_execute_quotation(ctx, quot.data.quotation);
        } else if (quot.type == JOY_LIST) {
            for (size_t j = 0; j < quot.data.list->length; j++) {
                joy_execute_value(ctx, quot.data.list->items[j]);
            }
        }
        JoyValue pred = POP();
        if (joy_value_truthy(pred)) {
            joy_list_push(pass, item);
        } else {
            joy_list_push(fail, item);
        }
        joy_value_free(&pred);
    }

    joy_value_free(&agg);
    joy_value_free(&quot);

    JoyValue rv1 = {.type = JOY_LIST, .data.list = pass};
    JoyValue rv2 = {.type = JOY_LIST, .data.list = fail};
    PUSH(rv1);
    PUSH(rv2);
}

static void prim_some(JoyContext* ctx) {
    /* A [B] -> X : true if B holds for some element of A */
    REQUIRE(2, "some");
    JoyValue quot = POP();
    JoyValue agg = POP();

    if (agg.type != JOY_LIST && agg.type != JOY_QUOTATION) {
        joy_error_type("some", "aggregate", agg.type);
    }

    size_t len = agg.type == JOY_LIST ? agg.data.list->length : agg.data.quotation->length;
    JoyValue* items = agg.type == JOY_LIST ? agg.data.list->items : agg.data.quotation->terms;

    bool found = false;
    for (size_t i = 0; i < len && !found; i++) {
        PUSH(joy_value_copy(items[i]));
        if (quot.type == JOY_QUOTATION) {
            joy_execute_quotation(ctx, quot.data.quotation);
        } else if (quot.type == JOY_LIST) {
            for (size_t j = 0; j < quot.data.list->length; j++) {
                joy_execute_value(ctx, quot.data.list->items[j]);
            }
        }
        JoyValue pred = POP();
        if (joy_value_truthy(pred)) {
            found = true;
        }
        joy_value_free(&pred);
    }

    joy_value_free(&agg);
    joy_value_free(&quot);
    PUSH(joy_boolean(found));
}

static void prim_all(JoyContext* ctx) {
    /* A [B] -> X : true if B holds for all elements of A */
    REQUIRE(2, "all");
    JoyValue quot = POP();
    JoyValue agg = POP();

    if (agg.type != JOY_LIST && agg.type != JOY_QUOTATION) {
        joy_error_type("all", "aggregate", agg.type);
    }

    size_t len = agg.type == JOY_LIST ? agg.data.list->length : agg.data.quotation->length;
    JoyValue* items = agg.type == JOY_LIST ? agg.data.list->items : agg.data.quotation->terms;

    bool all_pass = true;
    for (size_t i = 0; i < len && all_pass; i++) {
        PUSH(joy_value_copy(items[i]));
        if (quot.type == JOY_QUOTATION) {
            joy_execute_quotation(ctx, quot.data.quotation);
        } else if (quot.type == JOY_LIST) {
            for (size_t j = 0; j < quot.data.list->length; j++) {
                joy_execute_value(ctx, quot.data.list->items[j]);
            }
        }
        JoyValue pred = POP();
        if (!joy_value_truthy(pred)) {
            all_pass = false;
        }
        joy_value_free(&pred);
    }

    joy_value_free(&agg);
    joy_value_free(&quot);
    PUSH(joy_boolean(all_pass));
}

static void prim_enconcat(JoyContext* ctx) {
    /* X S T -> U : concatenate S and T with X inserted between */
    /* Equivalent to: swapd cons concat */
    REQUIRE(3, "enconcat");
    JoyValue t = POP();
    JoyValue s = POP();
    JoyValue x = POP();

    /* First, cons X onto S */
    if (s.type == JOY_LIST) {
        JoyList* s_with_x = joy_list_new(s.data.list->length + 1);
        for (size_t i = 0; i < s.data.list->length; i++) {
            joy_list_push(s_with_x, joy_value_copy(s.data.list->items[i]));
        }
        joy_list_push(s_with_x, joy_value_copy(x));

        /* Then concat with T */
        if (t.type == JOY_LIST) {
            JoyList* result = joy_list_concat(s_with_x, t.data.list);
            joy_list_free(s_with_x);
            joy_value_free(&s);
            joy_value_free(&t);
            joy_value_free(&x);
            JoyValue rv = {.type = JOY_LIST, .data.list = result};
            PUSH(rv);
        } else {
            joy_list_free(s_with_x);
            joy_value_free(&s);
            joy_value_free(&t);
            joy_value_free(&x);
            joy_error("enconcat: S and T must have same type");
        }
    } else if (s.type == JOY_QUOTATION) {
        JoyQuotation* s_with_x = joy_quotation_new(s.data.quotation->length + 1);
        for (size_t i = 0; i < s.data.quotation->length; i++) {
            joy_quotation_push(s_with_x, joy_value_copy(s.data.quotation->terms[i]));
        }
        joy_quotation_push(s_with_x, joy_value_copy(x));

        /* Then concat with T */
        if (t.type == JOY_QUOTATION) {
            JoyQuotation* result = joy_quotation_concat(s_with_x, t.data.quotation);
            joy_quotation_free(s_with_x);
            joy_value_free(&s);
            joy_value_free(&t);
            joy_value_free(&x);
            JoyValue rv = {.type = JOY_QUOTATION, .data.quotation = result};
            PUSH(rv);
        } else {
            joy_quotation_free(s_with_x);
            joy_value_free(&s);
            joy_value_free(&t);
            joy_value_free(&x);
            joy_error("enconcat: S and T must have same type");
        }
    } else if (s.type == JOY_STRING) {
        /* For strings, X must be a char */
        if (x.type != JOY_CHAR || t.type != JOY_STRING) {
            joy_value_free(&s);
            joy_value_free(&t);
            joy_value_free(&x);
            joy_error("enconcat: for strings, X must be char and T must be string");
        }
        size_t len = strlen(s.data.string) + 1 + strlen(t.data.string) + 1;
        char* result = malloc(len);
        strcpy(result, s.data.string);
        size_t slen = strlen(s.data.string);
        result[slen] = x.data.character;
        result[slen + 1] = '\0';
        strcat(result, t.data.string);
        joy_value_free(&s);
        joy_value_free(&t);
        joy_value_free(&x);
        PUSH(joy_string_owned(result));
    } else {
        joy_value_free(&s);
        joy_value_free(&t);
        joy_value_free(&x);
        joy_error_type("enconcat", "aggregate", s.type);
    }
}

/* ---------- Recursion Combinators ---------- */

static void binrec_aux(JoyContext* ctx, JoyValue* p, JoyValue* t, JoyValue* r1, JoyValue* r2);

static void execute_quot(JoyContext* ctx, JoyValue* quot) {
    if (quot->type == JOY_QUOTATION) {
        joy_execute_quotation(ctx, quot->data.quotation);
    } else if (quot->type == JOY_LIST) {
        for (size_t i = 0; i < quot->data.list->length; i++) {
            joy_execute_value(ctx, quot->data.list->items[i]);
        }
    }
}

static void binrec_aux(JoyContext* ctx, JoyValue* p, JoyValue* t, JoyValue* r1, JoyValue* r2) {
    /* Save stack for predicate test */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Execute predicate */
    execute_quot(ctx, p);
    JoyValue test_result = POP();
    bool is_base = joy_value_truthy(test_result);
    joy_value_free(&test_result);

    /* Restore stack */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;

    if (is_base) {
        /* Base case: execute terminal */
        execute_quot(ctx, t);
    } else {
        /* Split into two values */
        execute_quot(ctx, r1);

        /* Save first value */
        JoyValue first_arg = POP();

        /* Recurse on remaining */
        binrec_aux(ctx, p, t, r1, r2);
        JoyValue first_result = POP();

        /* Push first arg and recurse */
        PUSH(first_arg);
        binrec_aux(ctx, p, t, r1, r2);

        /* Push first result back */
        PUSH(first_result);

        /* Combine */
        execute_quot(ctx, r2);
    }
}

static void prim_binrec(JoyContext* ctx) {
    REQUIRE(4, "binrec");
    JoyValue r2 = POP();
    JoyValue r1 = POP();
    JoyValue t = POP();
    JoyValue p = POP();

    binrec_aux(ctx, &p, &t, &r1, &r2);

    joy_value_free(&p);
    joy_value_free(&t);
    joy_value_free(&r1);
    joy_value_free(&r2);
}

static void linrec_aux(JoyContext* ctx, JoyValue* p, JoyValue* t, JoyValue* r1, JoyValue* r2) {
    /* Save stack for predicate test */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Execute predicate */
    execute_quot(ctx, p);
    JoyValue test_result = POP();
    bool is_base = joy_value_truthy(test_result);
    joy_value_free(&test_result);

    /* Restore stack */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;

    if (is_base) {
        /* Base case */
        execute_quot(ctx, t);
    } else {
        /* Execute r1, recurse, execute r2 */
        execute_quot(ctx, r1);
        linrec_aux(ctx, p, t, r1, r2);
        execute_quot(ctx, r2);
    }
}

static void prim_linrec(JoyContext* ctx) {
    REQUIRE(4, "linrec");
    JoyValue r2 = POP();
    JoyValue r1 = POP();
    JoyValue t = POP();
    JoyValue p = POP();

    linrec_aux(ctx, &p, &t, &r1, &r2);

    joy_value_free(&p);
    joy_value_free(&t);
    joy_value_free(&r1);
    joy_value_free(&r2);
}

static void prim_tailrec(JoyContext* ctx) {
    REQUIRE(3, "tailrec");
    JoyValue r1 = POP();
    JoyValue t = POP();
    JoyValue p = POP();

    while (1) {
        /* Save stack for predicate test */
        JoyStack* saved = joy_stack_copy(ctx->stack);

        /* Execute predicate */
        execute_quot(ctx, &p);
        JoyValue test_result = POP();
        bool is_base = joy_value_truthy(test_result);
        joy_value_free(&test_result);

        /* Restore stack */
        joy_stack_free(ctx->stack);
        ctx->stack = saved;

        if (is_base) {
            execute_quot(ctx, &t);
            break;
        } else {
            execute_quot(ctx, &r1);
        }
    }

    joy_value_free(&p);
    joy_value_free(&t);
    joy_value_free(&r1);
}

static void prim_primrec(JoyContext* ctx) {
    /* X [I] [C] primrec -> execute I for initial value, combine with 1..X using C */
    REQUIRE(3, "primrec");
    JoyValue c = POP();
    JoyValue i = POP();
    JoyValue x = POP();

    /* Execute I to get initial value */
    execute_quot(ctx, &i);

    if (x.type == JOY_INTEGER) {
        /* For integer: combine with 1, 2, ..., X */
        int64_t n = x.data.integer;
        for (int64_t j = 1; j <= n; j++) {
            PUSH(joy_integer(j));
            execute_quot(ctx, &c);
        }
    } else if (x.type == JOY_LIST) {
        /* For aggregate: combine with each member */
        JoyList* lst = x.data.list;
        for (size_t j = 0; j < lst->length; j++) {
            PUSH(joy_value_copy(lst->items[j]));
            execute_quot(ctx, &c);
        }
    } else if (x.type == JOY_STRING) {
        /* For string: combine with each character */
        const char* s = x.data.string;
        while (*s) {
            PUSH(joy_char(*s));
            execute_quot(ctx, &c);
            s++;
        }
    } else {
        joy_error_type("primrec", "INTEGER, LIST, or STRING", x.type);
    }

    joy_value_free(&x);
    joy_value_free(&i);
    joy_value_free(&c);
}

static void prim_genrec(JoyContext* ctx) {
    REQUIRE(4, "genrec");
    JoyValue r2 = POP();
    JoyValue r1 = POP();
    JoyValue t = POP();
    JoyValue p = POP();

    /* Save stack for predicate test */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Execute predicate */
    execute_quot(ctx, &p);
    JoyValue test_result = POP();
    bool is_base = joy_value_truthy(test_result);
    joy_value_free(&test_result);

    /* Restore stack */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;

    if (is_base) {
        execute_quot(ctx, &t);
    } else {
        execute_quot(ctx, &r1);
        /* Push the quotation [[P] [T] [R1] [R2] genrec] for recursion */
        JoyQuotation* rec = joy_quotation_new(5);
        joy_quotation_push(rec, joy_value_copy(p));
        joy_quotation_push(rec, joy_value_copy(t));
        joy_quotation_push(rec, joy_value_copy(r1));
        joy_quotation_push(rec, joy_value_copy(r2));
        joy_quotation_push(rec, joy_symbol("genrec"));
        JoyValue rec_val = {.type = JOY_QUOTATION, .data.quotation = rec};
        PUSH(rec_val);
        execute_quot(ctx, &r2);
    }

    joy_value_free(&p);
    joy_value_free(&t);
    joy_value_free(&r1);
    joy_value_free(&r2);
}

/* ---------- I/O Operations ---------- */

static void prim_put(JoyContext* ctx) {
    REQUIRE(1, "put");
    JoyValue v = POP();
    joy_value_print(v);
    joy_value_free(&v);
}

static void prim_putch(JoyContext* ctx) {
    REQUIRE(1, "putch");
    JoyValue v = POP();
    if (v.type == JOY_CHAR) {
        putchar(v.data.character);
    } else if (v.type == JOY_INTEGER) {
        putchar((char)v.data.integer);
    } else {
        joy_error_type("putch", "CHAR or INTEGER", v.type);
    }
    joy_value_free(&v);
}

static void prim_putchars(JoyContext* ctx) {
    REQUIRE(1, "putchars");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_STRING, "putchars");
    printf("%s", v.data.string);
    joy_value_free(&v);
}

static void prim_newline(JoyContext* ctx) {
    (void)ctx;
    printf("\n");
}

static void prim_putln(JoyContext* ctx) {
    /* Print top of stack followed by newline */
    REQUIRE(1, "putln");
    JoyValue v = POP();
    joy_value_print(v);
    printf("\n");
    joy_value_free(&v);
}

static void prim_dot(JoyContext* ctx) {
    /* In Joy, '.' prints top of stack and pops it.
     * If stack is empty, it's a no-op (for smoke tests). */
    if (ctx->stack->depth == 0) return;
    JoyValue v = POP();
    joy_value_print(v);
    printf("\n");
    joy_value_free(&v);
}

static void prim_setecho(JoyContext* ctx) {
    /* I -> : set echo mode (0-3) */
    REQUIRE(1, "setecho");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_INTEGER, "setecho");
    ctx->echo = (int)v.data.integer;
}

static void prim_settracegc(JoyContext* ctx) {
    REQUIRE(1, "__settracegc");
    JoyValue v = POP();
    joy_value_free(&v);
    /* No-op: GC tracing not relevant for compiled code */
}

/* ---------- Set Operations ---------- */

static void prim_has(JoyContext* ctx) {
    /* A X has -> B : test if X is in aggregate A */
    REQUIRE(2, "has");
    JoyValue x = POP();
    JoyValue a = POP();
    bool result = false;

    if (a.type == JOY_SET) {
        if (x.type != JOY_INTEGER) {
            joy_value_free(&x);
            joy_value_free(&a);
            joy_error_type("has", "INTEGER for set", x.type);
        }
        int64_t elem = x.data.integer;
        if (elem >= 0 && elem < 64) {
            result = (a.data.set & (1ULL << elem)) != 0;
        }
    } else if (a.type == JOY_LIST) {
        for (size_t i = 0; i < a.data.list->length; i++) {
            if (joy_value_equal(a.data.list->items[i], x)) {
                result = true;
                break;
            }
        }
    } else if (a.type == JOY_STRING) {
        if (x.type != JOY_CHAR) {
            joy_value_free(&x);
            joy_value_free(&a);
            joy_error_type("has", "CHAR for string", x.type);
        }
        result = strchr(a.data.string, x.data.character) != NULL;
    } else {
        joy_value_free(&x);
        joy_value_free(&a);
        joy_error_type("has", "aggregate", a.type);
    }

    PUSH(joy_boolean(result));
    joy_value_free(&x);
    joy_value_free(&a);
}

/* ---------- Advanced Combinators ---------- */

static void prim_cond(JoyContext* ctx) {
    /* [[B1 T1] [B2 T2] ... [D]] -> ... */
    REQUIRE(1, "cond");
    JoyValue clauses = POP();

    JoyList* clause_list = NULL;
    JoyQuotation* clause_quot = NULL;
    size_t count = 0;

    if (clauses.type == JOY_LIST) {
        clause_list = clauses.data.list;
        count = clause_list->length;
    } else if (clauses.type == JOY_QUOTATION) {
        clause_quot = clauses.data.quotation;
        count = clause_quot->length;
    } else {
        joy_error_type("cond", "LIST or QUOTATION", clauses.type);
    }

    if (count == 0) {
        joy_value_free(&clauses);
        return;
    }

    /* Save stack for condition testing */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    for (size_t i = 0; i < count; i++) {
        JoyValue clause;
        if (clause_list) {
            clause = joy_value_copy(clause_list->items[i]);
        } else {
            clause = joy_value_copy(clause_quot->terms[i]);
        }

        if (clause.type != JOY_QUOTATION && clause.type != JOY_LIST) {
            joy_value_free(&clause);
            continue;
        }

        size_t clause_len = 0;
        JoyValue* clause_items = NULL;
        if (clause.type == JOY_QUOTATION) {
            clause_len = clause.data.quotation->length;
            clause_items = clause.data.quotation->terms;
        } else {
            clause_len = clause.data.list->length;
            clause_items = clause.data.list->items;
        }

        if (clause_len == 0) {
            joy_value_free(&clause);
            continue;
        }

        /* Last clause is the default - execute all elements as body */
        bool is_last = (i == count - 1);
        if (is_last) {
            joy_stack_free(ctx->stack);
            ctx->stack = joy_stack_copy(saved);
            for (size_t j = 0; j < clause_len; j++) {
                joy_execute_value(ctx, clause_items[j]);
            }
            joy_value_free(&clause);
            joy_stack_free(saved);
            joy_value_free(&clauses);
            return;
        }

        /* [[Bi] Ti...] - first element is condition quotation, rest is body */
        JoyValue condition = clause_items[0];

        /* Restore stack and test condition */
        joy_stack_free(ctx->stack);
        ctx->stack = joy_stack_copy(saved);
        execute_quot(ctx, &condition);

        JoyValue test_result = POP();
        bool passed = joy_value_truthy(test_result);
        joy_value_free(&test_result);

        if (passed) {
            /* Execute body on original stack (condition test is non-destructive) */
            joy_stack_free(ctx->stack);
            ctx->stack = joy_stack_copy(saved);
            for (size_t j = 1; j < clause_len; j++) {
                joy_execute_value(ctx, clause_items[j]);
            }
            joy_value_free(&clause);
            joy_stack_free(saved);
            joy_value_free(&clauses);
            return;
        }

        joy_value_free(&clause);
    }

    /* No clause matched - restore stack */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;
    joy_value_free(&clauses);
}

static void prim_infra(JoyContext* ctx) {
    /* L [P] -> L' : execute P with L as stack */
    REQUIRE(2, "infra");
    JoyValue quot = POP();
    JoyValue lst = POP();

    /* Save current stack */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Replace stack with list/quotation contents (list is TOS-first, stack is bottom-first) */
    joy_stack_clear(ctx->stack);
    if (lst.type == JOY_LIST) {
        /* Push in reverse order so first list element becomes TOS */
        for (size_t i = lst.data.list->length; i > 0; i--) {
            joy_stack_push(ctx->stack, joy_value_copy(lst.data.list->items[i-1]));
        }
    } else if (lst.type == JOY_QUOTATION) {
        for (size_t i = lst.data.quotation->length; i > 0; i--) {
            joy_stack_push(ctx->stack, joy_value_copy(lst.data.quotation->terms[i-1]));
        }
    } else {
        joy_stack_free(saved);
        joy_value_free(&quot);
        joy_value_free(&lst);
        joy_error_type("infra", "LIST or QUOTATION", lst.type);
    }

    /* Execute quotation */
    execute_quot(ctx, &quot);

    /* Collect result as list (TOS first order) */
    JoyList* result = joy_list_new(ctx->stack->depth);
    for (size_t i = ctx->stack->depth; i > 0; i--) {
        joy_list_push(result, joy_value_copy(ctx->stack->items[i-1]));
    }

    /* Restore original stack and push result */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;

    JoyValue result_val;
    result_val.type = JOY_LIST;
    result_val.data.list = result;
    PUSH(result_val);

    joy_value_free(&quot);
    joy_value_free(&lst);
}

/* ---------- Arity Combinators ---------- */

static void prim_nullary(JoyContext* ctx) {
    /* [P] -> R : execute P, push single result (save/restore stack) */
    REQUIRE(1, "nullary");
    JoyValue quot = POP();

    /* Save current stack */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Execute quotation */
    execute_quot(ctx, &quot);

    /* Get result (top of stack after execution) */
    JoyValue result = POP();

    /* Restore original stack and push result */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;
    PUSH(result);

    joy_value_free(&quot);
}

static void prim_unary(JoyContext* ctx) {
    /* X [P] -> R : execute P on X, push single result */
    REQUIRE(2, "unary");
    JoyValue quot = POP();
    JoyValue x = POP();

    /* Save current stack */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Clear stack, push X, execute P */
    joy_stack_clear(ctx->stack);
    PUSH(x);
    execute_quot(ctx, &quot);

    /* Get result */
    JoyValue result = POP();

    /* Restore original stack and push result */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;
    PUSH(result);

    joy_value_free(&quot);
}

static void prim_binary(JoyContext* ctx) {
    /* X Y [P] -> R : execute P on X Y, push single result */
    REQUIRE(3, "binary");
    JoyValue quot = POP();
    JoyValue y = POP();
    JoyValue x = POP();

    /* Save current stack */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Clear stack, push X Y, execute P */
    joy_stack_clear(ctx->stack);
    PUSH(x);
    PUSH(y);
    execute_quot(ctx, &quot);

    /* Get result */
    JoyValue result = POP();

    /* Restore original stack and push result */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;
    PUSH(result);

    joy_value_free(&quot);
}

static void prim_ternary(JoyContext* ctx) {
    /* X Y Z [P] -> R : execute P on X Y Z, push single result */
    REQUIRE(4, "ternary");
    JoyValue quot = POP();
    JoyValue z = POP();
    JoyValue y = POP();
    JoyValue x = POP();

    /* Save current stack */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Clear stack, push X Y Z, execute P */
    joy_stack_clear(ctx->stack);
    PUSH(x);
    PUSH(y);
    PUSH(z);
    execute_quot(ctx, &quot);

    /* Get result */
    JoyValue result = POP();

    /* Restore original stack and push result */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;
    PUSH(result);

    joy_value_free(&quot);
}

static void prim_unary2(JoyContext* ctx) {
    /* X1 X2 [P] -> R1 R2 : execute P on X1 and X2 separately */
    REQUIRE(3, "unary2");
    JoyValue quot = POP();
    JoyValue x2 = POP();
    JoyValue x1 = POP();

    /* Save current stack */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Execute P on X1 */
    joy_stack_clear(ctx->stack);
    PUSH(x1);
    execute_quot(ctx, &quot);
    JoyValue r1 = POP();

    /* Execute P on X2 */
    joy_stack_clear(ctx->stack);
    PUSH(x2);
    execute_quot(ctx, &quot);
    JoyValue r2 = POP();

    /* Restore original stack and push results */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;
    PUSH(r1);
    PUSH(r2);

    joy_value_free(&quot);
}

static void prim_unary3(JoyContext* ctx) {
    /* X1 X2 X3 [P] -> R1 R2 R3 : execute P on three values */
    REQUIRE(4, "unary3");
    JoyValue quot = POP();
    JoyValue x3 = POP();
    JoyValue x2 = POP();
    JoyValue x1 = POP();

    /* Save current stack */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Execute P on X1 */
    joy_stack_clear(ctx->stack);
    PUSH(x1);
    execute_quot(ctx, &quot);
    JoyValue r1 = POP();

    /* Execute P on X2 */
    joy_stack_clear(ctx->stack);
    PUSH(x2);
    execute_quot(ctx, &quot);
    JoyValue r2 = POP();

    /* Execute P on X3 */
    joy_stack_clear(ctx->stack);
    PUSH(x3);
    execute_quot(ctx, &quot);
    JoyValue r3 = POP();

    /* Restore original stack and push results */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;
    PUSH(r1);
    PUSH(r2);
    PUSH(r3);

    joy_value_free(&quot);
}

static void prim_unary4(JoyContext* ctx) {
    /* X1 X2 X3 X4 [P] -> R1 R2 R3 R4 : execute P on four values */
    REQUIRE(5, "unary4");
    JoyValue quot = POP();
    JoyValue x4 = POP();
    JoyValue x3 = POP();
    JoyValue x2 = POP();
    JoyValue x1 = POP();

    /* Save current stack */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Execute P on X1 */
    joy_stack_clear(ctx->stack);
    PUSH(x1);
    execute_quot(ctx, &quot);
    JoyValue r1 = POP();

    /* Execute P on X2 */
    joy_stack_clear(ctx->stack);
    PUSH(x2);
    execute_quot(ctx, &quot);
    JoyValue r2 = POP();

    /* Execute P on X3 */
    joy_stack_clear(ctx->stack);
    PUSH(x3);
    execute_quot(ctx, &quot);
    JoyValue r3 = POP();

    /* Execute P on X4 */
    joy_stack_clear(ctx->stack);
    PUSH(x4);
    execute_quot(ctx, &quot);
    JoyValue r4 = POP();

    /* Restore original stack and push results */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;
    PUSH(r1);
    PUSH(r2);
    PUSH(r3);
    PUSH(r4);

    joy_value_free(&quot);
}

static void prim_cleave(JoyContext* ctx) {
    /* X [P1] [P2] -> R1 R2 : apply two quotations to X */
    REQUIRE(3, "cleave");
    JoyValue quot2 = POP();
    JoyValue quot1 = POP();
    JoyValue x = POP();

    /* Save current stack */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Execute P1 on X */
    joy_stack_clear(ctx->stack);
    PUSH(joy_value_copy(x));
    execute_quot(ctx, &quot1);
    JoyValue r1 = POP();

    /* Execute P2 on X */
    joy_stack_clear(ctx->stack);
    PUSH(x);
    execute_quot(ctx, &quot2);
    JoyValue r2 = POP();

    /* Restore original stack and push results */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;
    PUSH(r1);
    PUSH(r2);

    joy_value_free(&quot1);
    joy_value_free(&quot2);
}

/* ---------- Application Combinators ---------- */

static void prim_app1(JoyContext* ctx) {
    /* [P] -> : pop quotation and execute it on remaining stack */
    /* Obsolescent in Joy, but still used in tests */
    REQUIRE(1, "app1");
    JoyValue quot = POP();
    execute_quot(ctx, &quot);
    joy_value_free(&quot);
}

static void prim_app11(JoyContext* ctx) {
    /* X Y [P] -> R : execute P on stack (consuming Y and [P]), pop second */
    /* Equivalent to: i popd */
    REQUIRE(3, "app11");
    JoyValue quot = POP();
    execute_quot(ctx, &quot);
    joy_value_free(&quot);
    /* popd - pop the second element */
    if (ctx->stack->depth >= 2) {
        JoyValue top = POP();
        JoyValue second = POP();
        joy_value_free(&second);
        PUSH(top);
    }
}

static void prim_app12(JoyContext* ctx) {
    /* X Y1 Y2 [P] -> Y1 Y2 R : apply P to X, Y1 Y2 unchanged */
    REQUIRE(4, "app12");
    JoyValue quot = POP();
    JoyValue y2 = POP();
    JoyValue y1 = POP();
    JoyValue x = POP();

    /* Save current stack */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Execute P on X */
    joy_stack_clear(ctx->stack);
    PUSH(x);
    execute_quot(ctx, &quot);
    JoyValue r = POP();

    /* Restore original stack and push Y1, Y2, then result */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;
    PUSH(y1);
    PUSH(y2);
    PUSH(r);

    joy_value_free(&quot);
}

static void prim_app2(JoyContext* ctx) {
    /* X1 X2 [P] -> R1 R2 : apply P to X1 and X2 separately */
    REQUIRE(3, "app2");
    JoyValue quot = POP();
    JoyValue x2 = POP();
    JoyValue x1 = POP();

    /* Save current stack */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Execute P on X1 */
    joy_stack_clear(ctx->stack);
    PUSH(x1);
    execute_quot(ctx, &quot);
    JoyValue r1 = POP();

    /* Execute P on X2 */
    joy_stack_clear(ctx->stack);
    PUSH(x2);
    execute_quot(ctx, &quot);
    JoyValue r2 = POP();

    /* Restore original stack and push results */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;
    PUSH(r1);
    PUSH(r2);

    joy_value_free(&quot);
}

static void prim_app3(JoyContext* ctx) {
    /* X1 X2 X3 [P] -> R1 R2 R3 : apply P to three values */
    REQUIRE(4, "app3");
    JoyValue quot = POP();
    JoyValue x3 = POP();
    JoyValue x2 = POP();
    JoyValue x1 = POP();

    /* Save current stack */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Execute P on X1 */
    joy_stack_clear(ctx->stack);
    PUSH(x1);
    execute_quot(ctx, &quot);
    JoyValue r1 = POP();

    /* Execute P on X2 */
    joy_stack_clear(ctx->stack);
    PUSH(x2);
    execute_quot(ctx, &quot);
    JoyValue r2 = POP();

    /* Execute P on X3 */
    joy_stack_clear(ctx->stack);
    PUSH(x3);
    execute_quot(ctx, &quot);
    JoyValue r3 = POP();

    /* Restore original stack and push results */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;
    PUSH(r1);
    PUSH(r2);
    PUSH(r3);

    joy_value_free(&quot);
}

static void prim_app4(JoyContext* ctx) {
    /* X1 X2 X3 X4 [P] -> R1 R2 R3 R4 : apply P to four values */
    REQUIRE(5, "app4");
    JoyValue quot = POP();
    JoyValue x4 = POP();
    JoyValue x3 = POP();
    JoyValue x2 = POP();
    JoyValue x1 = POP();

    /* Save current stack */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Execute P on X1 */
    joy_stack_clear(ctx->stack);
    PUSH(x1);
    execute_quot(ctx, &quot);
    JoyValue r1 = POP();

    /* Execute P on X2 */
    joy_stack_clear(ctx->stack);
    PUSH(x2);
    execute_quot(ctx, &quot);
    JoyValue r2 = POP();

    /* Execute P on X3 */
    joy_stack_clear(ctx->stack);
    PUSH(x3);
    execute_quot(ctx, &quot);
    JoyValue r3 = POP();

    /* Execute P on X4 */
    joy_stack_clear(ctx->stack);
    PUSH(x4);
    execute_quot(ctx, &quot);
    JoyValue r4 = POP();

    /* Restore original stack and push results */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;
    PUSH(r1);
    PUSH(r2);
    PUSH(r3);
    PUSH(r4);

    joy_value_free(&quot);
}

static void prim_construct(JoyContext* ctx) {
    /* [P] [[P1] [P2] ..] -> R1 R2 .. : execute P, then each Pi, collect results */
    REQUIRE(2, "construct");
    JoyValue quots = POP();  /* List of quotations */
    JoyValue p = POP();      /* Initial quotation */

    /* Execute P first */
    execute_quot(ctx, &p);

    /* Now execute each quotation in the list, saving stack before each */
    if (quots.type != JOY_LIST && quots.type != JOY_QUOTATION) {
        joy_value_free(&p);
        joy_value_free(&quots);
        joy_error_type("construct", "LIST or QUOTATION", quots.type);
    }

    size_t n = quots.type == JOY_LIST ? quots.data.list->length : quots.data.quotation->length;
    JoyValue* items = quots.type == JOY_LIST ? quots.data.list->items : quots.data.quotation->terms;

    /* Store results */
    JoyValue* results = malloc(n * sizeof(JoyValue));

    /* Save stack state after P execution */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    for (size_t i = 0; i < n; i++) {
        /* Restore stack to state after P */
        joy_stack_clear(ctx->stack);
        for (size_t j = 0; j < saved->depth; j++) {
            PUSH(joy_value_copy(saved->items[j]));
        }

        /* Execute Pi */
        JoyValue qi = items[i];
        execute_quot(ctx, &qi);

        /* Get result */
        results[i] = POP();
    }

    /* Restore original stack (before construct) and push all results */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;

    /* Clear the saved stack contents and push results */
    joy_stack_clear(ctx->stack);
    for (size_t i = 0; i < n; i++) {
        PUSH(results[i]);
    }

    free(results);
    joy_value_free(&p);
    joy_value_free(&quots);
}

/* ---------- Type Conditionals ---------- */

static void prim_ifinteger(JoyContext* ctx) {
    /* X [T] [E] -> ... : if X is integer, execute T, else E */
    REQUIRE(3, "ifinteger");
    JoyValue e_quot = POP();
    JoyValue t_quot = POP();
    JoyValue x = POP();
    bool is_type = (x.type == JOY_INTEGER);
    PUSH(x);
    if (is_type) {
        execute_quot(ctx, &t_quot);
    } else {
        execute_quot(ctx, &e_quot);
    }
    joy_value_free(&t_quot);
    joy_value_free(&e_quot);
}

static void prim_ifchar(JoyContext* ctx) {
    /* X [T] [E] -> ... : if X is char, execute T, else E */
    REQUIRE(3, "ifchar");
    JoyValue e_quot = POP();
    JoyValue t_quot = POP();
    JoyValue x = POP();
    bool is_type = (x.type == JOY_CHAR);
    PUSH(x);
    if (is_type) {
        execute_quot(ctx, &t_quot);
    } else {
        execute_quot(ctx, &e_quot);
    }
    joy_value_free(&t_quot);
    joy_value_free(&e_quot);
}

static void prim_iflogical(JoyContext* ctx) {
    /* X [T] [E] -> ... : if X is boolean, execute T, else E */
    REQUIRE(3, "iflogical");
    JoyValue e_quot = POP();
    JoyValue t_quot = POP();
    JoyValue x = POP();
    bool is_type = (x.type == JOY_BOOLEAN);
    PUSH(x);
    if (is_type) {
        execute_quot(ctx, &t_quot);
    } else {
        execute_quot(ctx, &e_quot);
    }
    joy_value_free(&t_quot);
    joy_value_free(&e_quot);
}

static void prim_ifset(JoyContext* ctx) {
    /* X [T] [E] -> ... : if X is set, execute T, else E */
    REQUIRE(3, "ifset");
    JoyValue e_quot = POP();
    JoyValue t_quot = POP();
    JoyValue x = POP();
    bool is_type = (x.type == JOY_SET);
    PUSH(x);
    if (is_type) {
        execute_quot(ctx, &t_quot);
    } else {
        execute_quot(ctx, &e_quot);
    }
    joy_value_free(&t_quot);
    joy_value_free(&e_quot);
}

static void prim_ifstring(JoyContext* ctx) {
    /* X [T] [E] -> ... : if X is string, execute T, else E */
    REQUIRE(3, "ifstring");
    JoyValue e_quot = POP();
    JoyValue t_quot = POP();
    JoyValue x = POP();
    bool is_type = (x.type == JOY_STRING);
    PUSH(x);
    if (is_type) {
        execute_quot(ctx, &t_quot);
    } else {
        execute_quot(ctx, &e_quot);
    }
    joy_value_free(&t_quot);
    joy_value_free(&e_quot);
}

static void prim_iflist(JoyContext* ctx) {
    /* X [T] [E] -> ... : if X is list or quotation, execute T, else E */
    REQUIRE(3, "iflist");
    JoyValue e_quot = POP();
    JoyValue t_quot = POP();
    JoyValue x = POP();
    bool is_type = (x.type == JOY_LIST || x.type == JOY_QUOTATION);
    PUSH(x);
    if (is_type) {
        execute_quot(ctx, &t_quot);
    } else {
        execute_quot(ctx, &e_quot);
    }
    joy_value_free(&t_quot);
    joy_value_free(&e_quot);
}

static void prim_iffloat(JoyContext* ctx) {
    /* X [T] [E] -> ... : if X is float, execute T, else E */
    REQUIRE(3, "iffloat");
    JoyValue e_quot = POP();
    JoyValue t_quot = POP();
    JoyValue x = POP();
    bool is_type = (x.type == JOY_FLOAT);
    PUSH(x);
    if (is_type) {
        execute_quot(ctx, &t_quot);
    } else {
        execute_quot(ctx, &e_quot);
    }
    joy_value_free(&t_quot);
    joy_value_free(&e_quot);
}

static void prim_iffile(JoyContext* ctx) {
    /* X [T] [E] -> ... : if X is file, execute T, else E */
    REQUIRE(3, "iffile");
    JoyValue e_quot = POP();
    JoyValue t_quot = POP();
    JoyValue x = POP();
    bool is_type = (x.type == JOY_FILE);
    PUSH(x);
    if (is_type) {
        execute_quot(ctx, &t_quot);
    } else {
        execute_quot(ctx, &e_quot);
    }
    joy_value_free(&t_quot);
    joy_value_free(&e_quot);
}

/* condnestrecaux - shared implementation for condlinrec and condnestrec

   Format: [ [C1] [C2] .. [D] ] where:
   - Each [Ci] is [[B] [T]] or [[B] [R1] [R2] ...]
   - [D] is the default: [[T]] or [[R1] [R2] ...] (NO condition B)

   Tests each B (except last clause which is default).
   If B is true: skip B, use rest as [T] or [R1 R2 ...]
   If no B matches: use last clause (default) as-is

   Then executes: first part, then for each subsequent: recurse, execute
*/
static void condnestrecaux(JoyContext* ctx, JoyValue* clauses) {
    size_t count = 0;
    JoyValue* items = NULL;

    if (clauses->type == JOY_LIST) {
        count = clauses->data.list->length;
        items = clauses->data.list->items;
    } else if (clauses->type == JOY_QUOTATION) {
        count = clauses->data.quotation->length;
        items = clauses->data.quotation->terms;
    } else {
        return;
    }

    if (count == 0) return;

    /* Save stack for condition testing */
    JoyStack* saved = joy_stack_copy(ctx->stack);

    /* Test B for all clauses EXCEPT the last (which is default) */
    bool matched = false;
    size_t matched_idx = count - 1;  /* Default to last clause */

    for (size_t i = 0; i < count - 1; i++) {
        JoyValue clause = items[i];

        size_t clause_len = 0;
        JoyValue* clause_items = NULL;
        if (clause.type == JOY_QUOTATION) {
            clause_len = clause.data.quotation->length;
            clause_items = clause.data.quotation->terms;
        } else if (clause.type == JOY_LIST) {
            clause_len = clause.data.list->length;
            clause_items = clause.data.list->items;
        } else {
            continue;
        }

        if (clause_len < 2) continue;

        /* Test condition B (first element) */
        joy_stack_free(ctx->stack);
        ctx->stack = joy_stack_copy(saved);
        execute_quot(ctx, &clause_items[0]);

        JoyValue test_result = POP();
        bool passed = joy_value_truthy(test_result);
        joy_value_free(&test_result);

        if (passed) {
            matched = true;
            matched_idx = i;
            break;
        }
    }

    /* Restore stack - ctx->stack now owns the saved memory */
    joy_stack_free(ctx->stack);
    ctx->stack = saved;
    /* Note: don't free saved separately - it's now ctx->stack */

    /* Get the clause to execute */
    JoyValue clause = items[matched_idx];
    size_t clause_len = 0;
    JoyValue* clause_items = NULL;
    if (clause.type == JOY_QUOTATION) {
        clause_len = clause.data.quotation->length;
        clause_items = clause.data.quotation->terms;
    } else if (clause.type == JOY_LIST) {
        clause_len = clause.data.list->length;
        clause_items = clause.data.list->items;
    } else {
        return;
    }

    /* Determine which parts to execute:
       - If matched: skip B (element 0), use elements 1..n as [T] or [R1 R2 ...]
       - If default (last): use all elements as [T] or [R1 R2 ...] */
    size_t start_idx = matched ? 1 : 0;
    size_t parts_count = clause_len - start_idx;

    if (parts_count == 0) {
        return;
    }

    /* Execute: first part, then for each subsequent: recurse, execute */
    execute_quot(ctx, &clause_items[start_idx]);

    for (size_t j = start_idx + 1; j < clause_len; j++) {
        condnestrecaux(ctx, clauses);  /* Recurse */
        execute_quot(ctx, &clause_items[j]);
    }
}

static void prim_condlinrec(JoyContext* ctx) {
    REQUIRE(1, "condlinrec");
    JoyValue clauses = POP();
    condnestrecaux(ctx, &clauses);
    joy_value_free(&clauses);
}

static void prim_condnestrec(JoyContext* ctx) {
    REQUIRE(1, "condnestrec");
    JoyValue clauses = POP();
    condnestrecaux(ctx, &clauses);
    joy_value_free(&clauses);
}

/* ---------- Tree Combinators ---------- */

/* Helper: check if value is a leaf (not a list/quotation) */
static bool is_tree_leaf(JoyValue* v) {
    return v->type != JOY_LIST && v->type != JOY_QUOTATION;
}

static void treestep_aux(JoyContext* ctx, JoyValue* t, JoyValue* p);

static void treestep_aux(JoyContext* ctx, JoyValue* t, JoyValue* p) {
    if (is_tree_leaf(t)) {
        /* Leaf: push t and execute p */
        PUSH(joy_value_copy(*t));
        execute_quot(ctx, p);
    } else {
        /* List/quotation: recursively treestep each element */
        size_t len = t->type == JOY_LIST ? t->data.list->length : t->data.quotation->length;
        JoyValue* items = t->type == JOY_LIST ? t->data.list->items : t->data.quotation->terms;
        for (size_t i = 0; i < len; i++) {
            treestep_aux(ctx, &items[i], p);
        }
    }
}

static void prim_treestep(JoyContext* ctx) {
    /* T [P] -> ... : step through tree T, applying P to each leaf */
    REQUIRE(2, "treestep");
    JoyValue p = POP();
    JoyValue t = POP();
    treestep_aux(ctx, &t, &p);
    joy_value_free(&t);
    joy_value_free(&p);
}

static JoyValue treerec_aux(JoyContext* ctx, JoyValue* t, JoyValue* o, JoyValue* c);

static JoyValue treerec_aux(JoyContext* ctx, JoyValue* t, JoyValue* o, JoyValue* c) {
    (void)c;  /* C specifies iteration strategy; we implement map-like behavior */
    if (is_tree_leaf(t)) {
        /* Leaf: push t and execute o, return top of stack */
        PUSH(joy_value_copy(*t));
        execute_quot(ctx, o);
        return POP();
    } else {
        /* List: recursively process each element, collect results into new list */
        size_t len = t->type == JOY_LIST ? t->data.list->length : t->data.quotation->length;
        JoyValue* items = t->type == JOY_LIST ? t->data.list->items : t->data.quotation->terms;

        /* Map treerec over elements, preserving tree structure */
        JoyList* results = joy_list_new(len);
        for (size_t i = 0; i < len; i++) {
            JoyValue r = treerec_aux(ctx, &items[i], o, c);
            joy_list_push(results, r);
        }

        JoyValue rv = {.type = JOY_LIST, .data.list = results};
        return rv;
    }
}

static void prim_treerec(JoyContext* ctx) {
    /* T [O] [C] -> ... : tree recursion with O for leaves, C for combining */
    REQUIRE(3, "treerec");
    JoyValue c = POP();
    JoyValue o = POP();
    JoyValue t = POP();
    JoyValue result = treerec_aux(ctx, &t, &o, &c);
    PUSH(result);
    joy_value_free(&t);
    joy_value_free(&o);
    joy_value_free(&c);
}

static JoyValue treegenrec_aux(JoyContext* ctx, JoyValue* t, JoyValue* o1, JoyValue* o2, JoyValue* c);

static JoyValue treegenrec_aux(JoyContext* ctx, JoyValue* t, JoyValue* o1, JoyValue* o2, JoyValue* c) {
    (void)c;  /* C specifies iteration strategy; we implement map-like behavior */
    if (is_tree_leaf(t)) {
        /* Leaf: push t and execute o1, return result */
        PUSH(joy_value_copy(*t));
        execute_quot(ctx, o1);
        return POP();
    } else {
        /* Branch: execute o2 (pre-process step, doesn't receive T), then recurse */
        size_t len = t->type == JOY_LIST ? t->data.list->length : t->data.quotation->length;
        JoyValue* items = t->type == JOY_LIST ? t->data.list->items : t->data.quotation->terms;

        /* Execute o2 as pre-processing (typically empty or side-effect only) */
        execute_quot(ctx, o2);

        /* Recursively process each element, preserving tree structure */
        JoyList* results = joy_list_new(len);
        for (size_t i = 0; i < len; i++) {
            JoyValue r = treegenrec_aux(ctx, &items[i], o1, o2, c);
            joy_list_push(results, r);
        }

        JoyValue rv = {.type = JOY_LIST, .data.list = results};
        return rv;
    }
}

static void prim_treegenrec(JoyContext* ctx) {
    /* T [O1] [O2] [C] -> ... : general tree recursion */
    REQUIRE(4, "treegenrec");
    JoyValue c = POP();
    JoyValue o2 = POP();
    JoyValue o1 = POP();
    JoyValue t = POP();
    JoyValue result = treegenrec_aux(ctx, &t, &o1, &o2, &c);
    PUSH(result);
    joy_value_free(&t);
    joy_value_free(&o1);
    joy_value_free(&o2);
    joy_value_free(&c);
}

/* ---------- Type Predicates ---------- */

static void prim_integer(JoyContext* ctx) {
    REQUIRE(1, "integer");
    JoyValue v = POP();
    PUSH(joy_boolean(v.type == JOY_INTEGER));
    joy_value_free(&v);
}

static void prim_float_p(JoyContext* ctx) {
    REQUIRE(1, "float");
    JoyValue v = POP();
    PUSH(joy_boolean(v.type == JOY_FLOAT));
    joy_value_free(&v);
}

static void prim_logical(JoyContext* ctx) {
    REQUIRE(1, "logical");
    JoyValue v = POP();
    PUSH(joy_boolean(v.type == JOY_BOOLEAN));
    joy_value_free(&v);
}

static void prim_char_p(JoyContext* ctx) {
    REQUIRE(1, "char");
    JoyValue v = POP();
    PUSH(joy_boolean(v.type == JOY_CHAR));
    joy_value_free(&v);
}

static void prim_string_p(JoyContext* ctx) {
    REQUIRE(1, "string");
    JoyValue v = POP();
    PUSH(joy_boolean(v.type == JOY_STRING));
    joy_value_free(&v);
}

static void prim_list(JoyContext* ctx) {
    REQUIRE(1, "list");
    JoyValue v = POP();
    PUSH(joy_boolean(v.type == JOY_LIST || v.type == JOY_QUOTATION));
    joy_value_free(&v);
}

static void prim_set_p(JoyContext* ctx) {
    REQUIRE(1, "set");
    JoyValue v = POP();
    PUSH(joy_boolean(v.type == JOY_SET));
    joy_value_free(&v);
}

static void prim_leaf(JoyContext* ctx) {
    /* X -> B : true if X is not an aggregate (not list/quotation/set/string) */
    REQUIRE(1, "leaf");
    JoyValue v = POP();
    bool is_leaf = (v.type != JOY_LIST && v.type != JOY_QUOTATION &&
                    v.type != JOY_SET && v.type != JOY_STRING);
    PUSH(joy_boolean(is_leaf));
    joy_value_free(&v);
}

static void prim_file_p(JoyContext* ctx) {
    /* F -> B : true if F is a file handle */
    REQUIRE(1, "file");
    JoyValue v = POP();
    PUSH(joy_boolean(v.type == JOY_FILE));
    joy_value_free(&v);
}

static void prim_user(JoyContext* ctx) {
    /* X -> B : true if X is a user-defined symbol (not a primitive) */
    REQUIRE(1, "user");
    JoyValue v = POP();

    bool result = false;
    if (v.type == JOY_SYMBOL) {
        JoyWord* word = joy_dict_lookup(ctx->dictionary, v.data.symbol);
        if (word && word->is_user) {
            result = true;
        }
    }

    PUSH(joy_boolean(result));
    joy_value_free(&v);
}

/* ---------- Type Conversion ---------- */

static void prim_ord(JoyContext* ctx) {
    REQUIRE(1, "ord");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_CHAR, "ord");
    PUSH(joy_integer((int64_t)(unsigned char)v.data.character));
}

static void prim_chr(JoyContext* ctx) {
    REQUIRE(1, "chr");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_INTEGER, "chr");
    PUSH(joy_char((char)v.data.integer));
}

/* ---------- Constants ---------- */

static void prim_true(JoyContext* ctx) {
    PUSH(joy_boolean(true));
}

static void prim_false(JoyContext* ctx) {
    PUSH(joy_boolean(false));
}

static void prim_maxint(JoyContext* ctx) {
    /* -> I : push maximum integer value */
    PUSH(joy_integer(INT64_MAX));
}

static void prim_setsize(JoyContext* ctx) {
    /* -> I : push size of sets (64) */
    PUSH(joy_integer(64));
}

/* ---------- File I/O ---------- */

static void prim_stdin(JoyContext* ctx) {
    /* -> S : push standard input stream */
    PUSH(joy_file(stdin));
}

static void prim_stdout(JoyContext* ctx) {
    /* -> S : push standard output stream */
    PUSH(joy_file(stdout));
}

static void prim_stderr(JoyContext* ctx) {
    /* -> S : push standard error stream */
    PUSH(joy_file(stderr));
}

/* ---------- File I/O Operations ---------- */

static void prim_fopen(JoyContext* ctx) {
    /* P M -> S : open file with path P and mode M */
    REQUIRE(2, "fopen");
    JoyValue mode = POP();
    JoyValue path = POP();
    EXPECT_TYPE(path, JOY_STRING, "fopen");
    EXPECT_TYPE(mode, JOY_STRING, "fopen");
    FILE* f = fopen(path.data.string, mode.data.string);
    joy_value_free(&path);
    joy_value_free(&mode);
    if (f) {
        PUSH(joy_file(f));
    } else {
        /* Push false on failure */
        PUSH(joy_boolean(false));
    }
}

static void prim_fclose(JoyContext* ctx) {
    /* S -> : close file stream */
    REQUIRE(1, "fclose");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_FILE, "fclose");
    if (v.data.file && v.data.file != stdin &&
        v.data.file != stdout && v.data.file != stderr) {
        fclose(v.data.file);
    }
    /* Note: joy_value_free doesn't close files, so we handle it here */
}

static void prim_fflush(JoyContext* ctx) {
    /* S -> S : flush file stream */
    REQUIRE(1, "fflush");
    JoyValue v = PEEK();
    EXPECT_TYPE(v, JOY_FILE, "fflush");
    if (v.data.file) {
        fflush(v.data.file);
    }
}

static void prim_feof(JoyContext* ctx) {
    /* S -> S B : test end-of-file */
    REQUIRE(1, "feof");
    JoyValue v = PEEK();
    EXPECT_TYPE(v, JOY_FILE, "feof");
    bool is_eof = v.data.file ? feof(v.data.file) != 0 : true;
    PUSH(joy_boolean(is_eof));
}

static void prim_ferror(JoyContext* ctx) {
    /* S -> S B : test file error */
    REQUIRE(1, "ferror");
    JoyValue v = PEEK();
    EXPECT_TYPE(v, JOY_FILE, "ferror");
    bool has_error = v.data.file ? ferror(v.data.file) != 0 : true;
    PUSH(joy_boolean(has_error));
}

static void prim_fgetch(JoyContext* ctx) {
    /* S -> S C : read character from file */
    REQUIRE(1, "fgetch");
    JoyValue v = PEEK();
    EXPECT_TYPE(v, JOY_FILE, "fgetch");
    int c = v.data.file ? fgetc(v.data.file) : EOF;
    if (c == EOF) {
        PUSH(joy_integer(-1));
    } else {
        PUSH(joy_char((char)c));
    }
}

static void prim_fgets(JoyContext* ctx) {
    /* S -> S L : read line from file as string */
    REQUIRE(1, "fgets");
    JoyValue v = PEEK();
    EXPECT_TYPE(v, JOY_FILE, "fgets");

    char buffer[4096];
    size_t len = 0;
    if (v.data.file) {
        int c;
        while (len < sizeof(buffer) - 1 && (c = fgetc(v.data.file)) != EOF && c != '\n') {
            buffer[len++] = (char)c;
        }
        if (c == '\n' && len < sizeof(buffer) - 1) {
            buffer[len++] = '\n';
        }
    }
    buffer[len] = '\0';
    PUSH(joy_string(buffer));
}

static void prim_fread(JoyContext* ctx) {
    /* S I -> S L : read I bytes from file as list of chars */
    REQUIRE(2, "fread");
    JoyValue count = POP();
    JoyValue v = PEEK();
    EXPECT_TYPE(v, JOY_FILE, "fread");
    EXPECT_TYPE(count, JOY_INTEGER, "fread");

    size_t n = count.data.integer > 0 ? (size_t)count.data.integer : 0;
    JoyList* chars = joy_list_new(n);
    joy_value_free(&count);

    if (v.data.file && n > 0) {
        for (size_t i = 0; i < n; i++) {
            int c = fgetc(v.data.file);
            if (c == EOF) break;
            joy_list_push(chars, joy_char((char)c));
        }
    }
    JoyValue result = {.type = JOY_LIST, .data.list = chars};
    PUSH(result);
}

static void prim_fput(JoyContext* ctx) {
    /* S X -> S : write X to file */
    REQUIRE(2, "fput");
    JoyValue x = POP();
    JoyValue v = PEEK();
    EXPECT_TYPE(v, JOY_FILE, "fput");

    if (v.data.file) {
        /* Use joy_value_print logic but to file */
        switch (x.type) {
            case JOY_INTEGER:
                fprintf(v.data.file, "%lld", (long long)x.data.integer);
                break;
            case JOY_FLOAT:
                fprintf(v.data.file, "%g", x.data.floating);
                break;
            case JOY_BOOLEAN:
                fprintf(v.data.file, "%s", x.data.boolean ? "true" : "false");
                break;
            case JOY_CHAR:
                fputc(x.data.character, v.data.file);
                break;
            case JOY_STRING:
                fprintf(v.data.file, "%s", x.data.string);
                break;
            default:
                /* For complex types, just indicate type */
                fprintf(v.data.file, "<%s>",
                    x.type == JOY_LIST ? "list" :
                    x.type == JOY_QUOTATION ? "quotation" :
                    x.type == JOY_SET ? "set" : "value");
                break;
        }
    }
    joy_value_free(&x);
}

static void prim_fputch(JoyContext* ctx) {
    /* S C -> S : write character to file */
    REQUIRE(2, "fputch");
    JoyValue c = POP();
    JoyValue v = PEEK();
    EXPECT_TYPE(v, JOY_FILE, "fputch");
    EXPECT_TYPE(c, JOY_CHAR, "fputch");

    if (v.data.file) {
        fputc(c.data.character, v.data.file);
    }
    joy_value_free(&c);
}

static void prim_fputchars(JoyContext* ctx) {
    /* S "abc.." -> S : write characters to file */
    REQUIRE(2, "fputchars");
    JoyValue s = POP();
    JoyValue v = PEEK();
    EXPECT_TYPE(v, JOY_FILE, "fputchars");
    EXPECT_TYPE(s, JOY_STRING, "fputchars");

    if (v.data.file) {
        fputs(s.data.string, v.data.file);
    }
    joy_value_free(&s);
}

static void prim_fputstring(JoyContext* ctx) {
    /* S "abc.." -> S : write string to file (same as fputchars) */
    REQUIRE(2, "fputstring");
    JoyValue s = POP();
    JoyValue v = PEEK();
    EXPECT_TYPE(v, JOY_FILE, "fputstring");
    EXPECT_TYPE(s, JOY_STRING, "fputstring");

    if (v.data.file) {
        fputs(s.data.string, v.data.file);
    }
    joy_value_free(&s);
}

static void prim_fwrite(JoyContext* ctx) {
    /* S L -> S : write list L of chars to file */
    REQUIRE(2, "fwrite");
    JoyValue list = POP();
    JoyValue v = PEEK();
    EXPECT_TYPE(v, JOY_FILE, "fwrite");

    if (list.type == JOY_LIST && v.data.file) {
        for (size_t i = 0; i < list.data.list->length; i++) {
            JoyValue item = list.data.list->items[i];
            if (item.type == JOY_CHAR) {
                fputc(item.data.character, v.data.file);
            } else if (item.type == JOY_INTEGER) {
                fputc((char)item.data.integer, v.data.file);
            }
        }
    }
    joy_value_free(&list);
}

static void prim_fseek(JoyContext* ctx) {
    /* S P W -> S B : seek in file, push success status */
    /* P=position, W=whence: 0=SET, 1=CUR, 2=END */
    /* C fseek returns 0 on success, non-zero on failure */
    REQUIRE(3, "fseek");
    JoyValue whence = POP();
    JoyValue pos = POP();
    JoyValue v = PEEK();
    EXPECT_TYPE(v, JOY_FILE, "fseek");
    EXPECT_TYPE(pos, JOY_INTEGER, "fseek");
    EXPECT_TYPE(whence, JOY_INTEGER, "fseek");

    int result = -1;  /* Failure by default */
    if (v.data.file) {
        int w = SEEK_SET;
        if (whence.data.integer == 1) w = SEEK_CUR;
        else if (whence.data.integer == 2) w = SEEK_END;
        result = fseek(v.data.file, (long)pos.data.integer, w);
    }
    joy_value_free(&pos);
    joy_value_free(&whence);
    /* Push false on success (0), true on failure (non-zero) */
    PUSH(joy_boolean(result != 0));
}

static void prim_ftell(JoyContext* ctx) {
    /* S -> S I : get file position */
    REQUIRE(1, "ftell");
    JoyValue v = PEEK();
    EXPECT_TYPE(v, JOY_FILE, "ftell");

    long pos = v.data.file ? ftell(v.data.file) : -1;
    PUSH(joy_integer(pos));
}

static void prim_fremove(JoyContext* ctx) {
    /* P -> B : remove file at path P */
    REQUIRE(1, "fremove");
    JoyValue path = POP();
    EXPECT_TYPE(path, JOY_STRING, "fremove");

    int result = remove(path.data.string);
    joy_value_free(&path);
    PUSH(joy_boolean(result == 0));
}

static void prim_frename(JoyContext* ctx) {
    /* P1 P2 -> B : rename file from P1 to P2 */
    REQUIRE(2, "frename");
    JoyValue newpath = POP();
    JoyValue oldpath = POP();
    EXPECT_TYPE(oldpath, JOY_STRING, "frename");
    EXPECT_TYPE(newpath, JOY_STRING, "frename");

    int result = rename(oldpath.data.string, newpath.data.string);
    joy_value_free(&oldpath);
    joy_value_free(&newpath);
    PUSH(joy_boolean(result == 0));
}

static void prim_finclude(JoyContext* ctx) {
    /* S -> : read and execute Joy code from file handle S */
    REQUIRE(1, "finclude");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_FILE, "finclude");

    FILE* f = v.data.file;
    if (!f) {
        joy_error("finclude: invalid file handle");
        return;
    }

    /* Read entire file from current position */
    long start = ftell(f);
    fseek(f, 0, SEEK_END);
    long end = ftell(f);
    long size = end - start;
    fseek(f, start, SEEK_SET);

    if (size <= 0) {
        return;  /* Empty or at end */
    }

    char* content = malloc(size + 1);
    if (!content) {
        joy_error("finclude: out of memory");
        return;
    }

    size_t read = fread(content, 1, size, f);
    content[read] = '\0';

    /* Parse and execute */
    joy_eval_line(ctx, content);
    free(content);
}

static void prim_include(JoyContext* ctx) {
    /* "filename" -> : load and execute Joy file */
    REQUIRE(1, "include");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_STRING, "include");

    int result = joy_load_file(ctx, v.data.string);
    if (result != 0) {
        fprintf(stderr, "include: could not load file '%s'\n", v.data.string);
    }
    joy_value_free(&v);
}

static void prim_filetime(JoyContext* ctx) {
    /* "filename" -> T : get file modification time, or false if not found */
    REQUIRE(1, "filetime");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_STRING, "filetime");

    struct stat st;
    if (stat(v.data.string, &st) == 0) {
        PUSH(joy_integer((int64_t)st.st_mtime));
    } else {
        PUSH(joy_boolean(false));
    }
    joy_value_free(&v);
}

/* ---------- Additional Math Functions ---------- */

static void prim_acos(JoyContext* ctx) {
    /* F -> G : arc cosine */
    REQUIRE(1, "acos");
    JoyValue v = POP();
    double x = (v.type == JOY_INTEGER) ? (double)v.data.integer : v.data.floating;
    joy_value_free(&v);
    PUSH(joy_float(acos(x)));
}

static void prim_asin(JoyContext* ctx) {
    /* F -> G : arc sine */
    REQUIRE(1, "asin");
    JoyValue v = POP();
    double x = (v.type == JOY_INTEGER) ? (double)v.data.integer : v.data.floating;
    joy_value_free(&v);
    PUSH(joy_float(asin(x)));
}

static void prim_atan(JoyContext* ctx) {
    /* F -> G : arc tangent */
    REQUIRE(1, "atan");
    JoyValue v = POP();
    double x = (v.type == JOY_INTEGER) ? (double)v.data.integer : v.data.floating;
    joy_value_free(&v);
    PUSH(joy_float(atan(x)));
}

static void prim_atan2(JoyContext* ctx) {
    /* F G -> H : two-argument arc tangent */
    REQUIRE(2, "atan2");
    JoyValue vb = POP();  /* TOS */
    JoyValue va = POP();  /* second */
    double b = (vb.type == JOY_INTEGER) ? (double)vb.data.integer : vb.data.floating;
    double a = (va.type == JOY_INTEGER) ? (double)va.data.integer : va.data.floating;
    joy_value_free(&va);
    joy_value_free(&vb);
    PUSH(joy_float(atan2(a, b)));  /* atan2(second, TOS) */
}

static void prim_cosh(JoyContext* ctx) {
    /* F -> G : hyperbolic cosine */
    REQUIRE(1, "cosh");
    JoyValue v = POP();
    double x = (v.type == JOY_INTEGER) ? (double)v.data.integer : v.data.floating;
    joy_value_free(&v);
    PUSH(joy_float(cosh(x)));
}

static void prim_sinh(JoyContext* ctx) {
    /* F -> G : hyperbolic sine */
    REQUIRE(1, "sinh");
    JoyValue v = POP();
    double x = (v.type == JOY_INTEGER) ? (double)v.data.integer : v.data.floating;
    joy_value_free(&v);
    PUSH(joy_float(sinh(x)));
}

static void prim_tanh(JoyContext* ctx) {
    /* F -> G : hyperbolic tangent */
    REQUIRE(1, "tanh");
    JoyValue v = POP();
    double x = (v.type == JOY_INTEGER) ? (double)v.data.integer : v.data.floating;
    joy_value_free(&v);
    PUSH(joy_float(tanh(x)));
}

static void prim_log10(JoyContext* ctx) {
    /* F -> G : base-10 logarithm */
    REQUIRE(1, "log10");
    JoyValue v = POP();
    double x = (v.type == JOY_INTEGER) ? (double)v.data.integer : v.data.floating;
    joy_value_free(&v);
    PUSH(joy_float(log10(x)));
}

/* ---------- String Conversion ---------- */

static void prim_strtol(JoyContext* ctx) {
    /* S I -> J : convert string to integer with base I */
    REQUIRE(2, "strtol");
    JoyValue vbase = POP();
    JoyValue vstr = POP();
    EXPECT_TYPE(vstr, JOY_STRING, "strtol");
    EXPECT_TYPE(vbase, JOY_INTEGER, "strtol");

    char* endptr;
    long result = strtol(vstr.data.string, &endptr, (int)vbase.data.integer);
    joy_value_free(&vstr);
    joy_value_free(&vbase);
    PUSH(joy_integer(result));
}

static void prim_strtod(JoyContext* ctx) {
    /* S -> R : convert string to float */
    REQUIRE(1, "strtod");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_STRING, "strtod");

    char* endptr;
    double result = strtod(v.data.string, &endptr);
    joy_value_free(&v);
    PUSH(joy_float(result));
}

/* ---------- Time and Random ---------- */

static void prim_time(JoyContext* ctx) {
    /* -> I : push current time as seconds since epoch */
    PUSH(joy_integer((int64_t)time(NULL)));
}

static void prim_clock(JoyContext* ctx) {
    /* -> I : push processor clock ticks */
    PUSH(joy_integer((int64_t)clock()));
}

static void prim_rand(JoyContext* ctx) {
    /* -> I : push random integer */
    PUSH(joy_integer(rand()));
}

static void prim_srand(JoyContext* ctx) {
    /* I -> : seed random number generator */
    REQUIRE(1, "srand");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_INTEGER, "srand");
    srand((unsigned int)v.data.integer);
    joy_value_free(&v);
}

static void prim_localtime(JoyContext* ctx) {
    /* I -> T : convert time_t to local time [year mon day hour min sec isdst yday wday] */
    REQUIRE(1, "localtime");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_INTEGER, "localtime");
    time_t t = (time_t)v.data.integer;
    joy_value_free(&v);

    struct tm* tm = localtime(&t);
    if (!tm) {
        /* Return empty list on error */
        JoyValue empty = {.type = JOY_LIST, .data.list = joy_list_new(0)};
        PUSH(empty);
        return;
    }

    /* Create list: [year mon day hour min sec isdst yday wday] (Python format) */
    JoyList* result = joy_list_new(9);
    joy_list_push(result, joy_integer(tm->tm_year + 1900));  /* Full year */
    joy_list_push(result, joy_integer(tm->tm_mon + 1));      /* 1-12 */
    joy_list_push(result, joy_integer(tm->tm_mday));
    joy_list_push(result, joy_integer(tm->tm_hour));
    joy_list_push(result, joy_integer(tm->tm_min));
    joy_list_push(result, joy_integer(tm->tm_sec));
    joy_list_push(result, joy_boolean(tm->tm_isdst > 0));
    joy_list_push(result, joy_integer(tm->tm_yday + 1));     /* 1-366 */
    joy_list_push(result, joy_integer(tm->tm_wday));         /* 0=Sun, 6=Sat */

    JoyValue rv = {.type = JOY_LIST, .data.list = result};
    PUSH(rv);
}

static void prim_gmtime(JoyContext* ctx) {
    /* I -> T : convert time_t to UTC time [year mon day hour min sec isdst yday wday] */
    REQUIRE(1, "gmtime");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_INTEGER, "gmtime");
    time_t t = (time_t)v.data.integer;
    joy_value_free(&v);

    struct tm* tm = gmtime(&t);
    if (!tm) {
        /* Return empty list on error */
        JoyValue empty = {.type = JOY_LIST, .data.list = joy_list_new(0)};
        PUSH(empty);
        return;
    }

    /* Create list: [year mon day hour min sec isdst yday wday] (Python format) */
    JoyList* result = joy_list_new(9);
    joy_list_push(result, joy_integer(tm->tm_year + 1900));  /* Full year */
    joy_list_push(result, joy_integer(tm->tm_mon + 1));      /* 1-12 */
    joy_list_push(result, joy_integer(tm->tm_mday));
    joy_list_push(result, joy_integer(tm->tm_hour));
    joy_list_push(result, joy_integer(tm->tm_min));
    joy_list_push(result, joy_integer(tm->tm_sec));
    joy_list_push(result, joy_boolean(tm->tm_isdst > 0));
    joy_list_push(result, joy_integer(tm->tm_yday + 1));     /* 1-366 */
    joy_list_push(result, joy_integer(tm->tm_wday));         /* 0=Sun, 6=Sat */

    JoyValue rv = {.type = JOY_LIST, .data.list = result};
    PUSH(rv);
}

static void prim_mktime(JoyContext* ctx) {
    /* T -> I : convert time list [year mon day hour min sec isdst yday wday] to time_t */
    REQUIRE(1, "mktime");
    JoyValue v = POP();
    if (v.type != JOY_LIST && v.type != JOY_QUOTATION) {
        joy_value_free(&v);
        joy_error_type("mktime", "LIST", v.type);
    }

    if (v.data.list->length < 9) {
        joy_value_free(&v);
        fprintf(stderr, "Error: mktime requires list of 9 integers\n");
        exit(1);
    }

    /* Input format: [year mon day hour min sec isdst yday wday] (Python format) */
    struct tm tm = {0};
    tm.tm_year = (int)v.data.list->items[0].data.integer - 1900;  /* Convert to years since 1900 */
    tm.tm_mon = (int)v.data.list->items[1].data.integer - 1;      /* Convert to 0-11 */
    tm.tm_mday = (int)v.data.list->items[2].data.integer;
    tm.tm_hour = (int)v.data.list->items[3].data.integer;
    tm.tm_min = (int)v.data.list->items[4].data.integer;
    tm.tm_sec = (int)v.data.list->items[5].data.integer;
    /* items[6] is isdst (boolean or int) */
    if (v.data.list->items[6].type == JOY_BOOLEAN) {
        tm.tm_isdst = v.data.list->items[6].data.boolean ? 1 : 0;
    } else {
        tm.tm_isdst = (int)v.data.list->items[6].data.integer;
    }
    tm.tm_yday = (int)v.data.list->items[7].data.integer - 1;     /* Convert to 0-365 */
    tm.tm_wday = (int)v.data.list->items[8].data.integer;

    joy_value_free(&v);

    time_t result = mktime(&tm);
    PUSH(joy_integer((int64_t)result));
}

static void prim_strftime(JoyContext* ctx) {
    /* T S1 -> S2 : format time struct with format string */
    REQUIRE(2, "strftime");
    JoyValue fmt = POP();
    JoyValue t = POP();

    if (fmt.type != JOY_STRING) {
        joy_value_free(&t);
        joy_value_free(&fmt);
        joy_error_type("strftime", "STRING", fmt.type);
    }
    if (t.type != JOY_LIST) {
        joy_value_free(&t);
        joy_value_free(&fmt);
        joy_error_type("strftime", "LIST", t.type);
    }
    if (t.data.list->length < 9) {
        joy_value_free(&t);
        joy_value_free(&fmt);
        fprintf(stderr, "Error: strftime requires time struct with 9 elements\n");
        exit(1);
    }

    /* Input format: [year mon day hour min sec isdst yday wday] (Python format) */
    struct tm tm = {0};
    tm.tm_year = (int)t.data.list->items[0].data.integer - 1900;  /* Convert to years since 1900 */
    tm.tm_mon = (int)t.data.list->items[1].data.integer - 1;      /* Convert to 0-11 */
    tm.tm_mday = (int)t.data.list->items[2].data.integer;
    tm.tm_hour = (int)t.data.list->items[3].data.integer;
    tm.tm_min = (int)t.data.list->items[4].data.integer;
    tm.tm_sec = (int)t.data.list->items[5].data.integer;
    /* items[6] is isdst (boolean or int) */
    if (t.data.list->items[6].type == JOY_BOOLEAN) {
        tm.tm_isdst = t.data.list->items[6].data.boolean ? 1 : 0;
    } else {
        tm.tm_isdst = (int)t.data.list->items[6].data.integer;
    }
    tm.tm_yday = (int)t.data.list->items[7].data.integer - 1;     /* Convert to 0-365 */
    tm.tm_wday = (int)t.data.list->items[8].data.integer;

    char buffer[256];
    size_t len = strftime(buffer, sizeof(buffer), fmt.data.string, &tm);

    joy_value_free(&t);
    joy_value_free(&fmt);

    if (len == 0) {
        PUSH(joy_string(""));
    } else {
        PUSH(joy_string(buffer));
    }
}

static void prim_format(JoyContext* ctx) {
    /* N C I J -> S : format integer N with char C, width I, precision J */
    REQUIRE(4, "format");
    JoyValue j = POP();  /* precision */
    JoyValue i = POP();  /* width */
    JoyValue c = POP();  /* format char */
    JoyValue n = POP();  /* integer to format */

    EXPECT_TYPE(j, JOY_INTEGER, "format");
    EXPECT_TYPE(i, JOY_INTEGER, "format");
    EXPECT_TYPE(c, JOY_CHAR, "format");
    EXPECT_TYPE(n, JOY_INTEGER, "format");

    char fmt[32];
    char buffer[256];
    int width = (int)i.data.integer;
    int precision = (int)j.data.integer;
    char spec = c.data.character;

    /* Build format string like "%-10.5d" */
    snprintf(fmt, sizeof(fmt), "%%%d.%d%c", width, precision, spec);

    /* Format the integer */
    snprintf(buffer, sizeof(buffer), fmt, (long)n.data.integer);

    joy_value_free(&n);
    joy_value_free(&c);
    joy_value_free(&i);
    joy_value_free(&j);

    PUSH(joy_string(buffer));
}

static void prim_formatf(JoyContext* ctx) {
    /* F C I J -> S : format float F with char C, width I, precision J */
    REQUIRE(4, "formatf");
    JoyValue j = POP();  /* precision */
    JoyValue i = POP();  /* width */
    JoyValue c = POP();  /* format char */
    JoyValue f = POP();  /* float to format */

    EXPECT_TYPE(j, JOY_INTEGER, "formatf");
    EXPECT_TYPE(i, JOY_INTEGER, "formatf");
    EXPECT_TYPE(c, JOY_CHAR, "formatf");

    double val = 0.0;
    if (f.type == JOY_FLOAT) {
        val = f.data.floating;
    } else if (f.type == JOY_INTEGER) {
        val = (double)f.data.integer;
    } else {
        joy_value_free(&f);
        joy_value_free(&c);
        joy_value_free(&i);
        joy_value_free(&j);
        joy_error_type("formatf", "FLOAT or INTEGER", f.type);
    }

    char fmt[32];
    char buffer[256];
    int width = (int)i.data.integer;
    int precision = (int)j.data.integer;
    char spec = c.data.character;

    /* Build format string like "%-10.5f" */
    snprintf(fmt, sizeof(fmt), "%%%d.%d%c", width, precision, spec);

    /* Format the float */
    snprintf(buffer, sizeof(buffer), fmt, val);

    joy_value_free(&f);
    joy_value_free(&c);
    joy_value_free(&i);
    joy_value_free(&j);

    PUSH(joy_string(buffer));
}

static void prim_opcase(JoyContext* ctx) {
    /* X [..[X Xs]..] -> [Xs] : case with quotation result */
    REQUIRE(2, "opcase");
    JoyValue cases = POP();
    JoyValue x = POP();

    if (cases.type != JOY_LIST && cases.type != JOY_QUOTATION) {
        joy_value_free(&x);
        joy_value_free(&cases);
        joy_error_type("opcase", "LIST or QUOTATION", cases.type);
    }

    size_t n = cases.type == JOY_LIST ? cases.data.list->length : cases.data.quotation->length;
    JoyValue* items = cases.type == JOY_LIST ? cases.data.list->items : cases.data.quotation->terms;

    /* Search for matching case */
    for (size_t i = 0; i < n; i++) {
        JoyValue c = items[i];
        if (c.type != JOY_LIST && c.type != JOY_QUOTATION) continue;

        size_t clen = c.type == JOY_LIST ? c.data.list->length : c.data.quotation->length;
        JoyValue* citems = c.type == JOY_LIST ? c.data.list->items : c.data.quotation->terms;

        if (clen == 0) continue;

        /* Check if first element matches x */
        if (joy_value_equal(x, citems[0])) {
            /* Return rest of case as quotation */
            JoyQuotation* result = joy_quotation_new(clen - 1);
            for (size_t j = 1; j < clen; j++) {
                joy_quotation_push(result, joy_value_copy(citems[j]));
            }
            joy_value_free(&x);
            joy_value_free(&cases);
            JoyValue rv = {.type = JOY_QUOTATION, .data.quotation = result};
            PUSH(rv);
            return;
        }
    }

    /* No match found - return last case (default) or empty */
    if (n > 0) {
        JoyValue last = items[n - 1];
        if (last.type == JOY_LIST || last.type == JOY_QUOTATION) {
            size_t llen = last.type == JOY_LIST ? last.data.list->length : last.data.quotation->length;
            JoyValue* litems = last.type == JOY_LIST ? last.data.list->items : last.data.quotation->terms;
            JoyQuotation* result = joy_quotation_new(llen > 0 ? llen - 1 : 0);
            for (size_t j = 1; j < llen; j++) {
                joy_quotation_push(result, joy_value_copy(litems[j]));
            }
            joy_value_free(&x);
            joy_value_free(&cases);
            JoyValue rv = {.type = JOY_QUOTATION, .data.quotation = result};
            PUSH(rv);
            return;
        }
    }

    /* Empty result */
    joy_value_free(&x);
    joy_value_free(&cases);
    JoyValue rv = {.type = JOY_QUOTATION, .data.quotation = joy_quotation_new(0)};
    PUSH(rv);
}

static void prim_case(JoyContext* ctx) {
    /* X [..[X Y]..] -> Y i : case with immediate execution */
    REQUIRE(2, "case");
    JoyValue cases = POP();
    JoyValue x = POP();

    if (cases.type != JOY_LIST && cases.type != JOY_QUOTATION) {
        joy_value_free(&x);
        joy_value_free(&cases);
        joy_error_type("case", "LIST or QUOTATION", cases.type);
    }

    size_t n = cases.type == JOY_LIST ? cases.data.list->length : cases.data.quotation->length;
    JoyValue* items = cases.type == JOY_LIST ? cases.data.list->items : cases.data.quotation->terms;

    /* Search for matching case */
    for (size_t i = 0; i < n; i++) {
        JoyValue c = items[i];
        if (c.type != JOY_LIST && c.type != JOY_QUOTATION) continue;

        size_t clen = c.type == JOY_LIST ? c.data.list->length : c.data.quotation->length;
        JoyValue* citems = c.type == JOY_LIST ? c.data.list->items : c.data.quotation->terms;

        if (clen < 2) continue;

        /* Check if first element matches x */
        if (joy_value_equal(x, citems[0])) {
            /* Get the body (rest of case) and execute it */
            JoyQuotation* body = joy_quotation_new(clen - 1);
            for (size_t j = 1; j < clen; j++) {
                joy_quotation_push(body, joy_value_copy(citems[j]));
            }
            joy_value_free(&x);
            joy_value_free(&cases);
            JoyValue quot = {.type = JOY_QUOTATION, .data.quotation = body};
            execute_quot(ctx, &quot);
            joy_value_free(&quot);
            return;
        }
    }

    /* No match - execute last case (default) */
    if (n > 0) {
        JoyValue last = items[n - 1];
        if (last.type == JOY_LIST || last.type == JOY_QUOTATION) {
            size_t llen = last.type == JOY_LIST ? last.data.list->length : last.data.quotation->length;
            JoyValue* litems = last.type == JOY_LIST ? last.data.list->items : last.data.quotation->terms;
            if (llen > 1) {
                JoyQuotation* body = joy_quotation_new(llen - 1);
                for (size_t j = 1; j < llen; j++) {
                    joy_quotation_push(body, joy_value_copy(litems[j]));
                }
                joy_value_free(&x);
                joy_value_free(&cases);
                JoyValue quot = {.type = JOY_QUOTATION, .data.quotation = body};
                execute_quot(ctx, &quot);
                joy_value_free(&quot);
                return;
            }
        }
    }

    joy_value_free(&x);
    joy_value_free(&cases);
}

static void prim_frexp(JoyContext* ctx) {
    /* F -> G I : split float into mantissa G and exponent I */
    REQUIRE(1, "frexp");
    JoyValue v = POP();
    double x = (v.type == JOY_INTEGER) ? (double)v.data.integer : v.data.floating;
    joy_value_free(&v);
    int exp;
    double mantissa = frexp(x, &exp);
    PUSH(joy_float(mantissa));
    PUSH(joy_integer(exp));
}

static void prim_ldexp(JoyContext* ctx) {
    /* F I -> G : multiply F by 2^I */
    REQUIRE(2, "ldexp");
    JoyValue vexp = POP();
    JoyValue vf = POP();
    EXPECT_TYPE(vexp, JOY_INTEGER, "ldexp");
    double f = (vf.type == JOY_INTEGER) ? (double)vf.data.integer : vf.data.floating;
    joy_value_free(&vf);
    joy_value_free(&vexp);
    PUSH(joy_float(ldexp(f, (int)vexp.data.integer)));
}

static void prim_modf(JoyContext* ctx) {
    /* F -> G H : split F into integer part G and fractional part H */
    REQUIRE(1, "modf");
    JoyValue v = POP();
    double x = (v.type == JOY_INTEGER) ? (double)v.data.integer : v.data.floating;
    joy_value_free(&v);
    double intpart;
    double fracpart = modf(x, &intpart);
    PUSH(joy_float(fracpart));
    PUSH(joy_float(intpart));
}

/* ---------- Additional Aggregate Operations ---------- */

static void prim_unswons(JoyContext* ctx) {
    /* A -> R F : rest and first of aggregate (opposite order of uncons) */
    REQUIRE(1, "unswons");
    JoyValue v = POP();

    switch (v.type) {
        case JOY_LIST: {
            if (v.data.list->length == 0) joy_error("unswons of empty list");
            JoyValue first = joy_value_copy(v.data.list->items[0]);
            JoyList* rest = joy_list_rest(v.data.list);
            joy_value_free(&v);
            JoyValue rv = {.type = JOY_LIST, .data.list = rest};
            PUSH(rv);
            PUSH(first);
            break;
        }
        case JOY_QUOTATION: {
            if (v.data.quotation->length == 0) joy_error("unswons of empty quotation");
            JoyValue first = joy_value_copy(v.data.quotation->terms[0]);
            JoyQuotation* rest = joy_quotation_new(v.data.quotation->length);
            for (size_t i = 1; i < v.data.quotation->length; i++) {
                joy_quotation_push(rest, joy_value_copy(v.data.quotation->terms[i]));
            }
            joy_value_free(&v);
            JoyValue rv = {.type = JOY_QUOTATION, .data.quotation = rest};
            PUSH(rv);
            PUSH(first);
            break;
        }
        case JOY_STRING: {
            if (v.data.string[0] == '\0') joy_error("unswons of empty string");
            char first = v.data.string[0];
            char* rest = strdup(v.data.string + 1);
            joy_value_free(&v);
            PUSH(joy_string_owned(rest));
            PUSH(joy_char(first));
            break;
        }
        case JOY_SET: {
            uint64_t set = v.data.set;
            if (set == 0) joy_error("unswons of empty set");
            int first = -1;
            for (int i = 0; i < 64; i++) {
                if (set & (1ULL << i)) {
                    first = i;
                    break;
                }
            }
            uint64_t rest = set & ~(1ULL << first);
            joy_value_free(&v);
            JoyValue rv = {.type = JOY_SET, .data.set = rest};
            PUSH(rv);
            PUSH(joy_integer(first));
            break;
        }
        default:
            joy_value_free(&v);
            joy_error_type("unswons", "aggregate", v.type);
    }
}

static void prim_of(JoyContext* ctx) {
    /* I A -> X : get element at index I from aggregate A (reverse of at) */
    REQUIRE(2, "of");
    JoyValue agg = POP();
    JoyValue idx = POP();
    EXPECT_TYPE(idx, JOY_INTEGER, "of");
    int64_t i = idx.data.integer;
    joy_value_free(&idx);

    if (i < 0) {
        joy_value_free(&agg);
        joy_error("of: negative index");
    }

    switch (agg.type) {
        case JOY_LIST:
            if ((size_t)i >= agg.data.list->length) {
                joy_value_free(&agg);
                joy_error("of: index out of bounds");
            }
            PUSH(joy_value_copy(agg.data.list->items[i]));
            break;
        case JOY_QUOTATION:
            if ((size_t)i >= agg.data.quotation->length) {
                joy_value_free(&agg);
                joy_error("of: index out of bounds");
            }
            PUSH(joy_value_copy(agg.data.quotation->terms[i]));
            break;
        case JOY_STRING:
            if ((size_t)i >= strlen(agg.data.string)) {
                joy_value_free(&agg);
                joy_error("of: index out of bounds");
            }
            PUSH(joy_char(agg.data.string[i]));
            break;
        case JOY_SET: {
            uint64_t set = agg.data.set;
            int count = 0;
            for (int j = 0; j < 64; j++) {
                if (set & (1ULL << j)) {
                    if (count == i) {
                        joy_value_free(&agg);
                        PUSH(joy_integer(j));
                        return;
                    }
                    count++;
                }
            }
            joy_value_free(&agg);
            joy_error("of: index out of bounds");
            break;
        }
        default:
            joy_value_free(&agg);
            joy_error_type("of", "aggregate", agg.type);
    }
    joy_value_free(&agg);
}

/* Helper for compare - Joy compare semantics:
 * - Non-empty lists/quotations: always return 1 (not comparable)
 * - Strings: lexicographic comparison
 * - Symbols: 0 if same, 1 if different
 * - Numeric types (int, float, char, bool, set): compare by value
 * - Files: 0 if same, compare by pointer otherwise
 * - Incompatible types: 1
 */
static int joy_compare_values(JoyValue a, JoyValue b) {
    /* Non-empty lists/quotations are not comparable - always 1 */
    if (a.type == JOY_LIST && a.data.list->length > 0) return 1;
    if (a.type == JOY_QUOTATION && a.data.quotation->length > 0) return 1;
    if (b.type == JOY_LIST && b.data.list->length > 0) return 1;
    if (b.type == JOY_QUOTATION && b.data.quotation->length > 0) return 1;

    /* String comparison (lexicographic) */
    if (a.type == JOY_STRING && b.type == JOY_STRING) {
        int cmp = strcmp(a.data.string, b.data.string);
        if (cmp < 0) return -1;
        if (cmp > 0) return 1;
        return 0;
    }

    /* Symbol comparison */
    if (a.type == JOY_SYMBOL && b.type == JOY_SYMBOL) {
        return strcmp(a.data.symbol, b.data.symbol) == 0 ? 0 : 1;
    }

    /* File comparison */
    if (a.type == JOY_FILE && b.type == JOY_FILE) {
        if (a.data.file == b.data.file) return 0;
        return (uintptr_t)a.data.file < (uintptr_t)b.data.file ? -1 : 1;
    }

    /* Try numeric comparison */
    double av, bv;
    if (joy_numeric_value(a, &av) && joy_numeric_value(b, &bv)) {
        if (av < bv) return -1;
        if (av > bv) return 1;
        return 0;
    }

    /* Incompatible types */
    return 1;
}

static void prim_compare(JoyContext* ctx) {
    /* A B -> I : compare A and B, return -1, 0, or 1 */
    REQUIRE(2, "compare");
    JoyValue b = POP();
    JoyValue a = POP();
    int result = joy_compare_values(a, b);
    joy_value_free(&a);
    joy_value_free(&b);
    PUSH(joy_integer(result));
}

/* Helper to get aggregate length (works for LIST and QUOTATION) */
static size_t joy_aggregate_length(JoyValue v) {
    if (v.type == JOY_LIST) return v.data.list->length;
    if (v.type == JOY_QUOTATION) return v.data.quotation->length;
    return 0;
}

/* Helper to get aggregate item (works for LIST and QUOTATION) */
static JoyValue joy_aggregate_item(JoyValue v, size_t i) {
    if (v.type == JOY_LIST) return v.data.list->items[i];
    if (v.type == JOY_QUOTATION) return v.data.quotation->terms[i];
    return joy_integer(0);
}

/* Helper for equal - recursive structural equality (unlike = which is shallow) */
static bool joy_equal_values(JoyValue a, JoyValue b) {
    /* Same type - direct comparison */
    if (a.type == b.type) {
        switch (a.type) {
            case JOY_INTEGER:
                return a.data.integer == b.data.integer;
            case JOY_FLOAT:
                return a.data.floating == b.data.floating;
            case JOY_BOOLEAN:
                return a.data.boolean == b.data.boolean;
            case JOY_CHAR:
                return a.data.character == b.data.character;
            case JOY_STRING:
                return strcmp(a.data.string, b.data.string) == 0;
            case JOY_SET:
                return a.data.set == b.data.set;
            case JOY_SYMBOL:
                return strcmp(a.data.symbol, b.data.symbol) == 0;
            case JOY_LIST:
                if (a.data.list->length != b.data.list->length) return false;
                for (size_t i = 0; i < a.data.list->length; i++) {
                    if (!joy_equal_values(a.data.list->items[i],
                                          b.data.list->items[i])) {
                        return false;
                    }
                }
                return true;
            case JOY_QUOTATION:
                if (a.data.quotation->length != b.data.quotation->length) return false;
                for (size_t i = 0; i < a.data.quotation->length; i++) {
                    if (!joy_equal_values(a.data.quotation->terms[i],
                                          b.data.quotation->terms[i])) {
                        return false;
                    }
                }
                return true;
            default:
                return false;
        }
    }

    /* LIST and QUOTATION can be compared by content */
    if ((a.type == JOY_LIST && b.type == JOY_QUOTATION) ||
        (a.type == JOY_QUOTATION && b.type == JOY_LIST)) {
        size_t len_a = joy_aggregate_length(a);
        size_t len_b = joy_aggregate_length(b);
        if (len_a != len_b) return false;
        for (size_t i = 0; i < len_a; i++) {
            if (!joy_equal_values(joy_aggregate_item(a, i),
                                  joy_aggregate_item(b, i))) {
                return false;
            }
        }
        return true;
    }

    /* Symbol comparison */
    if (a.type == JOY_SYMBOL && b.type == JOY_SYMBOL) {
        return strcmp(a.data.symbol, b.data.symbol) == 0;
    }

    /* Symbol-String comparison: symbol "foo" equals string "foo" */
    if (a.type == JOY_SYMBOL && b.type == JOY_STRING) {
        return strcmp(a.data.symbol, b.data.string) == 0;
    }
    if (a.type == JOY_STRING && b.type == JOY_SYMBOL) {
        return strcmp(a.data.string, b.data.symbol) == 0;
    }

    /* Numeric types can be compared across types using joy_numeric_value */
    double val_a, val_b;
    if (joy_numeric_value(a, &val_a) && joy_numeric_value(b, &val_b)) {
        return val_a == val_b;
    }

    return false;
}

static void prim_equal(JoyContext* ctx) {
    /* T U -> B : test if T and U are structurally equal */
    REQUIRE(2, "equal");
    JoyValue b = POP();
    JoyValue a = POP();
    bool result = joy_equal_values(a, b);
    joy_value_free(&a);
    joy_value_free(&b);
    PUSH(joy_boolean(result));
}

static void prim_in(JoyContext* ctx) {
    /* X A -> B : test if X is a member of aggregate A */
    REQUIRE(2, "in");
    JoyValue agg = POP();
    JoyValue x = POP();
    bool found = false;

    switch (agg.type) {
        case JOY_LIST:
            for (size_t i = 0; i < agg.data.list->length; i++) {
                if (joy_equal_values(x, agg.data.list->items[i])) {
                    found = true;
                    break;
                }
            }
            break;
        case JOY_QUOTATION:
            for (size_t i = 0; i < agg.data.quotation->length; i++) {
                if (joy_equal_values(x, agg.data.quotation->terms[i])) {
                    found = true;
                    break;
                }
            }
            break;
        case JOY_STRING:
            if (x.type == JOY_CHAR) {
                found = strchr(agg.data.string, x.data.character) != NULL;
            } else if (x.type == JOY_STRING) {
                found = strstr(agg.data.string, x.data.string) != NULL;
            }
            break;
        case JOY_SET:
            if (x.type == JOY_INTEGER) {
                int64_t n = x.data.integer;
                if (n >= 0 && n < 64) {
                    found = (agg.data.set & (1ULL << n)) != 0;
                }
            }
            break;
        default:
            joy_value_free(&x);
            joy_value_free(&agg);
            joy_error_type("in", "aggregate", agg.type);
    }

    joy_value_free(&x);
    joy_value_free(&agg);
    PUSH(joy_boolean(found));
}

static void prim_name(JoyContext* ctx) {
    /* sym -> "sym" : convert symbol to its name string, or type name for non-symbols */
    REQUIRE(1, "name");
    JoyValue v = POP();

    switch (v.type) {
        case JOY_SYMBOL: {
            char* s = strdup(v.data.symbol);
            joy_value_free(&v);
            PUSH(joy_string_owned(s));
            break;
        }
        case JOY_INTEGER:
            joy_value_free(&v);
            PUSH(joy_string(" integer type"));
            break;
        case JOY_FLOAT:
            joy_value_free(&v);
            PUSH(joy_string(" float type"));
            break;
        case JOY_BOOLEAN:
            joy_value_free(&v);
            PUSH(joy_string(" truth value type"));
            break;
        case JOY_CHAR:
            joy_value_free(&v);
            PUSH(joy_string(" character type"));
            break;
        case JOY_STRING:
            joy_value_free(&v);
            PUSH(joy_string(" string type"));
            break;
        case JOY_LIST:
            joy_value_free(&v);
            PUSH(joy_string(" list type"));
            break;
        case JOY_SET:
            joy_value_free(&v);
            PUSH(joy_string(" set type"));
            break;
        case JOY_QUOTATION:
            joy_value_free(&v);
            PUSH(joy_string(" list type"));
            break;
        case JOY_FILE:
            joy_value_free(&v);
            PUSH(joy_string(" file type"));
            break;
        default:
            joy_value_free(&v);
            PUSH(joy_string(" unknown type"));
    }
}

static void prim_intern(JoyContext* ctx) {
    /* "sym" -> sym : convert string to symbol */
    REQUIRE(1, "intern");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_STRING, "intern");
    PUSH(joy_symbol(v.data.string));
    joy_value_free(&v);
}

static void prim_body(JoyContext* ctx) {
    /* U -> [P] : get body of user-defined symbol */
    REQUIRE(1, "body");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_SYMBOL, "body");

    /* Look up the symbol in the dictionary */
    JoyWord* word = joy_dict_lookup(ctx->dictionary, v.data.symbol);
    if (!word) {
        /* Undefined symbol - return empty quotation (null body) */
        joy_value_free(&v);
        JoyQuotation* q = joy_quotation_new(0);
        JoyValue result = {.type = JOY_QUOTATION, .data.quotation = q};
        PUSH(result);
        return;
    }

    if (word->is_primitive) {
        /* Primitives have no body - return empty quotation */
        joy_value_free(&v);
        JoyQuotation* q = joy_quotation_new(0);
        JoyValue result = {.type = JOY_QUOTATION, .data.quotation = q};
        PUSH(result);
    } else {
        /* User-defined word - copy its quotation body */
        JoyQuotation* body_copy = joy_quotation_copy(word->body.quotation);
        joy_value_free(&v);
        JoyValue result = {.type = JOY_QUOTATION, .data.quotation = body_copy};
        PUSH(result);
    }
}

static void prim_assign(JoyContext* ctx) {
    /* X [name] assign -> : bind value X to symbol name */
    REQUIRE(2, "assign");
    JoyValue name_list = POP();
    JoyValue value = POP();

    /* name_list should be a list or quotation with a single symbol */
    const char* name = NULL;
    if (name_list.type == JOY_LIST && name_list.data.list->length == 1) {
        JoyValue first = name_list.data.list->items[0];
        if (first.type == JOY_SYMBOL) {
            name = first.data.symbol;
        }
    } else if (name_list.type == JOY_QUOTATION && name_list.data.quotation->length == 1) {
        JoyValue first = name_list.data.quotation->terms[0];
        if (first.type == JOY_SYMBOL) {
            name = first.data.symbol;
        }
    }

    if (!name) {
        joy_value_free(&name_list);
        joy_value_free(&value);
        joy_error("assign: expected [symbol]");
    }

    /* Define the word */
    joy_dict_define_value(ctx->dictionary, name, value);

    joy_value_free(&name_list);
    joy_value_free(&value);
}

static void prim_unassign(JoyContext* ctx) {
    /* [name] unassign -> : remove binding for symbol name */
    REQUIRE(1, "unassign");
    JoyValue name_list = POP();

    /* name_list should be a list or quotation with a single symbol */
    const char* name = NULL;
    if (name_list.type == JOY_LIST && name_list.data.list->length == 1) {
        JoyValue first = name_list.data.list->items[0];
        if (first.type == JOY_SYMBOL) {
            name = first.data.symbol;
        }
    } else if (name_list.type == JOY_QUOTATION && name_list.data.quotation->length == 1) {
        JoyValue first = name_list.data.quotation->terms[0];
        if (first.type == JOY_SYMBOL) {
            name = first.data.symbol;
        }
    }

    if (!name) {
        joy_value_free(&name_list);
        joy_error("unassign: expected [symbol]");
    }

    /* Remove the word from dictionary */
    joy_dict_remove(ctx->dictionary, name);

    joy_value_free(&name_list);
}

/* ---------- Type Casting ---------- */

static void prim_casting(JoyContext* ctx) {
    /* X T -> Y : cast value X to type T
     * Joy42 type codes (matching typeof):
     * 4 = BOOLEAN, 5 = CHAR, 6 = INTEGER, 7 = SET,
     * 8 = STRING, 9 = LIST, 10 = FLOAT, 11 = FILE
     */
    REQUIRE(2, "casting");
    JoyValue t = POP();
    JoyValue x = POP();
    EXPECT_TYPE(t, JOY_INTEGER, "casting");

    int64_t target_type = t.data.integer;
    joy_value_free(&t);

    switch (target_type) {
        case 4: { /* BOOLEAN */
            bool result = false;
            switch (x.type) {
                case JOY_BOOLEAN: result = x.data.boolean; break;
                case JOY_INTEGER: result = x.data.integer != 0; break;
                case JOY_FLOAT: result = x.data.floating != 0.0; break;
                case JOY_CHAR: result = x.data.character != 0; break;
                case JOY_STRING: result = x.data.string && strlen(x.data.string) > 0; break;
                case JOY_SET: result = x.data.set != 0; break;
                case JOY_LIST:
                case JOY_QUOTATION: result = x.data.list && x.data.list->length > 0; break;
                case JOY_FILE: result = x.data.file != NULL; break;
                default: break;
            }
            joy_value_free(&x);
            PUSH(joy_boolean(result));
            break;
        }
        case 5: { /* CHAR */
            char ch = '\0';
            if (x.type == JOY_CHAR) {
                ch = x.data.character;
            } else if (x.type == JOY_INTEGER) {
                ch = (char)(x.data.integer & 0xFF);
            } else if (x.type == JOY_STRING && x.data.string && strlen(x.data.string) > 0) {
                ch = x.data.string[0];
            }
            joy_value_free(&x);
            PUSH(joy_char(ch));
            break;
        }
        case 6: { /* INTEGER */
            int64_t val = 0;
            switch (x.type) {
                case JOY_INTEGER: val = x.data.integer; break;
                case JOY_CHAR: val = (unsigned char)x.data.character; break;
                case JOY_FLOAT: val = (int64_t)x.data.floating; break;
                case JOY_BOOLEAN: val = x.data.boolean ? 1 : 0; break;
                case JOY_SET: val = (int64_t)x.data.set; break;
                default: break;
            }
            joy_value_free(&x);
            PUSH(joy_integer(val));
            break;
        }
        case 7: { /* SET */
            uint64_t set_val = 0;
            if (x.type == JOY_SET) {
                set_val = x.data.set;
            } else if (x.type == JOY_INTEGER) {
                set_val = (uint64_t)x.data.integer;
            }
            joy_value_free(&x);
            JoyValue set_result;
            set_result.type = JOY_SET;
            set_result.data.set = set_val;
            PUSH(set_result);
            break;
        }
        case 8: { /* STRING */
            char buf[256];
            char* result = NULL;
            if (x.type == JOY_STRING) {
                PUSH(x);  /* Already a string, just push back */
                return;
            } else if (x.type == JOY_CHAR) {
                buf[0] = x.data.character;
                buf[1] = '\0';
                result = strdup(buf);
            } else if (x.type == JOY_INTEGER) {
                snprintf(buf, sizeof(buf), "%" PRId64, x.data.integer);
                result = strdup(buf);
            } else if (x.type == JOY_FLOAT) {
                snprintf(buf, sizeof(buf), "%g", x.data.floating);
                result = strdup(buf);
            } else {
                result = strdup("");
            }
            joy_value_free(&x);
            PUSH(joy_string_owned(result));
            break;
        }
        case 9: { /* LIST */
            if (x.type == JOY_LIST || x.type == JOY_QUOTATION) {
                PUSH(x);  /* Already a list/quotation */
                return;
            } else if (x.type == JOY_STRING) {
                /* Convert string to list of chars */
                size_t len = x.data.string ? strlen(x.data.string) : 0;
                JoyList* list = joy_list_new(len);
                for (size_t i = 0; i < len; i++) {
                    joy_list_push(list, joy_char(x.data.string[i]));
                }
                joy_value_free(&x);
                JoyValue list_result;
                list_result.type = JOY_LIST;
                list_result.data.list = list;
                PUSH(list_result);
            } else {
                joy_value_free(&x);
                JoyValue list_result;
                list_result.type = JOY_LIST;
                list_result.data.list = joy_list_new(0);
                PUSH(list_result);
            }
            break;
        }
        case 10: { /* FLOAT */
            double val = 0.0;
            if (x.type == JOY_FLOAT) {
                PUSH(x);  /* Already a float */
                return;
            } else if (x.type == JOY_INTEGER) {
                /* Bit-level reinterpretation: treat integer bits as IEEE 754 double */
                union { uint64_t i; double d; } u;
                u.i = (uint64_t)x.data.integer;
                val = u.d;
            } else if (x.type == JOY_CHAR) {
                val = (double)(unsigned char)x.data.character;
            } else if (x.type == JOY_BOOLEAN) {
                val = x.data.boolean ? 1.0 : 0.0;
            }
            joy_value_free(&x);
            PUSH(joy_float(val));
            break;
        }
        case 11: { /* FILE */
            joy_value_free(&x);
            JoyValue file_result;
            file_result.type = JOY_FILE;
            file_result.data.file = NULL;
            PUSH(file_result);
            break;
        }
        default:
            /* Unknown type code - just push back the original value */
            PUSH(x);
            break;
    }
}

/* ---------- System Interaction ---------- */

static void prim_system(JoyContext* ctx) {
    /* "command" -> : execute shell command */
    REQUIRE(1, "system");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_STRING, "system");
    int result = system(v.data.string);
    joy_value_free(&v);
    PUSH(joy_integer(result));
}

static void prim_getenv(JoyContext* ctx) {
    /* "variable" -> "value" : get environment variable */
    REQUIRE(1, "getenv");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_STRING, "getenv");
    char* value = getenv(v.data.string);
    joy_value_free(&v);
    if (value) {
        PUSH(joy_string(value));
    } else {
        PUSH(joy_string(""));
    }
}

static void prim_argc(JoyContext* ctx) {
    /* -> I : push argument count */
    PUSH(joy_integer(joy_argc));
}

static void prim_argv(JoyContext* ctx) {
    /* -> A : push command line arguments as list */
    JoyList* list = joy_list_new(joy_argc > 0 ? (size_t)joy_argc : 1);
    for (int i = 0; i < joy_argc; i++) {
        joy_list_push(list, joy_string(joy_argv[i]));
    }
    JoyValue v = {.type = JOY_LIST, .data.list = list};
    PUSH(v);
}

/* ---------- Interpreter Control ---------- */

static void prim_abort(JoyContext* ctx) {
    /* -> : abort execution with error status */
    (void)ctx;  /* unused */
    exit(1);
}

static void prim_quit(JoyContext* ctx) {
    /* -> : quit interpreter with success status */
    (void)ctx;  /* unused */
    exit(0);
}

static void prim_gc(JoyContext* ctx) {
    /* -> : force garbage collection (no-op in compiled code) */
    (void)ctx;  /* no GC in compiled C code - memory is managed manually */
}

static void prim_setautoput(JoyContext* ctx) {
    /* I -> : set autoput flag (0=off, 1=on) */
    REQUIRE(1, "setautoput");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_INTEGER, "setautoput");
    ctx->autoput = (int)v.data.integer;
    joy_value_free(&v);
}

static void prim_setundeferror(JoyContext* ctx) {
    /* I -> : set undeferror flag (0=off, 1=on) */
    REQUIRE(1, "setundeferror");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_INTEGER, "setundeferror");
    ctx->undeferror = (int)v.data.integer;
    joy_value_free(&v);
}

static void prim_autoput(JoyContext* ctx) {
    /* -> I : push autoput flag value */
    PUSH(joy_integer(ctx->autoput));
}

static void prim_undeferror(JoyContext* ctx) {
    /* -> I : push undeferror flag value */
    PUSH(joy_integer(ctx->undeferror));
}

static void prim_echo(JoyContext* ctx) {
    /* -> I : push echo flag value (0..3) */
    PUSH(joy_integer(ctx->echo));
}

static void prim_conts(JoyContext* ctx) {
    /* -> [[P] [Q] ..] : push continuation stack (empty in compiled code) */
    /* In compiled code, there's no continuation stack - execution is direct */
    JoyValue empty = {.type = JOY_LIST, .data.list = joy_list_new(0)};
    PUSH(empty);
}

static void prim_undefs(JoyContext* ctx) {
    /* -> [S1 S2 ..] : push list of undefined symbols (empty in compiled code) */
    /* In compiled code, all symbols are resolved at compile time */
    (void)ctx;
    JoyValue empty = {.type = JOY_LIST, .data.list = joy_list_new(0)};
    PUSH(empty);
}

static void prim_help(JoyContext* ctx) {
    /* -> : list defined symbols and primitives */
    (void)ctx;
    printf("Joy - compiled program\n");
    printf("Use 'manual' for full documentation.\n");
    printf("Help system has limited functionality in compiled code.\n");
}

static void prim_helpdetail(JoyContext* ctx) {
    /* [S1 S2 ..] -> : give brief help on symbols */
    REQUIRE(1, "helpdetail");
    JoyValue symbols = POP();
    printf("helpdetail: limited functionality in compiled code\n");
    joy_value_free(&symbols);
}

static void prim_manual(JoyContext* ctx) {
    /* -> : print manual of all primitives */
    (void)ctx;
    printf("Joy Language Manual\n");
    printf("===================\n\n");
    printf("This is a compiled Joy program.\n");
    printf("For full documentation, see the Joy language specification.\n");
    printf("\nCore primitives: dup pop swap + - * / < > = etc.\n");
    printf("Combinators: i x dip map fold linrec primrec etc.\n");
    printf("Aggregates: first rest cons size null etc.\n");
}

static void prim_get(JoyContext* ctx) {
    /* -> F : read factor from input (no-op in compiled code) */
    (void)ctx;
    /* No Joy parser available at runtime in compiled code.
     * Programs requiring get should use the Python interpreter. */
    fprintf(stderr, "Warning: 'get' is not supported in compiled code\n");
}

/* ---------- Registration ---------- */

void joy_register_primitives(JoyContext* ctx) {
    JoyDict* d = ctx->dictionary;

    /* Stack */
    joy_dict_define_primitive(d, "id", prim_id);
    joy_dict_define_primitive(d, "dup", prim_dup);
    joy_dict_define_primitive(d, "dup2", prim_dup2);
    joy_dict_define_primitive(d, "pop", prim_pop);
    joy_dict_define_primitive(d, "swap", prim_swap);
    joy_dict_define_primitive(d, "over", prim_over);
    joy_dict_define_primitive(d, "rollup", prim_rollup);
    joy_dict_define_primitive(d, "rolldown", prim_rolldown);
    joy_dict_define_primitive(d, "rotate", prim_rotate);
    joy_dict_define_primitive(d, "dupd", prim_dupd);
    joy_dict_define_primitive(d, "swapd", prim_swapd);
    joy_dict_define_primitive(d, "popd", prim_popd);
    joy_dict_define_primitive(d, "rollupd", prim_rollupd);
    joy_dict_define_primitive(d, "rolldownd", prim_rolldownd);
    joy_dict_define_primitive(d, "rotated", prim_rotated);
    joy_dict_define_primitive(d, "stack", prim_stack);
    joy_dict_define_primitive(d, "unstack", prim_unstack);
    joy_dict_define_primitive(d, "pick", prim_pick);

    /* Arithmetic */
    joy_dict_define_primitive(d, "+", prim_add);
    joy_dict_define_primitive(d, "-", prim_sub);
    joy_dict_define_primitive(d, "*", prim_mul);
    joy_dict_define_primitive(d, "/", prim_div);
    joy_dict_define_primitive(d, "rem", prim_rem);
    joy_dict_define_primitive(d, "succ", prim_succ);
    joy_dict_define_primitive(d, "pred", prim_pred);
    joy_dict_define_primitive(d, "abs", prim_abs);
    joy_dict_define_primitive(d, "neg", prim_neg);
    joy_dict_define_primitive(d, "sign", prim_sign);
    joy_dict_define_primitive(d, "max", prim_max);
    joy_dict_define_primitive(d, "min", prim_min);

    /* Math */
    joy_dict_define_primitive(d, "sin", prim_sin);
    joy_dict_define_primitive(d, "cos", prim_cos);
    joy_dict_define_primitive(d, "tan", prim_tan);
    joy_dict_define_primitive(d, "sqrt", prim_sqrt);
    joy_dict_define_primitive(d, "exp", prim_exp);
    joy_dict_define_primitive(d, "log", prim_log);
    joy_dict_define_primitive(d, "pow", prim_pow);
    joy_dict_define_primitive(d, "floor", prim_floor);
    joy_dict_define_primitive(d, "ceil", prim_ceil);
    joy_dict_define_primitive(d, "trunc", prim_trunc);
    joy_dict_define_primitive(d, "round", prim_round);

    /* Type introspection */
    joy_dict_define_primitive(d, "typeof", prim_typeof);
    joy_dict_define_primitive(d, "sametype", prim_sametype);

    /* Comparison */
    joy_dict_define_primitive(d, "=", prim_eq);
    joy_dict_define_primitive(d, "!=", prim_neq);
    joy_dict_define_primitive(d, "<", prim_lt);
    joy_dict_define_primitive(d, ">", prim_gt);
    joy_dict_define_primitive(d, "<=", prim_le);
    joy_dict_define_primitive(d, ">=", prim_ge);

    /* Logical */
    joy_dict_define_primitive(d, "and", prim_and);
    joy_dict_define_primitive(d, "or", prim_or);
    joy_dict_define_primitive(d, "not", prim_not);
    joy_dict_define_primitive(d, "xor", prim_xor);
    joy_dict_define_primitive(d, "choice", prim_choice);

    /* Aggregates */
    joy_dict_define_primitive(d, "first", prim_first);
    joy_dict_define_primitive(d, "rest", prim_rest);
    joy_dict_define_primitive(d, "cons", prim_cons);
    joy_dict_define_primitive(d, "swons", prim_swons);
    joy_dict_define_primitive(d, "uncons", prim_uncons);
    joy_dict_define_primitive(d, "concat", prim_concat);
    joy_dict_define_primitive(d, "swoncat", prim_swoncat);
    joy_dict_define_primitive(d, "size", prim_size);
    joy_dict_define_primitive(d, "at", prim_at);
    joy_dict_define_primitive(d, "drop", prim_drop);
    joy_dict_define_primitive(d, "take", prim_take);
    joy_dict_define_primitive(d, "null", prim_null);
    joy_dict_define_primitive(d, "small", prim_small);

    /* Combinators */
    joy_dict_define_primitive(d, "i", prim_i);
    joy_dict_define_primitive(d, "x", prim_x);
    joy_dict_define_primitive(d, "dip", prim_dip);
    joy_dict_define_primitive(d, "ifte", prim_ifte);
    joy_dict_define_primitive(d, "branch", prim_branch);
    joy_dict_define_primitive(d, "times", prim_times);
    joy_dict_define_primitive(d, "while", prim_while);
    joy_dict_define_primitive(d, "map", prim_map);
    joy_dict_define_primitive(d, "step", prim_step);
    joy_dict_define_primitive(d, "fold", prim_fold);
    joy_dict_define_primitive(d, "filter", prim_filter);

    /* Recursion combinators */
    joy_dict_define_primitive(d, "binrec", prim_binrec);
    joy_dict_define_primitive(d, "linrec", prim_linrec);
    joy_dict_define_primitive(d, "tailrec", prim_tailrec);
    joy_dict_define_primitive(d, "primrec", prim_primrec);
    joy_dict_define_primitive(d, "genrec", prim_genrec);

    /* I/O */
    joy_dict_define_primitive(d, "put", prim_put);
    joy_dict_define_primitive(d, "putch", prim_putch);
    joy_dict_define_primitive(d, "putchars", prim_putchars);
    joy_dict_define_primitive(d, ".", prim_dot);
    joy_dict_define_primitive(d, "newline", prim_newline);
    joy_dict_define_primitive(d, "putln", prim_putln);

    /* Debug commands (no-ops) */
    joy_dict_define_primitive(d, "setecho", prim_setecho);
    joy_dict_define_primitive(d, "__settracegc", prim_settracegc);

    /* Set operations */
    joy_dict_define_primitive(d, "has", prim_has);

    /* Advanced combinators */
    joy_dict_define_primitive(d, "cond", prim_cond);
    joy_dict_define_primitive(d, "infra", prim_infra);
    joy_dict_define_primitive(d, "condlinrec", prim_condlinrec);
    joy_dict_define_primitive(d, "condnestrec", prim_condnestrec);

    /* Tree combinators */
    joy_dict_define_primitive(d, "treestep", prim_treestep);
    joy_dict_define_primitive(d, "treerec", prim_treerec);
    joy_dict_define_primitive(d, "treegenrec", prim_treegenrec);

    /* Type predicates */
    joy_dict_define_primitive(d, "integer", prim_integer);
    joy_dict_define_primitive(d, "float", prim_float_p);
    joy_dict_define_primitive(d, "logical", prim_logical);
    joy_dict_define_primitive(d, "char", prim_char_p);
    joy_dict_define_primitive(d, "string", prim_string_p);
    joy_dict_define_primitive(d, "list", prim_list);
    joy_dict_define_primitive(d, "set", prim_set_p);
    joy_dict_define_primitive(d, "leaf", prim_leaf);
    joy_dict_define_primitive(d, "file", prim_file_p);
    joy_dict_define_primitive(d, "user", prim_user);

    /* Type conversion */
    joy_dict_define_primitive(d, "ord", prim_ord);
    joy_dict_define_primitive(d, "chr", prim_chr);

    /* Constants */
    joy_dict_define_primitive(d, "true", prim_true);
    joy_dict_define_primitive(d, "false", prim_false);
    joy_dict_define_primitive(d, "maxint", prim_maxint);
    joy_dict_define_primitive(d, "setsize", prim_setsize);

    /* Additional aggregate operations */
    joy_dict_define_primitive(d, "unswons", prim_unswons);
    joy_dict_define_primitive(d, "of", prim_of);
    joy_dict_define_primitive(d, "compare", prim_compare);
    joy_dict_define_primitive(d, "equal", prim_equal);
    joy_dict_define_primitive(d, "in", prim_in);
    joy_dict_define_primitive(d, "name", prim_name);
    joy_dict_define_primitive(d, "intern", prim_intern);
    joy_dict_define_primitive(d, "body", prim_body);
    joy_dict_define_primitive(d, "assign", prim_assign);
    joy_dict_define_primitive(d, "unassign", prim_unassign);
    joy_dict_define_primitive(d, "casting", prim_casting);

    /* File I/O */
    joy_dict_define_primitive(d, "stdin", prim_stdin);
    joy_dict_define_primitive(d, "stdout", prim_stdout);
    joy_dict_define_primitive(d, "stderr", prim_stderr);

    /* Additional math */
    joy_dict_define_primitive(d, "acos", prim_acos);
    joy_dict_define_primitive(d, "asin", prim_asin);
    joy_dict_define_primitive(d, "atan", prim_atan);
    joy_dict_define_primitive(d, "atan2", prim_atan2);
    joy_dict_define_primitive(d, "cosh", prim_cosh);
    joy_dict_define_primitive(d, "sinh", prim_sinh);
    joy_dict_define_primitive(d, "tanh", prim_tanh);
    joy_dict_define_primitive(d, "log10", prim_log10);

    /* String conversion */
    joy_dict_define_primitive(d, "strtol", prim_strtol);
    joy_dict_define_primitive(d, "strtod", prim_strtod);

    /* Time and random */
    joy_dict_define_primitive(d, "time", prim_time);
    joy_dict_define_primitive(d, "clock", prim_clock);
    joy_dict_define_primitive(d, "rand", prim_rand);
    joy_dict_define_primitive(d, "srand", prim_srand);
    joy_dict_define_primitive(d, "localtime", prim_localtime);
    joy_dict_define_primitive(d, "gmtime", prim_gmtime);
    joy_dict_define_primitive(d, "mktime", prim_mktime);
    joy_dict_define_primitive(d, "strftime", prim_strftime);
    joy_dict_define_primitive(d, "format", prim_format);
    joy_dict_define_primitive(d, "formatf", prim_formatf);
    joy_dict_define_primitive(d, "opcase", prim_opcase);
    joy_dict_define_primitive(d, "case", prim_case);

    /* Additional math */
    joy_dict_define_primitive(d, "div", prim_divmod);
    joy_dict_define_primitive(d, "frexp", prim_frexp);
    joy_dict_define_primitive(d, "ldexp", prim_ldexp);
    joy_dict_define_primitive(d, "modf", prim_modf);
    joy_dict_define_primitive(d, "trunc", prim_trunc);

    /* Aggregate combinators */
    joy_dict_define_primitive(d, "split", prim_split);
    joy_dict_define_primitive(d, "enconcat", prim_enconcat);
    joy_dict_define_primitive(d, "some", prim_some);
    joy_dict_define_primitive(d, "all", prim_all);

    /* Arity combinators */
    joy_dict_define_primitive(d, "nullary", prim_nullary);
    joy_dict_define_primitive(d, "unary", prim_unary);
    joy_dict_define_primitive(d, "unary2", prim_unary2);
    joy_dict_define_primitive(d, "unary3", prim_unary3);
    joy_dict_define_primitive(d, "unary4", prim_unary4);
    joy_dict_define_primitive(d, "binary", prim_binary);
    joy_dict_define_primitive(d, "ternary", prim_ternary);
    joy_dict_define_primitive(d, "cleave", prim_cleave);
    joy_dict_define_primitive(d, "construct", prim_construct);

    /* Application combinators */
    joy_dict_define_primitive(d, "app1", prim_app1);
    joy_dict_define_primitive(d, "app11", prim_app11);
    joy_dict_define_primitive(d, "app12", prim_app12);
    joy_dict_define_primitive(d, "app2", prim_app2);
    joy_dict_define_primitive(d, "app3", prim_app3);
    joy_dict_define_primitive(d, "app4", prim_app4);

    /* Type conditionals */
    joy_dict_define_primitive(d, "ifinteger", prim_ifinteger);
    joy_dict_define_primitive(d, "ifchar", prim_ifchar);
    joy_dict_define_primitive(d, "iflogical", prim_iflogical);
    joy_dict_define_primitive(d, "ifset", prim_ifset);
    joy_dict_define_primitive(d, "ifstring", prim_ifstring);
    joy_dict_define_primitive(d, "iflist", prim_iflist);
    joy_dict_define_primitive(d, "iffloat", prim_iffloat);
    joy_dict_define_primitive(d, "iffile", prim_iffile);

    /* System interaction */
    joy_dict_define_primitive(d, "system", prim_system);
    joy_dict_define_primitive(d, "getenv", prim_getenv);
    joy_dict_define_primitive(d, "argc", prim_argc);
    joy_dict_define_primitive(d, "argv", prim_argv);

    /* Interpreter control */
    joy_dict_define_primitive(d, "abort", prim_abort);
    joy_dict_define_primitive(d, "quit", prim_quit);
    joy_dict_define_primitive(d, "gc", prim_gc);
    joy_dict_define_primitive(d, "setautoput", prim_setautoput);
    joy_dict_define_primitive(d, "setundeferror", prim_setundeferror);
    joy_dict_define_primitive(d, "autoput", prim_autoput);
    joy_dict_define_primitive(d, "undeferror", prim_undeferror);
    joy_dict_define_primitive(d, "echo", prim_echo);
    joy_dict_define_primitive(d, "conts", prim_conts);
    joy_dict_define_primitive(d, "undefs", prim_undefs);
    joy_dict_define_primitive(d, "help", prim_help);
    joy_dict_define_primitive(d, "helpdetail", prim_helpdetail);
    joy_dict_define_primitive(d, "manual", prim_manual);
    joy_dict_define_primitive(d, "get", prim_get);

    /* File I/O */
    joy_dict_define_primitive(d, "fopen", prim_fopen);
    joy_dict_define_primitive(d, "fclose", prim_fclose);
    joy_dict_define_primitive(d, "fflush", prim_fflush);
    joy_dict_define_primitive(d, "feof", prim_feof);
    joy_dict_define_primitive(d, "ferror", prim_ferror);
    joy_dict_define_primitive(d, "fgetch", prim_fgetch);
    joy_dict_define_primitive(d, "fgets", prim_fgets);
    joy_dict_define_primitive(d, "fread", prim_fread);
    joy_dict_define_primitive(d, "fput", prim_fput);
    joy_dict_define_primitive(d, "fputch", prim_fputch);
    joy_dict_define_primitive(d, "fputchars", prim_fputchars);
    joy_dict_define_primitive(d, "fputstring", prim_fputstring);
    joy_dict_define_primitive(d, "fwrite", prim_fwrite);
    joy_dict_define_primitive(d, "fseek", prim_fseek);
    joy_dict_define_primitive(d, "ftell", prim_ftell);
    joy_dict_define_primitive(d, "fremove", prim_fremove);
    joy_dict_define_primitive(d, "frename", prim_frename);
    joy_dict_define_primitive(d, "finclude", prim_finclude);
    joy_dict_define_primitive(d, "include", prim_include);
    joy_dict_define_primitive(d, "filetime", prim_filetime);
}
