/**
 * @file scanner.c
 * @brief Lexer/scanner implementation for the Alda parser.
 */

#include "alda/scanner.h"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* Dynamic array for tokens */
typedef struct {
    AldaToken* data;
    size_t count;
    size_t capacity;
} TokenArray;

static int token_array_init(TokenArray* arr) {
    arr->capacity = 64;
    arr->count = 0;
    arr->data = (AldaToken*)malloc(arr->capacity * sizeof(AldaToken));
    return arr->data != NULL;
}

static int token_array_push(TokenArray* arr, AldaToken token) {
    if (arr->count >= arr->capacity) {
        size_t new_cap = arr->capacity * 2;
        AldaToken* new_data = (AldaToken*)realloc(arr->data, new_cap * sizeof(AldaToken));
        if (!new_data) return 0;
        arr->data = new_data;
        arr->capacity = new_cap;
    }
    arr->data[arr->count++] = token;
    return 1;
}

/* Scanner structure */
struct AldaScanner {
    const char* source;
    const char* filename;
    size_t start;
    size_t current;
    int line;
    int column;
    int line_start;
    int sexp_depth;
    AldaError* error;
    int has_pending;      /* Flag for pending accidental token */
    AldaToken pending;    /* Pending token (for suffix accidentals like 's' or 'b') */
};

/* Helper functions */
static int is_at_end(AldaScanner* s) {
    return s->source[s->current] == '\0';
}

static char peek(AldaScanner* s) {
    return s->source[s->current];
}

static char peek_next(AldaScanner* s) {
    if (is_at_end(s)) return '\0';
    return s->source[s->current + 1];
}

static char advance(AldaScanner* s) {
    char c = s->source[s->current++];
    s->column++;
    return c;
}

static int match(AldaScanner* s, char expected) {
    if (is_at_end(s)) return 0;
    if (s->source[s->current] != expected) return 0;
    s->current++;
    s->column++;
    return 1;
}

static void skip_whitespace(AldaScanner* s) {
    while (!is_at_end(s)) {
        char c = peek(s);
        if (c == ' ' || c == '\t' || c == '\r') {
            advance(s);
        } else if (c == '#') {
            /* Check if this is a comment (# followed by space/newline/EOF)
             * vs a sharp accidental (# followed by digit or note-related char) */
            char next_c = s->source[s->current + 1];
            if (next_c == '\0' || next_c == ' ' || next_c == '\t' ||
                next_c == '\n' || next_c == '\r' || isalpha((unsigned char)next_c)) {
                /* Comment: skip to end of line */
                while (!is_at_end(s) && peek(s) != '\n') {
                    advance(s);
                }
            } else {
                /* Sharp accidental - don't skip */
                break;
            }
        } else {
            break;
        }
    }
}

static int is_note_letter(char c) {
    return c >= 'a' && c <= 'g';
}

static int is_identifier_start(char c) {
    return isalpha((unsigned char)c) || c == '_';
}

static int is_identifier_char(char c) {
    return isalnum((unsigned char)c) || c == '_' || c == '-';
}

static int is_symbol_char(char c) {
    if (isalnum((unsigned char)c)) return 1;
    switch (c) {
        case '!': case '?': case '+': case '-': case '*': case '/':
        case '_': case '<': case '>': case '=': case '.': case ':':
            return 1;
        default:
            return 0;
    }
}

static void set_error(AldaScanner* s, const char* msg) {
    if (s->error) return; /* Keep first error */
    AldaSourcePos pos = alda_pos_new(s->line, s->column, s->filename);
    s->error = alda_error_new(ALDA_ERR_SCAN, msg, pos, s->source);
}

static AldaToken make_token(AldaScanner* s, AldaTokenType type) {
    AldaToken tok;
    tok.type = type;
    tok.lexeme_len = s->current - s->start;
    tok.lexeme = (char*)malloc(tok.lexeme_len + 1);
    if (tok.lexeme) {
        memcpy(tok.lexeme, s->source + s->start, tok.lexeme_len);
        tok.lexeme[tok.lexeme_len] = '\0';
    }
    tok.pos = alda_pos_new(s->line, (int)(s->start - (size_t)s->line_start + 1), s->filename);
    memset(&tok.literal, 0, sizeof(tok.literal));
    return tok;
}

static AldaToken error_token(AldaScanner* s, const char* msg) {
    set_error(s, msg);
    AldaToken tok;
    tok.type = ALDA_TOK_ERROR;
    tok.lexeme = (char*)malloc(strlen(msg) + 1);
    if (tok.lexeme) strcpy(tok.lexeme, msg);
    tok.lexeme_len = strlen(msg);
    tok.pos = alda_pos_new(s->line, s->column, s->filename);
    memset(&tok.literal, 0, sizeof(tok.literal));
    return tok;
}

/* Scan specific token types */

static AldaToken scan_number(AldaScanner* s) {
    while (isdigit((unsigned char)peek(s))) {
        advance(s);
    }

    /* Check for ms or s suffix */
    if (peek(s) == 'm' && peek_next(s) == 's') {
        advance(s); /* m */
        advance(s); /* s */
        AldaToken tok = make_token(s, ALDA_TOK_NOTE_LENGTH_MS);
        tok.literal.int_val = atoi(tok.lexeme);
        return tok;
    } else if (peek(s) == 's' && !isalpha((unsigned char)peek_next(s))) {
        advance(s); /* s */
        AldaToken tok = make_token(s, ALDA_TOK_NOTE_LENGTH_S);
        tok.literal.float_val = atof(tok.lexeme);
        return tok;
    }

    /* Regular note length */
    AldaToken tok = make_token(s, ALDA_TOK_NOTE_LENGTH);
    tok.literal.int_val = atoi(tok.lexeme);
    return tok;
}

static AldaToken scan_octave_set(AldaScanner* s) {
    /* Already consumed 'o', now get the number */
    while (isdigit((unsigned char)peek(s))) {
        advance(s);
    }
    AldaToken tok = make_token(s, ALDA_TOK_OCTAVE_SET);
    tok.literal.int_val = atoi(tok.lexeme + 1); /* Skip 'o' */
    return tok;
}

static AldaToken scan_name(AldaScanner* s) {
    while (is_identifier_char(peek(s))) {
        advance(s);
    }
    return make_token(s, ALDA_TOK_NAME);
}

static AldaToken scan_alias(AldaScanner* s) {
    /* Already consumed opening quote */
    while (!is_at_end(s) && peek(s) != '"') {
        if (peek(s) == '\n') {
            s->line++;
            s->line_start = (int)s->current + 1;
        }
        advance(s);
    }

    if (is_at_end(s)) {
        return error_token(s, "Unterminated string");
    }

    advance(s); /* Closing quote */
    return make_token(s, ALDA_TOK_ALIAS);
}

static AldaToken scan_marker(AldaScanner* s) {
    /* Already consumed '%' */
    while (is_identifier_char(peek(s))) {
        advance(s);
    }
    return make_token(s, ALDA_TOK_MARKER);
}

static AldaToken scan_at_marker(AldaScanner* s) {
    /* Already consumed '@' */
    while (is_identifier_char(peek(s))) {
        advance(s);
    }
    return make_token(s, ALDA_TOK_AT_MARKER);
}

static AldaToken scan_voice_marker(AldaScanner* s) {
    /* Already consumed 'V' */
    while (isdigit((unsigned char)peek(s))) {
        advance(s);
    }
    if (peek(s) == ':') {
        advance(s);
    }
    return make_token(s, ALDA_TOK_VOICE_MARKER);
}

static AldaToken scan_repeat(AldaScanner* s) {
    /* Already consumed '*' */
    while (isdigit((unsigned char)peek(s))) {
        advance(s);
    }
    AldaToken tok = make_token(s, ALDA_TOK_REPEAT);
    tok.literal.int_val = atoi(tok.lexeme + 1); /* Skip '*' */
    return tok;
}

static AldaToken scan_repetitions(AldaScanner* s) {
    /* Already consumed '\'' */
    /* Consume digits, commas, and hyphens */
    while (!is_at_end(s)) {
        char c = peek(s);
        if (isdigit((unsigned char)c) || c == ',' || c == '-') {
            advance(s);
        } else {
            break;
        }
    }
    return make_token(s, ALDA_TOK_REPETITIONS);
}

/* Lisp mode scanning */

static AldaToken scan_lisp_number(AldaScanner* s) {
    int has_dot = 0;

    /* Handle negative numbers */
    if (peek(s) == '-') {
        advance(s);
    }

    while (!is_at_end(s)) {
        char c = peek(s);
        if (isdigit((unsigned char)c)) {
            advance(s);
        } else if (c == '.' && !has_dot) {
            has_dot = 1;
            advance(s);
        } else {
            break;
        }
    }

    AldaToken tok = make_token(s, ALDA_TOK_NUMBER);
    tok.literal.float_val = atof(tok.lexeme);
    return tok;
}

static AldaToken scan_lisp_string(AldaScanner* s) {
    /* Already consumed opening quote */
    while (!is_at_end(s) && peek(s) != '"') {
        if (peek(s) == '\\' && peek_next(s) != '\0') {
            advance(s); /* Skip backslash */
        }
        if (peek(s) == '\n') {
            s->line++;
            s->line_start = (int)s->current + 1;
        }
        advance(s);
    }

    if (is_at_end(s)) {
        return error_token(s, "Unterminated string");
    }

    advance(s); /* Closing quote */
    return make_token(s, ALDA_TOK_STRING);
}

static AldaToken scan_symbol(AldaScanner* s) {
    while (is_symbol_char(peek(s))) {
        advance(s);
    }
    return make_token(s, ALDA_TOK_SYMBOL);
}

/* Main scanning logic */

static AldaToken scan_lisp_token(AldaScanner* s) {
    skip_whitespace(s);

    s->start = s->current;

    if (is_at_end(s)) {
        return make_token(s, ALDA_TOK_EOF);
    }

    char c = advance(s);

    if (c == '\n') {
        s->line++;
        s->line_start = (int)s->current;
        return make_token(s, ALDA_TOK_NEWLINE);
    }

    if (c == '(') {
        s->sexp_depth++;
        return make_token(s, ALDA_TOK_LEFT_PAREN);
    }

    if (c == ')') {
        s->sexp_depth--;
        return make_token(s, ALDA_TOK_RIGHT_PAREN);
    }

    if (c == '"') {
        return scan_lisp_string(s);
    }

    if (c == '\'') {
        /* Quote for quoted lists like '(g major) */
        return make_token(s, ALDA_TOK_QUOTE);
    }

    if (isdigit((unsigned char)c) || (c == '-' && isdigit((unsigned char)peek(s)))) {
        return scan_lisp_number(s);
    }

    if (is_symbol_char(c)) {
        return scan_symbol(s);
    }

    return error_token(s, "Unexpected character in S-expression");
}

static AldaToken scan_normal_token(AldaScanner* s) {
    /* Return pending token if we have one (from suffix accidentals) */
    if (s->has_pending) {
        s->has_pending = 0;
        return s->pending;
    }

    skip_whitespace(s);

    s->start = s->current;

    if (is_at_end(s)) {
        return make_token(s, ALDA_TOK_EOF);
    }

    char c = advance(s);

    /* Newline */
    if (c == '\n') {
        s->line++;
        s->line_start = (int)s->current;
        return make_token(s, ALDA_TOK_NEWLINE);
    }

    /* Single-character tokens */
    switch (c) {
        case '+':
        case '#': return make_token(s, ALDA_TOK_SHARP);
        case '-': return make_token(s, ALDA_TOK_FLAT);
        case '_': return make_token(s, ALDA_TOK_NATURAL);
        case '>': return make_token(s, ALDA_TOK_OCTAVE_UP);
        case '<': return make_token(s, ALDA_TOK_OCTAVE_DOWN);
        case '.': return make_token(s, ALDA_TOK_DOT);
        case '~': return make_token(s, ALDA_TOK_TIE);
        case '|': return make_token(s, ALDA_TOK_BARLINE);
        case '/': return make_token(s, ALDA_TOK_SEPARATOR);
        case ':': return make_token(s, ALDA_TOK_COLON);
        case '=': return make_token(s, ALDA_TOK_EQUALS);
        case '{': return make_token(s, ALDA_TOK_CRAM_OPEN);
        case '}': return make_token(s, ALDA_TOK_CRAM_CLOSE);
        case '[': return make_token(s, ALDA_TOK_BRACKET_OPEN);
        case ']': return make_token(s, ALDA_TOK_BRACKET_CLOSE);
        case '(': s->sexp_depth++; return make_token(s, ALDA_TOK_LEFT_PAREN);
        case ')': s->sexp_depth--; return make_token(s, ALDA_TOK_RIGHT_PAREN);
        default: break;
    }

    /* Rest letter: 'r' followed by non-letter (digits, whitespace, etc.) */
    if (c == 'r' && !isalpha((unsigned char)peek(s))) {
        return make_token(s, ALDA_TOK_REST_LETTER);
    }

    /* Octave set (o4) */
    if (c == 'o' && isdigit((unsigned char)peek(s))) {
        return scan_octave_set(s);
    }

    /* Voice marker (V1:) */
    if (c == 'V' && isdigit((unsigned char)peek(s))) {
        return scan_voice_marker(s);
    }

    /* Note letters: a-g
     * Can be followed by:
     * - Non-letter (digits, +, #, -, _, etc.) - just the note
     * - 's' suffix for sharp (cs, ds, fs, gs, as)
     * - 'b' suffix for flat (db, eb, gb, ab, bb) - but 'b' alone is note B
     */
    if (is_note_letter(c)) {
        char next = peek(s);

        /* Check for suffix accidentals: 's' for sharp, 'b' for flat */
        if (next == 's' && !isalpha((unsigned char)peek_next(s))) {
            /* Note followed by 's' (sharp suffix) - e.g., cs, fs */
            AldaToken tok = make_token(s, ALDA_TOK_NOTE_LETTER);
            tok.literal.char_val = c;
            /* Consume the 's' and queue it as a SHARP token */
            advance(s);
            s->start = s->current - 1;
            s->pending = make_token(s, ALDA_TOK_SHARP);
            s->has_pending = 1;
            return tok;
        } else if (c != 'b' && next == 'b' && !isalpha((unsigned char)peek_next(s))) {
            /* Non-b note followed by 'b' (flat suffix) - e.g., db, eb */
            AldaToken tok = make_token(s, ALDA_TOK_NOTE_LETTER);
            tok.literal.char_val = c;
            /* Consume the 'b' and queue it as a FLAT token */
            advance(s);
            s->start = s->current - 1;
            s->pending = make_token(s, ALDA_TOK_FLAT);
            s->has_pending = 1;
            return tok;
        } else if (!isalpha((unsigned char)next)) {
            /* Just a plain note letter */
            AldaToken tok = make_token(s, ALDA_TOK_NOTE_LETTER);
            tok.literal.char_val = c;
            return tok;
        }
        /* Otherwise fall through to identifier handling (e.g., "piano") */
    }

    /* Numbers (note lengths, durations) */
    if (isdigit((unsigned char)c)) {
        return scan_number(s);
    }

    /* Markers */
    if (c == '%') {
        return scan_marker(s);
    }

    if (c == '@') {
        return scan_at_marker(s);
    }

    /* Repeat */
    if (c == '*') {
        return scan_repeat(s);
    }

    /* Repetitions */
    if (c == '\'') {
        return scan_repetitions(s);
    }

    /* Alias (quoted string) */
    if (c == '"') {
        return scan_alias(s);
    }

    /* Names/identifiers */
    if (is_identifier_start(c)) {
        return scan_name(s);
    }

    return error_token(s, "Unexpected character");
}

/* Public API */

AldaScanner* alda_scanner_new(const char* source, const char* filename) {
    AldaScanner* s = (AldaScanner*)malloc(sizeof(AldaScanner));
    if (!s) return NULL;

    s->source = source;
    s->filename = filename;
    s->start = 0;
    s->current = 0;
    s->line = 1;
    s->column = 1;
    s->line_start = 0;
    s->sexp_depth = 0;
    s->error = NULL;
    s->has_pending = 0;
    memset(&s->pending, 0, sizeof(s->pending));

    return s;
}

void alda_scanner_free(AldaScanner* scanner) {
    if (scanner) {
        alda_error_free(scanner->error);
        free(scanner);
    }
}

AldaToken* alda_scanner_scan(AldaScanner* scanner, size_t* count) {
    TokenArray tokens;
    if (!token_array_init(&tokens)) {
        *count = 0;
        return NULL;
    }

    while (!is_at_end(scanner) && !scanner->error) {
        AldaToken tok;
        if (scanner->sexp_depth > 0) {
            tok = scan_lisp_token(scanner);
        } else {
            tok = scan_normal_token(scanner);
        }

        if (!token_array_push(&tokens, tok)) {
            alda_tokens_free(tokens.data, tokens.count);
            *count = 0;
            return NULL;
        }

        if (tok.type == ALDA_TOK_EOF || tok.type == ALDA_TOK_ERROR) {
            break;
        }
    }

    /* Add EOF if not already added */
    if (tokens.count == 0 || tokens.data[tokens.count - 1].type != ALDA_TOK_EOF) {
        AldaToken eof;
        eof.type = ALDA_TOK_EOF;
        eof.lexeme = NULL;
        eof.lexeme_len = 0;
        eof.pos = alda_pos_new(scanner->line, scanner->column, scanner->filename);
        memset(&eof.literal, 0, sizeof(eof.literal));
        token_array_push(&tokens, eof);
    }

    *count = tokens.count;
    return tokens.data;
}

int alda_scanner_has_error(AldaScanner* scanner) {
    return scanner->error != NULL;
}

const AldaError* alda_scanner_error(AldaScanner* scanner) {
    return scanner->error;
}

char* alda_scanner_error_string(AldaScanner* scanner) {
    if (!scanner->error) return NULL;
    return alda_error_format(scanner->error);
}
