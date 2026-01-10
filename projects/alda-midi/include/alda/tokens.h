/**
 * @file tokens.h
 * @brief Token types and structures for the Alda parser.
 */

#ifndef ALDA_TOKENS_H
#define ALDA_TOKENS_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Token types for the Alda language.
 */
typedef enum {
    /* Musical notation tokens */
    ALDA_TOK_NOTE_LETTER,       /* a-g */
    ALDA_TOK_REST_LETTER,       /* r */
    ALDA_TOK_SHARP,             /* + */
    ALDA_TOK_FLAT,              /* - */
    ALDA_TOK_NATURAL,           /* _ */
    ALDA_TOK_OCTAVE_SET,        /* o4 */
    ALDA_TOK_OCTAVE_UP,         /* > */
    ALDA_TOK_OCTAVE_DOWN,       /* < */
    ALDA_TOK_NOTE_LENGTH,       /* 1, 2, 4, 8, 16, 32 */
    ALDA_TOK_NOTE_LENGTH_MS,    /* 500ms */
    ALDA_TOK_NOTE_LENGTH_S,     /* 2s */
    ALDA_TOK_DOT,               /* . (dotted notes) */
    ALDA_TOK_TIE,               /* ~ */
    ALDA_TOK_BARLINE,           /* | */
    ALDA_TOK_SEPARATOR,         /* / (chord separator) */

    /* Structure tokens */
    ALDA_TOK_NAME,              /* identifiers (instrument names, variables) */
    ALDA_TOK_ALIAS,             /* quoted strings for part aliases */
    ALDA_TOK_COLON,             /* : */
    ALDA_TOK_EQUALS,            /* = */

    /* Lisp/S-expression tokens */
    ALDA_TOK_LEFT_PAREN,        /* ( */
    ALDA_TOK_RIGHT_PAREN,       /* ) */
    ALDA_TOK_QUOTE,             /* ' (quote for lists like '(g major)) */
    ALDA_TOK_SYMBOL,            /* lisp symbols (tempo, vol, etc.) */
    ALDA_TOK_NUMBER,            /* numeric literals in lisp context */
    ALDA_TOK_STRING,            /* string literals in lisp context */

    /* Control/structure tokens */
    ALDA_TOK_MARKER,            /* %name */
    ALDA_TOK_AT_MARKER,         /* @name */
    ALDA_TOK_VOICE_MARKER,      /* V1:, V2:, V0: */
    ALDA_TOK_CRAM_OPEN,         /* { */
    ALDA_TOK_CRAM_CLOSE,        /* } */
    ALDA_TOK_BRACKET_OPEN,      /* [ */
    ALDA_TOK_BRACKET_CLOSE,     /* ] */
    ALDA_TOK_REPEAT,            /* *3 */
    ALDA_TOK_REPETITIONS,       /* '1-3,5 */

    /* Whitespace and control */
    ALDA_TOK_NEWLINE,
    ALDA_TOK_EOF,
    ALDA_TOK_ERROR,

    ALDA_TOK_COUNT              /* Number of token types */
} AldaTokenType;

/**
 * @brief Source position for error reporting.
 */
typedef struct {
    int line;                   /* 1-based line number */
    int column;                 /* 1-based column number */
    const char* filename;       /* Source filename (may be NULL) */
} AldaSourcePos;

/**
 * @brief Token literal value (union for different types).
 */
typedef union {
    int int_val;                /* Integer value (note lengths, octaves, etc.) */
    double float_val;           /* Float value (seconds) */
    char char_val;              /* Character value (note letters) */
} AldaTokenLiteral;

/**
 * @brief Token structure.
 */
typedef struct {
    AldaTokenType type;         /* Token type */
    char* lexeme;               /* Original text (owned, null-terminated) */
    size_t lexeme_len;          /* Length of lexeme */
    AldaTokenLiteral literal;   /* Parsed literal value */
    AldaSourcePos pos;          /* Source position */
} AldaToken;

/**
 * @brief Get the name of a token type.
 * @param type The token type.
 * @return A string representation of the token type.
 */
const char* alda_token_type_name(AldaTokenType type);

/**
 * @brief Create a new token.
 * @param type Token type.
 * @param lexeme Token text (will be copied).
 * @param lexeme_len Length of lexeme.
 * @param pos Source position.
 * @return Newly allocated token. Caller must free with alda_token_free().
 */
AldaToken* alda_token_new(AldaTokenType type, const char* lexeme,
                          size_t lexeme_len, AldaSourcePos pos);

/**
 * @brief Free a token.
 * @param token Token to free.
 */
void alda_token_free(AldaToken* token);

/**
 * @brief Free an array of tokens.
 * @param tokens Array of tokens.
 * @param count Number of tokens.
 */
void alda_tokens_free(AldaToken* tokens, size_t count);

/**
 * @brief Create a source position.
 * @param line Line number (1-based).
 * @param column Column number (1-based).
 * @param filename Source filename (may be NULL).
 * @return Source position struct.
 */
AldaSourcePos alda_pos_new(int line, int column, const char* filename);

#ifdef __cplusplus
}
#endif

#endif /* ALDA_TOKENS_H */
