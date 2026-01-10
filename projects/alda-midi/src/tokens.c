/**
 * @file tokens.c
 * @brief Token implementation for the Alda parser.
 */

#include "alda/tokens.h"
#include <stdlib.h>
#include <string.h>

static const char* TOKEN_TYPE_NAMES[] = {
    "NOTE_LETTER",
    "REST_LETTER",
    "SHARP",
    "FLAT",
    "NATURAL",
    "OCTAVE_SET",
    "OCTAVE_UP",
    "OCTAVE_DOWN",
    "NOTE_LENGTH",
    "NOTE_LENGTH_MS",
    "NOTE_LENGTH_S",
    "DOT",
    "TIE",
    "BARLINE",
    "SEPARATOR",
    "NAME",
    "ALIAS",
    "COLON",
    "EQUALS",
    "LEFT_PAREN",
    "RIGHT_PAREN",
    "QUOTE",
    "SYMBOL",
    "NUMBER",
    "STRING",
    "MARKER",
    "AT_MARKER",
    "VOICE_MARKER",
    "CRAM_OPEN",
    "CRAM_CLOSE",
    "BRACKET_OPEN",
    "BRACKET_CLOSE",
    "REPEAT",
    "REPETITIONS",
    "NEWLINE",
    "EOF",
    "ERROR",
};

const char* alda_token_type_name(AldaTokenType type) {
    if (type >= 0 && type < ALDA_TOK_COUNT) {
        return TOKEN_TYPE_NAMES[type];
    }
    return "UNKNOWN";
}

AldaToken* alda_token_new(AldaTokenType type, const char* lexeme,
                          size_t lexeme_len, AldaSourcePos pos) {
    AldaToken* token = (AldaToken*)malloc(sizeof(AldaToken));
    if (!token) return NULL;

    token->type = type;
    token->lexeme_len = lexeme_len;
    token->pos = pos;
    memset(&token->literal, 0, sizeof(token->literal));

    if (lexeme && lexeme_len > 0) {
        token->lexeme = (char*)malloc(lexeme_len + 1);
        if (!token->lexeme) {
            free(token);
            return NULL;
        }
        memcpy(token->lexeme, lexeme, lexeme_len);
        token->lexeme[lexeme_len] = '\0';
    } else {
        token->lexeme = NULL;
    }

    return token;
}

void alda_token_free(AldaToken* token) {
    if (token) {
        free(token->lexeme);
        free(token);
    }
}

void alda_tokens_free(AldaToken* tokens, size_t count) {
    if (tokens) {
        for (size_t i = 0; i < count; i++) {
            free(tokens[i].lexeme);
        }
        free(tokens);
    }
}

AldaSourcePos alda_pos_new(int line, int column, const char* filename) {
    AldaSourcePos pos;
    pos.line = line;
    pos.column = column;
    pos.filename = filename;
    return pos;
}
