/**
 * @file interpreter.h
 * @brief Alda AST interpreter - walks AST and generates MIDI events.
 */

#ifndef ALDA_INTERPRETER_H
#define ALDA_INTERPRETER_H

#include "context.h"
#include "ast.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Main Interpreter Functions
 * ============================================================================ */

/**
 * @brief Interpret an Alda AST and schedule MIDI events.
 * @param ctx Alda context.
 * @param root Root AST node.
 * @return 0 on success, -1 on error.
 */
int alda_interpret_ast(AldaContext* ctx, AldaNode* root);

/**
 * @brief Parse and interpret an Alda source string.
 * @param ctx Alda context.
 * @param source Alda source code.
 * @param filename Source filename (for error messages).
 * @return 0 on success, -1 on error.
 */
int alda_interpret_string(AldaContext* ctx, const char* source, const char* filename);

/**
 * @brief Load and interpret an Alda file.
 * @param ctx Alda context.
 * @param filename Path to Alda file.
 * @return 0 on success, -1 on error.
 */
int alda_interpret_file(AldaContext* ctx, const char* filename);

/* ============================================================================
 * Pitch Calculation
 * ============================================================================ */

/**
 * @brief Calculate MIDI pitch from note letter, accidentals, octave, and key signature.
 * @param letter Note letter (a-g, case-insensitive).
 * @param accidentals Accidental string (+ for sharp, - for flat, _ for natural).
 * @param octave Octave number (0-9).
 * @param key_sig Key signature array [7] for C,D,E,F,G,A,B (+1=sharp, -1=flat, 0=natural).
 *                Can be NULL if no key signature.
 * @return MIDI pitch (0-127), or -1 on error.
 */
int alda_calculate_pitch(char letter, const char* accidentals, int octave, const int* key_sig);

/* ============================================================================
 * Duration Calculation from AST
 * ============================================================================ */

/**
 * @brief Calculate duration in ticks from a duration AST node.
 * @param ctx Alda context.
 * @param part Part state (for tempo).
 * @param duration Duration node (ALDA_NODE_DURATION or NULL for default).
 * @return Duration in ticks.
 */
int alda_ast_duration_to_ticks(AldaContext* ctx, AldaPartState* part, AldaNode* duration);

#ifdef __cplusplus
}
#endif

#endif /* ALDA_INTERPRETER_H */
