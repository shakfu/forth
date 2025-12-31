/* context.c - ForthContext management */

/* Disable macros to access ctx members directly */
#define FORTH_NO_MACROS
#include "forth_midi.h"

/* Global context instance */
ForthContext g_ctx;

/* Check if sleep is disabled */
int forth_no_sleep(void) {
    return g_ctx.no_sleep_mode;
}

/* Set no-sleep mode (called from main) */
void forth_set_no_sleep(int v) {
    g_ctx.no_sleep_mode = v;
}

/* Reset runtime state (preserves dictionary and MIDI connections) */
void forth_context_reset(ForthContext* ctx) {
    if (!ctx) return;

    /* Reset stack */
    ctx->stack.top = -1;  /* -1 means empty stack */

    /* Reset compile mode */
    ctx->compile_mode = 0;
    ctx->current_definition_name[0] = '\0';
    ctx->current_definition_body[0] = '\0';
    ctx->definition_body_len = 0;

    /* Free and reset blocks */
    for (int i = 0; i < ctx->block_count; i++) {
        if (ctx->block_storage[i]) {
            free(ctx->block_storage[i]);
            ctx->block_storage[i] = NULL;
        }
    }
    ctx->block_count = 0;
    ctx->block_capture_mode = 0;
    ctx->block_body_len = 0;
    ctx->block_nesting = 0;

    /* Reset conditionals */
    ctx->cond_skip_mode = 0;
    ctx->cond_skip_nesting = 0;
    ctx->cond_in_true_branch = 0;

    /* Reset bracket sequences */
    for (int i = 0; i < ctx->bracket_seq_count; i++) {
        if (ctx->bracket_seq_storage[i]) {
            seq_release(ctx->bracket_seq_storage[i]);
            ctx->bracket_seq_storage[i] = NULL;
        }
    }
    ctx->bracket_seq_count = 0;
    ctx->seq_capture_mode = 0;
    ctx->seq_capture_count = 0;
    ctx->seq_capture_chord_mode = 0;
    ctx->seq_capture_chord_count = 0;
    ctx->current_bracket_seq = NULL;

    /* Reset pending parameters */
    ctx->pending_channel = -1;
    ctx->pending_velocity = -1;
    ctx->pending_duration = -1;
    ctx->pending_gate = -1;

    /* Reset articulation */
    ctx->articulation_staccato = 0;
    ctx->articulation_accent = 0;
    ctx->articulation_tenuto = 0;

    /* Reset loop control state */
    ctx->loop_capture_mode = 0;
    ctx->loop_nesting = 0;
    ctx->loop_body[0] = '\0';
    ctx->loop_body_len = 0;
    ctx->loop_cond[0] = '\0';
    ctx->loop_cond_len = 0;
    ctx->return_stack_top = 0;
}

/* Initialize context with default values */
void forth_context_init(ForthContext* ctx) {
    if (!ctx) return;

    /* Core interpreter state */
    ctx->stack.top = -1;  /* -1 means empty stack */
    ctx->dict_count = 0;

    /* Compile mode state */
    ctx->compile_mode = 0;
    ctx->current_definition_name[0] = '\0';
    ctx->current_definition_body[0] = '\0';
    ctx->definition_body_len = 0;

    /* Anonymous block state */
    for (int i = 0; i < MAX_BLOCKS; i++) {
        ctx->block_storage[i] = NULL;
    }
    ctx->block_count = 0;
    ctx->block_capture_mode = 0;
    ctx->current_block_body[0] = '\0';
    ctx->block_body_len = 0;
    ctx->block_nesting = 0;

    /* Conditional execution state */
    ctx->cond_skip_mode = 0;
    ctx->cond_skip_nesting = 0;
    ctx->cond_in_true_branch = 0;

    /* Track last executed word */
    ctx->last_executed_word[0] = '\0';

    /* File loading depth and error context */
    ctx->load_depth = 0;
    ctx->current_file = NULL;
    ctx->current_line = 0;

    /* MIDI output handles */
    ctx->midi_observer = NULL;
    ctx->midi_out = NULL;
    for (int i = 0; i < MAX_PORTS; i++) {
        ctx->out_ports[i] = NULL;
    }
    ctx->out_port_count = 0;

    /* MIDI input handles */
    ctx->midi_in = NULL;
    for (int i = 0; i < MAX_INPUT_PORTS; i++) {
        ctx->in_ports[i] = NULL;
    }
    ctx->in_port_count = 0;
    ctx->input_queue.head = 0;
    ctx->input_queue.tail = 0;

    /* Context defaults for concise notation */
    ctx->default_channel = 1;       /* 1-16 */
    ctx->default_velocity = 80;     /* 0-127 */
    ctx->default_duration = 500;    /* milliseconds */
    ctx->current_pitch = 60;        /* Middle C */

    /* Articulation flags */
    ctx->articulation_staccato = 0;
    ctx->articulation_accent = 0;
    ctx->articulation_tenuto = 0;

    /* Sequences */
    ctx->sequence_count = 0;
    ctx->current_seq = -1;
    ctx->global_bpm = 120;

    /* Recording system */
    for (int i = 0; i < MAX_RECORDING_LINES; i++) {
        ctx->recording_buffer[i] = NULL;
    }
    ctx->recording_count = 0;
    ctx->recording_active = 0;

    /* MIDI capture system */
    ctx->capture_count = 0;
    ctx->capture_active = 0;

    /* Generative music PRNG state */
    ctx->prng_seed = 12345;

    /* Sleep disable flag */
    ctx->no_sleep_mode = 0;

    /* Named parameter system state */
    ctx->default_gate = 100;  /* Gate percentage (1-100) */
    ctx->pending_channel = -1;
    ctx->pending_velocity = -1;
    ctx->pending_duration = -1;
    ctx->pending_gate = -1;

    /* Bracket sequence system */
    for (int i = 0; i < MAX_BRACKET_SEQS; i++) {
        ctx->bracket_seq_storage[i] = NULL;
    }
    ctx->bracket_seq_count = 0;
    ctx->seq_capture_mode = 0;
    ctx->seq_capture_count = 0;
    ctx->seq_capture_chord_mode = 0;
    ctx->seq_capture_chord_count = 0;
    ctx->current_bracket_seq = NULL;

    /* Loop control state */
    ctx->loop_capture_mode = 0;
    ctx->loop_nesting = 0;
    ctx->loop_body[0] = '\0';
    ctx->loop_body_len = 0;
    ctx->loop_cond[0] = '\0';
    ctx->loop_cond_len = 0;
    ctx->return_stack_top = 0;
}
