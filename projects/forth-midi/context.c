/* context.c - ForthContext management */

#include "forth_midi.h"

/* Global context instance */
ForthContext g_ctx;

/* Initialize context with default values */
void forth_context_init(ForthContext* ctx) {
    if (!ctx) return;

    /* Core interpreter state */
    ctx->stack.top = 0;
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

    /* File loading depth */
    ctx->load_depth = 0;

    /* MIDI handles */
    ctx->midi_observer = NULL;
    ctx->midi_out = NULL;
    for (int i = 0; i < MAX_PORTS; i++) {
        ctx->out_ports[i] = NULL;
    }
    ctx->out_port_count = 0;

    /* Context defaults for concise notation */
    ctx->default_channel = 0;
    ctx->default_velocity = 100;
    ctx->default_duration = 250;
    ctx->current_pitch = 60;  /* Middle C */

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

    /* Named parameter system state */
    ctx->default_gate = 80;
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
}
