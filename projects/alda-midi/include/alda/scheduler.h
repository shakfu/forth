/**
 * @file scheduler.h
 * @brief Event scheduling and playback for Alda interpreter.
 */

#ifndef ALDA_SCHEDULER_H
#define ALDA_SCHEDULER_H

#include "context.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Event Queue Management
 * ============================================================================ */

/**
 * @brief Ensure event queue has capacity for more events.
 * @param ctx Alda context.
 * @param additional Number of additional events needed.
 * @return 0 on success, -1 on allocation failure.
 */
int alda_events_reserve(AldaContext* ctx, int additional);

/**
 * @brief Schedule a generic event.
 * @param ctx Alda context.
 * @param tick Absolute tick position.
 * @param type Event type.
 * @param channel MIDI channel (0-15).
 * @param data1 First data byte.
 * @param data2 Second data byte.
 * @param part_index Source part index.
 * @return 0 on success, -1 on error.
 */
int alda_schedule_event(AldaContext* ctx, int tick, AldaEventType type,
                        int channel, int data1, int data2, int part_index);

/**
 * @brief Schedule a note (both note-on and note-off events).
 * @param ctx Alda context.
 * @param part Part state.
 * @param start_tick Start tick position.
 * @param pitch MIDI pitch (0-127).
 * @param velocity MIDI velocity (0-127).
 * @param duration_ticks Duration in ticks.
 * @return 0 on success, -1 on error.
 */
int alda_schedule_note(AldaContext* ctx, AldaPartState* part,
                       int start_tick, int pitch, int velocity, int duration_ticks);

/**
 * @brief Schedule a note with slur control.
 * @param ctx Alda context.
 * @param part Part state.
 * @param start_tick Start tick position.
 * @param pitch MIDI pitch (0-127).
 * @param velocity MIDI velocity (0-127).
 * @param duration_ticks Duration in ticks.
 * @param slurred If non-zero, skip quantization (play full duration).
 * @return 0 on success, -1 on error.
 */
int alda_schedule_note_slurred(AldaContext* ctx, AldaPartState* part,
                               int start_tick, int pitch, int velocity,
                               int duration_ticks, int slurred);

/**
 * @brief Schedule a program change.
 * @param ctx Alda context.
 * @param part Part state.
 * @param tick Tick position.
 * @return 0 on success, -1 on error.
 */
int alda_schedule_program_change(AldaContext* ctx, AldaPartState* part, int tick);

/**
 * @brief Schedule a pan change (CC 10).
 * @param ctx Alda context.
 * @param part Part state.
 * @param tick Tick position.
 * @param pan Pan value (0-127, 64=center).
 * @return 0 on success, -1 on error.
 */
int alda_schedule_pan(AldaContext* ctx, AldaPartState* part, int tick, int pan);

/**
 * @brief Schedule a tempo change.
 * @param ctx Alda context.
 * @param tick Tick position.
 * @param tempo New tempo in BPM.
 * @return 0 on success, -1 on error.
 */
int alda_schedule_tempo(AldaContext* ctx, int tick, int tempo);

/**
 * @brief Clear all scheduled events.
 * @param ctx Alda context.
 */
void alda_events_clear(AldaContext* ctx);

/* ============================================================================
 * Event Sorting and Playback
 * ============================================================================ */

/**
 * @brief Sort events by tick (note-offs before note-ons at same tick).
 * @param ctx Alda context.
 */
void alda_events_sort(AldaContext* ctx);

/**
 * @brief Play all scheduled events.
 * @param ctx Alda context.
 * @return 0 on success, -1 on error.
 */
int alda_events_play(AldaContext* ctx);

/* ============================================================================
 * Duration Calculation
 * ============================================================================ */

/**
 * @brief Convert note duration to ticks.
 *
 * @param denominator Note value denominator (1=whole, 2=half, 4=quarter, etc.)
 * @param dots Number of dots.
 * @return Duration in ticks.
 */
int alda_duration_to_ticks(int denominator, int dots);

/**
 * @brief Convert milliseconds to ticks.
 * @param ms Milliseconds.
 * @param tempo BPM.
 * @return Duration in ticks.
 */
int alda_ms_to_ticks(int ms, int tempo);

/**
 * @brief Convert seconds to ticks.
 * @param seconds Seconds.
 * @param tempo BPM.
 * @return Duration in ticks.
 */
int alda_seconds_to_ticks(double seconds, int tempo);

/**
 * @brief Convert ticks to milliseconds.
 * @param ticks Tick count.
 * @param tempo BPM.
 * @return Duration in milliseconds.
 */
int alda_ticks_to_ms(int ticks, int tempo);

/**
 * @brief Apply quantization to duration.
 * @param duration_ticks Original duration in ticks.
 * @param quant Quantization percentage (0-100).
 * @return Quantized duration in ticks.
 */
int alda_apply_quant(int duration_ticks, int quant);

#ifdef __cplusplus
}
#endif

#endif /* ALDA_SCHEDULER_H */
