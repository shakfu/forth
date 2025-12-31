/* scheduler.h - Thunk-based async scheduler for s7-midi
 *
 * Uses a simple cooperative model where each "voice" is a Scheme procedure
 * that returns either:
 *   - A number (milliseconds to wait before next call)
 *   - #f (voice is complete)
 *
 * This avoids s7's continuation complexities by not trying to suspend/resume
 * Scheme evaluation - instead each voice maintains its own state in a closure.
 *
 * Single-threaded: runs libuv event loop on main thread during (run).
 */

#ifndef S7_MIDI_SCHEDULER_H
#define S7_MIDI_SCHEDULER_H

#include "s7.h"
#include <uv.h>

/* ============================================================================
 * Constants
 * ============================================================================ */

#define MAX_VOICES 16

/* ============================================================================
 * Types
 * ============================================================================ */

typedef struct {
    int active;                    /* Is this voice slot in use? */
    int voice_id;                  /* Unique ID for this voice */

    /* Scheme thunk - stored as gc-protected s7_pointer */
    s7_pointer thunk;              /* The voice procedure */
    int gc_loc;                    /* GC protection location */

    /* Timing state */
    uint64_t wake_time_ms;         /* Absolute time to resume */
    int waiting;                   /* Currently waiting on timer? */

    /* libuv timer handle */
    uv_timer_t timer;

    /* Control flags */
    int stop_requested;

    /* Name for debugging */
    char name[32];
} Voice;

typedef struct {
    /* s7 scheme instance */
    s7_scheme *sc;

    /* libuv event loop (single-threaded) */
    uv_loop_t *loop;

    /* Voice management */
    Voice voices[MAX_VOICES];
    int next_voice_id;
    int active_count;

    /* Scheduler state */
    int running;                   /* Is run active? */
    int shutdown_requested;
} SchedulerState;

/* ============================================================================
 * Public API
 * ============================================================================ */

/* Initialize the scheduler system (call once at startup) */
int s7_scheduler_init(s7_scheme *sc);

/* Cleanup the scheduler system (call at shutdown) */
void s7_scheduler_cleanup(void);

/* Register scheduler functions in s7 */
void s7_scheduler_register(s7_scheme *sc);

/* Check if scheduler has active voices */
int s7_scheduler_active_count(void);

#endif /* S7_MIDI_SCHEDULER_H */
