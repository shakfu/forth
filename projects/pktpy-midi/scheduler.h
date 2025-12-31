/* scheduler.h - Generator-based async scheduler for pktpy-midi
 *
 * Enables non-blocking MIDI playback using Python generators and libuv timers.
 * Multiple "voices" can run concurrently with independent timing.
 */

#ifndef PKTPY_MIDI_SCHEDULER_H
#define PKTPY_MIDI_SCHEDULER_H

#include "pocketpy.h"
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
    int slot_index;                /* Index in the generators list */

    /* Timing state */
    uint64_t wake_time_ms;         /* Absolute time to resume */
    int waiting;                   /* Currently waiting on yield? */

    /* libuv timer handle */
    uv_timer_t timer;

    /* Control flags */
    int stop_requested;

    /* Name for debugging */
    char name[32];
} Voice;

typedef struct {
    /* libuv event loop */
    uv_loop_t *loop;
    uv_thread_t thread;
    uv_async_t wake_async;         /* Wake loop for new work */
    uv_mutex_t mutex;              /* Protect voice list */

    /* Voice management */
    Voice voices[MAX_VOICES];
    int next_voice_id;
    int active_count;

    /* Scheduler state */
    int running;                   /* Is run() active? */
    int shutdown_requested;

    /* Pending resume queue (timer callbacks add here) */
    int pending_resumes[MAX_VOICES];
    int pending_count;
    uv_mutex_t pending_mutex;
    uv_async_t resume_async;       /* Signal main thread to process resumes */
} SchedulerState;

/* ============================================================================
 * Public API
 * ============================================================================ */

/* Initialize the scheduler system (call once at startup) */
int pk_scheduler_init(void);

/* Cleanup the scheduler system (call at shutdown) */
void pk_scheduler_cleanup(void);

/* Register scheduler functions in the midi module */
void pk_scheduler_register(py_GlobalRef mod);

/* Check if scheduler has active voices */
int pk_scheduler_active_count(void);

#endif /* PKTPY_MIDI_SCHEDULER_H */
