/* scheduler.c - Thunk-based async scheduler for s7-midi
 *
 * Each voice is a Scheme procedure that returns ms-to-wait or #f when done.
 * Uses single-threaded libuv for timing - no thread safety issues.
 */

#include "scheduler.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* ============================================================================
 * Static State
 * ============================================================================ */

static SchedulerState sched = {0};

/* ============================================================================
 * Time Helpers
 * ============================================================================ */

static uint64_t now_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000 + (uint64_t)ts.tv_nsec / 1000000;
}

/* ============================================================================
 * Voice Management
 * ============================================================================ */

static Voice* find_free_voice(void) {
    for (int i = 0; i < MAX_VOICES; i++) {
        if (!sched.voices[i].active) {
            return &sched.voices[i];
        }
    }
    return NULL;
}

static Voice* find_voice_by_id(int voice_id) {
    for (int i = 0; i < MAX_VOICES; i++) {
        if (sched.voices[i].active && sched.voices[i].voice_id == voice_id) {
            return &sched.voices[i];
        }
    }
    return NULL;
}

static void cleanup_voice(Voice* v) {
    if (!v->active) return;

    /* Stop timer if running */
    uv_timer_stop(&v->timer);

    /* Unprotect from GC */
    if (v->gc_loc >= 0) {
        s7_gc_unprotect_at(sched.sc, v->gc_loc);
        v->gc_loc = -1;
    }

    v->thunk = NULL;
    v->active = 0;
    v->waiting = 0;
    v->stop_requested = 0;
    v->name[0] = '\0';

    sched.active_count--;
}

/* ============================================================================
 * Timer Callback (runs on main thread via libuv)
 * ============================================================================ */

static void on_timer(uv_timer_t* handle) {
    Voice* v = (Voice*)handle->data;

    if (!v->active || v->stop_requested) {
        return;
    }

    v->waiting = 0;

    /* Call the thunk - it should return ms to wait or #f */
    s7_pointer result = s7_call(sched.sc, v->thunk, s7_nil(sched.sc));

    if (s7_is_boolean(result) && result == s7_f(sched.sc)) {
        /* Voice completed */
        cleanup_voice(v);
    } else if (s7_is_integer(result) || s7_is_real(result)) {
        /* Voice wants to wait */
        int64_t wait_ms = s7_is_integer(result)
            ? s7_integer(result)
            : (int64_t)s7_real(result);
        if (wait_ms < 0) wait_ms = 0;

        /* Set wake time and start timer */
        v->wake_time_ms = now_ms() + (uint64_t)wait_ms;
        v->waiting = 1;
        uv_timer_start(&v->timer, on_timer, (uint64_t)wait_ms, 0);
    } else {
        /* Invalid return - treat as done */
        fprintf(stderr, "Voice '%s': invalid return (expected number or #f)\n",
                v->name[0] ? v->name : "(unnamed)");
        cleanup_voice(v);
    }
}

/* ============================================================================
 * Public API
 * ============================================================================ */

int s7_scheduler_init(s7_scheme *sc) {
    if (sched.loop != NULL) {
        return 0;  /* Already initialized */
    }

    sched.sc = sc;
    sched.next_voice_id = 1;
    sched.active_count = 0;
    sched.running = 0;
    sched.shutdown_requested = 0;

    /* Create event loop */
    sched.loop = malloc(sizeof(uv_loop_t));
    if (!sched.loop) {
        return -1;
    }

    if (uv_loop_init(sched.loop) != 0) {
        free(sched.loop);
        sched.loop = NULL;
        return -1;
    }

    /* Initialize voice timer handles */
    for (int i = 0; i < MAX_VOICES; i++) {
        Voice* v = &sched.voices[i];
        v->active = 0;
        v->voice_id = 0;
        v->thunk = NULL;
        v->gc_loc = -1;

        uv_timer_init(sched.loop, &v->timer);
        v->timer.data = v;
    }

    return 0;
}

void s7_scheduler_cleanup(void) {
    if (!sched.loop) return;

    /* Stop all voices */
    for (int i = 0; i < MAX_VOICES; i++) {
        if (sched.voices[i].active) {
            sched.voices[i].stop_requested = 1;
            uv_timer_stop(&sched.voices[i].timer);
            if (sched.voices[i].gc_loc >= 0) {
                s7_gc_unprotect_at(sched.sc, sched.voices[i].gc_loc);
            }
        }
    }

    /* Close handles */
    for (int i = 0; i < MAX_VOICES; i++) {
        uv_close((uv_handle_t*)&sched.voices[i].timer, NULL);
    }

    /* Process remaining close callbacks */
    uv_run(sched.loop, UV_RUN_DEFAULT);

    /* Cleanup */
    uv_loop_close(sched.loop);
    free(sched.loop);
    sched.loop = NULL;
    sched.sc = NULL;
}

int s7_scheduler_active_count(void) {
    return sched.active_count;
}

/* ============================================================================
 * Scheme Functions
 * ============================================================================ */

/* (spawn thunk [name]) -> voice-id
 * Creates a new voice from a procedure.
 * The procedure takes no arguments and returns either:
 *   - A number (ms to wait before being called again)
 *   - #f (voice is done)
 */
static s7_pointer g_spawn(s7_scheme *sc, s7_pointer args) {
    s7_pointer thunk = s7_car(args);
    const char* name = "";

    if (!s7_is_procedure(thunk)) {
        return s7_wrong_type_error(sc, s7_make_symbol(sc, "spawn"), 1, thunk,
                                   s7_make_string(sc, "a procedure"));
    }

    if (s7_cdr(args) != s7_nil(sc)) {
        s7_pointer name_arg = s7_cadr(args);
        if (s7_is_string(name_arg)) {
            name = s7_string(name_arg);
        }
    }

    /* Initialize scheduler if needed */
    if (!sched.loop) {
        if (s7_scheduler_init(sc) != 0) {
            return s7_error(sc, s7_make_symbol(sc, "scheduler-error"),
                           s7_list(sc, 1, s7_make_string(sc, "Failed to initialize scheduler")));
        }
    }

    Voice* v = find_free_voice();
    if (!v) {
        return s7_error(sc, s7_make_symbol(sc, "scheduler-error"),
                       s7_list(sc, 1, s7_make_string(sc, "No free voice slots")));
    }

    /* Protect thunk from GC */
    v->gc_loc = s7_gc_protect(sc, thunk);
    v->thunk = thunk;

    /* Initialize voice */
    v->active = 1;
    v->voice_id = sched.next_voice_id++;
    v->waiting = 0;
    v->stop_requested = 0;
    v->wake_time_ms = 0;
    strncpy(v->name, name, sizeof(v->name) - 1);
    v->name[sizeof(v->name) - 1] = '\0';

    sched.active_count++;
    int voice_id = v->voice_id;

    /* Schedule immediate call (0ms timer) */
    uv_timer_start(&v->timer, on_timer, 0, 0);

    return s7_make_integer(sc, voice_id);
}

/* (run) - Run the scheduler until all voices complete */
static s7_pointer g_run(s7_scheme *sc, s7_pointer args) {
    (void)args;

    if (!sched.loop) {
        /* No voices spawned yet */
        return s7_unspecified(sc);
    }

    sched.running = 1;

    /* Run the event loop until all voices complete */
    while (sched.active_count > 0 && sched.running) {
        /* Run one iteration of the event loop */
        int result = uv_run(sched.loop, UV_RUN_ONCE);
        if (result == 0 && sched.active_count > 0) {
            /* No pending operations but voices still active - brief sleep */
            uv_sleep(1);
        }
    }

    sched.running = 0;
    return s7_unspecified(sc);
}

/* (stop [voice-id]) - Stop a specific voice or all voices */
static s7_pointer g_stop(s7_scheme *sc, s7_pointer args) {
    if (args != s7_nil(sc)) {
        /* Stop specific voice */
        s7_pointer id_arg = s7_car(args);
        if (!s7_is_integer(id_arg)) {
            return s7_wrong_type_error(sc, s7_make_symbol(sc, "stop"), 1, id_arg,
                                       s7_make_string(sc, "an integer"));
        }
        int id = s7_integer(id_arg);
        Voice* v = find_voice_by_id(id);
        if (v) {
            v->stop_requested = 1;
            uv_timer_stop(&v->timer);
            cleanup_voice(v);
            return s7_t(sc);
        }
        return s7_f(sc);
    }

    /* Stop all voices */
    sched.running = 0;
    for (int i = 0; i < MAX_VOICES; i++) {
        if (sched.voices[i].active) {
            sched.voices[i].stop_requested = 1;
            uv_timer_stop(&sched.voices[i].timer);
            cleanup_voice(&sched.voices[i]);
        }
    }
    return s7_unspecified(sc);
}

/* (voices) -> count of active voices */
static s7_pointer g_voices(s7_scheme *sc, s7_pointer args) {
    (void)args;
    return s7_make_integer(sc, sched.active_count);
}

/* (scheduler-status) -> alist with scheduler info */
static s7_pointer g_scheduler_status(s7_scheme *sc, s7_pointer args) {
    (void)args;

    s7_pointer result = s7_nil(sc);

    /* running */
    result = s7_cons(sc,
                     s7_cons(sc, s7_make_symbol(sc, "running"),
                            sched.running ? s7_t(sc) : s7_f(sc)),
                     result);

    /* active */
    result = s7_cons(sc,
                     s7_cons(sc, s7_make_symbol(sc, "active"),
                            s7_make_integer(sc, sched.active_count)),
                     result);

    /* voices list */
    s7_pointer voices_list = s7_nil(sc);
    for (int i = MAX_VOICES - 1; i >= 0; i--) {
        if (sched.voices[i].active) {
            s7_pointer voice_info = s7_list(sc, 3,
                s7_make_integer(sc, sched.voices[i].voice_id),
                s7_make_string(sc, sched.voices[i].name),
                sched.voices[i].waiting ? s7_t(sc) : s7_f(sc));
            voices_list = s7_cons(sc, voice_info, voices_list);
        }
    }
    result = s7_cons(sc,
                     s7_cons(sc, s7_make_symbol(sc, "voices"), voices_list),
                     result);

    return result;
}

/* ============================================================================
 * Module Registration
 * ============================================================================ */

void s7_scheduler_register(s7_scheme *sc) {
    s7_define_function(sc, "spawn", g_spawn, 1, 1, false,
        "(spawn thunk [name]) creates a voice from a procedure that returns ms or #f");

    s7_define_function(sc, "run", g_run, 0, 0, false,
        "(run) runs scheduler until all voices complete");

    s7_define_function(sc, "stop", g_stop, 0, 1, false,
        "(stop [voice-id]) stops a specific voice or all voices");

    s7_define_function(sc, "voices", g_voices, 0, 0, false,
        "(voices) returns count of active voices");

    s7_define_function(sc, "scheduler-status", g_scheduler_status, 0, 0, false,
        "(scheduler-status) returns alist with scheduler info");
}
