/* scheduler.c - Generator-based async scheduler for pktpy-midi
 *
 * This module enables non-blocking MIDI playback using Python generators.
 * Voices (generators) yield with yield_ms(n) and are resumed after n ms.
 */

#define PK_IS_PUBLIC_INCLUDE
#include "pocketpy.h"
#include "scheduler.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* ============================================================================
 * Static State
 * ============================================================================ */

static SchedulerState sched = {0};

/* We use register 7 (py_r7) to store the generators list */
#define GENERATORS_REG py_getreg(7)

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

/* Get generator from the list by slot index */
static py_Ref get_generator(int slot_index) {
    py_Ref list = GENERATORS_REG;
    if (py_isnil(list) || !py_istype(list, tp_list)) {
        return NULL;
    }
    if (slot_index < 0 || slot_index >= py_list_len(list)) {
        return NULL;
    }
    return py_list_getitem(list, slot_index);
}

/* Set generator in the list (replace with None when done) */
static void clear_generator(int slot_index) {
    py_Ref list = GENERATORS_REG;
    if (py_isnil(list) || !py_istype(list, tp_list)) {
        return;
    }
    if (slot_index >= 0 && slot_index < py_list_len(list)) {
        py_Ref slot = py_list_getitem(list, slot_index);
        py_newnone(slot);
    }
}

static void cleanup_voice(Voice* v) {
    if (!v->active) return;

    /* Stop timer if running */
    uv_timer_stop(&v->timer);

    /* Clear generator reference in the list */
    clear_generator(v->slot_index);

    v->active = 0;
    v->slot_index = -1;
    v->waiting = 0;
    v->stop_requested = 0;
    v->name[0] = '\0';

    uv_mutex_lock(&sched.mutex);
    sched.active_count--;
    uv_mutex_unlock(&sched.mutex);
}

/* ============================================================================
 * Timer Callback (runs in libuv thread)
 * ============================================================================ */

static void on_timer(uv_timer_t* handle) {
    Voice* v = (Voice*)handle->data;

    if (!v->active || v->stop_requested) {
        return;
    }

    /* Add to pending resumes queue */
    uv_mutex_lock(&sched.pending_mutex);
    if (sched.pending_count < MAX_VOICES) {
        sched.pending_resumes[sched.pending_count++] = v->voice_id;
    }
    uv_mutex_unlock(&sched.pending_mutex);

    /* Signal main thread */
    uv_async_send(&sched.resume_async);
}

/* ============================================================================
 * Async Signal Handlers
 * ============================================================================ */

static void on_wake(uv_async_t* handle) {
    (void)handle;
    if (sched.shutdown_requested) {
        uv_stop(sched.loop);
    }
}

static void on_resume_signal(uv_async_t* handle) {
    (void)handle;
    /* This just wakes the main thread; actual processing happens in run() */
}

/* ============================================================================
 * Event Loop Thread
 * ============================================================================ */

static void loop_thread_fn(void* arg) {
    (void)arg;

    while (!sched.shutdown_requested) {
        uv_run(sched.loop, UV_RUN_DEFAULT);

        if (!sched.shutdown_requested) {
            /* Brief sleep to avoid busy-waiting when loop has no handles */
            uv_sleep(10);
        }
    }
}

/* ============================================================================
 * Resume Processing (called from main thread)
 * ============================================================================ */

static void process_pending_resumes(void) {
    int pending[MAX_VOICES];
    int count;

    /* Copy pending list under lock */
    uv_mutex_lock(&sched.pending_mutex);
    count = sched.pending_count;
    memcpy(pending, sched.pending_resumes, count * sizeof(int));
    sched.pending_count = 0;
    uv_mutex_unlock(&sched.pending_mutex);

    /* Process each pending resume */
    for (int i = 0; i < count; i++) {
        Voice* v = find_voice_by_id(pending[i]);
        if (!v || !v->active || v->stop_requested) {
            continue;
        }

        v->waiting = 0;

        /* Get the generator from our list */
        py_Ref gen = get_generator(v->slot_index);
        if (!gen || py_isnone(gen)) {
            cleanup_voice(v);
            continue;
        }

        /* Resume the generator with py_next */
        int status = py_next(gen);

        if (status == 0) {
            /* StopIteration - generator completed */
            cleanup_voice(v);
        } else if (status == 1) {
            /* Generator yielded a value - should be ms to wait */
            py_Ref yielded = py_retval();
            int64_t wait_ms = 0;

            if (py_isint(yielded)) {
                wait_ms = py_toint(yielded);
                if (wait_ms < 0) wait_ms = 0;
            }

            /* Set wake time and start timer */
            v->wake_time_ms = now_ms() + (uint64_t)wait_ms;
            v->waiting = 1;
            uv_timer_start(&v->timer, on_timer, (uint64_t)wait_ms, 0);

            /* Wake the event loop */
            uv_async_send(&sched.wake_async);
        } else {
            /* Error in generator */
            fprintf(stderr, "Voice '%s' error: ",
                    v->name[0] ? v->name : "(unnamed)");
            py_printexc();
            py_StackRef p0 = py_peek(0);
            py_clearexc(p0);
            cleanup_voice(v);
        }
    }
}

/* ============================================================================
 * Public API
 * ============================================================================ */

int pk_scheduler_init(void) {
    if (sched.loop != NULL) {
        return 0;  /* Already initialized */
    }

    sched.next_voice_id = 1;
    sched.active_count = 0;
    sched.running = 0;
    sched.shutdown_requested = 0;
    sched.pending_count = 0;

    /* Initialize the generators list in register 7 */
    py_newlist(GENERATORS_REG);

    /* Initialize mutexes */
    uv_mutex_init(&sched.mutex);
    uv_mutex_init(&sched.pending_mutex);

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

    /* Initialize async handles */
    uv_async_init(sched.loop, &sched.wake_async, on_wake);
    uv_async_init(sched.loop, &sched.resume_async, on_resume_signal);

    /* Initialize voice slots */
    for (int i = 0; i < MAX_VOICES; i++) {
        Voice* v = &sched.voices[i];
        v->active = 0;
        v->voice_id = 0;
        v->slot_index = -1;

        uv_timer_init(sched.loop, &v->timer);
        v->timer.data = v;
    }

    /* Start event loop thread */
    if (uv_thread_create(&sched.thread, loop_thread_fn, NULL) != 0) {
        uv_loop_close(sched.loop);
        free(sched.loop);
        sched.loop = NULL;
        return -1;
    }

    return 0;
}

void pk_scheduler_cleanup(void) {
    if (!sched.loop) return;

    /* Stop all voices */
    for (int i = 0; i < MAX_VOICES; i++) {
        if (sched.voices[i].active) {
            sched.voices[i].stop_requested = 1;
            uv_timer_stop(&sched.voices[i].timer);
        }
    }

    /* Signal shutdown */
    sched.shutdown_requested = 1;
    uv_async_send(&sched.wake_async);

    /* Wait for thread */
    uv_thread_join(&sched.thread);

    /* Close handles */
    for (int i = 0; i < MAX_VOICES; i++) {
        uv_close((uv_handle_t*)&sched.voices[i].timer, NULL);
    }
    uv_close((uv_handle_t*)&sched.wake_async, NULL);
    uv_close((uv_handle_t*)&sched.resume_async, NULL);

    /* Process remaining close callbacks */
    uv_run(sched.loop, UV_RUN_DEFAULT);

    /* Clear the generators list */
    py_newnone(GENERATORS_REG);

    /* Cleanup */
    uv_mutex_destroy(&sched.mutex);
    uv_mutex_destroy(&sched.pending_mutex);
    uv_loop_close(sched.loop);
    free(sched.loop);
    sched.loop = NULL;
}

int pk_scheduler_active_count(void) {
    return sched.active_count;
}

/* ============================================================================
 * Python Functions
 * ============================================================================ */

/* spawn(func, [name]) -> voice_id
 * Creates a new voice from a generator function
 * The function is called immediately to get the generator
 */
static bool pk_spawn(int argc, py_StackRef argv) {
    if (argc < 1 || argc > 2) {
        return py_exception(tp_TypeError, "spawn() takes 1-2 arguments");
    }

    py_Ref func = &argv[0];
    const char* name = "";
    if (argc >= 2 && py_isstr(&argv[1])) {
        name = py_tostr(&argv[1]);
    }

    /* Initialize scheduler if needed */
    if (!sched.loop) {
        if (pk_scheduler_init() != 0) {
            return py_exception(tp_RuntimeError, "Failed to initialize scheduler");
        }
    }

    uv_mutex_lock(&sched.mutex);
    Voice* v = find_free_voice();
    if (!v) {
        uv_mutex_unlock(&sched.mutex);
        return py_exception(tp_RuntimeError, "No free voice slots (max %d)", MAX_VOICES);
    }

    /* Call the function to get a generator */
    if (!py_call(func, 0, NULL)) {
        uv_mutex_unlock(&sched.mutex);
        return false;  /* Exception already set */
    }

    /* Check that we got a generator */
    py_Ref result = py_retval();
    if (!py_istype(result, tp_generator)) {
        uv_mutex_unlock(&sched.mutex);
        return py_exception(tp_TypeError, "spawn() requires a generator function (use yield)");
    }

    /* Add generator to our list */
    py_Ref list = GENERATORS_REG;
    int slot_index = py_list_len(list);
    py_list_append(list, result);

    /* Initialize voice */
    v->active = 1;
    v->voice_id = sched.next_voice_id++;
    v->slot_index = slot_index;
    v->waiting = 0;
    v->stop_requested = 0;
    v->wake_time_ms = 0;
    strncpy(v->name, name, sizeof(v->name) - 1);
    v->name[sizeof(v->name) - 1] = '\0';

    sched.active_count++;
    int voice_id = v->voice_id;
    uv_mutex_unlock(&sched.mutex);

    /* Schedule immediate resume (0ms timer) */
    uv_timer_start(&v->timer, on_timer, 0, 0);

    py_newint(py_retval(), voice_id);
    return true;
}

/* run() - Run the scheduler until all voices complete
 * This is a blocking call that processes voice resumes
 */
static bool pk_run(int argc, py_StackRef argv) {
    (void)argv;
    if (argc != 0) {
        return py_exception(tp_TypeError, "run() takes 0 arguments");
    }

    if (!sched.loop) {
        /* No voices spawned yet */
        py_newnone(py_retval());
        return true;
    }

    sched.running = 1;

    while (sched.active_count > 0 && sched.running) {
        /* Process any pending resumes */
        process_pending_resumes();

        /* Brief sleep if nothing pending */
        uv_mutex_lock(&sched.pending_mutex);
        int has_pending = sched.pending_count > 0;
        uv_mutex_unlock(&sched.pending_mutex);

        if (!has_pending && sched.active_count > 0) {
            uv_sleep(1);
        }
    }

    sched.running = 0;
    py_newnone(py_retval());
    return true;
}

/* poll() - Process any pending voice resumes without blocking
 * Returns True if there are still active voices, False otherwise
 * Use this for non-blocking playback while keeping REPL responsive
 *
 * Example:
 *   midi.spawn(melody)
 *   while midi.poll():
 *       # REPL stays responsive
 *       pass
 */
static bool pk_poll(int argc, py_StackRef argv) {
    (void)argv;
    if (argc != 0) {
        return py_exception(tp_TypeError, "poll() takes 0 arguments");
    }

    if (!sched.loop || sched.active_count == 0) {
        py_newbool(py_retval(), false);
        return true;
    }

    /* Process any pending resumes */
    process_pending_resumes();

    /* Return True if voices still active */
    py_newbool(py_retval(), sched.active_count > 0);
    return true;
}

/* stop([voice_id]) - Stop a specific voice or all voices */
static bool pk_stop(int argc, py_StackRef argv) {
    if (argc > 1) {
        return py_exception(tp_TypeError, "stop() takes 0-1 arguments");
    }

    if (argc == 1 && !py_isnone(&argv[0])) {
        /* Stop specific voice */
        if (!py_isint(&argv[0])) {
            return py_exception(tp_TypeError, "voice_id must be an integer");
        }
        int id = (int)py_toint(&argv[0]);
        Voice* v = find_voice_by_id(id);
        if (v) {
            v->stop_requested = 1;
            uv_timer_stop(&v->timer);
            cleanup_voice(v);
            py_newbool(py_retval(), true);
        } else {
            py_newbool(py_retval(), false);
        }
        return true;
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
    py_newnone(py_retval());
    return true;
}

/* voices() -> int - Return count of active voices */
static bool pk_voices(int argc, py_StackRef argv) {
    (void)argv;
    if (argc != 0) {
        return py_exception(tp_TypeError, "voices() takes 0 arguments");
    }
    py_newint(py_retval(), sched.active_count);
    return true;
}

/* status() -> dict - Return scheduler status */
static bool pk_status(int argc, py_StackRef argv) {
    (void)argv;
    if (argc != 0) {
        return py_exception(tp_TypeError, "status() takes 0 arguments");
    }

    /* Build a simple dict with running and active count using registers */
    py_newdict(py_r0());  /* dict in r0 */

    py_newbool(py_r1(), sched.running);
    py_dict_setitem_by_str(py_r0(), "running", py_r1());

    py_newint(py_r1(), sched.active_count);
    py_dict_setitem_by_str(py_r0(), "active", py_r1());

    py_assign(py_retval(), py_r0());
    return true;
}

/* ============================================================================
 * Module Registration
 * ============================================================================ */

void pk_scheduler_register(py_GlobalRef mod) {
    py_bindfunc(mod, "spawn", pk_spawn);
    py_bindfunc(mod, "run", pk_run);
    py_bindfunc(mod, "poll", pk_poll);
    py_bindfunc(mod, "stop", pk_stop);
    py_bindfunc(mod, "voices", pk_voices);
    py_bindfunc(mod, "status", pk_status);
}
