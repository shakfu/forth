/* scheduler.c - Coroutine-based async scheduler for lua-midi
 *
 * This module enables non-blocking MIDI playback using Lua coroutines.
 * Voices (coroutines) yield with yield_ms(n) and are resumed after n ms.
 */

#include "scheduler.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* ============================================================================
 * Static State
 * ============================================================================ */

static SchedulerSystem sched = {0};

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

    /* Release registry reference to allow GC of coroutine */
    if (v->registry_ref != LUA_NOREF && sched.main_L) {
        luaL_unref(sched.main_L, LUA_REGISTRYINDEX, v->registry_ref);
    }

    v->active = 0;
    v->co = NULL;
    v->registry_ref = LUA_NOREF;
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

static void process_pending_resumes(lua_State *L) {
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

        /* Resume the coroutine */
        int nres;
        int status = lua_resume(v->co, L, 0, &nres);

        if (status == LUA_OK) {
            /* Coroutine completed */
            cleanup_voice(v);
        } else if (status == LUA_YIELD) {
            /* Coroutine yielded, waiting flag set by yield_ms */
        } else {
            /* Error in coroutine */
            const char* err = lua_tostring(v->co, -1);
            fprintf(stderr, "Voice '%s' error: %s\n",
                    v->name[0] ? v->name : "(unnamed)", err ? err : "unknown");
            lua_pop(v->co, 1);
            cleanup_voice(v);
        }
    }
}

/* ============================================================================
 * Public API - Init/Cleanup
 * ============================================================================ */

int scheduler_init(lua_State *L) {
    if (sched.loop != NULL) {
        return 0;  /* Already initialized */
    }

    sched.main_L = L;
    sched.next_voice_id = 1;
    sched.active_count = 0;
    sched.running = 0;
    sched.shutdown_requested = 0;
    sched.pending_count = 0;

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
        v->co = NULL;
        v->registry_ref = LUA_NOREF;

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

void scheduler_cleanup(void) {
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

    /* Cleanup */
    uv_mutex_destroy(&sched.mutex);
    uv_mutex_destroy(&sched.pending_mutex);
    uv_loop_close(sched.loop);
    free(sched.loop);
    sched.loop = NULL;
    sched.main_L = NULL;
}

int scheduler_has_pending(void) {
    uv_mutex_lock(&sched.pending_mutex);
    int has = sched.pending_count > 0;
    uv_mutex_unlock(&sched.pending_mutex);
    return has;
}

int scheduler_process_pending(lua_State *L) {
    process_pending_resumes(L);
    return sched.active_count;
}

/* ============================================================================
 * Lua Functions
 * ============================================================================ */

/* spawn(func, [name]) -> voice_id
 * Creates a new voice (coroutine) from the given function
 */
static int l_spawn(lua_State *L) {
    luaL_checktype(L, 1, LUA_TFUNCTION);
    const char* name = luaL_optstring(L, 2, "");

    if (!sched.loop) {
        if (scheduler_init(L) != 0) {
            return luaL_error(L, "Failed to initialize scheduler");
        }
    }

    uv_mutex_lock(&sched.mutex);
    Voice* v = find_free_voice();
    if (!v) {
        uv_mutex_unlock(&sched.mutex);
        return luaL_error(L, "No free voice slots (max %d)", MAX_VOICES);
    }

    /* Create coroutine */
    lua_State *co = lua_newthread(L);  /* pushes thread onto stack */

    /* Store in registry to prevent GC */
    int ref = luaL_ref(L, LUA_REGISTRYINDEX);

    /* Copy function to coroutine stack */
    lua_pushvalue(L, 1);
    lua_xmove(L, co, 1);

    /* Initialize voice */
    v->active = 1;
    v->voice_id = sched.next_voice_id++;
    v->co = co;
    v->registry_ref = ref;
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

    lua_pushinteger(L, voice_id);
    return 1;
}

/* yield_ms(ms) - Pause the current voice for ms milliseconds
 * Must be called from within a coroutine (spawned voice)
 */
static int l_yield_ms(lua_State *L) {
    lua_Integer ms = luaL_checkinteger(L, 1);
    if (ms < 0) ms = 0;

    /* Find which voice this coroutine belongs to */
    Voice* v = NULL;
    for (int i = 0; i < MAX_VOICES; i++) {
        if (sched.voices[i].active && sched.voices[i].co == L) {
            v = &sched.voices[i];
            break;
        }
    }

    if (!v) {
        return luaL_error(L, "yield_ms must be called from a spawned voice");
    }

    if (v->stop_requested) {
        return luaL_error(L, "Voice was stopped");
    }

    /* Set wake time and start timer */
    v->wake_time_ms = now_ms() + (uint64_t)ms;
    v->waiting = 1;
    uv_timer_start(&v->timer, on_timer, (uint64_t)ms, 0);

    /* Wake the event loop to notice the new timer */
    uv_async_send(&sched.wake_async);

    /* Yield the coroutine */
    return lua_yield(L, 0);
}

/* run() - Run the scheduler until all voices complete or stop() is called
 * This is a blocking call that processes voice resumes
 */
static int l_run(lua_State *L) {
    if (!sched.loop) {
        /* No voices spawned yet */
        return 0;
    }

    sched.running = 1;

    while (sched.active_count > 0 && sched.running) {
        /* Process any pending resumes */
        process_pending_resumes(L);

        /* Brief sleep if nothing pending */
        if (!scheduler_has_pending() && sched.active_count > 0) {
            uv_sleep(1);
        }
    }

    sched.running = 0;
    return 0;
}

/* poll() - Process any pending voice resumes without blocking
 * Returns true if there are still active voices, false otherwise
 * Use this for non-blocking playback while keeping REPL responsive
 *
 * Example:
 *   spawn(melody)
 *   while poll() do
 *       -- REPL stays responsive, can check input, etc.
 *   end
 */
static int l_poll(lua_State *L) {
    if (!sched.loop || sched.active_count == 0) {
        lua_pushboolean(L, 0);
        return 1;
    }

    /* Process any pending resumes */
    process_pending_resumes(L);

    /* Return true if voices still active */
    lua_pushboolean(L, sched.active_count > 0);
    return 1;
}

/* stop([voice_id]) - Stop a specific voice or all voices
 * With no argument, stops all voices
 */
static int l_stop(lua_State *L) {
    if (lua_gettop(L) >= 1 && !lua_isnil(L, 1)) {
        /* Stop specific voice */
        lua_Integer id = luaL_checkinteger(L, 1);
        Voice* v = find_voice_by_id((int)id);
        if (v) {
            v->stop_requested = 1;
            uv_timer_stop(&v->timer);
            cleanup_voice(v);
            lua_pushboolean(L, 1);
        } else {
            lua_pushboolean(L, 0);
        }
        return 1;
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
    return 0;
}

/* status() -> table
 * Returns {running=bool, active=int, voices={{id=n, name=s}, ...}}
 */
static int l_status(lua_State *L) {
    lua_newtable(L);

    lua_pushboolean(L, sched.running);
    lua_setfield(L, -2, "running");

    lua_pushinteger(L, sched.active_count);
    lua_setfield(L, -2, "active");

    /* Build voices array */
    lua_newtable(L);
    int idx = 1;
    for (int i = 0; i < MAX_VOICES; i++) {
        if (sched.voices[i].active) {
            lua_newtable(L);
            lua_pushinteger(L, sched.voices[i].voice_id);
            lua_setfield(L, -2, "id");
            lua_pushstring(L, sched.voices[i].name);
            lua_setfield(L, -2, "name");
            lua_pushboolean(L, sched.voices[i].waiting);
            lua_setfield(L, -2, "waiting");
            lua_rawseti(L, -2, idx++);
        }
    }
    lua_setfield(L, -2, "voices");

    return 1;
}

/* voices() -> int
 * Returns count of active voices (convenience function)
 */
static int l_voices(lua_State *L) {
    lua_pushinteger(L, sched.active_count);
    return 1;
}

/* ============================================================================
 * Module Registration
 * ============================================================================ */

static const luaL_Reg scheduler_funcs[] = {
    {"spawn", l_spawn},
    {"yield_ms", l_yield_ms},
    {"run", l_run},
    {"poll", l_poll},
    {"stop", l_stop},
    {"status", l_status},
    {"voices", l_voices},
    {NULL, NULL}
};

int luaopen_scheduler(lua_State *L) {
    luaL_newlib(L, scheduler_funcs);
    return 1;
}
