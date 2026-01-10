/**
 * @file test_shared_suite.c
 * @brief Test runner for shared Alda test suite.
 *
 * Validates alda-midi output against .expected files.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <dirent.h>

#include <alda/alda.h>
#include <alda/interpreter.h>
#include <alda/context.h>
#include <alda/scheduler.h>

/* ============================================================================
 * Configuration
 * ============================================================================ */

#define MAX_EXPECTED_NOTES    1024
#define MAX_EXPECTED_PROGRAMS 64
#define MAX_EXPECTED_CCS      256
#define MAX_EXPECTED_TEMPOS   64
#define TIME_TOLERANCE        0.01   /* 10ms tolerance */
#define DURATION_TOLERANCE    0.01

/* ============================================================================
 * Expected Data Structures
 * ============================================================================ */

typedef struct {
    int pitch;
    float start;
    float duration;
    int velocity;
    int channel;
} ExpectedNote;

typedef struct {
    int program;
    int channel;
    float time;
} ExpectedProgram;

typedef struct {
    int control;
    int value;
    int channel;
    float time;
} ExpectedCC;

typedef struct {
    float bpm;
    float time;
} ExpectedTempo;

typedef struct {
    ExpectedNote notes[MAX_EXPECTED_NOTES];
    int note_count;

    ExpectedProgram programs[MAX_EXPECTED_PROGRAMS];
    int program_count;

    ExpectedCC ccs[MAX_EXPECTED_CCS];
    int cc_count;

    ExpectedTempo tempos[MAX_EXPECTED_TEMPOS];
    int tempo_count;
} ExpectedOutput;

/* ============================================================================
 * Parsing .expected Files
 * ============================================================================ */

int parse_expected_file(const char* path, ExpectedOutput* out) {
    FILE* f = fopen(path, "r");
    if (!f) {
        fprintf(stderr, "Cannot open expected file: %s\n", path);
        return -1;
    }

    memset(out, 0, sizeof(ExpectedOutput));

    char line[256];
    while (fgets(line, sizeof(line), f)) {
        /* Skip comments and empty lines */
        if (line[0] == '#' || line[0] == '\n' || line[0] == '\r') {
            continue;
        }

        char type[16];
        if (sscanf(line, "%15s", type) != 1) {
            continue;
        }

        if (strcmp(type, "NOTE") == 0) {
            if (out->note_count >= MAX_EXPECTED_NOTES) continue;
            ExpectedNote* n = &out->notes[out->note_count];
            if (sscanf(line, "NOTE %d %f %f %d %d",
                       &n->pitch, &n->start, &n->duration,
                       &n->velocity, &n->channel) == 5) {
                out->note_count++;
            }
        }
        else if (strcmp(type, "PROGRAM") == 0) {
            if (out->program_count >= MAX_EXPECTED_PROGRAMS) continue;
            ExpectedProgram* p = &out->programs[out->program_count];
            if (sscanf(line, "PROGRAM %d %d %f",
                       &p->program, &p->channel, &p->time) == 3) {
                out->program_count++;
            }
        }
        else if (strcmp(type, "CC") == 0) {
            if (out->cc_count >= MAX_EXPECTED_CCS) continue;
            ExpectedCC* c = &out->ccs[out->cc_count];
            if (sscanf(line, "CC %d %d %d %f",
                       &c->control, &c->value, &c->channel, &c->time) == 4) {
                out->cc_count++;
            }
        }
        else if (strcmp(type, "TEMPO") == 0) {
            if (out->tempo_count >= MAX_EXPECTED_TEMPOS) continue;
            ExpectedTempo* t = &out->tempos[out->tempo_count];
            if (sscanf(line, "TEMPO %f %f", &t->bpm, &t->time) == 2) {
                out->tempo_count++;
            }
        }
    }

    fclose(f);
    return 0;
}

/* ============================================================================
 * Tempo Map for Tick-to-Second Conversion
 * ============================================================================ */

#define MAX_TEMPO_CHANGES 64

typedef struct {
    int tick;
    int tempo;
} TempoEntry;

typedef struct {
    TempoEntry entries[MAX_TEMPO_CHANGES];
    int count;
} TempoMap;

/* Build tempo map from context events */
void build_tempo_map(AldaContext* ctx, TempoMap* map, int default_tempo) {
    map->count = 0;

    /* Add initial tempo */
    map->entries[0].tick = 0;
    map->entries[0].tempo = default_tempo;
    map->count = 1;

    /* Find all tempo change events */
    for (int i = 0; i < ctx->event_count && map->count < MAX_TEMPO_CHANGES; i++) {
        AldaScheduledEvent* evt = &ctx->events[i];
        if (evt->type == ALDA_EVT_TEMPO) {
            map->entries[map->count].tick = evt->tick;
            map->entries[map->count].tempo = evt->data1;
            map->count++;
        }
    }
}

/* Get tempo at a specific tick position */
int get_tempo_at_tick(TempoMap* map, int tick) {
    int tempo = ALDA_DEFAULT_TEMPO;
    for (int i = 0; i < map->count; i++) {
        if (map->entries[i].tick <= tick) {
            tempo = map->entries[i].tempo;
        } else {
            break;
        }
    }
    return tempo;
}

/**
 * Convert tick position to seconds using tempo map.
 * Handles tempo changes by calculating time segment by segment.
 */
float ticks_to_seconds_with_map(int ticks, TempoMap* map) {
    float seconds = 0.0f;
    int current_tick = 0;

    for (int i = 0; i < map->count && current_tick < ticks; i++) {
        int segment_start = map->entries[i].tick;
        int segment_end = (i + 1 < map->count) ? map->entries[i + 1].tick : ticks;
        int tempo = map->entries[i].tempo;

        /* Clamp segment to our range */
        if (segment_start < current_tick) segment_start = current_tick;
        if (segment_end > ticks) segment_end = ticks;

        if (segment_end > segment_start) {
            int delta_ticks = segment_end - segment_start;
            seconds += (float)delta_ticks / ALDA_TICKS_PER_QUARTER * 60.0f / (float)tempo;
            current_tick = segment_end;
        }
    }

    return seconds;
}

/**
 * Convert tick duration to seconds at a specific tempo.
 */
float duration_to_seconds(int duration_ticks, int tempo) {
    return (float)duration_ticks / ALDA_TICKS_PER_QUARTER * 60.0f / (float)tempo;
}

/**
 * Convert tick position to seconds (legacy, single tempo).
 */
float ticks_to_seconds(int ticks, int tempo) {
    return (float)ticks / ALDA_TICKS_PER_QUARTER * 60.0f / (float)tempo;
}

/* ============================================================================
 * Compare Functions
 * ============================================================================ */

int compare_floats(float a, float b, float tolerance) {
    return fabsf(a - b) < tolerance;
}

/* Comparison function for sorting notes by start time then pitch */
int compare_notes_by_start_pitch(const void* a, const void* b) {
    const ExpectedNote* na = (const ExpectedNote*)a;
    const ExpectedNote* nb = (const ExpectedNote*)b;
    if (na->start != nb->start) {
        return (na->start > nb->start) ? 1 : -1;
    }
    return na->pitch - nb->pitch;
}

/* ============================================================================
 * Extract Notes from Context Events
 * ============================================================================ */

typedef struct {
    int pitch;
    int channel;
    int start_tick;
    int velocity;
} PendingNote;

int extract_notes_from_context(AldaContext* ctx, ExpectedNote* notes, int max_notes, TempoMap* tempo_map) {
    int note_count = 0;
    PendingNote pending[256];
    int pending_count = 0;

    /* Sort events first */
    alda_events_sort(ctx);

    for (int i = 0; i < ctx->event_count && note_count < max_notes; i++) {
        AldaScheduledEvent* evt = &ctx->events[i];

        if (evt->type == ALDA_EVT_NOTE_ON) {
            /* Store pending note-on */
            if (pending_count < 256) {
                pending[pending_count].pitch = evt->data1;
                pending[pending_count].channel = evt->channel;
                pending[pending_count].start_tick = evt->tick;
                pending[pending_count].velocity = evt->data2;
                pending_count++;
            }
        }
        else if (evt->type == ALDA_EVT_NOTE_OFF) {
            /* Find matching note-on and create note */
            for (int j = 0; j < pending_count; j++) {
                if (pending[j].pitch == evt->data1 && pending[j].channel == evt->channel) {
                    ExpectedNote* n = &notes[note_count];
                    n->pitch = pending[j].pitch;
                    n->channel = pending[j].channel;
                    /* Use tempo map for accurate time conversion */
                    n->start = ticks_to_seconds_with_map(pending[j].start_tick, tempo_map);
                    /* Duration uses tempo at start of note */
                    int tempo_at_note = get_tempo_at_tick(tempo_map, pending[j].start_tick);
                    n->duration = duration_to_seconds(evt->tick - pending[j].start_tick, tempo_at_note);
                    n->velocity = pending[j].velocity;
                    note_count++;

                    /* Remove from pending */
                    pending[j] = pending[pending_count - 1];
                    pending_count--;
                    break;
                }
            }
        }
    }

    return note_count;
}

int extract_programs_from_context(AldaContext* ctx, ExpectedProgram* programs, int max, int tempo) {
    int count = 0;
    for (int i = 0; i < ctx->event_count && count < max; i++) {
        AldaScheduledEvent* evt = &ctx->events[i];
        if (evt->type == ALDA_EVT_PROGRAM) {
            programs[count].program = evt->data1;
            programs[count].channel = evt->channel;
            programs[count].time = ticks_to_seconds(evt->tick, tempo);
            count++;
        }
    }
    return count;
}

int extract_ccs_from_context(AldaContext* ctx, ExpectedCC* ccs, int max, int tempo) {
    int count = 0;
    for (int i = 0; i < ctx->event_count && count < max; i++) {
        AldaScheduledEvent* evt = &ctx->events[i];
        if (evt->type == ALDA_EVT_CC || evt->type == ALDA_EVT_PAN) {
            ccs[count].control = evt->data1;
            ccs[count].value = evt->data2;
            ccs[count].channel = evt->channel;
            ccs[count].time = ticks_to_seconds(evt->tick, tempo);
            count++;
        }
    }
    return count;
}

/* ============================================================================
 * Test Runner
 * ============================================================================ */

typedef struct {
    int passed;
    int failed;
    int skipped;
} TestStats;

int run_test(const char* alda_path, const char* expected_path, TestStats* stats) {
    ExpectedOutput expected;
    if (parse_expected_file(expected_path, &expected) != 0) {
        stats->skipped++;
        return -1;
    }

    /* Initialize context */
    AldaContext ctx;
    alda_context_init(&ctx);
    alda_set_no_sleep(&ctx, 1);  /* Disable timing for tests */

    /* Parse and interpret */
    if (alda_interpret_file(&ctx, alda_path) != 0) {
        fprintf(stderr, "FAIL: %s - parse/interpret error\n", alda_path);
        alda_context_cleanup(&ctx);
        stats->failed++;
        return -1;
    }

    /* Extract actual output */
    int default_tempo = ctx.global_tempo > 0 ? ctx.global_tempo : ALDA_DEFAULT_TEMPO;

    /* Build tempo map for accurate timing conversions */
    TempoMap tempo_map;
    build_tempo_map(&ctx, &tempo_map, default_tempo);

    ExpectedNote actual_notes[MAX_EXPECTED_NOTES];
    int actual_note_count = extract_notes_from_context(&ctx, actual_notes, MAX_EXPECTED_NOTES, &tempo_map);

    ExpectedProgram actual_programs[MAX_EXPECTED_PROGRAMS];
    int actual_program_count = extract_programs_from_context(&ctx, actual_programs, MAX_EXPECTED_PROGRAMS, default_tempo);

    ExpectedCC actual_ccs[MAX_EXPECTED_CCS];
    int actual_cc_count = extract_ccs_from_context(&ctx, actual_ccs, MAX_EXPECTED_CCS, default_tempo);

    /* Compare note counts */
    int passed = 1;

    if (actual_note_count != expected.note_count) {
        fprintf(stderr, "FAIL: %s - note count mismatch (expected %d, got %d)\n",
                alda_path, expected.note_count, actual_note_count);
        passed = 0;
    }

    /* Sort notes for comparison */
    qsort(actual_notes, actual_note_count, sizeof(ExpectedNote), compare_notes_by_start_pitch);
    qsort(expected.notes, expected.note_count, sizeof(ExpectedNote), compare_notes_by_start_pitch);

    /* Compare notes */
    int min_notes = actual_note_count < expected.note_count ? actual_note_count : expected.note_count;
    for (int i = 0; i < min_notes && passed; i++) {
        ExpectedNote* a = &actual_notes[i];
        ExpectedNote* e = &expected.notes[i];

        if (a->pitch != e->pitch) {
            fprintf(stderr, "FAIL: %s - note %d pitch mismatch (expected %d, got %d)\n",
                    alda_path, i, e->pitch, a->pitch);
            passed = 0;
        }
        if (!compare_floats(a->start, e->start, TIME_TOLERANCE)) {
            fprintf(stderr, "FAIL: %s - note %d start mismatch (expected %.4f, got %.4f)\n",
                    alda_path, i, e->start, a->start);
            passed = 0;
        }
        if (!compare_floats(a->duration, e->duration, DURATION_TOLERANCE)) {
            fprintf(stderr, "FAIL: %s - note %d duration mismatch (expected %.4f, got %.4f)\n",
                    alda_path, i, e->duration, a->duration);
            passed = 0;
        }
        if (a->velocity != e->velocity) {
            fprintf(stderr, "FAIL: %s - note %d velocity mismatch (expected %d, got %d)\n",
                    alda_path, i, e->velocity, a->velocity);
            passed = 0;
        }
    }

    alda_context_cleanup(&ctx);

    if (passed) {
        printf("PASS: %s (%d notes)\n", alda_path, actual_note_count);
        stats->passed++;
    } else {
        stats->failed++;
    }

    return passed ? 0 : -1;
}

/* ============================================================================
 * Main
 * ============================================================================ */

int main(int argc, char** argv) {
    const char* suite_dir = NULL;

    if (argc >= 2) {
        suite_dir = argv[1];
    } else {
        /* Default path relative to build directory */
        suite_dir = "../docs/alda-midi/shared_suite";
    }

    printf("Running shared test suite from: %s\n\n", suite_dir);

    DIR* dir = opendir(suite_dir);
    if (!dir) {
        fprintf(stderr, "Cannot open suite directory: %s\n", suite_dir);
        return 1;
    }

    TestStats stats = {0, 0, 0};

    struct dirent* entry;
    while ((entry = readdir(dir)) != NULL) {
        /* Find .alda files */
        const char* name = entry->d_name;
        size_t len = strlen(name);
        if (len < 5 || strcmp(name + len - 5, ".alda") != 0) {
            continue;
        }

        /* Build paths */
        char alda_path[512];
        char expected_path[512];
        snprintf(alda_path, sizeof(alda_path), "%s/%s", suite_dir, name);

        /* Replace .alda with .expected */
        snprintf(expected_path, sizeof(expected_path), "%s/%s", suite_dir, name);
        strcpy(expected_path + strlen(expected_path) - 5, ".expected");

        /* Check if .expected exists */
        FILE* f = fopen(expected_path, "r");
        if (!f) {
            printf("SKIP: %s (no .expected file)\n", name);
            stats.skipped++;
            continue;
        }
        fclose(f);

        run_test(alda_path, expected_path, &stats);
    }

    closedir(dir);

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed, %d skipped\n",
           stats.passed, stats.failed, stats.skipped);
    printf("========================================\n");

    return stats.failed > 0 ? 1 : 0;
}
