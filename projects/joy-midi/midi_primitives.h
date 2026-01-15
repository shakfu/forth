/*
 * midi_primitives.h - MIDI primitive declarations for Joy
 */

#ifndef MIDI_PRIMITIVES_H
#define MIDI_PRIMITIVES_H

#include "joy_runtime.h"

/* Port management */
void midi_list_(JoyContext* ctx);
void midi_virtual_(JoyContext* ctx);
void midi_open_(JoyContext* ctx);
void midi_close_(JoyContext* ctx);

/* Note operations */
void midi_note_(JoyContext* ctx);
void midi_note_on_(JoyContext* ctx);
void midi_note_off_(JoyContext* ctx);
void midi_chord_(JoyContext* ctx);

/* Control messages */
void midi_cc_(JoyContext* ctx);
void midi_program_(JoyContext* ctx);
void midi_panic_(JoyContext* ctx);

/* Utilities */
void midi_sleep_(JoyContext* ctx);
void pitch_(JoyContext* ctx);
void tempo_(JoyContext* ctx);
void quant_(JoyContext* ctx);
void vol_(JoyContext* ctx);

/* Music theory */
void major_chord_(JoyContext* ctx);
void minor_chord_(JoyContext* ctx);
void dim_chord_(JoyContext* ctx);
void aug_chord_(JoyContext* ctx);
void dom7_chord_(JoyContext* ctx);
void maj7_chord_(JoyContext* ctx);
void min7_chord_(JoyContext* ctx);
void transpose_(JoyContext* ctx);

/* Channel operations */
void channel_(JoyContext* ctx);
void chan_(JoyContext* ctx);

/* Init/cleanup */
void midi_init(void);
void midi_cleanup(void);

/* Low-level MIDI send (used by music_notation.c) */
void send_note_on(int pitch, int velocity);
void send_note_off(int pitch);

/* ============================================================================
 * Schedule System - for parallel parts and sequence composition
 * ============================================================================ */

/* A single scheduled MIDI event */
typedef struct {
    int time_ms;        /* Start time relative to sequence start */
    int channel;        /* MIDI channel (1-16) */
    int pitch;          /* MIDI pitch (0-127) */
    int velocity;       /* Velocity (0-127) */
    int duration_ms;    /* Note duration in ms */
} ScheduledEvent;

/* A MIDI schedule (collection of events) */
typedef struct {
    ScheduledEvent* events;
    size_t count;
    size_t capacity;
    int total_duration_ms;  /* Length of the schedule */
} MidiSchedule;

/* Schedule management */
MidiSchedule* schedule_new(void);
void schedule_free(MidiSchedule* sched);
void schedule_add_event(MidiSchedule* sched, int time_ms, int channel,
                        int pitch, int velocity, int duration_ms);
void schedule_play(MidiSchedule* sched);

/* Scheduling mode - when active, play adds to schedule instead of playing */
void schedule_begin(int channel);  /* Start scheduling mode for a channel */
void schedule_end(void);           /* End scheduling mode */
bool is_scheduling(void);          /* Check if in scheduling mode */
int get_schedule_channel(void);    /* Get current scheduling channel */
int get_schedule_time(void);       /* Get current time offset in schedule */
void advance_schedule_time(int ms);/* Advance time in current schedule */

/* Global accumulator for sequence composition */
void accumulator_init(void);
void accumulator_add_schedule(MidiSchedule* sched);
void accumulator_flush(void);      /* Play and clear the accumulator */
int accumulator_get_offset(void);  /* Get current time offset */
void accumulator_advance(int ms);  /* Advance offset for next sequence */

/* Current schedule access (for building schedules) */
MidiSchedule* get_current_schedule(void);
void clear_current_schedule(void);

#endif /* MIDI_PRIMITIVES_H */
