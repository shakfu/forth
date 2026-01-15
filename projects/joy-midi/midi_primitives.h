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

#endif /* MIDI_PRIMITIVES_H */
