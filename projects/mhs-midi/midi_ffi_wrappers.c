/* midi_ffi_wrappers.c - MicroHs FFI wrappers for MIDI functions
 *
 * These wrappers convert between MicroHs runtime stack representation
 * and C function calls, enabling the MIDI FFI to work in the REPL.
 */

#include "mhsffi.h"
#include "midi_ffi.h"

/* ========================================================================
 * 0-arity functions
 * ======================================================================== */

from_t mhs_midi_init(int s) {
    return mhs_from_Int(s, 0, midi_init());
}

from_t mhs_midi_cleanup(int s) {
    midi_cleanup();
    return mhs_from_Unit(s, 0);
}

from_t mhs_midi_list_ports(int s) {
    return mhs_from_Int(s, 0, midi_list_ports());
}

from_t mhs_midi_close(int s) {
    midi_close();
    return mhs_from_Unit(s, 0);
}

from_t mhs_midi_is_open(int s) {
    return mhs_from_Int(s, 0, midi_is_open());
}

from_t mhs_midi_panic(int s) {
    midi_panic();
    return mhs_from_Unit(s, 0);
}

/* ========================================================================
 * 1-arity functions
 * ======================================================================== */

from_t mhs_midi_port_name(int s) {
    return mhs_from_Ptr(s, 1, (void*)midi_port_name(mhs_to_Int(s, 0)));
}

from_t mhs_midi_open(int s) {
    return mhs_from_Int(s, 1, midi_open(mhs_to_Int(s, 0)));
}

from_t mhs_midi_open_virtual(int s) {
    return mhs_from_Int(s, 1, midi_open_virtual(mhs_to_Ptr(s, 0)));
}

from_t mhs_midi_sleep(int s) {
    midi_sleep(mhs_to_Int(s, 0));
    return mhs_from_Unit(s, 1);
}

from_t mhs_midi_cents_to_bend(int s) {
    return mhs_from_Int(s, 1, midi_cents_to_bend(mhs_to_Int(s, 0)));
}

/* ========================================================================
 * 2-arity functions
 * ======================================================================== */

from_t mhs_midi_note_off(int s) {
    return mhs_from_Int(s, 2, midi_note_off(mhs_to_Int(s, 0), mhs_to_Int(s, 1)));
}

from_t mhs_midi_program(int s) {
    return mhs_from_Int(s, 2, midi_program(mhs_to_Int(s, 0), mhs_to_Int(s, 1)));
}

from_t mhs_midi_pitch_bend(int s) {
    return mhs_from_Int(s, 2, midi_pitch_bend(mhs_to_Int(s, 0), mhs_to_Int(s, 1)));
}

/* ========================================================================
 * 3-arity functions
 * ======================================================================== */

from_t mhs_midi_send(int s) {
    return mhs_from_Int(s, 3, midi_send(
        (uint8_t)mhs_to_Int(s, 0),
        (uint8_t)mhs_to_Int(s, 1),
        (uint8_t)mhs_to_Int(s, 2)));
}

from_t mhs_midi_note_on(int s) {
    return mhs_from_Int(s, 3, midi_note_on(
        mhs_to_Int(s, 0),
        mhs_to_Int(s, 1),
        mhs_to_Int(s, 2)));
}

from_t mhs_midi_cc(int s) {
    return mhs_from_Int(s, 3, midi_cc(
        mhs_to_Int(s, 0),
        mhs_to_Int(s, 1),
        mhs_to_Int(s, 2)));
}

/* ========================================================================
 * Recording functions
 * ======================================================================== */

from_t mhs_midi_record_start(int s) {
    return mhs_from_Int(s, 1, midi_record_start(mhs_to_Int(s, 0)));
}

from_t mhs_midi_record_stop(int s) {
    return mhs_from_Int(s, 0, midi_record_stop());
}

from_t mhs_midi_save_mid(int s) {
    return mhs_from_Int(s, 1, midi_save_mid(mhs_to_Ptr(s, 0)));
}

from_t mhs_midi_save_hs(int s) {
    return mhs_from_Int(s, 1, midi_save_hs(mhs_to_Ptr(s, 0)));
}

from_t mhs_midi_record_count(int s) {
    return mhs_from_Int(s, 0, midi_record_count());
}

from_t mhs_midi_record_active(int s) {
    return mhs_from_Int(s, 0, midi_record_active());
}

/* ========================================================================
 * MIDI File I/O functions
 * ======================================================================== */

from_t mhs_midi_read_mid(int s) {
    return mhs_from_Int(s, 1, midi_read_mid(mhs_to_Ptr(s, 0)));
}

/* ========================================================================
 * FFI table - names must match Midi.hs foreign import declarations
 * ======================================================================== */

static const struct ffi_entry midi_ffi_table[] = {
    /* 0-arity */
    { "midi_init",       0, mhs_midi_init },
    { "midi_cleanup",    0, mhs_midi_cleanup },
    { "midi_list_ports", 0, mhs_midi_list_ports },
    { "midi_close",      0, mhs_midi_close },
    { "midi_is_open",    0, mhs_midi_is_open },
    { "midi_panic",      0, mhs_midi_panic },
    /* 1-arity */
    { "midi_port_name",     1, mhs_midi_port_name },
    { "midi_open",          1, mhs_midi_open },
    { "midi_open_virtual",  1, mhs_midi_open_virtual },
    { "midi_sleep",         1, mhs_midi_sleep },
    { "midi_cents_to_bend", 1, mhs_midi_cents_to_bend },
    /* 2-arity */
    { "midi_note_off",   2, mhs_midi_note_off },
    { "midi_program",    2, mhs_midi_program },
    { "midi_pitch_bend", 2, mhs_midi_pitch_bend },
    /* 3-arity */
    { "midi_send",    3, mhs_midi_send },
    { "midi_note_on", 3, mhs_midi_note_on },
    { "midi_cc",      3, mhs_midi_cc },
    /* recording functions */
    { "midi_record_start",   1, mhs_midi_record_start },
    { "midi_record_stop",    0, mhs_midi_record_stop },
    { "midi_save_mid", 1, mhs_midi_save_mid },
    { "midi_save_hs", 1, mhs_midi_save_hs },
    { "midi_record_count",   0, mhs_midi_record_count },
    { "midi_record_active",  0, mhs_midi_record_active },
    /* MIDI file I/O */
    { "midi_read_mid",      1, mhs_midi_read_mid },
    /* sentinel */
    { 0, 0, 0 }
};

/* Override xffi_table - this provides our MIDI FFI to the MicroHs runtime */
const struct ffi_entry *xffi_table = midi_ffi_table;
