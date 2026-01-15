/*
 * joy_midi.c - Register MIDI primitives with Joy
 */

#include "joy_runtime.h"
#include "joy_midi.h"
#include "midi_primitives.h"
#include "music_notation.h"

void joy_midi_register_primitives(JoyContext* ctx) {
    JoyDict* dict = ctx->dictionary;

    /* Port management */
    joy_dict_define_primitive(dict, "midi-list", midi_list_);
    joy_dict_define_primitive(dict, "midi-virtual", midi_virtual_);
    joy_dict_define_primitive(dict, "midi-open", midi_open_);
    joy_dict_define_primitive(dict, "midi-close", midi_close_);

    /* Note operations */
    joy_dict_define_primitive(dict, "midi-note", midi_note_);
    joy_dict_define_primitive(dict, "midi-note-on", midi_note_on_);
    joy_dict_define_primitive(dict, "midi-note-off", midi_note_off_);
    joy_dict_define_primitive(dict, "midi-chord", midi_chord_);

    /* Control messages */
    joy_dict_define_primitive(dict, "midi-cc", midi_cc_);
    joy_dict_define_primitive(dict, "midi-program", midi_program_);
    joy_dict_define_primitive(dict, "midi-panic", midi_panic_);

    /* Utilities */
    joy_dict_define_primitive(dict, "midi-sleep", midi_sleep_);
    joy_dict_define_primitive(dict, "pitch", pitch_);
    joy_dict_define_primitive(dict, "tempo", tempo_);
    joy_dict_define_primitive(dict, "quant", quant_);
    joy_dict_define_primitive(dict, "vol", vol_);

    /* Music theory */
    joy_dict_define_primitive(dict, "major", major_chord_);
    joy_dict_define_primitive(dict, "minor", minor_chord_);
    joy_dict_define_primitive(dict, "dim", dim_chord_);
    joy_dict_define_primitive(dict, "aug", aug_chord_);
    joy_dict_define_primitive(dict, "dom7", dom7_chord_);
    joy_dict_define_primitive(dict, "maj7", maj7_chord_);
    joy_dict_define_primitive(dict, "min7", min7_chord_);
    joy_dict_define_primitive(dict, "transpose", transpose_);

    /* Musical notation playback */
    joy_dict_define_primitive(dict, "play", music_play_);
    joy_dict_define_primitive(dict, "chord", music_chord_);

    /* Channel operations */
    joy_dict_define_primitive(dict, "channel", channel_);
    joy_dict_define_primitive(dict, "chan", chan_);

    /* Initialize MIDI observer */
    midi_init();

    /* Set up post-eval hook for SEQ playback */
    ctx->post_eval_hook = accumulator_flush;
}

void joy_midi_cleanup(void) {
    midi_cleanup();
}
