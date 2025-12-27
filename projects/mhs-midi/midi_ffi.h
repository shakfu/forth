/* midi_ffi.h - C FFI bindings for MicroHs MIDI library */
#ifndef MIDI_FFI_H
#define MIDI_FFI_H

#include <stdint.h>

/* Initialize the MIDI system - call once at startup */
int midi_init(void);

/* Cleanup the MIDI system */
void midi_cleanup(void);

/* List available MIDI output ports, returns count */
int midi_list_ports(void);

/* Get port name by index, returns pointer to static buffer */
const char* midi_port_name(int index);

/* Open a MIDI output port by index, returns 0 on success */
int midi_open(int port_index);

/* Open a virtual MIDI port with given name, returns 0 on success */
int midi_open_virtual(const char* name);

/* Close the current MIDI output */
void midi_close(void);

/* Check if MIDI output is open */
int midi_is_open(void);

/* Send a raw 3-byte MIDI message */
int midi_send(uint8_t status, uint8_t data1, uint8_t data2);

/* Send note on: channel (1-16), pitch (0-127), velocity (0-127) */
int midi_note_on(int channel, int pitch, int velocity);

/* Send note off: channel (1-16), pitch (0-127) */
int midi_note_off(int channel, int pitch);

/* Send control change: channel (1-16), controller (0-127), value (0-127) */
int midi_cc(int channel, int controller, int value);

/* Send program change: channel (1-16), program (0-127) */
int midi_program(int channel, int program);

/* Send pitch bend: channel (1-16), value (-8192 to 8191, 0 = center) */
int midi_pitch_bend(int channel, int value);

/* Sleep for given milliseconds */
void midi_sleep(int ms);

/* All notes off on all channels */
void midi_panic(void);

#endif /* MIDI_FFI_H */
