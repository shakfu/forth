# MIDI Language Specification

This document specifies the required features for language implementations in the midi-langs project. It is implementation-agnostic and serves as a guide for adding new languages.

## Overview

A conforming implementation provides:

1. **MIDI I/O** - Port management and message sending
2. **Pitch System** - Note representation and manipulation
3. **Music Theory** - Chords, scales, and intervals
4. **Timing** - Tempo, durations, and synchronization
5. **Recording** - Event capture and file I/O

Implementations may use the shared `music_theory` C library or implement these features natively.

---

## 1. MIDI I/O

### 1.1 Port Management

| Function | Description |
|----------|-------------|
| `list-ports` | List available MIDI output ports with indices and names |
| `open` | Open a virtual MIDI port with default name |
| `open(name)` | Open a virtual MIDI port with custom name |
| `open(index)` | Open a hardware port by index |
| `close` | Close the current port, send all-notes-off |
| `is-open?` | Check if port is currently open |

**Requirements:**
- Virtual ports must appear in system MIDI routing (e.g., macOS IAC, ALSA virtual)
- `close` must send CC 123 (All Notes Off) on all 16 channels before closing
- A "panic" function should be available to immediately silence all notes

### 1.2 Note Messages

| Function | Parameters | Description |
|----------|------------|-------------|
| `note-on` | pitch, velocity, channel | Send Note On (0x90) |
| `note-off` | pitch, [velocity], channel | Send Note Off (0x80) |
| `note` | pitch, [velocity], [duration], [channel] | Play note for duration |

**Requirements:**
- `pitch`: 0-127 (MIDI note number)
- `velocity`: 0-127, default 80 (mezzo-forte)
- `duration`: milliseconds, default 500
- `channel`: 1-16 (user-facing), internally 0-15

**Semantics:**
- `note-on`/`note-off`: Send message immediately, return immediately
- `note`: Send note-on, wait for duration, send note-off, then return

### 1.3 Chord and Arpeggio

| Function | Parameters | Description |
|----------|------------|-------------|
| `chord` | pitches, [velocity], [duration], [channel] | Play notes simultaneously |
| `arpeggio` | pitches, [velocity], [duration], [channel] | Play notes sequentially |

**Requirements:**
- `pitches`: List/array of MIDI note numbers
- `chord`: All notes start and end together
- `arpeggio`: Each note plays for `duration`, total time = `duration * count`

### 1.4 Control Messages

| Function | Parameters | Description |
|----------|------------|-------------|
| `cc` | channel, controller, value | Control Change (0xB0) |
| `program` | channel, program | Program Change (0xC0) |
| `pitch-bend` | channel, value | Pitch Bend (0xE0) |
| `all-notes-off` | [channel] | CC 123 on channel or all |

**Common CC Numbers:**
| Number | Name | Purpose |
|--------|------|---------|
| 1 | Modulation | Vibrato/tremolo |
| 7 | Volume | Channel volume |
| 10 | Pan | Stereo position |
| 11 | Expression | Dynamic control |
| 64 | Sustain | Damper pedal |
| 91 | Reverb | Effect send |
| 93 | Chorus | Effect send |

**Pitch Bend:**
- Range: 0-16383
- Center (no bend): 8192
- Standard bend range: +/- 2 semitones

---

## 2. Pitch System

### 2.1 Pitch Constants

All pitches from C-1 (MIDI 0) to G9 (MIDI 127) must be available.

**Naming Convention:**
- Naturals: `c4`, `d4`, `e4`, `f4`, `g4`, `a4`, `b4`
- Sharps: `cs4` or `c#4` (implementation-dependent)
- Flats: `db4`, `eb4`, `gb4`, `ab4`, `bb4`

**Middle C (C4) = MIDI 60**

### 2.2 Pitch Parsing

| Function | Input | Output |
|----------|-------|--------|
| `note` or `parse-pitch` | string | MIDI number |

**Accepted Formats:**
- `"C4"` -> 60
- `"c4"` -> 60 (case-insensitive)
- `"C#4"` or `"Cs4"` -> 61
- `"Db4"` -> 61
- `"C-1"` -> 0
- `"G9"` -> 127

### 2.3 Pitch Manipulation

| Function | Description |
|----------|-------------|
| `transpose(pitch, semitones)` | Add semitones to pitch |
| `octave-up(pitch)` | Transpose +12 |
| `octave-down(pitch)` | Transpose -12 |

---

## 3. Music Theory

### 3.1 Chord Builders

All chord builders take a root pitch and return a list of pitches.

| Function | Intervals | Example (C4) |
|----------|-----------|--------------|
| `major` | 0, 4, 7 | 60, 64, 67 |
| `minor` | 0, 3, 7 | 60, 63, 67 |
| `dim` | 0, 3, 6 | 60, 63, 66 |
| `aug` | 0, 4, 8 | 60, 64, 68 |
| `dom7` | 0, 4, 7, 10 | 60, 64, 67, 70 |
| `maj7` | 0, 4, 7, 11 | 60, 64, 67, 71 |
| `min7` | 0, 3, 7, 10 | 60, 63, 67, 70 |

**Optional Extended Chords:**
- `dim7`: 0, 3, 6, 9
- `half-dim7`: 0, 3, 6, 10
- `sus2`: 0, 2, 7
- `sus4`: 0, 5, 7

### 3.2 Scale Constants

Scales are represented as arrays of semitone intervals from the root.

**Required Scales (minimum set):**

| Scale | Intervals |
|-------|-----------|
| major | 0, 2, 4, 5, 7, 9, 11 |
| minor | 0, 2, 3, 5, 7, 8, 10 |
| dorian | 0, 2, 3, 5, 7, 9, 10 |
| phrygian | 0, 1, 3, 5, 7, 8, 10 |
| lydian | 0, 2, 4, 6, 7, 9, 11 |
| mixolydian | 0, 2, 4, 5, 7, 9, 10 |
| locrian | 0, 1, 3, 5, 6, 8, 10 |
| harmonic-minor | 0, 2, 3, 5, 7, 8, 11 |
| melodic-minor | 0, 2, 3, 5, 7, 9, 11 |
| pentatonic | 0, 2, 4, 7, 9 |
| pentatonic-minor | 0, 3, 5, 7, 10 |
| blues | 0, 3, 5, 6, 7, 10 |
| whole-tone | 0, 2, 4, 6, 8, 10 |
| chromatic | 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 |

**Recommended Additional Scales:**

See `music_theory.h` for the full list of 49 scales including:
- Bebop scales (dominant, major, minor)
- World scales (Hungarian minor, double harmonic, Persian, etc.)
- Japanese scales (hirajoshi, in-sen, iwato, kumoi)
- Arabic Maqamat (hijaz, nahawand, nikriz, etc.)
- Indian Ragas (bhairav, todi, marwa, etc.)

### 3.3 Scale Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `build-scale` | (root, intervals) -> pitches | Build one octave of scale |
| `scale-degree` | (root, intervals, degree) -> pitch | Get nth degree (1-based) |
| `in-scale?` | (pitch, root, intervals) -> bool | Check membership |
| `quantize` | (pitch, root, intervals) -> pitch | Snap to nearest scale tone |

**Scale Degree Rules:**
- Degrees are 1-based: 1 = root, 2 = second, etc.
- Extended degrees wrap with octaves: 8 = root + octave, 9 = second + octave
- Example: degree 9 of C major = D5 (MIDI 74)

---

## 4. Timing

### 4.1 Duration Constants

Based on 120 BPM (default tempo):

| Constant | Milliseconds | Musical Value |
|----------|--------------|---------------|
| `whole` | 2000 | Whole note |
| `half` | 1000 | Half note |
| `quarter` | 500 | Quarter note |
| `eighth` | 250 | Eighth note |
| `sixteenth` | 125 | Sixteenth note |

### 4.2 Duration Helpers

| Function | Description |
|----------|-------------|
| `dotted(duration)` | Return 1.5x duration |
| `bpm(tempo)` | Return quarter note duration for tempo |

### 4.3 Tempo

| Function | Description |
|----------|-------------|
| `set-tempo(bpm)` | Set global tempo, update duration constants |
| `get-tempo()` | Get current tempo |

**Requirements:**
- Tempo range: 20-300 BPM
- Changing tempo should update duration constants proportionally

### 4.4 Sleep/Rest

| Function | Description |
|----------|-------------|
| `sleep(ms)` | Sleep for milliseconds |
| `rest([duration])` | Alias for sleep, defaults to quarter |

---

## 5. Dynamics

Velocity presets using standard musical dynamics:

| Constant | Velocity | Dynamic |
|----------|----------|---------|
| `ppp` | 16 | Pianississimo |
| `pp` | 33 | Pianissimo |
| `p` | 49 | Piano |
| `mp` | 64 | Mezzo-piano |
| `mf` | 80 | Mezzo-forte |
| `f` | 96 | Forte |
| `ff` | 112 | Fortissimo |
| `fff` | 127 | Fortississimo |

**Default velocity: `mf` (80)**

---

## 6. Recording and File I/O

### 6.1 MIDI Event Recording

| Function | Description |
|----------|-------------|
| `record-start([bpm])` | Start recording events with timestamps |
| `record-stop()` | Stop recording, return event count |
| `record-status()` | Return (active?, count, bpm) |

**Recorded Event Types:**
- Note On (pitch, velocity, channel, timestamp)
- Note Off (pitch, velocity, channel, timestamp)
- Control Change (controller, value, channel, timestamp)

### 6.2 Save Formats

| Function | Description |
|----------|-------------|
| `save-source(filename)` | Save as executable source file |
| `write-mid(filename)` | Save as standard MIDI file |

**Source File Requirements:**
- Must be valid source code in the implementation language
- When executed, should reproduce the recorded performance
- Should include timing/tempo metadata

**MIDI File Requirements:**
- Format 0 or 1
- Single track for recorded events
- Correct delta times based on PPQN (typically 480)

### 6.3 Read MIDI Files

| Function | Description |
|----------|-------------|
| `read-mid(filename)` | Read and parse MIDI file |

**Return Value:**
- Number of tracks
- PPQN (pulses per quarter note)
- Tempo (microseconds per beat)
- Total duration (milliseconds)
- Event list with: track, tick, channel, type, data1, data2

---

## 7. Microtonal Support (Optional)

For implementations supporting quarter tones and other microtonal intervals.

### 7.1 Pitch Bend Helpers

| Function | Description |
|----------|-------------|
| `cents-to-bend(cents)` | Convert cents to pitch bend value |
| `pitch-bend-cents(channel, cents)` | Send pitch bend in cents |

**Conversion (assuming +/- 2 semitone range):**
```
bend = 8192 + (cents * 8192 / 200)
```

### 7.2 Microtonal Scales (Cents)

For scales with quarter tones, provide cents-based variants:

| Scale | Cents Intervals |
|-------|-----------------|
| maqam-bayati | 0, 150, 300, 500, 700, 800, 1000 |
| maqam-rast | 0, 200, 350, 500, 700, 900, 1050 |
| shruti (22-tone) | Full 22-shruti intervals |

### 7.3 Cents-to-Note Conversion

| Function | Description |
|----------|-------------|
| `cents-to-note(root, cents)` | Return (midi-note, bend-cents) pair |

**Algorithm:**
1. Calculate base note: `root + floor(cents / 100)`
2. Calculate bend: `cents mod 100`

---

## 8. Generative Features (Optional)

### 8.1 Random

| Function | Description |
|----------|-------------|
| `random()` | Return random number (typically 0-99 or 0.0-1.0) |
| `random-range(lo, hi)` | Return random in range |
| `seed(n)` | Seed the random generator |

### 8.2 Selection

| Function | Description |
|----------|-------------|
| `pick(list)` | Select random element |
| `shuffle(list)` | Return shuffled list |

### 8.3 Probability

| Function | Description |
|----------|-------------|
| `chance(percent, action)` | Execute action with probability |
| `weighted-choice(items, weights)` | Select based on weights |

### 8.4 Euclidean Rhythms

| Function | Description |
|----------|-------------|
| `euclidean(hits, steps)` | Return Bjorklund pattern as boolean list |

---

## 9. REPL Convenience (Optional)

For interactive use, provide short aliases:

| Short | Full | Description |
|-------|------|-------------|
| `n` | `note` | Play single note |
| `ch` | `chord` | Play chord |
| `arp` | `arpeggio` | Arpeggiate |

**Global Port Variable:**
- Provide a global/default MIDI output for REPL use
- `open()` should set this global
- Short functions use this global

---

## 10. Testing Requirements

Implementations must include tests covering:

1. **Port Management** - Open, close, list ports
2. **Note Playback** - Single notes, chords, arpeggios
3. **Pitch Parsing** - All note name formats
4. **Chord Builders** - All chord types
5. **Scale Operations** - Build, degree, quantize
6. **Timing** - Tempo changes, duration calculations
7. **Recording** - Capture and playback

---

## 11. Integration

### 11.1 Shared Library

Implementations may use the shared `music_theory` C library:

```c
#include "music_theory.h"

// Pitch parsing
int pitch = music_parse_pitch("C#4");  // 61

// Chord building
int chord[4];
int count = music_build_chord(60, CHORD_MAJOR, 3, chord);

// Scale operations
int scale[7];
music_build_scale(60, SCALE_MAJOR, 7, scale);
int degree = music_scale_degree(60, SCALE_MAJOR, 7, 5);  // 67 (G4)
```

### 11.2 MIDI FFI

Use the `midi_ffi` library for cross-platform MIDI I/O:

```c
#include "midi_ffi.h"

midi_init();
midi_open_virtual("MyApp");
midi_note_on(1, 60, 80);  // channel, pitch, velocity
midi_sleep(500);
midi_note_off(1, 60);
midi_close();
```

### 11.3 Documentation

Each implementation should provide:

1. **README.md** - Overview, quick start, examples
2. **api-reference.md** - Complete function reference
3. **examples/** - Working example programs

---

## Appendix A: MIDI Message Reference

| Message | Status | Data 1 | Data 2 |
|---------|--------|--------|--------|
| Note Off | 0x80+ch | pitch | velocity |
| Note On | 0x90+ch | pitch | velocity |
| Control Change | 0xB0+ch | controller | value |
| Program Change | 0xC0+ch | program | - |
| Pitch Bend | 0xE0+ch | LSB | MSB |

**Channel:** 0-15 internally, 1-16 user-facing

**Pitch Bend:** 14-bit value (0-16383), center = 8192

---

## Appendix B: Example Implementation Checklist

- [ ] Port management (list, open virtual, open hardware, close)
- [ ] Note on/off messages
- [ ] Blocking note playback
- [ ] Chord playback
- [ ] Arpeggio playback
- [ ] Control Change messages
- [ ] Program Change
- [ ] Pitch Bend
- [ ] All Notes Off / Panic
- [ ] Pitch constants (C-1 to G9)
- [ ] Pitch parsing (note names to MIDI)
- [ ] Chord builders (major, minor, dim, aug, dom7, maj7, min7)
- [ ] Scale constants (minimum 14 scales)
- [ ] Scale operations (build, degree, in-scale?, quantize)
- [ ] Duration constants (whole, half, quarter, eighth, sixteenth)
- [ ] Tempo management
- [ ] Dynamics constants (ppp to fff)
- [ ] Sleep/rest functions
- [ ] Recording start/stop
- [ ] Save to source file
- [ ] Write to MIDI file
- [ ] Read MIDI file
- [ ] Help/documentation command
- [ ] Test suite
