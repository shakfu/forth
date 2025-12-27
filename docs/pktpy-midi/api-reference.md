# API Reference

## Module Functions

```python
import midi
```

### midi.open

```python
midi.open() -> MidiOut
midi.open(name: str) -> MidiOut
midi.open(index: int) -> MidiOut
```

Open a MIDI output port.

- `open()` - Create virtual port named "pktpyMIDI"
- `open("MyApp")` - Create virtual port with custom name
- `open(0)` - Open hardware port by index

```python
m = midi.open()                  # Virtual port
m = midi.open("MySynth")         # Named virtual port
m = midi.open(0)                 # First hardware port
```

### midi.list_ports

```python
midi.list_ports() -> list[tuple[int, str]]
```

List available MIDI output ports. Returns list of (index, name) tuples.

```python
for idx, name in midi.list_ports():
    print(f"{idx}: {name}")
```

### midi.note

```python
midi.note(name: str) -> int
```

Parse note name to MIDI number.

- Supports: C, D, E, F, G, A, B
- Accidentals: # or s (sharp), b (flat)
- Octaves: -1 to 9

```python
midi.note("C4")   # 60
midi.note("C#4")  # 61
midi.note("Db4")  # 61
midi.note("A4")   # 69
```

---

## MidiOut Class

The `MidiOut` class represents an open MIDI output port.

### Context Manager

```python
with midi.open() as m:
    m.note("C4")
# Automatically closes and sends all-notes-off
```

### MidiOut.note

```python
m.note(pitch, velocity=80, duration=500, channel=1)
```

Play a single note.

- `pitch` - MIDI number (0-127) or note name string
- `velocity` - Note velocity (0-127), default 80
- `duration` - Duration in milliseconds, default 500
- `channel` - MIDI channel (1-16), default 1

```python
m.note("C4")                    # Defaults
m.note(60)                      # By MIDI number
m.note("E4", 100)               # With velocity
m.note("G4", 80, 250)           # With duration
m.note("C5", 80, 500, 2)        # On channel 2
```

### MidiOut.chord

```python
m.chord(pitches, velocity=80, duration=500, channel=1)
```

Play multiple notes simultaneously.

- `pitches` - List or tuple of MIDI numbers or note names
- `velocity` - Note velocity (0-127)
- `duration` - Duration in milliseconds
- `channel` - MIDI channel (1-16)

```python
m.chord(["C4", "E4", "G4"])                # C major
m.chord([60, 64, 67])                       # By MIDI numbers
m.chord(midi.major("C4"), midi.mf, midi.half)  # With helpers
```

### MidiOut.arpeggio

```python
m.arpeggio(pitches, velocity=80, note_duration=None, spacing=None, channel=1)
```

Play notes sequentially (arpeggiated).

- `pitches` - List of pitches to arpeggiate
- `velocity` - Note velocity (0-127)
- `note_duration` - Duration of each note (default: eighth)
- `spacing` - Time between note starts (default: same as duration)
- `channel` - MIDI channel (1-16)

```python
m.arpeggio(midi.major("C4"))
m.arpeggio(midi.min7("A3"), velocity=80, note_duration=100)
```

### MidiOut.note_on / note_off

```python
m.note_on(pitch, velocity=80, channel=1)
m.note_off(pitch, velocity=0, channel=1)
```

Low-level note control. Use `note()` for most cases.

```python
m.note_on(60)
midi.sleep(500)
m.note_off(60)
```

### MidiOut.cc

```python
m.cc(control, value, channel=1)
```

Send Control Change message.

- `control` - CC number (0-127)
- `value` - CC value (0-127)
- `channel` - MIDI channel (1-16)

```python
m.cc(1, 64)      # Modulation wheel to middle
m.cc(7, 100)     # Volume
m.cc(64, 127)    # Sustain pedal on
```

### MidiOut.program_change

```python
m.program_change(program, channel=1)
```

Send Program Change message.

- `program` - Program number (0-127)
- `channel` - MIDI channel (1-16)

```python
m.program_change(0)     # Piano
m.program_change(25)    # Acoustic guitar
```

### MidiOut.all_notes_off

```python
m.all_notes_off(channel=None)
```

Send All Notes Off. If channel is None, sends on all channels.

```python
m.all_notes_off()     # All channels
m.all_notes_off(1)    # Channel 1 only
```

### MidiOut.close

```python
m.close()
```

Close the MIDI port. Automatically called when using context manager.

### MidiOut.is_open

```python
m.is_open -> bool
```

Property indicating whether the port is open.

```python
m = midi.open()
print(m.is_open)  # True
m.close()
print(m.is_open)  # False
```

### CC Helpers

Convenience methods for common control changes:

```python
m.modulation(value, channel=1)   # CC 1 - Modulation wheel
m.volume(value, channel=1)       # CC 7 - Channel volume
m.pan(value, channel=1)          # CC 10 - Pan (0=left, 64=center, 127=right)
m.sustain(on=True, channel=1)    # CC 64 - Sustain pedal
```

---

## Pitch Constants

All pitches follow the pattern `<note><octave>`:

- Notes: c, d, e, f, g, a, b
- Sharps: cs, ds, fs, gs, as
- Flats: db, eb, gb, ab, bb
- Octaves: 0-8

Middle C (MIDI 60) is `c4`.

```python
midi.c4   # 60
midi.cs4  # 61 (C sharp)
midi.db4  # 61 (D flat)
midi.a4   # 69
midi.c5   # 72
```

| Pitch | MIDI | Pitch | MIDI | Pitch | MIDI |
|-------|------|-------|------|-------|------|
| c4    | 60   | d4    | 62   | e4    | 64   |
| f4    | 65   | g4    | 67   | a4    | 69   |
| b4    | 71   | c5    | 72   | cs4   | 61   |

---

## Duration Constants

Based on 120 BPM by default. Use `midi.set_tempo()` to change.

| Constant          | Milliseconds | Musical Value |
|-------------------|--------------|---------------|
| `midi.whole`      | 2000         | Whole note    |
| `midi.half`       | 1000         | Half note     |
| `midi.quarter`    | 500          | Quarter note  |
| `midi.eighth`     | 250          | Eighth note   |
| `midi.sixteenth`  | 125          | 16th note     |

### midi.dotted

```python
midi.dotted(duration) -> int
```

Returns 1.5x the given duration.

```python
midi.dotted(midi.quarter)  # 750
midi.dotted(midi.half)     # 1500
```

---

## Tempo

### midi.set_tempo

```python
midi.set_tempo(bpm: int)
```

Set tempo and update all duration constants.

```python
midi.set_tempo(120)  # 120 BPM (default)
midi.set_tempo(60)   # 60 BPM - durations double
print(midi.quarter)  # 1000 at 60 BPM
```

### midi.get_tempo

```python
midi.get_tempo() -> int
```

Get current tempo in BPM.

```python
print(midi.get_tempo())  # 120
```

### midi.bpm

```python
midi.bpm(tempo: int) -> int
```

Calculate quarter note duration for a given tempo.

```python
midi.bpm(120)  # 500
midi.bpm(60)   # 1000
```

---

## Velocities (Dynamics)

| Constant   | Value | Dynamic        |
|------------|-------|----------------|
| `midi.ppp` | 16    | Pianississimo  |
| `midi.pp`  | 33    | Pianissimo     |
| `midi.p`   | 49    | Piano          |
| `midi.mp`  | 64    | Mezzo-piano    |
| `midi.mf`  | 80    | Mezzo-forte    |
| `midi.f`   | 96    | Forte          |
| `midi.ff`  | 112   | Fortissimo     |
| `midi.fff` | 127   | Fortississimo  |

---

## Chord Builders

All chord builders accept a root pitch (int or str) and return a list of MIDI numbers.

### midi.major

```python
midi.major(root) -> list[int]
```

Build major triad: root, major 3rd, perfect 5th.

```python
midi.major("C4")  # [60, 64, 67]
midi.major(60)    # [60, 64, 67]
```

### midi.minor

```python
midi.minor(root) -> list[int]
```

Build minor triad: root, minor 3rd, perfect 5th.

```python
midi.minor("C4")  # [60, 63, 67]
```

### midi.dim

```python
midi.dim(root) -> list[int]
```

Build diminished triad: root, minor 3rd, diminished 5th.

```python
midi.dim("C4")  # [60, 63, 66]
```

### midi.aug

```python
midi.aug(root) -> list[int]
```

Build augmented triad: root, major 3rd, augmented 5th.

```python
midi.aug("C4")  # [60, 64, 68]
```

### midi.dom7

```python
midi.dom7(root) -> list[int]
```

Build dominant 7th chord.

```python
midi.dom7("C4")  # [60, 64, 67, 70]
```

### midi.maj7

```python
midi.maj7(root) -> list[int]
```

Build major 7th chord.

```python
midi.maj7("C4")  # [60, 64, 67, 71]
```

### midi.min7

```python
midi.min7(root) -> list[int]
```

Build minor 7th chord.

```python
midi.min7("C4")  # [60, 63, 67, 70]
```

---

## Pitch Helpers

### midi.transpose

```python
midi.transpose(pitch, semitones) -> int
```

Transpose pitch by semitones.

```python
midi.transpose("C4", 2)   # 62 (D4)
midi.transpose(60, -12)   # 48 (C3)
```

### midi.octave_up

```python
midi.octave_up(pitch) -> int
```

Transpose up one octave (+12 semitones).

```python
midi.octave_up("C4")  # 72 (C5)
```

### midi.octave_down

```python
midi.octave_down(pitch) -> int
```

Transpose down one octave (-12 semitones).

```python
midi.octave_down(60)  # 48 (C3)
```

---

## Timing

### midi.sleep

```python
midi.sleep(ms: int)
```

Sleep for given milliseconds.

```python
midi.sleep(500)  # Wait half second
```

### midi.rest

```python
midi.rest(duration=None)
```

Rest (silence) for given duration. Default is quarter note.

```python
midi.rest()              # Quarter note rest
midi.rest(midi.half)     # Half note rest
```

---

## CC Constants

Common Control Change numbers:

| Constant             | Value | Description      |
|---------------------|-------|------------------|
| `midi.CC_MODULATION` | 1     | Modulation wheel |
| `midi.CC_BREATH`     | 2     | Breath controller|
| `midi.CC_VOLUME`     | 7     | Channel volume   |
| `midi.CC_PAN`        | 10    | Pan position     |
| `midi.CC_EXPRESSION` | 11    | Expression       |
| `midi.CC_SUSTAIN`    | 64    | Sustain pedal    |
| `midi.CC_REVERB`     | 91    | Reverb send      |
| `midi.CC_CHORUS`     | 93    | Chorus send      |
