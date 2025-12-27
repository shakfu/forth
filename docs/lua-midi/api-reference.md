# API Reference

## Global Constants

All constants are available as globals for concise syntax. The `midi.` prefix also works.

### Pitch Constants

```lua
-- All pitches from C0 to B8 (examples)
c4, cs4, d4, ds4, e4, f4, fs4, g4, gs4, a4, as4, b4   -- Octave 4 (middle C = c4 = 60)

-- Flat aliases
db4, eb4, gb4, ab4, bb4   -- D-flat, E-flat, G-flat, A-flat, B-flat
```

### Dynamics (Velocity)

| Constant | Value | Dynamic        |
|----------|-------|----------------|
| `ppp`    | 16    | Pianississimo  |
| `pp`     | 33    | Pianissimo     |
| `p`      | 49    | Piano          |
| `mp`     | 64    | Mezzo-piano    |
| `mf`     | 80    | Mezzo-forte    |
| `f`      | 96    | Forte          |
| `ff`     | 112   | Fortissimo     |
| `fff`    | 127   | Fortississimo  |

### Duration Constants

Based on 120 BPM by default. Use `midi.set_tempo()` to change.

| Constant    | Milliseconds | Musical Value |
|-------------|--------------|---------------|
| `whole`     | 2000         | Whole note    |
| `half`      | 1000         | Half note     |
| `quarter`   | 500          | Quarter note  |
| `eighth`    | 250          | Eighth note   |
| `sixteenth` | 125          | 16th note     |

### Chord Builders

All return a table of MIDI numbers.

```lua
major(c4)    -- => {60, 64, 67}  Major triad
minor(c4)    -- => {60, 63, 67}  Minor triad
dim(c4)      -- => {60, 63, 66}  Diminished triad
aug(c4)      -- => {60, 64, 68}  Augmented triad
dom7(c4)     -- => {60, 64, 67, 70}  Dominant 7th
maj7(c4)     -- => {60, 64, 67, 71}  Major 7th
min7(c4)     -- => {60, 63, 67, 70}  Minor 7th
```

### Utility Functions

```lua
transpose(c4, 7)    -- => 67 (G4), transpose by semitones
octave_up(c4)       -- => 72 (C5)
octave_down(c4)     -- => 48 (C3)
dotted(quarter)     -- => 750, 1.5x duration
rest(quarter)       -- Rest for duration
sleep(500)          -- Sleep for milliseconds
```

---

## Port Management

### midi.list_ports

```lua
midi.list_ports() -> table
```

List available MIDI output ports. Returns table of `{index, name}` pairs.

```lua
midi.list_ports()
-- => {{0, "IAC Driver Bus 1"}, {1, "USB MIDI Device"}}
```

### midi.open

```lua
midi.open() -> MidiOut
midi.open(name) -> MidiOut
midi.open(index) -> MidiOut
```

Open a MIDI output port.

```lua
m = midi.open()                -- Virtual port "luaMIDI"
m = midi.open("MySynth")       -- Named virtual port
m = midi.open(0)               -- First hardware port
```

### m:close

```lua
m:close()
```

Close the MIDI port. Automatically sends all-notes-off on all channels.

### m:is_open

```lua
m:is_open() -> boolean
```

Check if the MIDI port is open.

---

## Note Playing

### m:note

```lua
m:note(pitch, [velocity], [duration], [channel])
```

Play a single note (blocking).

- `pitch` - MIDI number (0-127) or note name string
- `velocity` - Note velocity (0-127), default 80
- `duration` - Duration in milliseconds, default 500
- `channel` - MIDI channel (1-16), default 1

```lua
m:note(c4)                  -- Defaults
m:note(60)                  -- By MIDI number
m:note("C4")                -- By note name
m:note(c4, mf)              -- With velocity
m:note(c4, mf, quarter)     -- With duration
m:note(c4, 80, 500, 2)      -- On channel 2
```

### m:chord

```lua
m:chord(pitches, [velocity], [duration], [channel])
```

Play multiple notes simultaneously.

```lua
m:chord({60, 64, 67})           -- C major by numbers
m:chord({c4, e4, g4})           -- C major by constants
m:chord(major(c4), mf, half)    -- Using chord builder
m:chord(dom7(g3), f, quarter, 2)  -- On channel 2
```

### m:arpeggio

```lua
m:arpeggio(pitches, [velocity], [duration], [channel])
```

Play notes sequentially (arpeggiated).

```lua
m:arpeggio(major(c4))                -- Defaults
m:arpeggio(min7(a3), mf, sixteenth)  -- Fast arpeggio
```

### m:note_on

```lua
m:note_on(pitch, [velocity], [channel])
```

Send Note On message (non-blocking).

```lua
m:note_on(60)           -- Note on, default velocity
m:note_on(c4, 100)      -- With velocity
m:note_on(c4, 100, 2)   -- On channel 2
```

### m:note_off

```lua
m:note_off(pitch, [velocity], [channel])
```

Send Note Off message.

```lua
m:note_off(60)
m:note_off(c4, 64, 2)   -- With release velocity on channel 2
```

---

## Control Messages

### m:cc

```lua
m:cc(control, value, [channel])
```

Send Control Change message.

```lua
m:cc(1, 64)         -- Modulation wheel to middle
m:cc(7, 100)        -- Volume
m:cc(64, 127)       -- Sustain pedal on
m:cc(64, 0)         -- Sustain pedal off
```

Common CC numbers:

- 1: Modulation wheel
- 7: Channel volume
- 10: Pan
- 11: Expression
- 64: Sustain pedal
- 91: Reverb
- 93: Chorus

### m:program

```lua
m:program(program, [channel])
```

Send Program Change message.

```lua
m:program(0)       -- Piano
m:program(25)      -- Acoustic guitar
m:program(48, 2)   -- Strings on channel 2
```

### m:all_notes_off

```lua
m:all_notes_off([channel])
```

Send All Notes Off. If channel is omitted, sends on all channels (1-16).

```lua
m:all_notes_off()      -- All channels
m:all_notes_off(1)     -- Channel 1 only
```

---

## Pitch Helpers

### midi.note

```lua
midi.note(name) -> integer
```

Parse note name to MIDI number.

```lua
midi.note("C4")     -- => 60
midi.note("C#4")    -- => 61
midi.note("Db4")    -- => 61
```

### transpose / octave_up / octave_down

```lua
transpose(c4, 2)     -- => 62 (D4)
transpose(60, -12)   -- => 48 (C3)
octave_up(c4)        -- => 72 (C5)
octave_down(c4)      -- => 48 (C3)
```

---

## Tempo

### midi.set_tempo

```lua
midi.set_tempo(bpm)
```

Set tempo and update all duration constants.

```lua
midi.set_tempo(120)     -- 120 BPM (default)
midi.set_tempo(60)      -- 60 BPM - durations double
quarter                 -- => 1000 at 60 BPM
```

### midi.get_tempo

```lua
midi.get_tempo() -> integer
```

Get current tempo in BPM.

### midi.bpm

```lua
midi.bpm(tempo) -> integer
```

Calculate quarter note duration for a given tempo (without changing global tempo).

```lua
midi.bpm(120)    -- => 500
midi.bpm(60)     -- => 1000
```

---

## Timing

### sleep

```lua
sleep(ms)
```

Sleep for given milliseconds.

```lua
sleep(500)      -- Wait half second
sleep(1000)     -- Wait one second
```

### rest

```lua
rest([duration])
```

Rest (silence) for given duration. Default is quarter note.

```lua
rest()          -- Quarter note rest
rest(half)      -- Half note rest
rest(1000)      -- 1 second rest
```

### dotted

```lua
dotted(duration) -> integer
```

Returns 1.5x the given duration.

```lua
dotted(quarter)   -- => 750
dotted(half)      -- => 1500
```

---

## REPL Convenience Functions

These functions use a global `midi._out` variable for simpler interactive sessions.

### open / close

```lua
open()              -- Create virtual port, set as default
open("MyApp")       -- Named virtual port
open(0)             -- Hardware port by index
close()             -- Close default port
```

### n

```lua
n(pitch, [velocity], [duration], [channel])
```

Play a note on the default MIDI port.

```lua
n(c4)                   -- Play C4 with defaults
n(c4, mf)               -- With velocity
n(c4, mf, quarter)      -- With duration
n(c4, 80, 500, 2)       -- On channel 2
```

### ch

```lua
ch(pitches, [velocity], [duration], [channel])
```

Play a chord on the default MIDI port.

```lua
ch(major(c4))               -- C major chord
ch(minor(a3), mf, half)     -- A minor, half note
```

### arp

```lua
arp(pitches, [velocity], [duration], [channel])
```

Arpeggiate notes on the default MIDI port.

```lua
arp(major(c4))                  -- Arpeggiate C major
arp(min7(a3), mf, sixteenth)    -- Fast A minor 7 arpeggio
```

---

## Utilities

### help

```lua
help()
```

Display available functions and usage information.
