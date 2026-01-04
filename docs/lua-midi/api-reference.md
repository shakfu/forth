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

### Scale Constants

All scales are available in the `midi.scales` table. Each scale is an array of semitone intervals from the root.

```lua
-- Diatonic modes
midi.scales.major          -- {0, 2, 4, 5, 7, 9, 11}
midi.scales.dorian         -- {0, 2, 3, 5, 7, 9, 10}
midi.scales.phrygian       -- {0, 1, 3, 5, 7, 8, 10}
midi.scales.lydian         -- {0, 2, 4, 6, 7, 9, 11}
midi.scales.mixolydian     -- {0, 2, 4, 5, 7, 9, 10}
midi.scales.minor          -- {0, 2, 3, 5, 7, 8, 10}
midi.scales.locrian        -- {0, 1, 3, 5, 6, 8, 10}

-- Other scales
midi.scales.harmonic_minor -- {0, 2, 3, 5, 7, 8, 11}
midi.scales.melodic_minor  -- {0, 2, 3, 5, 7, 9, 11}
midi.scales.pentatonic     -- {0, 2, 4, 7, 9}
midi.scales.blues          -- {0, 3, 5, 6, 7, 10}
midi.scales.whole_tone     -- {0, 2, 4, 6, 8, 10}

-- Exotic scales
midi.scales.hungarian_minor
midi.scales.double_harmonic
midi.scales.phrygian_dominant
midi.scales.hirajoshi      -- Japanese
midi.scales.in_sen         -- Japanese

-- Arabic Maqamat (12-TET approximations)
midi.scales.maqam_hijaz
midi.scales.maqam_nahawand
midi.scales.maqam_nikriz

-- Indian Ragas (12-TET approximations)
midi.scales.raga_bhairav
midi.scales.raga_todi
midi.scales.raga_marwa
```

See `midi.scales` for all 55 available scales.

### Microtonal Scales (Cents)

For scales with quarter tones, use `midi.scales_cents`. Values are in cents (100 cents = 1 semitone).

```lua
midi.scales_cents.maqam_bayati   -- {0, 150, 300, 500, 700, 800, 1000}
midi.scales_cents.maqam_rast     -- {0, 200, 350, 500, 700, 900, 1050}
midi.scales_cents.shruti         -- 22-shruti Indian scale
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

## Scale Functions

### midi.build_scale

```lua
midi.build_scale(root, intervals) -> table
```

Build a scale from a root pitch and interval array. Returns a table of MIDI pitches.

```lua
midi.build_scale(60, {0, 2, 4, 5, 7, 9, 11})  -- C major: {60, 62, 64, 65, 67, 69, 71}
midi.build_scale(c4, midi.scales.minor)        -- C minor scale
midi.build_scale(d4, midi.scales.dorian)       -- D dorian scale
```

### scale (helper)

```lua
scale(root, name) -> table
```

Build a scale using a scale name string. Looks up intervals from `midi.scales`.

```lua
scale(c4, "major")           -- C major scale
scale(a4, "minor")           -- A natural minor
scale(c4, "blues")           -- C blues scale
scale(d4, "maqam_hijaz")     -- D Hijaz (Arabic)
scale(c4, "raga_bhairav")    -- C Bhairav (Indian)
```

### midi.scale_degree

```lua
midi.scale_degree(root, intervals, degree) -> integer
```

Get a specific scale degree. Degrees are 1-based (1 = root, 2 = second, etc.). Supports extended degrees beyond the octave.

```lua
midi.scale_degree(60, midi.scales.major, 1)   -- 60 (root)
midi.scale_degree(60, midi.scales.major, 3)   -- 64 (third)
midi.scale_degree(60, midi.scales.major, 5)   -- 67 (fifth)
midi.scale_degree(60, midi.scales.major, 9)   -- 74 (ninth = 2nd + octave)
```

### degree (helper)

```lua
degree(root, name, n) -> integer
```

Get a scale degree using a scale name string.

```lua
degree(c4, "major", 3)    -- 64 (E4, third of C major)
degree(c4, "major", 5)    -- 67 (G4, fifth of C major)
degree(c4, "minor", 7)    -- 70 (Bb4, seventh of C minor)
degree(c4, "major", 9)    -- 74 (D5, ninth)
degree(c4, "major", 11)   -- 77 (F5, eleventh)
```

### midi.in_scale

```lua
midi.in_scale(pitch, root, intervals) -> boolean
```

Check if a pitch belongs to a scale (in any octave).

```lua
midi.in_scale(64, 60, midi.scales.major)  -- true (E is in C major)
midi.in_scale(66, 60, midi.scales.major)  -- false (F# is not in C major)
midi.in_scale(76, 60, midi.scales.major)  -- true (E5 is in C major)
```

### in_scale (helper)

```lua
in_scale(pitch, root, name) -> boolean
```

Check if a pitch is in a named scale.

```lua
in_scale(e4, c4, "major")    -- true
in_scale(fs4, c4, "major")   -- false
in_scale(fs4, g4, "major")   -- true (F# is in G major)
```

### midi.quantize

```lua
midi.quantize(pitch, root, intervals) -> integer
```

Quantize (snap) a pitch to the nearest tone in a scale.

```lua
midi.quantize(66, 60, midi.scales.major)   -- 65 (F# -> F) or 67 (-> G)
midi.quantize(61, 60, midi.scales.major)   -- 60 (C# -> C) or 62 (-> D)
```

### quantize (helper)

```lua
quantize(pitch, root, name) -> integer
```

Quantize to a named scale.

```lua
quantize(fs4, c4, "major")    -- Snap F# to C major
quantize(61, c4, "pentatonic") -- Snap C# to C pentatonic
```

---

## Microtonal / Pitch Bend

### m:pitch_bend

```lua
m:pitch_bend(cents, [channel])
```

Send a pitch bend message. Cents are relative to the current note (-200 to +200 for standard 2-semitone bend range).

```lua
m:pitch_bend(0)       -- Center (no bend)
m:pitch_bend(100)     -- Bend up 1 semitone
m:pitch_bend(-50)     -- Bend down quarter tone
m:pitch_bend(50, 2)   -- Quarter tone up on channel 2
```

### midi.cents_to_note

```lua
midi.cents_to_note(root, cents) -> note, bend
```

Convert a cents interval to a MIDI note number and pitch bend offset. Useful for playing microtonal scales.

```lua
local note, bend = midi.cents_to_note(c4, 150)  -- 150 cents from C4
-- note = 61 (C#4), bend = 50 (quarter tone sharp)

local note, bend = midi.cents_to_note(c4, 350)  -- 350 cents from C4
-- note = 63 (Eb4), bend = 50 (quarter tone sharp)
```

### Playing Microtonal Scales

```lua
m = midi.open()

-- Play Maqam Bayati with quarter tones
local bayati = midi.scales_cents.maqam_bayati
for _, cents in ipairs(bayati) do
    local note, bend = midi.cents_to_note(c4, cents)
    m:pitch_bend(bend)
    m:note(note, mf, quarter)
end
m:pitch_bend(0)  -- Reset bend

m:close()
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

---

## MIDI Recording

Record MIDI events for replay or export. Records note-on, note-off, and CC events with timestamps.

### record_midi

```lua
record_midi([bpm])
```

Start recording MIDI events. Optional BPM parameter (default 120) is stored with the recording for reference.

```lua
record_midi()        -- Start at 120 BPM
record_midi(140)     -- Start at 140 BPM
```

### record_stop

```lua
record_stop()
```

Stop recording MIDI events. Prints the number of events recorded.

```lua
record_stop()
-- => MIDI recording stopped. 42 events recorded.
```

### save_midi

```lua
save_midi(filename)
```

Save recorded events to a Lua replay script. The generated file can be executed to replay the recorded performance.

```lua
save_midi("my_song.lua")
-- => Saved 42 events to my_song.lua
```

The generated file contains:
- Event data as a Lua table
- Replay code that recreates the timing

### record_status

```lua
record_status() -> (active, count, bpm)
```

Get current recording status. Returns three values: whether recording is active, event count, and BPM.

```lua
local active, count, bpm = record_status()
print(active)   -- true/false
print(count)    -- number of events
print(bpm)      -- recording BPM
```

### Recording Example

```lua
m = midi.open()

record_midi(120)

m:note(c4, mf, quarter)
m:note(e4, mf, quarter)
m:chord(major(g4), f, half)

record_stop()
save_midi("melody.lua")

m:close()
```

---

## MIDI File I/O

Read and write standard MIDI files (.mid format).

### write_mid

```lua
write_mid(filename)
```

Write recorded events to a standard MIDI file. The file can be opened in any DAW or MIDI player.

```lua
m = midi.open()
record_midi(120)

m:note(c4, mf, quarter)
m:note(e4, mf, quarter)
m:chord(major(g4), f, half)

record_stop()
write_mid("melody.mid")   -- Standard MIDI file

m:close()
```

### read_mid

```lua
read_mid(filename) -> table
```

Read a standard MIDI file and return its contents as a table with metadata and events.

```lua
local data = read_mid("song.mid")

-- Metadata
print(data.num_tracks)  -- Number of tracks
print(data.ppqn)        -- Pulses per quarter note
print(data.tempo)       -- Tempo in microseconds per beat
print(data.duration)    -- Duration in milliseconds
print(data.format)      -- MIDI format (0, 1, or 2)

-- Events
for _, event in ipairs(data.events) do
    local track, tick, channel, type, data1, data2 = table.unpack(event)
    print(string.format("t=%d ch=%d type=%02X d1=%d d2=%d",
                        tick, channel, type, data1, data2))
end
```

Event types (in the `type` field):
- `0x90` = Note On
- `0x80` = Note Off
- `0xB0` = Control Change
- `0xC0` = Program Change
- `0xE0` = Pitch Bend

---

## Async Scheduler

The async scheduler enables concurrent playback of multiple musical voices using Lua coroutines and libuv timers. Each voice runs independently with its own timing, allowing polyrhythms, counterpoint, and complex arrangements.

### spawn

```lua
spawn(func, [name]) -> voice_id
```

Create a new voice (coroutine) from a function. The function will run concurrently with other voices when `run()` is called.

- `func` - Function to execute as a voice
- `name` - Optional name for debugging (default: "")
- Returns: Integer voice ID

```lua
spawn(function()
    play(c4, mf, quarter)
    play(e4, mf, quarter)
    play(g4, mf, quarter)
end, "melody")

spawn(function()
    for i = 1, 4 do
        play(c3, f, half)
    end
end, "bass")

run()  -- Both voices play concurrently
```

### yield_ms

```lua
yield_ms(ms)
```

Pause the current voice for the specified number of milliseconds. Other voices continue running during this time. Must be called from within a spawned voice.

```lua
spawn(function()
    play(c4)
    yield_ms(1000)  -- Wait 1 second
    play(e4)
end)
```

### run

```lua
run()
```

Run the scheduler until all voices complete. This is a blocking call that processes voice resumes.

```lua
spawn(function() play(c4) end)
spawn(function() play(e4) end)
run()  -- Blocks until both voices finish
```

### stop

```lua
stop([voice_id]) -> boolean
```

Stop a specific voice by ID, or stop all voices if no ID is given.

```lua
local id = spawn(function()
    while true do
        play(c4, mf, quarter)
    end
end)

-- Later...
stop(id)    -- Stop specific voice
stop()      -- Stop all voices
```

### voices

```lua
voices() -> integer
```

Get the count of currently active voices.

```lua
spawn(function() yield_ms(1000) end)
spawn(function() yield_ms(2000) end)
print(voices())  -- => 2
```

### scheduler.status

```lua
scheduler.status() -> table
```

Get detailed scheduler status information.

```lua
local s = scheduler.status()
print(s.running)   -- boolean: is run() active?
print(s.active)    -- number: active voice count
for _, v in ipairs(s.voices) do
    print(v.id, v.name, v.waiting)
end
```

---

## Async Note Helpers

These functions are designed for use inside spawned voices. They play notes using non-blocking waits, allowing other voices to run during note durations.

### play

```lua
play(pitch, [velocity], [duration], [channel])
```

Play a single note asynchronously. Uses `yield_ms` internally so other voices can run.

```lua
spawn(function()
    play(c4)                    -- Defaults
    play(c4, mf)                -- With velocity
    play(c4, mf, quarter)       -- With duration
    play(c4, mf, quarter, 2)    -- On channel 2
end)
run()
```

### play_chord

```lua
play_chord(pitches, [velocity], [duration], [channel])
```

Play multiple notes simultaneously, then wait asynchronously.

```lua
spawn(function()
    play_chord(major(c4), mf, half)
    play_chord(major(f3), mf, half)
    play_chord(major(g3), mf, half)
    play_chord(major(c4), f, whole)
end)
run()
```

### play_arp

```lua
play_arp(pitches, [velocity], [duration], [channel])
```

Play notes sequentially (arpeggiated) with async timing.

```lua
spawn(function()
    play_arp(major(c4), mf, sixteenth)
    play_arp(minor(a3), mf, sixteenth)
end)
run()
```

---

## Async Examples

### Two-Voice Counterpoint

```lua
open()

spawn(function()
    -- Upper voice: melody
    for _, p in ipairs({c5, d5, e5, f5, g5, f5, e5, d5, c5}) do
        play(p, mf, quarter)
    end
end, "melody")

spawn(function()
    -- Lower voice: bass line
    play(c3, f, half)
    play(g3, f, half)
    play(a3, f, half)
    play(f3, f, half)
    play(c3, f, whole)
end, "bass")

run()
close()
```

### Polyrhythm (3 against 4)

```lua
open()
midi.set_tempo(120)

spawn(function()
    -- Voice 1: 4 notes per cycle
    for i = 1, 4 do
        play(c4, mf, quarter)
    end
end)

spawn(function()
    -- Voice 2: 3 notes per cycle (dotted rhythm)
    local triplet = math.floor(quarter * 4 / 3)
    for i = 1, 3 do
        play(g4, mf, triplet)
    end
end)

run()
close()
```

### Generative Voices

```lua
open()

-- Random melody voice using deterministic RNG (reproducible with same seed)
spawn(function()
    local scale_pitches = scale(c4, "pentatonic")
    local seed = 42  -- Set seed for reproducibility
    for i = 1, 16 do
        local p, vel
        p, seed = pick(seed, scale_pitches)     -- deterministic pick
        vel, seed = random_range(seed, 60, 100) -- deterministic range
        play(p, vel, sixteenth)
    end
end, "random_melody")

-- Steady bass drone
spawn(function()
    for i = 1, 4 do
        play(c2, f, whole)
    end
end, "drone")

run()
close()
```

### Stop Voice Early

```lua
open()

local melody_id = spawn(function()
    while true do
        for _, p in ipairs(scale(c4, "major")) do
            play(p, mf, eighth)
        end
    end
end, "infinite_melody")

spawn(function()
    yield_ms(2000)  -- Let melody play for 2 seconds
    stop(melody_id) -- Then stop it
end, "stopper")

run()
close()
```
