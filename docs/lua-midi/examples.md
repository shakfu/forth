# lua-midi Examples

## Basic Examples

### Hello MIDI

Play a C major scale:

```lua
m = midi.open()

for _, p in ipairs({c4, d4, e4, f4, g4, a4, b4, c5}) do
    m:note(p, mf, quarter)
end

m:close()
```

### Using Note Names

Play notes using string notation:

```lua
m = midi.open()

m:note(midi.note("C4"))
m:note(midi.note("D4"))
m:note(midi.note("E4"))
m:note(midi.note("F4"))
m:note(midi.note("G4"), f, half)  -- Louder, longer

m:close()
```

### Chord Progression

Play a I-IV-V-I progression:

```lua
m = midi.open()

-- I - C major
m:chord(major(c4), mf, half)

-- IV - F major
m:chord(major(f3), mf, half)

-- V - G major
m:chord(major(g3), mf, half)

-- I - C major
m:chord(major(c4), f, whole)

m:close()
```

### Melody with Dynamics

```lua
m = midi.open()

-- Crescendo
m:note(c4, pp, quarter)
m:note(d4, p, quarter)
m:note(e4, mp, quarter)
m:note(f4, mf, quarter)
m:note(g4, f, quarter)
m:note(a4, ff, half)

-- Decrescendo
m:note(g4, f, quarter)
m:note(f4, mf, quarter)
m:note(e4, mp, quarter)
m:note(d4, p, quarter)
m:note(c4, pp, whole)

m:close()
```

---

## Intermediate Examples

### Arpeggiated Chords

```lua
m = midi.open()

-- Arpeggiate C major up and down
m:arpeggio({c4, e4, g4, c5}, mf, eighth)
m:arpeggio({c5, g4, e4, c4}, mf, eighth)

rest(quarter)

-- Arpeggiate A minor
m:arpeggio(minor(a4), ff, sixteenth)

-- Reverse using table manipulation
local am = minor(a4)
local reversed = {}
for i = #am, 1, -1 do
    table.insert(reversed, am[i])
end
m:arpeggio(reversed, ff, sixteenth)

m:close()
```

### Tempo Changes

```lua
-- Start slow
midi.set_tempo(60)

m = midi.open()

m:note(c4, mf, quarter)  -- 1000ms at 60 BPM
m:note(e4, mf, quarter)
m:note(g4, mf, quarter)

-- Speed up
midi.set_tempo(120)
m:note(c5, mf, quarter)  -- 500ms at 120 BPM
m:note(e5, mf, quarter)
m:note(g5, mf, quarter)

-- Even faster
midi.set_tempo(180)
m:note(c6, mf, quarter)  -- ~333ms at 180 BPM
m:note(e6, mf, quarter)
m:note(g6, mf, quarter)

m:close()
```

### Dotted Rhythms

```lua
m = midi.open()

-- Dotted quarter - eighth pattern
m:note(c4, mf, dotted(quarter))
m:note(d4, mf, eighth)
m:note(e4, mf, dotted(quarter))
m:note(f4, mf, eighth)
m:note(g4, f, half)

m:close()
```

### All Chord Types

```lua
m = midi.open()
local root = c4

-- Triads
m:chord(major(root), mf, half)
m:chord(minor(root), mf, half)
m:chord(dim(root), mf, half)
m:chord(aug(root), mf, half)

rest(quarter)

-- Seventh chords
m:chord(dom7(root), mf, half)
m:chord(maj7(root), mf, half)
m:chord(min7(root), mf, half)

m:close()
```

---

## Scale Examples

### Playing Scales

```lua
m = midi.open()

-- Play C major scale using helper
for _, pitch in ipairs(scale(c4, "major")) do
    m:note(pitch, mf, eighth)
end

rest(quarter)

-- Play D dorian scale
for _, pitch in ipairs(scale(d4, "dorian")) do
    m:note(pitch, mf, eighth)
end

m:close()
```

### Modal Exploration

Play through all diatonic modes starting on C:

```lua
m = midi.open()

local modes = {"major", "dorian", "phrygian", "lydian", "mixolydian", "minor", "locrian"}

for _, mode in ipairs(modes) do
    print("Playing " .. mode)
    for _, pitch in ipairs(scale(c4, mode)) do
        m:note(pitch, mf, sixteenth)
    end
    rest(quarter)
end

m:close()
```

### Using Scale Degrees

Build chords from scale degrees:

```lua
m = midi.open()

-- Play I-IV-V-I in C major using scale degrees
local root = c4

-- I chord (1, 3, 5)
m:chord({
    degree(root, "major", 1),
    degree(root, "major", 3),
    degree(root, "major", 5)
}, mf, half)

-- IV chord (4, 6, 8)
m:chord({
    degree(root, "major", 4),
    degree(root, "major", 6),
    degree(root, "major", 8)
}, mf, half)

-- V chord (5, 7, 9)
m:chord({
    degree(root, "major", 5),
    degree(root, "major", 7),
    degree(root, "major", 9)
}, mf, half)

-- I chord
m:chord({
    degree(root, "major", 1),
    degree(root, "major", 3),
    degree(root, "major", 5)
}, f, whole)

m:close()
```

### Scale-Constrained Melody

Quantize random pitches to a scale:

```lua
m = midi.open()

local root = c4
local seed = 42  -- Deterministic seed for reproducibility

-- Generate random pitches and quantize to C pentatonic
for i = 1, 16 do
    local offset
    offset, seed = random_range(seed, -12, 12)
    local random_pitch = c4 + offset
    local quantized = quantize(random_pitch, root, "pentatonic")
    m:note(quantized, mf, sixteenth)
end

m:close()
```

### Arabic Maqam

Play Maqam Hijaz (12-TET approximation):

```lua
m = midi.open()

-- Hijaz is similar to Phrygian dominant
local hijaz = scale(d4, "maqam_hijaz")

-- Ascending
for _, pitch in ipairs(hijaz) do
    m:note(pitch, mf, quarter)
end

-- Add the octave
m:note(d5, mf, half)

-- Descending
for i = #hijaz, 1, -1 do
    m:note(hijaz[i], mf, quarter)
end

m:close()
```

### Microtonal Maqam with Quarter Tones

Play authentic Maqam Bayati with quarter tones:

```lua
m = midi.open()

local bayati = midi.scales_cents.maqam_bayati
local root = d4

-- Play scale with pitch bend for quarter tones
for _, cents in ipairs(bayati) do
    local note, bend = midi.cents_to_note(root, cents)
    m:pitch_bend(bend)
    m:note(note, mf, quarter)
end

-- Add octave
m:pitch_bend(0)
m:note(root + 12, mf, half)

m:close()
```

### Indian Raga

Play Raga Bhairav:

```lua
m = midi.open()

local bhairav = scale(c4, "raga_bhairav")

-- Aroha (ascending)
for _, pitch in ipairs(bhairav) do
    m:note(pitch, mp, quarter)
end
m:note(c5, mf, half)

rest(quarter)

-- Avaroha (descending) with ornamentation
local seed = 123  -- Deterministic seed
for i = #bhairav, 1, -1 do
    -- Add slight grace note (50% chance)
    local r
    r, seed = chance(seed, 50)
    if i > 1 and r then
        m:note(bhairav[i] + 1, pp, sixteenth)
    end
    m:note(bhairav[i], mf, quarter)
end

m:close()
```

### Blues Scale Improvisation

```lua
m = midi.open()
midi.set_tempo(100)

local blues = scale(c4, "blues")

-- Add octave above for more range
local full_blues = {}
for _, p in ipairs(blues) do
    table.insert(full_blues, p)
end
for _, p in ipairs(blues) do
    table.insert(full_blues, p + 12)
end

-- Random blues licks with deterministic RNG
local seed = 42  -- Set seed for reproducible results
for bar = 1, 4 do
    for beat = 1, 4 do
        local note_count
        note_count, seed = random_range(seed, 1, 3)
        for n = 1, note_count do
            local pitch, vel
            pitch, seed = pick(seed, full_blues)
            vel, seed = random_range(seed, 60, 100)
            local dur = sixteenth
            m:note(pitch, vel, dur)
        end
    end
end

-- End on the root
m:note(c4, f, whole)

m:close()
```

---

## Advanced Examples

### Higher-Order Functions

Use Lua's functional features for musical patterns:

```lua
m = midi.open()

-- Create a note-playing function with fixed parameters
function make_player(velocity, duration)
    return function(pitch)
        m:note(pitch, velocity, duration)
    end
end

-- Define different "instruments"
local loud_short = make_player(fff, sixteenth)
local soft_long = make_player(pp, half)

-- Use them
for _, p in ipairs({c4, e4, g4, c5}) do
    loud_short(p)
end

for _, p in ipairs({c5, g4, e4, c4}) do
    soft_long(p)
end

m:close()
```

### Pattern Repetition

```lua
m = midi.open()

-- Define a pattern as a function
function pattern1()
    m:note(c4, mf, eighth)
    m:note(e4, mf, eighth)
    m:note(g4, mf, eighth)
    m:note(e4, mf, eighth)
end

-- Repeat it 4 times
for i = 1, 4 do
    pattern1()
end

-- Or use a helper
function times(n, func)
    for i = 1, n do
        func()
    end
end

times(4, pattern1)

m:close()
```

### Transpose a Melody

```lua
m = midi.open()

local melody = {c4, d4, e4, f4, g4}

-- Original
for _, p in ipairs(melody) do
    m:note(p, mf, eighth)
end
rest(quarter)

-- Up a perfect fifth
for _, p in ipairs(melody) do
    m:note(transpose(p, 7), mf, eighth)
end
rest(quarter)

-- Down an octave
for _, p in ipairs(melody) do
    m:note(octave_down(p), mf, eighth)
end

m:close()
```

### Control Changes

```lua
m = midi.open()

-- Set volume
m:cc(7, 100)

-- Enable sustain pedal
m:cc(64, 127)

-- Play notes with sustain
m:note(c4, mf, quarter)
m:note(e4, mf, quarter)
m:note(g4, mf, quarter)

-- Release sustain
m:cc(64, 0)

rest(quarter)

-- Modulation sweep
m:note_on(c4, 80)
for i = 0, 127, 8 do
    m:cc(1, i)
    sleep(50)
end
for i = 127, 0, -8 do
    m:cc(1, i)
    sleep(50)
end
m:note_off(c4)

m:close()
```

### Program Changes

```lua
m = midi.open()

-- Piano (program 0)
m:program(0)
m:note(c4, mf, quarter)
m:note(e4, mf, quarter)
m:note(g4, mf, quarter)

-- Strings (program 48)
m:program(48)
m:chord(major(c4), mp, whole)

-- Brass (program 61)
m:program(61)
m:note(c5, ff, half)

m:close()
```

### Generative Music

```lua
m = midi.open()
midi.set_tempo(140)

-- Use the built-in deterministic RNG functions:
--   pick(seed, table) -> element, next_seed
--   random_range(seed, min, max) -> value, next_seed

-- Play 32 random notes from C major 7
local chord = maj7(c4)
local seed = 42  -- Set seed for reproducible results
for i = 1, 32 do
    local note, vel
    note, seed = pick(seed, chord)
    vel, seed = random_range(seed, 60, 100)
    m:note(note, vel, sixteenth)
end

m:close()
```

### Probability-Based Patterns

```lua
m = midi.open()

-- Use the built-in chance(seed, percent) function for probability
local seed = 42

-- Play note with probability
local function maybe_play(pitch, prob)
    local should_play
    should_play, seed = chance(seed, prob)
    if should_play then
        m:note(pitch, mf, sixteenth)
    else
        rest(sixteenth)
    end
end

-- Sparse texture
for i = 1, 32 do
    maybe_play(c4, 30)   -- 30% chance
    maybe_play(e4, 50)   -- 50% chance
    maybe_play(g4, 70)   -- 70% chance
end

m:close()
```

### Multi-Channel Composition

```lua
m = midi.open()

-- Set up instruments
m:program(0, 1)    -- Piano on channel 1
m:program(32, 2)   -- Bass on channel 2

-- Helper for channel-specific playing
function play_on_channel(ch, pitch, vel, dur)
    m:note(pitch, vel, dur, ch)
end

-- Simple bass line and melody
function bar()
    -- Bass note (channel 2, plays first)
    play_on_channel(2, c2, 100, half)
    -- Melody notes (channel 1)
    play_on_channel(1, c4, 80, quarter)
    play_on_channel(1, e4, 80, quarter)
end

-- Play 4 bars
for i = 1, 4 do
    bar()
end

m:close()
```

---

## REPL Session Examples

Start the REPL:

```bash
./build/lua_midi
```

### Using Convenience Functions (Recommended)

The simplest way to use lua-midi interactively:

```lua
> open()
MidiOut(virtual, "luaMIDI")
> n(c4)
> n(e4)
> n(g4)
> ch(major(c4))
> arp(min7(a3), mf, sixteenth)
> close()
```

### Quick Note Test (Explicit Port)

```lua
> m = midi.open()
MidiOut(virtual, "luaMIDI")
> m:note(c4)
> m:note(e4)
> m:note(g4)
> m:close()
```

### Interactive Chord Exploration

```lua
> m = midi.open()
> m:chord(major(c4))
> m:chord(minor(a3))
> m:chord(dom7(g3))
> m:close()
```

### Testing Dynamics

```lua
> m = midi.open()
> m:note(c4, ppp)   -- Very soft
> m:note(c4, mf)    -- Medium
> m:note(c4, fff)   -- Very loud
> m:close()
```

### List MIDI Ports

```lua
> midi.list_ports()
{{0, "IAC Driver Bus 1"}, {1, "USB MIDI Device"}}
```

### Checking Values

```lua
> c4
60
> major(c4)
{60, 64, 67}
> mf
80
> quarter
500
> midi.bpm(60)
1000
```

---

## Async Scheduler Examples

The async scheduler enables concurrent playback using Lua coroutines.

### Basic Two Voices

```lua
open()

spawn(function()
    for i = 1, 4 do
        play(c4, mf, quarter)
        play(e4, mf, quarter)
    end
end, "melody")

spawn(function()
    for i = 1, 4 do
        play(c2, f, half)
    end
end, "bass")

run()
close()
```

### Polyrhythm (3 against 4)

```lua
open()
midi.set_tempo(100)

-- Total duration: 2 seconds (4 quarters at 120 BPM = 2000ms)
local cycle_ms = 2000

spawn(function()
    -- 4 evenly spaced notes
    local dur = cycle_ms / 4
    for i = 1, 4 do
        play(c4, mf, dur - 10)
        yield_ms(10)
    end
end, "fours")

spawn(function()
    -- 3 evenly spaced notes
    local dur = cycle_ms / 3
    for i = 1, 3 do
        play(g4, f, dur - 10)
        yield_ms(10)
    end
end, "threes")

run()
close()
```

### Canon (Round)

```lua
open()
midi.set_tempo(120)

local melody = {c4, d4, e4, f4, g4, f4, e4, d4}

-- First voice starts immediately
spawn(function()
    for _, p in ipairs(melody) do
        play(p, mf, quarter)
    end
end, "voice1")

-- Second voice enters after 2 beats
spawn(function()
    yield_ms(quarter * 2)  -- Wait 2 beats
    for _, p in ipairs(melody) do
        play(p + 12, mp, quarter)  -- Up an octave, softer
    end
end, "voice2")

run()
close()
```

### Generative Texture

```lua
open()
midi.set_tempo(90)

local pentatonic = scale(c4, "pentatonic")

-- Random high notes (deterministic with seed)
spawn(function()
    local seed = 100  -- Different seed per voice
    for i = 1, 16 do
        local p, vel
        p, seed = pick(seed, pentatonic)
        vel, seed = random_range(seed, 40, 80)
        play(p + 12, vel, sixteenth)
    end
end, "high")

-- Random low notes (slower, different seed)
spawn(function()
    local seed = 200
    for i = 1, 8 do
        local p, vel
        p, seed = pick(seed, pentatonic)
        vel, seed = random_range(seed, 60, 100)
        play(p - 12, vel, eighth)
    end
end, "low")

-- Sustained drone
spawn(function()
    play(c2, f, whole)
    play(c2, f, whole)
end, "drone")

run()
close()
```

### Euclidean Rhythm

```lua
open()
midi.set_tempo(120)

-- Play a euclidean rhythm: 5 hits distributed over 8 steps
local pattern = euclidean(5, 8)

spawn(function()
    for rep = 1, 4 do  -- 4 repetitions
        for _, hit in ipairs(pattern) do
            if hit then
                play(c4, f, sixteenth * 0.8)
            end
            yield_ms(sixteenth)
        end
    end
end, "kick")

spawn(function()
    -- Off-beat hi-hat
    for rep = 1, 4 do
        for i = 1, 8 do
            yield_ms(sixteenth / 2)
            play(fs5, pp, sixteenth * 0.3)
            yield_ms(sixteenth / 2)
        end
    end
end, "hihat")

run()
close()
```

### Interactive Voice Control

```lua
open()

-- Start an infinite loop voice
local loop_id = spawn(function()
    while true do
        for _, p in ipairs(scale(c4, "blues")) do
            play(p, mf, sixteenth)
        end
    end
end, "loop")

-- Stopper voice - stops the loop after 3 seconds
spawn(function()
    yield_ms(3000)
    print("Stopping loop...")
    stop(loop_id)
end, "stopper")

run()
close()
```

### Multi-Channel Ensemble

```lua
open()
midi.set_tempo(100)

-- Channel 1: Lead melody
spawn(function()
    local melody = {g4, a4, b4, c5, d5, c5, b4, a4, g4}
    for _, p in ipairs(melody) do
        play(p, mf, quarter, 1)
    end
end, "lead")

-- Channel 2: Harmony (thirds below)
spawn(function()
    local harmony = {e4, f4, g4, a4, b4, a4, g4, f4, e4}
    for _, p in ipairs(harmony) do
        play(p, mp, quarter, 2)
    end
end, "harmony")

-- Channel 3: Bass
spawn(function()
    local bass = {c3, g2, a2, e2, c3}
    for _, p in ipairs(bass) do
        play(p, f, half, 3)
    end
end, "bass")

run()
close()
```
