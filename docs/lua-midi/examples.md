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

-- Random note from a table
function random_element(t)
    return t[math.random(#t)]
end

-- Random velocity in range
function random_velocity(min, max)
    return min + math.random(max - min)
end

-- Play 32 random notes from C major 7
local chord = maj7(c4)
for i = 1, 32 do
    m:note(
        random_element(chord),
        random_velocity(60, 100),
        sixteenth
    )
end

m:close()
```

### Probability-Based Patterns

```lua
m = midi.open()

-- Play note with probability
function maybe_play(pitch, prob)
    if math.random(100) < prob then
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
