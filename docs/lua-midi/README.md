# lua-midi

A Lua-based MIDI language using [Lua 5.5](https://www.lua.org/), providing a familiar scripting approach to MIDI programming.

## Features

- Full Lua 5.5 programming language
- Concise syntax with global pitch/duration/dynamic constants
- Object-oriented MIDI port interface
- Musical abstractions: pitches, durations, velocities, chords
- Low-level MIDI control: note on/off, CC, program change
- Virtual and hardware MIDI port support
- Chord builders and transpose helpers
- Tempo-aware duration constants
- Readline support for command history

## Quick Start

### 1. Build

```bash
make
```

### 2. Run interactively

```bash
./build/lua_midi
```

### 3. Play some notes

Using convenience functions:

```lua
> open()
MidiOut(virtual, "luaMIDI")
> n(c4)
> ch(major(c4))
> arp({c4, e4, g4, c5}, mf, eighth)
> close()
```

Or using explicit port management:

```lua
> m = midi.open()
MidiOut(virtual, "luaMIDI")
> m:note(c4, mf, quarter)
> m:chord(major(c4), mf, half)
> m:arpeggio({c4, e4, g4}, mf, eighth)
> m:close()
```

## Usage Modes

### Interactive REPL

```bash
./build/lua_midi
```

Type `help()` for available functions, `quit()` or Ctrl-D to exit.

### Evaluate Expression

```bash
./build/lua_midi -e 'print(1 + 2)'
./build/lua_midi -e 'print(major(c4))'
```

### Run Lua File

```bash
./build/lua_midi script.lua
```

## Example Program

```lua
-- melody.lua - Simple melody with chords

m = midi.open()
midi.set_tempo(120)

-- Play a C major scale
for _, p in ipairs({c4, d4, e4, f4, g4, a4, b4, c5}) do
    m:note(p, mf, quarter)
end

-- Play chord progression
m:chord(major(c4), mf, half)    -- I
m:chord(major(f3), mf, half)    -- IV
m:chord(major(g3), f, half)     -- V
m:chord(major(c4), mf, whole)   -- I

m:close()
```

Run with:

```bash
./build/lua_midi melody.lua
```

## Why Lua for MIDI?

Lua's design makes it excellent for musical scripting:

- **Simple syntax**: Clean, readable code for musical patterns
- **Tables**: Flexible representation for chords and sequences
- **First-class functions**: Create reusable musical patterns
- **Metatables**: Object-oriented MIDI port interface
- **REPL**: Interactive exploration and live coding
- **Embeddable**: Easy integration with C/C++ audio applications

## Global Constants

All musical constants are available as globals for concise code:

```lua
-- Pitches: c0-b8, cs0-cs8 (sharps), db0-bb8 (flats)
c4, e4, g4, cs4, db4, etc.

-- Dynamics
ppp, pp, p, mp, mf, f, ff, fff

-- Durations
whole, half, quarter, eighth, sixteenth

-- Chord builders
major, minor, dim, aug, dom7, maj7, min7

-- Utilities
transpose, octave_up, octave_down, dotted, rest, sleep
```

The `midi.` prefix also works: `midi.c4`, `midi.mf`, etc.

## REPL Tips

**Important**: In the Lua REPL, use global variables (not `local`) to persist values across lines:

```lua
-- This works:
> m = midi.open()
> m:note(c4)

-- This does NOT work (m is not available on the next line):
> local m = midi.open()
> m:note(c4)  -- Error: m is nil
```

For simpler interactive use, the convenience functions (`open()`, `n()`, `ch()`, `arp()`, `close()`) manage a global MIDI port automatically.

## Documentation

- [API Reference](api-reference.md) - Complete function documentation
- [Examples](examples.md) - More code examples
- [Architecture](architecture.md) - How the FFI works

## Requirements

- Built project (`make` must complete successfully)
- macOS (CoreMIDI), Linux (ALSA), or Windows (WinMM)
