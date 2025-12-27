# mhs-midi

A Haskell MIDI library for MicroHs, providing a functional approach to MIDI programming with an interactive REPL.

## Features

- Interactive REPL with MIDI support
- Compile Haskell files to standalone executables
- Musical abstractions: pitches, durations, velocities, chords
- Low-level MIDI control: note on/off, CC, program change, pitch bend
- Virtual and hardware MIDI port support

## Quick Start

### 1. Build

```bash
make
```

### 2. Start the REPL

```bash
./scripts/mhs-midi-repl
```

### 3. Play some notes

```haskell
> import MidiPrelude
> open >>= print
True
> n c4
> mapM_ n [c4, e4, g4]
> ch [c4, e4, g4]
> close
```

## Usage Modes

### Interactive REPL

```bash
./scripts/mhs-midi-repl
```

Note: IO actions in the REPL need `>> return ()` suffix (or `>>= print` for results). Use `MidiPrelude` for ergonomic functions.

### Run Haskell File (Interpreted)

```bash
./scripts/mhs-midi-repl -r MyProgram.hs
```

### Compile to Executable

```bash
./scripts/mhs-midi-compile MyProgram.hs -o my_program
./my_program
```

## Example Program

```haskell
module MyMelody(main) where
import MidiPrelude

main :: IO ()
main = do
    open

    -- Play a melody using mapM_
    mapM_ n [c4, e4, g4]

    -- Play a chord
    ch [c4, e4, g4]

    close
```

### Partial Application

The pitch-last parameter order enables partial application:

```haskell
module Expressive(main) where
import MidiPrelude

-- Define custom note functions
loud = note 1 fff quarter
soft = note 1 pp half
fast = note 1 mf sixteenth

main :: IO ()
main = do
    open

    -- Use them with just pitch
    mapM_ loud [c4, e4, g4]
    mapM_ soft [c5, g4, e4]
    mapM_ fast [c4, d4, e4, f4, g4, a4, b4, c5]

    close
```

## Documentation

- [API Reference](api-reference.md) - Complete function documentation
- [Examples](examples.md) - More code examples
- [Architecture](architecture.md) - How the FFI works

## Requirements

- Built project (`make` must complete successfully)
- macOS (CoreMIDI), Linux (ALSA), or Windows (WinMM)
