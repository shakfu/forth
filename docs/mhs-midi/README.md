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
> import Midi
> midiOpenVirtual "MyPort" >>= print
True
> playNote c4 quarter >> return ()
> chord [c4, e4, g4] half >> return ()
> midiClose >> return ()
```

## Usage Modes

### Interactive REPL

```bash
./scripts/mhs-midi-repl
```

Note: IO actions in the REPL need `>>= print` or `>> return ()` suffix since MicroHs doesn't auto-execute IO like GHCi.

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
import Midi

main :: IO ()
main = do
    midiOpenVirtual "MyMelody"

    -- Play a simple melody
    playNote c4 quarter
    playNote e4 quarter
    playNote g4 quarter
    chord [c4, e4, g4] half

    midiClose
```

## Documentation

- [API Reference](api-reference.md) - Complete function documentation
- [Examples](examples.md) - More code examples
- [Architecture](architecture.md) - How the FFI works

## Requirements

- Built project (`make` must complete successfully)
- macOS (CoreMIDI), Linux (ALSA), or Windows (WinMM)
