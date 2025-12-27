# pktpy-midi

A Python-based MIDI language using [PocketPy](https://pocketpy.dev), providing a Pythonic API for MIDI programming.

## Features

- Pythonic API with context managers and classes
- Musical abstractions: pitches, durations, velocities, chords
- Low-level MIDI control: note on/off, CC, program change
- Virtual and hardware MIDI port support
- Chord builders: major, minor, diminished, augmented, 7ths
- Tempo-aware duration constants

## Quick Start

### 1. Build

```bash
make
```

### 2. Run an example

```bash
./build/pktpy_midi examples/pktpy_midi/hello.py
```

Or start interactive mode:

```bash
./build/pktpy_midi
```

### 3. Play some notes

```python
import midi

with midi.open() as m:
    m.note("C4")
    m.note("E4")
    m.note("G4")
    m.chord(midi.major("C4"), midi.mf, midi.quarter)
```

## Usage Modes

### Interactive REPL

```bash
./build/pktpy_midi
```

### Run Python File

```bash
./build/pktpy_midi script.py
```

## Example Program

```python
import midi

# Set tempo to 120 BPM
midi.set_tempo(120)

with midi.open() as m:
    # Play a C major scale
    for note in [midi.c4, midi.d4, midi.e4, midi.f4,
                 midi.g4, midi.a4, midi.b4, midi.c5]:
        m.note(note, midi.mf, midi.quarter)

    # Play some chords
    m.chord(midi.major("C4"), midi.mf, midi.half)
    m.chord(midi.minor("A3"), midi.mp, midi.half)
    m.chord(midi.dom7("G3"), midi.f, midi.whole)
```

### Using Arpeggios

```python
import midi

with midi.open() as m:
    # Arpeggiate a chord
    m.arpeggio(midi.major("C4"), midi.mf, midi.eighth)

    # With custom spacing
    m.arpeggio(midi.min7("A3"), velocity=80, note_duration=100, spacing=150)
```

## Documentation

- [API Reference](api-reference.md) - Complete function documentation
- [Examples](examples.md) - More code examples
- [Architecture](architecture.md) - How the C module works

## Requirements

- Built project (`make` must complete successfully)
- macOS (CoreMIDI), Linux (ALSA), or Windows (WinMM)
