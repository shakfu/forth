# alda-midi

A C implementation of the [Alda](https://alda.io/) music language with MIDI output using libremidi.

## Features

- Full Alda language parser (38 tokens, 28 AST node types)
- Music-first notation: notes, chords, rests, ties, dots
- Part declarations with 128 GM instruments
- Attributes: tempo, volume, dynamics, quantization, panning
- Voices for polyphony (V1:, V2:, V0:)
- Auto MIDI channel assignment (1-16)
- Non-blocking REPL with concurrent playback mode
- File playback and interactive REPL modes
- Virtual and hardware MIDI port support

## Quick Start

### 1. Build

```bash
make
```

### 2. Run interactively

```bash
./build/alda_midi
```

### 3. Play some notes

```
alda> piano:
alda> c d e f g
alda> c/e/g
```

## Usage Modes

### Interactive REPL

```bash
./build/alda_midi           # Start REPL
./build/alda_midi -c        # Start in concurrent mode (polyphony)
```

Type `help` for commands, `quit` or Ctrl-D to exit.

### Play a File

```bash
./build/alda_midi song.alda
./build/alda_midi -v song.alda    # Verbose output
```

### CLI Options

| Option | Description |
|--------|-------------|
| `-h, --help` | Show help message |
| `-v, --verbose` | Enable verbose output |
| `-l, --list` | List available MIDI ports |
| `-p, --port N` | Use hardware port by index |
| `-o, --output NAME` | Use port matching name |
| `--virtual NAME` | Create virtual port with name |
| `--no-sleep` | Disable timing (for testing) |
| `-c, --concurrent` | Enable concurrent playback mode |

## REPL Commands

| Command | Description |
|---------|-------------|
| `help` | Show help |
| `quit`, `exit` | Exit the REPL |
| `list` | List MIDI ports |
| `stop` | Stop current playback |
| `panic` | All notes off |
| `concurrent` | Enable concurrent mode |
| `sequential` | Disable concurrent mode (default) |

## Example Programs

### Simple Scale

```alda
piano:
  c d e f g a b > c
```

### With Tempo and Dynamics

```alda
piano:
  (tempo 120)
  (mf)
  o4 c4 d e f | g2 g | a4 a a a | g2.
```

### Chord Progression

```alda
piano:
  c4/e/g c/e/g | f/a/c f/a/c | g/b/d g/b/d | c1/e/g
```

### Multiple Instruments

```alda
piano:
  o4 c8 d e f g a b > c

violin:
  o5 c2 g | e1

cello:
  o3 c1 | g1
```

### Voices (Polyphony)

```alda
piano:
  V1: c1 d e f
  V2: e1 f g a
  V0: c1/e/g
```

## Concurrent Mode

In concurrent mode (`-c` flag or `concurrent` command), each REPL input plays immediately without waiting for previous playback to finish. This allows polyphonic layering:

```bash
./build/alda_midi -c
```

```
alda> piano: c1 d e f g a b > c
alda> violin: e2 f g a b > c d e    # Plays alongside piano
alda> cello: c1 g c g               # Adds bass layer
```

Each instrument is assigned a different MIDI channel automatically, enabling true polyphony when connected to a GM-compatible synthesizer.

## Alda Language Quick Reference

### Notes

```alda
c d e f g a b      # Natural notes
c# d# f# g# a#     # Sharps
db eb gb ab bb     # Flats
c4 d8 e16          # Durations: 4=quarter, 8=eighth, 16=sixteenth
c4. c4..           # Dotted notes
c4~4               # Tied notes
```

### Octaves

```alda
o4 c d e           # Set octave to 4
> c                # Octave up
< c                # Octave down
```

### Chords and Rests

```alda
c/e/g              # Chord (simultaneous notes)
r4                 # Quarter rest
r2.                # Dotted half rest
```

### Attributes

```alda
(tempo 140)        # Set tempo in BPM
(volume 80)        # Set volume 0-100
(quant 90)         # Set quantization 0-100
(panning 64)       # Set pan 0-127 (64=center)
(pp) (p) (mp) (mf) (f) (ff) (fff)   # Dynamics
```

### Parts

```alda
piano:             # Declare piano part
violin:            # Declare violin part
piano/violin:      # Apply events to both parts
```

### Voices

```alda
V1: c d e f        # Voice 1
V2: e f g a        # Voice 2 (independent timing)
V0: c1             # Merge voices (continue at max tick)
```

## Documentation

- [Language Reference](language-reference.md) - Complete syntax documentation
- [Examples](examples.md) - More code examples
- [Alda Language Guide](alda-language/README.md) - Comprehensive Alda documentation

## Requirements

- Built project (`make` must complete successfully)
- macOS (CoreMIDI), Linux (ALSA), or Windows (WinMM)
- GM-compatible synthesizer for playback

## MIDI Playback with FluidSynth

alda-midi creates a virtual MIDI port. To hear sound, connect it to a synthesizer.

### Quick Setup

```bash
# Terminal 1: Start FluidSynth
export ALDA_SF2_DIR=~/Music/sf2    # Set your SoundFont directory
python scripts/fluidsynth-gm.py

# Terminal 2: Run alda-midi
./build/alda_midi
```

### Install FluidSynth

| Platform | Command |
|----------|---------|
| macOS | `brew install fluidsynth` |
| Ubuntu/Debian | `sudo apt install fluidsynth` |
| Fedora | `sudo dnf install fluidsynth` |

Download a GM SoundFont (e.g., [FluidR3_GM.sf2](https://musical-artifacts.com/artifacts/738)) and place it in your `ALDA_SF2_DIR`.

### Script Options

```bash
python scripts/fluidsynth-gm.py --help
python scripts/fluidsynth-gm.py --list          # List available SoundFonts
python scripts/fluidsynth-gm.py -g 0.5          # Lower gain
python scripts/fluidsynth-gm.py MySoundFont.sf2 # Use specific SoundFont
```

### Other Playback Options

- **DAW**: Route the "AldaMIDI" virtual port to Logic, Ableton, Reaper, etc.
- **Hardware**: Connect to GM-compatible synthesizers
- **Software synths**: Any virtual instrument accepting MIDI input
