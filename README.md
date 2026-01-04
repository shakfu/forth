# midi-langs

A work-in-progress collection of mini MIDI-capable languages for generating and transforming MIDI sequences:

| Language | Based On | Style |
|----------|----------|-------|
| **alda-midi** | [Alda](https://alda.io/) | Music-first notation |
| **forth-midi** | Custom Forth | Concise stack-based notation |
| **lua-midi** | [Lua 5.5](https://www.lua.org/) | Scripting with tables/closures |
| **mhs-midi** | [MicroHs](https://github.com/augustss/MicroHs) | Minimal functional Haskell |
| **pktpy-midi** | [PocketPy](https://pocketpy.dev) | Pythonic with context managers |
| **s7-midi** | [s7 Scheme](https://ccrma.stanford.edu/software/snd/snd/s7.html) | Lisp with macros |

All use [libremidi](https://github.com/celtera/libremidi) for cross-platform MIDI I/O.

This is an early stage project, so expect instability in the code-base as we iterate over the apis and features of each implementation.

## Building

```bash
make              # Build everything
make test         # Run all tests (33 tests)
make clean        # Remove build directory
```

Requires GCC/Clang and CMake 3.16+.

## Quick Examples

### alda-midi

```bash
./build/alda_midi           # Start REPL
./build/alda_midi -c        # Start REPL in concurrent mode (polyphony)
./build/alda_midi song.alda # Play a file
```

```alda
piano:
  (tempo 120)
  o4 c4 d e f g a b > c2

violin:
  V1: c4 d e f
  V2: e4 f g a
  V0: c1
```

In concurrent mode (`-c` or `concurrent` command), multiple parts entered separately play simultaneously on different MIDI channels.

[Full documentation](docs/alda-midi/README.md) | [Language Reference](docs/alda-midi/language-reference.md)

### forth-midi

```bash
./build/forth_midi
```

```forth
midi-open c4, e4, g4, (c4 e4 g4), midi-close
```

[Full documentation](docs/forth-midi/README.md) | [API](docs/forth-midi/api-reference.md) | [Tutorial](docs/forth-midi/tutorial.md)

### lua-midi

```bash
./build/lua_midi
```

```lua
open()
n(c4); n(e4); n(g4)
ch(major(c4))
close()
```

[Full documentation](docs/lua-midi/README.md) | [API Reference](docs/lua-midi/api-reference.md)

### mhs-midi

```bash
./scripts/mhs-midi-repl
```

```haskell
import MidiRepl
open
mapM_ n [c4, e4, g4]
ch [c4, e4, g4]
close
```

[Full documentation](docs/mhs-midi/README.md) | [API Reference](docs/mhs-midi/api-reference.md)

### pktpy-midi

```bash
./build/pktpy_midi
```

```python
import midi
with midi.open() as m:
    for p in ["C4", "E4", "G4"]:
        m.note(p)
    m.chord(midi.major("C4"))
```

[Full documentation](docs/pktpy-midi/README.md) | [API Reference](docs/pktpy-midi/api-reference.md)

### s7-midi

```bash
./build/s7_midi
```

```scheme
(open)
(n c4) (n e4) (n g4)
(ch (major c4))
(close)
```

[Full documentation](docs/s7-midi/README.md) | [API Reference](docs/s7-midi/api-reference.md)

## Common Features

All languages share functionality from a common C library (`projects/common/music_theory.c`):

- **Pitch parsing**: Note names like "C4", "C#4", "Db5" to MIDI numbers
- **55 scales**: Major, minor, modes, pentatonic, blues, world scales, maqamat, ragas
- **10 microtonal scales**: Quarter-tone maqamat, Turkish makamlar, Indian shruti
- **Scale operations**: Build scales, get degrees, check membership, quantize
- **Chord intervals**: Major, minor, diminished, augmented, 7th chords
- **Dynamics**: ppp to fff velocity constants
- **Durations**: Whole, half, quarter, eighth, sixteenth note values

## Prelude System

Interpreted languages have prelude files in their native syntax:

```
projects/s7-midi/prelude.scm    -> Scheme constants and helpers
projects/lua-midi/prelude.lua   -> Lua constants and helpers
projects/pktpy-midi/prelude.py  -> Python constants and helpers
```

These are converted to C headers at build time via `scripts/prelude2c.py`.

```bash
make preludes     # Regenerate all prelude headers
```

## Architecture

```
projects/
  alda-midi/        # Alda interpreter (~3000 lines C)
  common/           # Shared music theory library
  forth-midi/       # Forth interpreter (~2700 lines C)
  lua-midi/         # Lua 5.5 + MIDI bindings
  mhs-midi/         # MicroHs + MIDI FFI
  pktpy-midi/       # PocketPy + MIDI bindings
  s7-midi/          # s7 Scheme + MIDI bindings
thirdparty/
  libremidi/        # MIDI I/O library (auto-built)
  MicroHs/          # Haskell compiler
  s7/               # Scheme interpreter
  lua-5.5.0/        # Lua interpreter
docs/               # Per-language documentation
tests/              # Test suite
```

## MIDI Playback

All languages output MIDI via virtual ports. To hear sound, connect to a General MIDI synthesizer.

### FluidSynth (Recommended)

Use the included helper script:

```bash
# Set up SoundFont directory (one-time)
export ALDA_SF2_DIR=~/Music/sf2

# Start FluidSynth (in one terminal)
python scripts/fluidsynth-gm.py

# Run any midi-langs interpreter (in another terminal)
./build/alda_midi
./build/forth_midi
./build/lua_midi
```

Install FluidSynth and a SoundFont:

| Platform | Install |
|----------|---------|
| macOS | `brew install fluidsynth` |
| Ubuntu/Debian | `sudo apt install fluidsynth` |
| Fedora | `sudo dnf install fluidsynth` |

Download a GM SoundFont like [FluidR3_GM.sf2](https://musical-artifacts.com/artifacts/738) and place it in your `ALDA_SF2_DIR`.

### Other Options

- **DAW**: Route the virtual MIDI port to any DAW (Logic, Ableton, Reaper)
- **Hardware**: Connect to GM-compatible synthesizers or sound modules
- **Software synths**: Use any virtual instrument that accepts MIDI input

## Documentation

| Language | Docs |
|----------|------|
| alda-midi | [README](docs/alda-midi/README.md), [Language](docs/alda-midi/language-reference.md), [Examples](docs/alda-midi/examples.md) |
| forth-midi | [README](docs/forth-midi/README.md), [API](docs/forth-midi/api-reference.md), [Tutorial](docs/forth-midi/tutorial.md) |
| lua-midi | [README](docs/lua-midi/README.md), [API](docs/lua-midi/api-reference.md), [Examples](docs/lua-midi/examples.md) |
| mhs-midi | [README](docs/mhs-midi/README.md), [API](docs/mhs-midi/api-reference.md), [Examples](docs/mhs-midi/examples.md) |
| pktpy-midi | [README](docs/pktpy-midi/README.md), [API](docs/pktpy-midi/api-reference.md), [Examples](docs/pktpy-midi/examples.md) |
| s7-midi | [README](docs/s7-midi/README.md), [API](docs/s7-midi/api-reference.md), [Examples](docs/s7-midi/examples.md) |

See [CHANGELOG.md](CHANGELOG.md) for recent changes.
