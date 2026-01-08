# midi-langs - a polyglot toolkit for MIDI creation

A collection of minimalist yet powerful language implementations designed to generate, manipulate, and transform MIDI sequences in distinct stylistic and functional paradigms.

Each implementation leverages [libremidi](https://github.com/celtera/libremidi) for seamless cross-platform MIDI I/O:

| Implementation | Language| Style & Approach |
|----------|----------|-------|
| **alda-midi** | [Alda](https://alda.io/) | Music-first notation--compose with elegant, human-readable syntax. |
| **forth-midi** | Custom Forth | Concise stack-based programming for minimalist control over MIDI events. |
| **lua-midi** | [Lua 5.5](https://www.lua.org/) | Lightweight scripting with tables, closures, and event-driven workflows. |
| **mhs-midi** | [MicroHs](https://github.com/augustss/MicroHs) | Pure functional Haskell for compositions, algorithmic patterns, and type-safe transformations. |
| **pktpy-midi** | [PocketPy](https://pocketpy.dev) | Pythonic abstraction with context managers and expressive APIs. |
| **s7-midi** | [s7 Scheme](https://ccrma.stanford.edu/software/snd/snd/s7.html) | Macros + Lisp for meta-programming and dynamic MIDI patterns. |

Note: This is an evolving project—-expect API iterations as implementations mature.

## Building

```sh
make              # Build everything
make test         # Run all tests (33 tests)
make clean        # Remove build directory
```

Requires GCC/Clang and CMake 3.16+.

**Linux (Ubuntu/Debian):** Install ALSA and readline development libraries:

```sh
sudo apt-get install libasound2-dev libreadline-dev
```

## Quick Examples

### alda-midi

```sh
% ./alda_midi --help
Usage: ./alda_midi [options] [file.alda]

Alda music language interpreter with MIDI output.
If no file is provided, starts an interactive REPL.

Options:
  -h, --help        Show this help message
  -v, --verbose     Enable verbose output
  -l, --list        List available MIDI ports
  -p, --port N      Use MIDI port N (0-based index)
  -o, --output NAME Use MIDI port matching NAME
  --virtual NAME    Create virtual MIDI port with NAME
  --no-sleep        Disable timing delays (for testing)
  -s, --sequential  Use sequential playback mode (wait for each input)

Built-in Synth Options:
  -sf, --soundfont PATH  Use built-in synth with soundfont (.sf2)

By default, connects to the first available MIDI port (or creates a virtual
port if none exist) and uses concurrent mode for polyphonic playback.

Examples:
  ./alda_midi                            Start interactive REPL
  ./alda_midi song.alda                  Play an Alda file
  ./alda_midi -l                         List MIDI ports
  ./alda_midi -p 0 song.alda             Play using port 0
  ./alda_midi --virtual iMIDI song.alda  Create virtual port + play song
  ./alda_midi -sf gm.sf2 song.alda       Use built-in synth
```

You can enter alda syntax in a repl or create `.alda` files in a text-editor:

```alda
piano:
  (tempo 120)
  o4 c4 d e f g a b > c2

violin:
  V1: c4 d e f
  V2: e4 f g a
  V0: c1
```

By default, alda-midi uses concurrent mode where multiple parts play simultaneously on different MIDI channels. Use `-s` for sequential mode.

[Full documentation](docs/alda-midi/README.md) | [Language Reference](docs/alda-midi/language-reference.md)

### forth-midi

```sh
% ./build/forth_midi --help
Usage: ./build/forth_midi [options] [file.4th ...]
Options:
  --script FILE   Run FILE in batch mode (no REPL, exit on error)
  --no-sleep      Disable all sleep/delay calls (for testing)
  --help          Show this help

Without --script, files are loaded then REPL starts.
```

```forth
midi-open
\ Concise notation with probability and articulation
mf c4, e4. g4> c5-,          \ staccato, accent, tenuto
c4|e4|g4, 75%,               \ random selection, 75% chance
(c4 e4 g4),                  \ chord

\ Generative pattern with anonymous block
{ c4, d4, e4, } 4 *          \ repeat block 4 times

\ Async sequence playback
seq-new 0 seq-start
  c4, e4, g4,
0 seq-end
seq-play&                    \ non-blocking playback
```

[Full documentation](docs/forth-midi/README.md) | [API](docs/forth-midi/api-reference.md) | [Tutorial](docs/forth-midi/tutorial.md)

### lua-midi

```sh
% ./build/lua_midi --help
Usage: ./build/lua_midi [options] [file.lua]
Options:
  -e EXPR    Execute Lua statement
  --version  Show version
  --help     Show this help

Without arguments, starts an interactive REPL.
```

```lua
open()
-- Concurrent voices with coroutines
spawn(function()
    for _, p in ipairs(scale(c4, "pentatonic")) do
        play(p, mf, eighth)
    end
end, "melody")

spawn(function()
    ch(major(c3), ff, whole)  -- sustained chord
end, "harmony")

run()  -- wait for all voices
close()
```

[Full documentation](docs/lua-midi/README.md) | [API Reference](docs/lua-midi/api-reference.md)

### mhs-midi

```sh
% ./scripts/mhs-midi --help
usage: mhs-midi [-h] {repl,compile,run} ...

Commands:
  repl              Start interactive REPL (default)
  compile FILE.hs   Compile to standalone executable
  run FILE.hs       Compile and immediately run
```

**Interactive REPL:**
```sh
./scripts/mhs-midi              # Start REPL
./scripts/mhs-midi repl         # Same, explicit
```

**Standalone binary** (no external files needed):
```sh
cmake --build build --target mhs-midi-standalone
./build/mhs-midi-standalone -r MyFile.hs
```

**Write and compile a program:**
```haskell
-- DemoMhs.hs
module DemoMhs(main) where

import MusicPerform

main = do
    midiOpenVirtual "demo"
    -- Pure functional Music DSL with combinators
    let melody = line [c4, e4, g4, c5] mf eighth
        bass   = note c3 ff whole
        piece  = melody ||| bass  -- parallel composition

    perform piece

    -- Concurrent voices with native threads
    spawn "arp" $ perform (line (arpUp [c4, e4, g4]) mp sixteenth)
    spawn "pad" $ perform (chord [c3, g3, e4] pp whole)
    run
    midiClose
```

```sh
./scripts/mhs-midi compile demo.hs        # Creates ./demo
./scripts/mhs-midi compile demo.hs -o out # Creates ./out
./scripts/mhs-midi run demo.hs            # Compile and run
```

[Full documentation](docs/mhs-midi/README.md) | [API Reference](docs/mhs-midi/api-reference.md)

### pktpy-midi

```sh
% ./build/pktpy_midi --help
Usage: ./build/pktpy_midi [options] [file.py]

PocketPy interpreter with MIDI support.

Options:
  -e EXPR        Execute Python statement
  -l, --list     List available MIDI output ports
  --profile      Enable profiler (file mode only)
  --debug        Enable debugger (file mode only)
  -v, --version  Show version information
  -h, --help     Show this help message

Without arguments, starts an interactive REPL.
```

A pktpy-midi file is a regular python3 file which can import the custom `midi` module (and other [pocketpy](https://pocketpy.dev) modules):

```python
import midi

# Generator-based async voices
def arpeggio():
    with midi.open() as m:
        for p in midi.scale(midi.c4, "dorian"):
            yield from midi.play(m, p, midi.mf, midi.eighth)

def drone():
    with midi.open() as m:
        yield from midi.play(m, midi.c2, midi.pp, midi.whole * 4)

# Concurrent playback
midi.spawn(arpeggio, "arp")
midi.spawn(drone, "drone")
midi.run()
```

[Full documentation](docs/pktpy-midi/README.md) | [API Reference](docs/pktpy-midi/api-reference.md)

### s7-midi

```sh
./build/s7_midi
```

```scheme
(open)
;; Functional transformations
(define melody (scale c4 'dorian))
(define bass (invert melody c4))  ; melodic inversion

;; Thunk-based concurrent voices
(spawn (make-melody-voice melody mf eighth) "melody")
(spawn (make-melody-voice bass ff quarter) "bass")

;; Euclidean rhythm generator
(spawn (lambda ()
  (euclidean 3 8  ; 3 hits over 8 steps
    (lambda () (n c2 ff sixteenth))))
  "rhythm")

(run)
(close)
```

[Full documentation](docs/s7-midi/README.md) | [API Reference](docs/s7-midi/api-reference.md)

## Common Features

Most implementations share functionality from a common C library (`projects/common/music_theory.c`):

| Implementation | Uses music_theory.c |
|----------------|---------------------|
| forth-midi | Yes |
| lua-midi | Yes |
| mhs-midi | Yes |
| pktpy-midi | Yes |
| s7-midi | Yes |
| alda-midi | No (uses own parser/interpreter) |

The shared library provides:

- **Pitch parsing**: Note names like "C4", "C#4", "Db5" to MIDI numbers
- **55 scales**: Major, minor, modes, pentatonic, blues, world scales, maqamat, ragas
- **10 microtonal scales**: Quarter-tone maqamat, Turkish makamlar, Indian shruti
- **Scale operations**: Build scales, get degrees, check membership, quantize
- **Chord intervals**: Major, minor, diminished, augmented, 7th chords
- **Dynamics**: ppp to fff velocity constants
- **Durations**: Whole, half, quarter, eighth, sixteenth note values

## Prelude System

Interpreted languages have prelude files in their native syntax:

```sh
projects/s7-midi/prelude.scm    -> Scheme constants and helpers
projects/lua-midi/prelude.lua   -> Lua constants and helpers
projects/pktpy-midi/prelude.py  -> Python constants and helpers
```

These are converted to C headers at build time via `scripts/prelude2c.py`.

```sh
make preludes     # Regenerate all prelude headers
```

## Architecture

```sh
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

```sh
# Set up SoundFont directory (one-time)
export ALDA_SF2_DIR=~/Music/sf2

# Start FluidSynth (in one terminal)
python scripts/fluidsynth-gm.py

# alternatively you can directly specify the soundfont
python scripts/fluidsynth-gm.py ~/Music/sf2/FluidR3_GM.sf2

# Run any midi-langs interpreter (in another terminal)
./build/alda_midi
./build/forth_midi
./build/lua_midi
```

Install [FluidSynth](https://www.fluidsynth.org) and a SoundFont:

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

## Prior Art

This is not a comprehensive list:

- [abcmidi](https://abcmidi.sourceforge.io) - Abc2midi is a program that converts an abc music notation file to a MIDI file.

- [alda](https://alda.io) - a text-based programming language for music composition. Designed for musicians who don’t know how to program, as well as programmers who don’t know how to music. 

- [chuck](https://chuck.cs.princeton.edu) - a strongly-timed programming language for real-time sound synthesis and music creation.

- [extempore](https://extemporelang.github.io) - an audiovisual live programming environment.

- [lilypond](https://lilypond.org) - a compiled system: it is run on a text file describing the music. The resulting output is viewed on-screen or printed. In some ways, LilyPond is more similar to a programming language than graphical score editing software.

- [melrose](https://github.com/emicklei/melrose) - a tool to create and play music by programming melodies. It uses a custom language to compose notes and create loops and tracks to play.

- [miti](https://github.com/schollz/miti) - a musical instrument textual interface. Basically, its MIDI, but with human-readable text.

- [mma](https://www.mellowood.ca/mma/) - an accompaniment generator. It creates MIDI tracks for a soloist to perform over from a user supplied file containing chords and MMA directives.

- [opusmodus](https://www.opusmodus.com) - a commercial music composition development environment. -- "currently the most advanced software for computer-assisted composition available".

- [textbeat](https://github.com/flipcoder/textbeat) - Compose music in a plaintext format or type music directly in the shell. The format is vertical and column-based, similar to early music trackers, but with syntax inspired by jazz/music theory.



