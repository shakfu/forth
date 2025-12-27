# Changelog

All notable changes to midi-langs are documented in this file.

## [Unreleased]

## [0.1.4]

### Added

- **lua_midi**: New Lua-based MIDI language using [Lua 5.5](https://www.lua.org/)
  - `projects/lua_midi/` - Complete project structure
  - `midi_module.c` - Lua FFI bindings wrapping libremidi
  - Lua 5.5 interpreter (embedded from `thirdparty/lua-5.5.0/`)
  - Core features:
    - `midi.open()` - Virtual port with default name "luaMIDI"
    - `midi.open("name")` - Virtual port with custom name
    - `midi.open(index)` - Hardware port by index
    - `midi.list_ports()` - List available MIDI ports
    - `m:close()` - Close MIDI port
    - `m:is_open()` - Check if port is open
  - Note playing:
    - `m:note(pitch, [vel], [dur], [ch])` - Play single note
    - `m:chord(pitches, [vel], [dur], [ch])` - Play chord
    - `m:arpeggio(pitches, [vel], [dur], [ch])` - Arpeggiate
  - REPL convenience functions (use global `midi._out` port):
    - `open()`, `open("name")`, `open(index)` - Open port and set default
    - `close()` - Close default port
    - `n(pitch, [vel], [dur], [ch])` - Play note on default port
    - `ch(pitches, [vel], [dur], [ch])` - Play chord on default port
    - `arp(pitches, [vel], [dur], [ch])` - Arpeggiate on default port
  - Pitch helpers:
    - `midi.note("C4")` - Parse note names to MIDI numbers
    - `midi.c0`-`midi.c8`, `midi.cs0`-`midi.cs8`, etc. - Pitch constants
    - `midi.transpose(pitch, semitones)`, `midi.octave_up()`, `midi.octave_down()`
  - Dynamics: `midi.ppp` through `midi.fff` (velocity values 16-127)
  - Durations: `midi.whole`, `midi.half`, `midi.quarter`, `midi.eighth`, `midi.sixteenth`, `midi.dotted(dur)`
  - Chord builders: `midi.major()`, `midi.minor()`, `midi.dim()`, `midi.aug()`, `midi.dom7()`, `midi.maj7()`, `midi.min7()`
  - Tempo: `midi.set_tempo(bpm)`, `midi.get_tempo()`, `midi.bpm(tempo)`
  - Timing: `midi.sleep(ms)`, `midi.rest([dur])`
  - Low-level: `m:note_on`, `m:note_off`, `m:cc`, `m:program`, `m:all_notes_off`
  - Test suite with 26 tests

- **s7_midi**: New Scheme-based MIDI language using [s7](https://ccrma.stanford.edu/software/snd/snd/s7.html)
  - `projects/s7_midi/` - Complete project structure
  - `midi_module.c` - s7 FFI bindings wrapping libremidi
  - s7 Scheme interpreter (embedded from `thirdparty/s7/`)
  - Core features:
    - `(midi-open)` - Virtual port with default name "s7MIDI"
    - `(midi-open "name")` - Virtual port with custom name
    - `(midi-open index)` - Hardware port by index
    - `(midi-list-ports)` - List available MIDI ports
    - `(midi-close m)` - Close MIDI port
    - `(midi-out? x)`, `(midi-open? m)` - Type predicates
  - Note playing:
    - `(midi-note m pitch [vel] [dur] [ch])` - Play single note
    - `(midi-chord m pitches [vel] [dur] [ch])` - Play chord
    - `(midi-arpeggio m pitches [vel] [dur] [ch])` - Arpeggiate
  - REPL convenience functions (use global `*midi*` port):
    - `(open)`, `(open "name")`, `(open index)` - Open port and set `*midi*`
    - `(close)` - Close `*midi*` port
    - `(n pitch [vel] [dur] [ch])` - Play note on `*midi*`
    - `(ch pitches [vel] [dur] [ch])` - Play chord on `*midi*`
    - `(arp pitches [vel] [dur] [ch])` - Arpeggiate on `*midi*`
  - Pitch helpers:
    - `(note "C4")` or `(note 'c4)` - Parse note names to MIDI numbers
    - `c0`-`c8`, `cs0`-`cs8`, etc. - Pitch constants
    - `(transpose pitch semitones)`, `(octave-up)`, `(octave-down)`
  - Dynamics: `ppp` through `fff` (velocity values 16-127)
  - Durations: `whole`, `half`, `quarter`, `eighth`, `sixteenth`, `(dotted dur)`
  - Chord builders: `(major root)`, `(minor root)`, `(dim root)`, `(aug root)`, `(dom7 root)`, `(maj7 root)`, `(min7 root)`
  - Tempo: `(set-tempo! bpm)`, `(get-tempo)`, `(bpm tempo)`
  - Timing: `(midi-sleep ms)`, `(rest [dur])`
  - Low-level: `midi-note-on`, `midi-note-off`, `midi-cc`, `midi-program`, `midi-all-notes-off`
  - Test suite with 26 tests

- **pktpy_midi**: New Python-based MIDI language using [PocketPy](https://pocketpy.dev)
  - `projects/pktpy_midi/` - Complete project structure
  - `midi_module.c` - C bindings for libremidi with Pythonic API
  - PocketPy v2.1.6 embedded interpreter
  - Core features:
    - `midi.open()` - Virtual port with default name "pktpyMIDI"
    - `midi.open("name")` - Virtual port with custom name
    - `midi.open(index)` - Hardware port by index
    - `midi.list_ports()` - List available MIDI ports
    - Context manager support (`with midi.open() as m:`)
  - Note playing:
    - `MidiOut.note(pitch, velocity, duration, channel)` - Play single note
    - `MidiOut.chord(pitches, velocity, duration, channel)` - Play chord
    - `MidiOut.arpeggio(pitches, velocity, note_duration, spacing, channel)` - Arpeggiate
  - Pitch helpers:
    - `midi.note("C4")` - Parse note names to MIDI numbers
    - `midi.c4`, `midi.cs4`, etc. - Pitch constants (c0-b8)
    - `midi.transpose(pitch, semitones)` - Transpose pitch
    - `midi.octave_up()`, `midi.octave_down()` - Octave shifts
  - Dynamics: `midi.ppp` through `midi.fff` (velocity values 16-127)
  - Durations: `midi.whole`, `midi.half`, `midi.quarter`, `midi.eighth`, `midi.sixteenth`
  - Chord builders: `midi.major()`, `midi.minor()`, `midi.dim()`, `midi.aug()`, `midi.dom7()`, `midi.maj7()`, `midi.min7()`
  - Tempo: `midi.set_tempo(bpm)`, `midi.get_tempo()`, `midi.bpm(tempo)`
  - Timing: `midi.sleep(ms)`, `midi.rest(duration)`
  - CC helpers: `modulation()`, `volume()`, `pan()`, `sustain()`
  - Low-level: `note_on`, `note_off`, `cc`, `program_change`, `all_notes_off`
  - Test suite with 23 tests

- **mhs-midi REPL**: Interactive Haskell REPL with MIDI FFI support
  - `./scripts/mhs-midi-repl` - Start REPL with caching for fast startup
  - `./scripts/mhs-midi-repl -r File.hs` - Run a Haskell file (interpreted)
  - `./scripts/mhs-midi-compile File.hs -o output` - Compile to standalone executable
  - Enables `import Midi` for interactive MIDI programming
  - Uses MicroHs `xffi_table` extension mechanism for FFI injection
  - Documentation in `docs/mhs-midi/`:
    - API reference with all functions and types
    - Examples from basic to advanced
    - Architecture overview of FFI integration

## [0.1.3]

### Added

- **mhs-midi**: New Haskell-based MIDI language using [MicroHs](https://github.com/augustss/MicroHs)
  - `projects/mhs_midi/` - Complete project structure
  - `midi_ffi.c/h` - C FFI bindings wrapping libremidi
  - `lib/Midi.hs` - High-level Haskell MIDI library with:
    - Pitch names: `c0`-`c8`, `d0`-`d8`, etc. (with sharps `cs4`, `fs4`)
    - Durations: `whole`, `half`, `quarter`, `eighth`, `sixteenth`, `dotted`
    - Dynamics: `ppp`, `pp`, `p`, `mp`, `mf`, `ff`, `fff`
    - Note playing: `play`, `playNote`, `playChord`, `chord`
    - Sequences: `melody`, `arpeggio`, `times`
    - MIDI control: `midiOpen`, `midiOpenVirtual`, `midiClose`, `midiCC`, `midiProgram`
  - Example programs:
    - `hello_midi` - C major scale
    - `chords` - I-IV-V-I chord progression
    - `melody_example` - Twinkle Twinkle Little Star
    - `arpeggio` - Arpeggiated patterns
    - `list_ports` - List available MIDI ports

- **MicroHs Integration**
  - Added `thirdparty/MicroHs/` dependency
  - CMake integration for compiling Haskell to native executables
  - FFI bindings using MicroHs `foreign import ccall`

## [0.1.2]

### Added

- **MIDI Capture System**: Record MIDI events with timing
  - `capture` - Start capturing all MIDI output with timestamps
  - `save-midi filename` - Save captured events as sequence file
  - Pairs note-on/off events, converts timing to ticks
  - Output files can be loaded and transformed with sequence operations

- **Command Recording**: Record interactive sessions
  - `rec` - Start recording input commands
  - `save filename` - Save recorded commands to .4th file
  - Control commands (rec, stop, save, load, capture) are excluded from recordings

- **File Loading**: Execute .4th files
  - `load filename` - Load and execute a Forth file
  - Supports nested loading (up to 8 levels deep)
  - Skips empty lines and Forth comments (`\`)

- **CMake Build System**
  - Converted from Makefile to CMake with CTest integration
  - Makefile frontend preserved for convenience (`make`, `make test`, `make clean`)
  - Executables output to `build/` directory
  - Quick tests labeled for fast feedback during development

- **Comprehensive Test Suite**: 86 tests covering all features
  - Pitch parsing, arithmetic, stack operations
  - Word definitions, conditionals, anonymous blocks
  - Alternatives, probability, packed notes
  - File loading, recording, MIDI capture
  - Error handling

## [0.1.1]

### Added

- **Concise Notation**: Primary note input system
  - Pitch names with octaves: `c4,` `c#4,` `db4,`
  - Comma triggers note playback
  - Chords with parentheses: `(c4 e4 g4),`
  - Rests: `r,`
  - Default parameters: `ch!`, `vel!`, `dur!`
  - Explicit parameters: `ch pitch vel dur,`

- **Dynamics**: Velocity control with musical markings
  - `ppp` (16), `pp` (32), `p` (48), `mp` (64)
  - `mf` (80), `f` (96), `ff` (112), `fff` (127)

- **Articulation**: Note modifiers
  - Staccato: `c4.,` (50% duration)
  - Accent: `c4>,` (+20 velocity)
  - Tenuto: `c4-,` (full duration)

- **Relative Intervals**: Movement from last pitch
  - Semitone steps: `+2,` `-3,`
  - Octave jumps: `^,` `v,`

- **Generative Features**
  - Alternatives: `c4|e4|g4,` (random selection)
  - Probability: `c4 75%,` (chance to play)
  - Random: `random` (push 0-99)

- **Control Flow**
  - Conditionals: `if ... then`, `if ... else ... then`
  - Anonymous blocks: `{ ... } n *`
  - Word definitions: `: name ... ;`
  - Loops: `name n times`

- **Sequences**: MIDI event storage and manipulation
  - Create/select: `seq-new`, `seq`, `seq@`
  - Add notes: `seq-note`, `seq-note-ch`, `seq-add`
  - Playback: `seq-play`, `seq-show`, `seq-length`
  - Transform: `seq-transpose`, `seq-reverse`, `seq-stretch`
  - Clear: `seq-clear`

- **Packed Notes**: Single-value note representation
  - Create: `note` (pitch vel ch dur)
  - Play: `note!`
  - Extract: `pitch@`, `vel@`, `ch@`, `dur@`
  - Transform: `transpose`
  - Debug: `note.`

- **Chord Builders**: Build chords from root note
  - Triads: `major`, `minor`, `dim`, `aug`
  - Sevenths: `dom7`, `maj7`, `min7`
  - Playback: `play-chord`, `chord>seq`, `arp>seq`

- **Duration Constants**
  - `whole` (1920), `half` (960), `quarter` (480)
  - `eighth` (240), `sixteenth` (120)

- **MIDI Output**
  - Virtual port: `midi-virtual`
  - Hardware ports: `midi-list`, `midi-open`, `midi-close`
  - Control: `cc`, `pc`, `pb`, `panic`

- **Timing**
  - Tempo: `bpm!`, `bpm@`
  - Sleep: `ms`

- **Stack Operations**
  - Arithmetic: `+`, `-`, `*`, `/`
  - Stack: `dup`, `drop`, `swap`, `over`, `rot`, `clear`
  - Comparison: `=`, `<`, `>`
  - Bitwise: `and`, `or`, `xor`, `not`
  - Output: `.`, `.s`, `cr`, `space`

## [0.1.0]

### Added

- Initial Forth interpreter (`forth.c`)
- Basic MIDI integration with libremidi
- Note-on/note-off commands (later replaced by concise notation)
- libremidi v5.3.1 dependency
