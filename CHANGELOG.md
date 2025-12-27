# Changelog

All notable changes to midi-langs are documented in this file.

## [Unreleased]

### Added

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

## [0.2.0]

### Added

- **mhs-midi**: New Haskell-based MIDI language using MicroHs
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
