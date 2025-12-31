# Changelog

All notable changes to midi-langs are documented in this file.

## [Unreleased]

### Added

- **mhs-midi Async Scheduler**: Concurrent voice playback using native Haskell threads
  - `spawn name action` - Spawn a new voice with a name, runs in its own thread
  - `run` - Block until all spawned voices complete
  - `stop voiceId` - Stop a specific voice by ID
  - `stopAll` - Stop all active voices
  - `voices` - Get count of active voices
  - `status` - Get scheduler status `(running, count, names)`
  - Async note helpers:
    - `asyncNote` - Play a note asynchronously
    - `asyncChord` - Play a chord asynchronously
    - `asyncPerform` - Perform a Music value asynchronously
  - Pure Haskell implementation using `forkIO`, `MVar`, `ThreadId`
  - No C FFI required for scheduling (unlike other languages that use libuv)
  - Re-exported from `MusicPerform` module for convenience
  - Full test coverage (6 async tests)

- **pktpy-midi Async Tests**: 20 new tests for async scheduler (Tests 28-47)
  - Function existence tests: spawn, run, stop, voices, status
  - Functional tests: spawn/run, voice_id return, multiple voices
  - Control tests: stop specific voice, stop all voices
  - Helper tests: midi.play, midi.play_chord, midi.wait generators
  - Edge case tests: run with no voices, voices count after complete

- **Async Models Documentation**: Cross-language comparison guide
  - New `docs/async-models.md` documenting all five async implementations
  - Comparison tables for threading model, blocking behavior, ease of use
  - Side-by-side code examples showing same musical idea in all languages
  - Guidance on when to use which language for async tasks

- **Non-blocking `poll()` for REPL Responsiveness**: lua-midi, pktpy-midi, s7-midi
  - Added `poll()` function to process ready voices without blocking
  - Returns true/`#t` if voices still active, false/`#f` when done
  - Allows REPL to stay responsive during playback
  - Usage: `while poll() do ... end` (lua), `while midi.poll(): ...` (pktpy), `(let loop () (when (poll) (loop)))` (s7)
  - Full test coverage: lua-midi (Tests 47-51), pktpy-midi (Tests 48-52), s7-midi (Tests 39-43)

- **s7-midi Async Scheduler**: Non-blocking concurrent playback using libuv and thunk-based cooperative multitasking
  - `(spawn thunk [name])` - Create a new voice from a procedure
  - `(run)` - Run scheduler until all voices complete
  - `(stop [voice-id])` - Stop a specific voice or all voices
  - `(voices)` - Get count of active voices
  - `(scheduler-status)` - Get scheduler status alist
  - Thunk-based design: each voice is a procedure that returns:
    - Number (ms to wait before next call)
    - `#f` (voice is complete)
  - Voice builder helpers in prelude:
    - `(make-sequence-voice steps)` - Create voice from `(action . delay)` pairs
    - `(make-note-voice pitch vel dur)` - Single note voice
    - `(make-melody-voice pitches vel dur)` - Sequential notes
    - `(make-chord-voice pitches vel dur)` - Chord voice
    - `(make-repeat-voice thunk n delay)` - Repeat thunk n times
    - `(make-loop-voice thunk delay)` - Loop forever
  - Async playback conveniences:
    - `(async-note pitch [vel] [dur])` - Spawn note voice
    - `(async-chord pitches [vel] [dur])` - Spawn chord voice
    - `(async-melody pitches [vel] [dur])` - Spawn melody voice
  - Up to 16 concurrent voices supported
  - Single-threaded libuv event loop (runs on main thread during `(run)`)
  - Full test coverage (8 new tests for async functionality)

- **pktpy-midi Async Scheduler**: Non-blocking concurrent playback using libuv and Python generators
  - `spawn(func, [name])` - Create a new voice from a generator function
  - `run()` - Run scheduler until all voices complete
  - `stop([voice_id])` - Stop a specific voice or all voices
  - `voices()` - Get count of active voices
  - `status()` - Get scheduler status dict (running, active)
  - Async note helpers for use inside spawned voices:
    - `midi.play(out, pitch, vel, dur, ch)` - Play note (generator)
    - `midi.play_chord(out, pitches, vel, dur, ch)` - Play chord (generator)
    - `midi.play_arp(out, pitches, vel, dur, spacing, ch)` - Arpeggiate (generator)
    - `midi.wait(ms)` - Wait for ms (generator)
  - Up to 16 concurrent voices supported
  - Thread-safe design with dedicated libuv event loop thread
  - Note: PocketPy's `yield from` has issues; use `for ms in ...: yield ms` pattern instead
  - See [yield-from.md](docs/pktpy-midi/yield-from.md) for details on the PocketPy limitation

- **lua-midi Async Scheduler**: Non-blocking concurrent playback using libuv and Lua coroutines
  - `spawn(func, [name])` - Create a new voice (coroutine) from a function
  - `yield_ms(ms)` - Pause the current voice for N milliseconds (non-blocking)
  - `run()` - Run scheduler until all voices complete
  - `stop([voice_id])` - Stop a specific voice or all voices
  - `voices()` - Get count of active voices
  - `scheduler.status()` - Get detailed scheduler status table
  - Async note helpers for use inside spawned voices:
    - `play(pitch, vel, dur, ch)` - Play note with non-blocking wait
    - `play_chord(pitches, vel, dur, ch)` - Play chord with non-blocking wait
    - `play_arp(pitches, vel, dur, ch)` - Arpeggiate with non-blocking wait
  - Up to 16 concurrent voices supported
  - Thread-safe design with dedicated libuv event loop thread
  - Full test coverage (16 new tests)

- **forth-midi Async Sequence Playback**: Non-blocking playback using libuv
  - `seq-play&` - Play current sequence asynchronously (REPL remains responsive)
  - `seq-loop&` - Play current sequence in a continuous loop
  - `seq-stop` - Stop current sequence's async playback
  - `seq-stop-all` - Stop all async playback
  - `seq-playing?` - Check if any async playback is active (returns flag)
  - `seq-active` - Get count of active async players
  - Up to 8 simultaneous sequences can play concurrently
  - Thread-safe design with dedicated event loop thread
  - Added libuv dependency (`thirdparty/libuv/`)

- **forth-midi Sequence Recording Mode**: Record notes directly into sequences
  - `N seq-start` - Begin recording into sequence N (creates if needed)
  - `N seq-end` - End recording into sequence N
  - Notes, chords, and rests played between seq-start/seq-end are captured with timing
  - Dynamics and duration changes apply during recording

- **forth-midi Script/Batch Mode**: CLI flags for non-interactive execution
  - `--script FILE` - Run FILE and exit (no REPL, returns non-zero on error)
  - `--no-sleep` - Disable all sleep/delay calls (for CI/testing)
  - `--help` - Show usage information
  - Files can still be loaded before REPL with positional arguments

- **forth-midi Sequence Export**: Save sequences to files
  - `seq-write-mid filename` - Write current sequence to standard MIDI file
  - `seq-save filename` - Export sequence as .4th source file with seq-note commands

- **forth-midi Context Introspection**: Debug helper
  - `ctx@` - Display current context defaults (channel, velocity, duration, gate, bpm, pitch, midi status)

- **forth-midi Load Diagnostics**: Better error messages when loading files
  - Errors now show `filename:line:` prefix when loading .4th files
  - Helps locate problems in scripts quickly

- **forth-midi MIDI Port Substring Search**: Enhanced midi-open
  - `midi-open` now searches hardware ports by substring before creating virtual port
  - Example: `midi-open` finds "IAC Driver" if available, otherwise creates virtual port

- **Testing Infrastructure**:
  - C unit tests for forth-midi (`test_forth_midi_unit.c`)
    - Tests for `parse_pitch()` with various note names, sharps, flats, articulation suffixes
    - Tests for packed note encoding/decoding
    - Tests for stack operations
    - Tests for tick/ms timing conversions
  - Golden .4th test programs in `tests/golden/`:
    - `test_basic.4th` - Arithmetic, comparisons, stack ops, conditionals, word definitions
    - `test_notation.4th` - Pitch parsing, octave commands, parameter settings
    - `test_blocks.4th` - Anonymous blocks with `{ } n *`
    - `test_generative.4th` - Random, seed operations
    - `test_sequences.4th` - Sequence creation and manipulation
  - Static library `forth_midi_lib` for linking unit tests

- **MIDI File I/O**: Read and write standard MIDI files (.mid) across all language implementations
  - Common C library `midi_file.h/cpp` wrapping libremidi's reader/writer classes
  - **s7-midi**: `(write-mid filename)` and `(read-mid filename)` functions
  - **pktpy-midi**: `midi.write_mid(filename)` and `midi.read_mid(filename)` functions
  - **lua-midi**: `midi.write_mid(filename)` and `midi.read_mid(filename)` functions
  - **forth-midi**: `write-mid filename` and `read-mid filename` commands
  - Write functions save captured MIDI events to standard Type 1 MIDI files
  - Read functions parse MIDI files and return metadata (tracks, ppqn, tempo, duration, format) and events
  - Round-trip tested: write then read back produces consistent data
  - Integrated with existing recording/capture systems in each language

- **s7-midi Readline Support**: Interactive REPL with readline and autocomplete
  - Tab completion for MIDI functions (`midi-open`, `midi-note`, `midi-chord`, etc.)
  - Tab completion for Scheme keywords (`define`, `lambda`, `let`, `if`, etc.)
  - Tab completion for prelude functions (`major`, `minor`, `scale`, `transpose`, etc.)
  - Tab completion for pitch names (`c4`, `cs4`, `db4`, etc.)
  - Tab completion for dynamics (`ppp` through `fff`) and durations (`whole`, `half`, etc.)
  - Tab completion for scale definitions (`scale-major`, `scale-blues`, etc.)
  - Command history with up/down arrow navigation
  - Graceful fallback to basic REPL if readline unavailable

- **lua-midi Readline Autocomplete**: Added tab completion to existing readline support
  - Tab completion for Lua keywords (`function`, `local`, `if`, `for`, etc.)
  - Tab completion for Lua standard library (`print`, `pairs`, `ipairs`, `math`, etc.)
  - Tab completion for MIDI module functions (`midi.open`, `midi.note`, `midi.scales`, etc.)
  - Tab completion for MidiOut methods (`:note`, `:chord`, `:arpeggio`, etc.)
  - Tab completion for prelude functions (`open`, `n`, `ch`, `major`, `minor`, etc.)
  - Tab completion for pitch names (`c4`, `cs4`, `db4`, etc.)
  - Tab completion for dynamics and durations
  - Tab completion for scale names (`major`, `dorian`, `blues`, etc.)

- **pktpy-midi Readline Support**: Interactive REPL with readline and autocomplete
  - Tab completion for Python keywords (`def`, `class`, `if`, `for`, `import`, etc.)
  - Tab completion for Python builtins (`print`, `len`, `range`, `list`, etc.)
  - Tab completion for MIDI module functions (`midi.open`, `midi.note`, `midi.scale`, etc.)
  - Tab completion for MidiOut methods (`.note`, `.chord`, `.arpeggio`, etc.)
  - Tab completion for pitch names (`midi.c4`, `midi.cs4`, etc.)
  - Tab completion for dynamics (`midi.ppp` through `midi.fff`) and durations
  - Tab completion for scale constants (`midi.SCALE_MAJOR`, `midi.SCALE_BLUES`, etc.)
  - Tab completion for CC constants (`midi.CC_MODULATION`, `midi.CC_VOLUME`, etc.)
  - Command history with up/down arrow navigation
  - Graceful fallback to pocketpy's built-in REPL if readline unavailable

- **mhs-midi Pure Generative Algorithms**: Music.hs now includes pure generative functions
  - Pure PRNG: `nextRandom`, `randomRange`, `randomList` (Linear Congruential Generator)
  - Deterministic: `euclideanRhythm`, `arpUp`, `arpDown`, `arpUpDown`, `retrograde`, `invert`
  - Seed-based random: `shuffle`, `pick`, `pickN`, `randomWalk`, `drunkWalk`
  - All functions are pure (no IO) and use explicit seeds for reproducibility

- **mhs-midi Generative Music**: New generative functions in MidiPerform module
  - Random selection: `pick`, `chance`, `oneOf`, `maybeDo`
  - Random sequences: `scramble`, `randomNote`, `randomMelody`
  - Algorithmic patterns: `walk`, `drunk`, `euclidean`
  - Scales for generative use: `major`, `minor`, `pentatonic`, `blues`, `dorian`, etc.
  - Random number FFI: `midiSeedRandom`, `midiRandom`, `midiRandomRange`

### Changed

- **forth-midi Unified Bracket Syntax**: Sequences and lists now use single `[ ]` syntax
  - Removed `[[` and `]]` list notation - use `[ ]` for both sequences and lists
  - Sequences can now hold plain numbers in addition to pitches, chords, rests, dynamics, and durations
  - Updated generative operations to work with sequences:
    - `shuffle` - Fisher-Yates shuffle on sequence elements
    - `reverse` - reverse sequence in place
    - `pick` - pick random element from sequence
    - `pick-n` - pick n random elements, returns new sequence
    - `invert` - invert pitches around axis
    - `arp-up-down` - create sequence with middle reversed appended
    - `random-walk` - outputs sequence instead of stack values
    - `drunk-walk` - takes scale sequence as input, outputs sequence
    - `weighted-pick` - pick from sequence with value/weight pairs
  - All generative operations maintain backward compatibility with alternatives syntax (`c4|e4|g4`)
  - Removed `LIST_MARKER` constant (no longer needed)
  - Removed dead code: `op_list_begin`, `op_list_end`, `op_list_print`, `op_list_len`

- **forth-midi Named Parameters and New Features**:
  - Named parameter syntax: `vel=100` (one-shot), `ch:=2` (persistent)
  - Gate parameter: `gate!`, `gate@`, `gate=80` - percentage of duration to sound (1-100)
  - New sequence operations:
    - `concat` - concatenate two sequences
  - Polymorphic `transpose` - works on both packed notes and bracket sequences
  - Notes and chords now use effective parameters (support one-shot overrides and gate)

- **mhs-midi Module Refactoring**: Clean separation of concerns
  - `Music.hs` - Pure music theory + DSL (no IO, no MIDI concepts)
    - Event type now `ENote Pitch Velocity Duration` (removed Channel)
    - Constructors: `note`, `rest`, `chord`, `line` (no hardcoded defaults)
    - Combinators: `(+:+)`, `(|||)`, `timesM`
    - Transformations: `transpose`, `louder`, `softer`, `stretch`, `compress`
  - `Midi.hs` - Pure FFI bindings only
  - `MusicPerform.hs` - Bridge from pure Music to MIDI
    - `perform :: Music -> IO ()` - perform on channel 1
    - `performOn :: Channel -> Music -> IO ()` - perform on specific channel
    - Microtonal: `centsToBend`, `pitchBendCents`
  - `MidiPerform.hs` - Immediate MIDI playback with generative functions
    - Direct IO: `note`, `chord`, `rest`, `melody`, `arpeggio`
    - Generative: `pick`, `drunk`, `walk`, `euclidean`, `scramble`
    - REPL helpers: `open`, `close`, `panic`, `ports`
  - Renamed `MidiRepl.hs` to `MidiPerform.hs`
  - Deleted `MidiPrelude.hs` (merged into MusicPerform.hs)

### Added

- **Common Music Theory Library**: Shared C library for music-related utilities
  - `projects/common/music_theory.h` - Header with API and constants
  - `projects/common/music_theory.c` - Implementation
  - Pitch parsing: `music_parse_pitch()` converts note names ("C4", "C#4", "Db5") to MIDI numbers
  - Pitch formatting: `music_pitch_to_name()` converts MIDI numbers back to note names
  - Chord intervals: `CHORD_MAJOR`, `CHORD_MINOR`, `CHORD_DIM`, `CHORD_AUG`, `CHORD_DOM7`, `CHORD_MAJ7`, `CHORD_MIN7`, `CHORD_DIM7`, `CHORD_HALF_DIM7`, `CHORD_SUS2`, `CHORD_SUS4`
  - Chord building: `music_build_chord()` builds chords from root + intervals
  - Scale intervals (35 scales):
    - Diatonic modes: `SCALE_MAJOR`, `SCALE_DORIAN`, `SCALE_PHRYGIAN`, `SCALE_LYDIAN`, `SCALE_MIXOLYDIAN`, `SCALE_MINOR`, `SCALE_LOCRIAN`
    - Minor variants: `SCALE_HARMONIC_MINOR`, `SCALE_MELODIC_MINOR`
    - Pentatonic: `SCALE_PENTATONIC_MAJOR`, `SCALE_PENTATONIC_MINOR`
    - Symmetric: `SCALE_WHOLE_TONE`, `SCALE_CHROMATIC`, `SCALE_DIMINISHED_HW`, `SCALE_DIMINISHED_WH`, `SCALE_AUGMENTED`
    - Bebop: `SCALE_BEBOP_DOMINANT`, `SCALE_BEBOP_MAJOR`, `SCALE_BEBOP_MINOR`
    - Exotic: `SCALE_HUNGARIAN_MINOR`, `SCALE_DOUBLE_HARMONIC`, `SCALE_NEAPOLITAN_MAJOR`, `SCALE_NEAPOLITAN_MINOR`, `SCALE_PHRYGIAN_DOMINANT`, `SCALE_PERSIAN`, `SCALE_ALTERED`, `SCALE_ENIGMATIC`
    - Japanese: `SCALE_HIRAJOSHI`, `SCALE_IN_SEN`, `SCALE_IWATO`, `SCALE_KUMOI`
    - World: `SCALE_BLUES`, `SCALE_EGYPTIAN`, `SCALE_ROMANIAN_MINOR`, `SCALE_SPANISH_8_TONE`
    - Arabic Maqamat (12-TET): `SCALE_MAQAM_HIJAZ`, `SCALE_MAQAM_NAHAWAND`, `SCALE_MAQAM_NIKRIZ`, `SCALE_MAQAM_ATHAR_KURD`, `SCALE_MAQAM_SHAWQ_AFZA`, `SCALE_MAQAM_JIHARKAH`
    - Indian Ragas (12-TET): `SCALE_RAGA_BHAIRAV`, `SCALE_RAGA_TODI`, `SCALE_RAGA_MARWA`, `SCALE_RAGA_PURVI`, `SCALE_RAGA_CHARUKESHI`, `SCALE_RAGA_ASAVARI`, `SCALE_RAGA_BILAWAL`, `SCALE_RAGA_KHAMAJ`, `SCALE_RAGA_KALYAN`, `SCALE_RAGA_BHIMPALASI`, `SCALE_RAGA_DARBARI`
  - Microtonal scales (cents-based, for quarter tones and other microtonal intervals):
    - Arabic Maqamat: `SCALE_MAQAM_BAYATI_CENTS`, `SCALE_MAQAM_RAST_CENTS`, `SCALE_MAQAM_SABA_CENTS`, `SCALE_MAQAM_SIKAH_CENTS`, `SCALE_MAQAM_HUZAM_CENTS`, `SCALE_MAQAM_IRAQ_CENTS`, `SCALE_MAQAM_BASTANIKAR_CENTS`
    - Turkish Makamlar: `SCALE_MAKAM_USSAK_CENTS`, `SCALE_MAKAM_HUSEYNI_CENTS`
    - Indian: `SCALE_SHRUTI_CENTS` (22-shruti scale)
  - Microtonal functions:
    - `MicrotonalNote` struct - combines MIDI note with pitch bend offset
    - `music_cents_to_note()` - convert cents interval to MIDI note + bend
    - `music_cents_to_bend()` - convert cents offset to MIDI pitch bend value
    - `music_build_microtonal_scale()` - build scale with pitch bend values
    - `music_microtonal_degree()` - get nth degree of a microtonal scale
  - Scale functions:
    - `music_build_scale()` - build scale pitches from root + intervals
    - `music_scale_degree()` - get nth degree of a scale (supports extended degrees like 9, 11, 13)
    - `music_in_scale()` - check if a pitch belongs to a scale (any octave)
    - `music_quantize_to_scale()` - snap a pitch to the nearest scale tone
  - Dynamics parsing: `music_parse_dynamics()` converts "ppp"-"fff" to velocity values
  - Duration calculation: `music_duration_ms()` calculates note duration from beats and BPM
  - Constants: `DYN_PPP`-`DYN_FFF`, `DUR_WHOLE`-`DUR_SIXTEENTH`, `NOTE_C0`-`NOTE_G9`, `CC_*`

- **Prelude System**: Extracted prelude code from C files into native language source files
  - `projects/s7-midi/prelude.scm` - Scheme prelude (pitch constants, chord builders, etc.)
  - `projects/lua-midi/prelude.lua` - Lua prelude
  - `projects/pktpy-midi/prelude.py` - Python prelude
  - Preludes are converted to C headers (`*_prelude.h`) at build time

- **scripts/prelude2c.py**: Generic script to convert prelude files to C headers
  - Supports `.scm`, `.lua`, `.py`, `.hs` file extensions
  - Handles language-specific comment stripping
  - Generates C string constants for embedding in interpreters
  - Usage: `./scripts/prelude2c.py projects/s7-midi/prelude.scm`

- **Makefile target**: `make preludes` to regenerate all prelude headers
  - Automatic dependency tracking (only regenerates when source changes)
  - Integrated into build process (runs before `configure`)

### Changed

- **forth-midi**: Refactored to use common library
  - `parse_pitch()` now wraps `music_parse_pitch()` with articulation suffix handling
  - Dynamics functions use `DYN_*` constants from music_theory.h
- **lua-midi**: Refactored to use common `music_parse_pitch()` instead of local implementation
  - Added scale support with 55 12-TET scales and 10 microtonal scales
  - New FFI functions: `midi.build_scale()`, `midi.scale_degree()`, `midi.in_scale()`, `midi.quantize()`
  - New MidiOut method: `m:pitch_bend(cents, [ch])` for microtonal playback
  - Scale intervals in `midi.scales` table (major, minor, dorian, blues, pentatonic, maqam_hijaz, raga_bhairav, etc.)
  - Microtonal scales in `midi.scales_cents` table (maqam_bayati, maqam_rast, shruti, etc.)
  - Helper functions: `scale(root, name)`, `degree(root, name, n)`, `in_scale()`, `quantize()`
  - Helper: `midi.cents_to_note(root, cents)` for microtonal interval calculation
- **s7-midi**: Refactored to use common `music_parse_pitch()` instead of local implementation
  - Added scale support with 55 12-TET scales and 10 microtonal scales
  - New FFI functions: `build-scale`, `scale-degree`, `in-scale?`, `quantize-to-scale`
  - New function: `midi-pitch-bend` for microtonal playback
  - Scale intervals as `scale-*` variables (scale-major, scale-dorian, scale-blues, etc.)
  - Microtonal scales as `scale-*-cents` variables (scale-maqam-bayati-cents, etc.)
  - Helper functions: `(scale root 'name)`, `(degree root 'name n)`, `(quantize pitch root 'name)`
  - `*scales*` alist for name-based scale lookup
  - Helper: `(cents-to-note root cents)` for microtonal interval calculation
- **pktpy-midi**: Refactored to use common `music_parse_pitch()` instead of local implementation
  - Added scale support with 55 12-TET scales and 10 microtonal scales
  - New FFI functions: `midi.build_scale()`, `midi.scale_degree()`, `midi.in_scale()`, `midi.quantize_to_scale()`
  - New MidiOut method: `m.pitch_bend(cents, [channel])` for microtonal playback
  - Scale intervals as `midi.SCALE_*` tuples (SCALE_MAJOR, SCALE_DORIAN, SCALE_BLUES, etc.)
  - Microtonal scales as `midi.SCALE_*_CENTS` tuples (SCALE_MAQAM_BAYATI_CENTS, etc.)
  - Lookup dictionaries: `midi.scales` and `midi.scales_cents` for name-based access
  - Helper functions: `midi.scale(root, name)`, `midi.degree(root, name, n)`, `midi.quantize(pitch, root, name)`
  - Helper: `midi.cents_to_note(root, cents)` for microtonal interval calculation
- **mhs-midi**: Added scale support with 55 12-TET scales and 10 microtonal scales
  - New FFI function: `midi_cents_to_bend` for microtonal pitch bend calculation
  - Scale intervals as `scale*` constants (scaleMajor, scaleDorian, scaleBlues, etc.)
  - Microtonal scales as `scale*Cents` constants (scaleMaqamBayatiCents, etc.)
  - Scale functions: `buildScale`, `scaleDegree`, `inScale`, `quantize`
  - Microtonal helpers: `centsToBend`, `centsToNote`, `pitchBendCents`
  - Pure Haskell implementation for scale operations (no FFI needed for computation)
- **forth-midi**: Added scale support with 49 12-TET scales
  - Scale constant words: `scale-major`, `scale-minor`, `scale-dorian`, `scale-blues`, `scale-pentatonic`, etc.
  - Scale operations: `scale` (build scale pitches), `degree` (get nth degree), `in-scale?` (membership check), `quantize` (snap to scale)
  - Utility: `scales` word lists all 49 available scales
  - Microtonal helpers: `cents>bend` (convert cents to pitch bend), `pb-cents` (send pitch bend in cents)
  - Updated `help` output with Scales and Microtonal sections
  - New documentation: `docs/forth-midi/scales.md`

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
  - Virtual port: `midi-open`, `midi-open-as <name>`
  - Hardware ports: `midi-list`, `midi-open-port`, `midi-close`
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
