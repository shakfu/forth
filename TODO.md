# Future Improvements

Ideas for extending both MIDI implementations.


## All Implementations

### REPL Improvements

- [ ] Readline support for all REPLs (command history, line editing)
  - [x] forth-midi: readline support
  - [x] lua-midi: readline support (optional)
  - [x] s7-midi: add readline support
  - [ ] pktpy-midi: add readline support
  - [ ] mhs-midi: add readline support
- [ ] Autocomplete for all REPLs
  - [x] forth-midi: experimental autocomplete (pitches, words, builtins)
  - [x] lua-midi: add autocomplete for MIDI functions, pitches, dynamics
  - [x] s7-midi: add autocomplete for Scheme functions, pitches
  - [ ] pktpy-midi: add autocomplete for midi module, pitches
  - [ ] mhs-midi: add autocomplete for Haskell functions, pitches

### Testing

- [ ] Property-based testing for Music transformations
- [ ] MIDI output verification (mock port)
- [ ] Performance benchmarks

### Documentation

- [ ] Video tutorials
- [ ] Example compositions
- [ ] Comparison guide (when to use which)

### Integration

- [ ] OSC support for external control
- [ ] Ableton Link for tempo sync
- [ ] Jack audio connection kit support (Linux)

---

## forth-midi

### Sequencer / Timing

- [ ] Non-blocking sequences with absolute timing
- [ ] Tempo changes mid-sequence (`bpm` word)
- [ ] Swing/shuffle timing modifier
- [ ] Polyrhythms (independent time divisions)

### Patterns

- [ ] Named pattern storage (`pattern! pattern@`)
- [ ] Pattern transformations (reverse, invert, retrograde)
- [ ] Euclidean rhythms (`euclidean` word)
- [ ] Markov chains for generative sequences

### Scales / Harmony

- [x] Scale definitions (major, minor, modes) - 49 scales via `scale-*` words
- [x] Scale-constrained random (`quantize` word snaps to scale)
- [ ] Chord symbols (`Cmaj7`, `Dm`, `G7`)
- [ ] Automatic voice leading

### MIDI Input

- [ ] MIDI input handling (note triggers)
- [ ] Live looping / overdub
- [ ] MIDI learn for CC mapping

### Control Flow

- [ ] `do ... loop` counted loops
- [ ] `begin ... until` / `begin ... while ... repeat`
- [ ] Variables (`variable`, `!`, `@`)
- [ ] Local variables in word definitions

### File I/O

- [ ] Load/save word definitions
- [ ] MIDI file export
- [ ] Import patterns from files

---

## mhs-midi

### Music Module Enhancements

- [ ] `reverse :: Music -> Music` - play backwards
- [ ] `invert :: Pitch -> Music -> Music` - melodic inversion around pivot
- [ ] `retrograde :: Music -> Music` - reverse pitch order, keep rhythm
- [ ] `augment :: Rational -> Music -> Music` - fractional tempo scaling
- [ ] `diminish :: Rational -> Music -> Music`
- [ ] `cut :: Duration -> Music -> Music` - truncate to duration
- [ ] `remove :: Duration -> Music -> Music` - skip initial duration

### Parallel Sequences

- [ ] True concurrent playback using threads
- [ ] `forkMusic :: Music -> IO ThreadId`
- [ ] Synchronized start for multiple voices

### Timing Abstractions

- [ ] `Tempo` type with BPM
- [ ] `withTempo :: Tempo -> Music -> Music`
- [ ] Metric modulation helpers
- [ ] Rubato / tempo curves

### Scales and Chords

- [x] `Scale` type (root + intervals) - `type Scale = [Int]` with 55 scales + 10 microtonal
- [x] `scaleNote :: Scale -> Int -> Pitch` - implemented as `scaleDegree`
- [ ] `ChordType` (maj, min, dim, aug, 7th, etc.)
- [ ] `chordPitches :: Pitch -> ChordType -> [Pitch]`

### Alternative Interpreters

- [ ] `toMidiFile :: Music -> ByteString` - export to MIDI file
- [ ] `toEvents :: Music -> [(Time, MidiEvent)]` - timed event list
- [ ] `duration :: Music -> Duration` - compute total duration (pure)
- [ ] `pitches :: Music -> [Pitch]` - extract all pitches (pure)

### MIDI Input

- [ ] Input port support in FFI
- [ ] `onNoteOn :: (Pitch -> Velocity -> IO ()) -> IO ()`
- [ ] Live reactive programming

### MidiRepl Enhancements

- [ ] `openPort :: Int -> IO ()` - open hardware port by index
- [ ] `program :: Int -> IO ()` - quick program change
- [ ] `cc :: Int -> Int -> IO ()` - quick control change
- [ ] `bend :: Int -> IO ()` - quick pitch bend
