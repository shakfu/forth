# Future Improvements

Ideas for extending MIDI implementations.

## All Implementations

### Polyphony and Sequencing cocurrent patterns

### Testing

- [ ] Property-based testing for Music transformations
- [ ] MIDI output verification (mock port)
- [ ] Performance benchmarks

### Documentation

- [ ] Video tutorials
- [x] Example compositions - see `examples/` directory
- [x] Comparison guide (when to use which)
- [ ] Troubleshooting guide (common issues and solutions)
- [ ] Document performance characteristics (startup time, memory usage)

### Integration

- [ ] OSC support for external control
- [ ] Ableton Link for tempo sync
- [ ] Jack audio connection kit support (Linux)
- [ ] MusicXML import/export (maybe [libmusicxml])(https://github.com/grame-cncm/libmusicxml))

### Enhance existing async features

- [x] REPL responsiveness during run - added `poll()` to lua/pktpy/s7
- [ ] Inter-voice communication/sync primitives
- [ ] Async MIDI input handling

### Unify/document async patterns

- [x] Create cross-language async examples - see `docs/async-models.md`
- [x] Document the different async models - see `docs/async-models.md`


---

## forth-midi

### Sequencer / Timing

- [ ] Timeline-based sequencing (more intuitive for complex compositions)
- [ ] Non-blocking sequences with absolute timing
- [ ] Tempo changes mid-sequence (`bpm` word)
- [ ] Swing/shuffle timing modifier
- [ ] Polyrhythms (independent time divisions)

### Patterns

- [ ] Named pattern storage (`pattern! pattern@`)
- [ ] Pattern transformations (reverse, invert, retrograde)
- [x] Euclidean rhythms (`euclidean` word)
- [ ] Markov chains for generative sequences

### Scales / Harmony

- [x] Scale definitions (major, minor, modes) - 49 scales via `scale-*` words
- [x] Scale-constrained random (`quantize` word snaps to scale)
- [ ] Chord symbols (`Cmaj7`, `Dm`, `G7`)
- [ ] Automatic voice leading

### MIDI Input

- [x] MIDI input handling - polling-based input with message queue
  - `midi-input-list`, `midi-input-open`, `midi-input-virtual`, `midi-input-close`
  - `midi-input?` (check pending), `midi-input@` (read message), `midi-input-flush`
  - Lock-free ring buffer for callback thread safety
- [ ] MIDI input handlers (note triggers) - event-driven callbacks
- [ ] Live looping / overdub
- [ ] MIDI learn for CC mapping

### Control Flow

- [x] `do ... loop` counted loops - with `i`, `j`, `+loop`, `leave`
- [x] `begin ... until` / `begin ... while ... repeat`
- [ ] Variables (`variable`, `!`, `@`)
- [ ] Local variables in word definitions

### File I/O

- [ ] Load/save word definitions
- [ ] MIDI file export
- [ ] Import patterns from files

---

## pktpy-midi

### Improve pktpy-midi async

- [x] Add pktpy-midi async tests - 20 async tests added (Tests 28-47)
- [ ] Voice naming/listing in `status()`
- [x] `midi.ms(n)` / `midi.yield_ms(n)` helper for cleaner generator syntax
- [ ] Exception handling improvements
- [x] Fix sequential `run()` calls - fixed by adding `uv_async_send` after timer start

---

## mhs-midi

### Music Module Enhancements

- [x] `retrograde :: Music -> Music` - reverse pitch order, keep rhythm
- [x] `invert :: Pitch -> Music -> Music` - melodic inversion around pivot
- [ ] `augment :: Rational -> Music -> Music` - fractional tempo scaling
- [ ] `diminish :: Rational -> Music -> Music`
- [ ] `cut :: Duration -> Music -> Music` - truncate to duration
- [ ] `remove :: Duration -> Music -> Music` - skip initial duration

### Async Scheduler (Completed)

- [x] True concurrent playback using native Haskell threads (`forkIO`)
- [x] `spawn :: String -> IO () -> IO VoiceId` - spawn named voice
- [x] `run :: IO ()` - wait for all voices
- [x] `stop`, `stopAll`, `voices`, `status` - scheduler control
- [x] `asyncNote`, `asyncChord`, `asyncPerform` - async helpers
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
- [x] `duration :: Music -> Duration` - compute total duration (pure)
- [ ] `pitches :: Music -> [Pitch]` - extract all pitches (pure)

### MIDI Input

- [ ] Input port support in FFI
- [ ] `onNoteOn :: (Pitch -> Velocity -> IO ()) -> IO ()`
- [ ] Live reactive programming

### MidiPerform Enhancements

- [ ] `openPort :: Int -> IO ()` - open hardware port by index
- [ ] `program :: Int -> IO ()` - quick program change
- [ ] `cc :: Int -> Int -> IO ()` - quick control change
- [ ] `bend :: Int -> IO ()` - quick pitch bend
