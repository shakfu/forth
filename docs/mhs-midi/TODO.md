## TODO

## Music Module Enhancements

- [x] `retrograde :: Music -> Music` - reverse pitch order, keep rhythm
- [x] `invert :: Pitch -> Music -> Music` - melodic inversion around pivot
- [ ] `augment :: Rational -> Music -> Music` - fractional tempo scaling
- [ ] `diminish :: Rational -> Music -> Music`
- [ ] `cut :: Duration -> Music -> Music` - truncate to duration
- [ ] `remove :: Duration -> Music -> Music` - skip initial duration

## Async Scheduler (Completed)

- [x] True concurrent playback using native Haskell threads (`forkIO`)
- [x] `spawn :: String -> IO () -> IO VoiceId` - spawn named voice
- [x] `run :: IO ()` - wait for all voices
- [x] `stop`, `stopAll`, `voices`, `status` - scheduler control
- [x] `asyncNote`, `asyncChord`, `asyncPerform` - async helpers
- [ ] Synchronized start for multiple voices

## Timing Abstractions

- [ ] `Tempo` type with BPM
- [ ] `withTempo :: Tempo -> Music -> Music`
- [ ] Metric modulation helpers
- [ ] Rubato / tempo curves

## Scales and Chords

- [x] `Scale` type (root + intervals) - `type Scale = [Int]` with 55 scales + 10 microtonal
- [x] `scaleNote :: Scale -> Int -> Pitch` - implemented as `scaleDegree`
- [ ] `ChordType` (maj, min, dim, aug, 7th, etc.)
- [ ] `chordPitches :: Pitch -> ChordType -> [Pitch]`

## Alternative Interpreters

- [ ] `toMidiFile :: Music -> ByteString` - export to MIDI file
- [ ] `toEvents :: Music -> [(Time, MidiEvent)]` - timed event list
- [x] `duration :: Music -> Duration` - compute total duration (pure)
- [ ] `pitches :: Music -> [Pitch]` - extract all pitches (pure)

## MIDI Input

- [ ] Input port support in FFI
- [ ] `onNoteOn :: (Pitch -> Velocity -> IO ()) -> IO ()`
- [ ] Live reactive programming

## MidiPerform Enhancements

- [ ] `openPort :: Int -> IO ()` - open hardware port by index
- [ ] `program :: Int -> IO ()` - quick program change
- [ ] `cc :: Int -> Int -> IO ()` - quick control change
- [ ] `bend :: Int -> IO ()` - quick pitch bend

## Implement to `fmemopen` on windows

- [ ] Here are some options to check out.

	1. [an actual implementation](https://github.com/Arryboom/fmemopen_windows)

	2. [include tmp file fallback for windows](https://github.com/Cibiv/PDA/blob/master/fmemopen.c)

	3. [another tmp-based implementation](https://github.com/openSUSE/libsolv/blob/master/win32/fmemopen.c)
