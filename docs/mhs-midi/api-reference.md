# API Reference

## Module Overview

| Module | Import | Purpose |
|--------|--------|---------|
| `MidiPerform` | `import MidiPerform` | Immediate IO, generative music |
| `MusicPerform` | `import MusicPerform` | Pure Music DSL + perform + Async |
| `Async` | `import Async` | Concurrent scheduler (re-exported by MusicPerform) |
| `Music` | `import Music` | Pure music theory (no IO) |
| `Midi` | `import Midi` | Low-level FFI bindings |

---

## MidiPerform Module

```haskell
import MidiPerform
```

Immediate MIDI playback with generative functions. All functions are IO actions.

### MIDI Control

```haskell
open     :: IO ()  -- open virtual port "mhsMIDI"
openPort :: Int -> IO ()  -- open hardware port by index
close    :: IO ()  -- close MIDI port
panic    :: IO ()  -- all notes off
ports    :: IO ()  -- list available ports
```

### Note Playing

```haskell
note :: Pitch -> IO ()
-- Play note with defaults (channel 1, mf, quarter)

noteWith :: Channel -> Velocity -> Duration -> Pitch -> IO ()
-- Play note with explicit parameters

chord :: [Pitch] -> IO ()
-- Play chord with defaults

chordWith :: Channel -> Velocity -> Duration -> [Pitch] -> IO ()
-- Play chord with explicit parameters

arpeggio :: [Pitch] -> IO ()
-- Arpeggiate with defaults (mf, sixteenth)

arpeggioWith :: Channel -> Velocity -> Duration -> [Pitch] -> IO ()
-- Arpeggiate with explicit parameters
```

### Timing

```haskell
rest :: Duration -> IO ()
wait :: Duration -> IO ()  -- alias for rest
```

### Sequences

```haskell
melody :: [Pitch] -> IO ()
-- Play melody with defaults (mf, quarter)

melodyWith :: Channel -> Velocity -> Duration -> [Pitch] -> IO ()
-- Play melody with explicit parameters

times :: Int -> IO () -> IO ()
-- Repeat an action n times
```

### Generative Functions

```haskell
seed :: Int -> IO ()
-- Seed random number generator

pick :: [Pitch] -> IO ()
-- Play random note from list

chance :: Int -> IO () -> IO ()
-- Execute action with probability (0-100)

oneOf :: [IO ()] -> IO ()
-- Execute one random action from list

maybeDo :: IO () -> IO ()
-- 50% chance to execute

scramble :: [Pitch] -> IO ()
-- Play notes in random order

randomNote :: Pitch -> Pitch -> IO ()
-- Play random note in range

randomMelody :: Int -> Pitch -> Pitch -> IO ()
-- Play n random notes in range

walk :: Int -> Pitch -> Int -> IO ()
-- Random walk: n notes, starting pitch, max step size

drunk :: Int -> Pitch -> [Pitch] -> Int -> IO ()
-- Drunk walk constrained to scale: n notes, start, scale, max degrees

euclidean :: Int -> Int -> IO () -> IO ()
-- Euclidean rhythm: hits, steps, action
```

### Scales (for Generative Use)

```haskell
major :: Pitch -> [Pitch]
minor :: Pitch -> [Pitch]
pentatonic :: Pitch -> [Pitch]
blues :: Pitch -> [Pitch]
chromatic :: Pitch -> [Pitch]
dorian :: Pitch -> [Pitch]
phrygian :: Pitch -> [Pitch]
lydian :: Pitch -> [Pitch]
mixolydian :: Pitch -> [Pitch]
harmonicMinor :: Pitch -> [Pitch]
melodicMinor :: Pitch -> [Pitch]
```

### Low-level MIDI

```haskell
noteOn :: Channel -> Pitch -> Velocity -> IO ()
noteOff :: Channel -> Pitch -> IO ()
cc :: Channel -> Int -> Int -> IO ()
program :: Channel -> Int -> IO ()
bend :: Channel -> Int -> IO ()
```

### Constants

Pitches, durations, and velocities are the same as in Music.hs.

---

## MusicPerform Module

```haskell
import MusicPerform
```

Re-exports Music and Midi, adds `perform` to bridge pure Music to MIDI.

### Performance

```haskell
perform :: Music -> IO ()
-- Perform music on channel 1

performOn :: Channel -> Music -> IO ()
-- Perform music on specific channel
```

### Microtonal

```haskell
centsToBend :: Int -> IO Int
-- Convert cents to pitch bend value

pitchBendCents :: Channel -> Int -> IO ()
-- Send pitch bend in cents
```

---

## Async Module

```haskell
import Async
-- Or use via: import MusicPerform (which re-exports Async)
```

Concurrent voice scheduler using native Haskell threads (`forkIO`).

### Types

```haskell
type VoiceId = ThreadId
-- Voice identifier (wrapper around Haskell ThreadId)
```

### Scheduler Functions

```haskell
spawn :: String -> IO () -> IO VoiceId
-- Spawn a new voice with a name. The action runs in its own thread.

run :: IO ()
-- Block until all spawned voices complete. Polls every 10ms.

stop :: VoiceId -> IO Bool
-- Stop a specific voice by its ID. Returns True if found and stopped.

stopAll :: IO ()
-- Stop all active voices immediately.

voices :: IO Int
-- Get count of currently active voices.

status :: IO (Bool, Int, [String])
-- Get scheduler status: (isRunning, voiceCount, voiceNames)
```

### Async Music Helpers

```haskell
asyncNote :: Channel -> Pitch -> Velocity -> Duration -> IO VoiceId
-- Play a single note asynchronously

asyncChord :: Channel -> [Pitch] -> Velocity -> Duration -> IO VoiceId
-- Play a chord asynchronously

asyncPerform :: Music -> IO VoiceId
-- Perform a Music value asynchronously on channel 1
```

### Example

```haskell
import MusicPerform

main :: IO ()
main = do
    midiOpenVirtual "AsyncDemo"

    -- Two concurrent voices
    spawn "melody" $ do
        perform (line [c4, e4, g4] mf quarter)
        perform (line [g4, e4, c4] mf quarter)

    spawn "bass" $ do
        perform (note c2 ff half)
        perform (note g2 ff half)

    run  -- Wait for both to finish

    -- Using helpers
    asyncNote 1 c4 80 500
    asyncChord 1 [c4, e4, g4] 80 1000
    run

    midiClose
```

---

## Music Module (Pure)

```haskell
import Music
```

Pure music theory with no IO. All functions are pure.

### Types

```haskell
type Pitch = Int      -- MIDI note number (0-127)
type Duration = Int   -- milliseconds
type Velocity = Int   -- 0-127
type Channel = Int    -- 1-16

data Event
    = ENote Pitch Velocity Duration
    | ERest Duration

data Music
    = MEvent Event
    | MSeq [Music]    -- sequential
    | MPar [Music]    -- parallel
```

### Constructors

```haskell
note :: Pitch -> Velocity -> Duration -> Music
-- Single note

rest :: Duration -> Music
-- Rest (silence)

chord :: [Pitch] -> Velocity -> Duration -> Music
-- Chord (parallel notes)

line :: [Pitch] -> Velocity -> Duration -> Music
-- Line (sequential notes)
```

### Combinators

```haskell
(+:+) :: Music -> Music -> Music
-- Sequential composition (infixr 5)

(|||) :: Music -> Music -> Music
-- Parallel composition (infixr 4)

timesM :: Int -> Music -> Music
-- Repeat n times
```

### Transformations

```haskell
transpose :: Int -> Music -> Music
-- Transpose by semitones

louder :: Int -> Music -> Music
-- Increase velocity

softer :: Int -> Music -> Music
-- Decrease velocity

stretch :: Int -> Music -> Music
-- Multiply durations (2 = twice as slow)

compress :: Int -> Music -> Music
-- Divide durations (2 = twice as fast)

mapEvents :: (Event -> Event) -> Music -> Music
-- Map function over all events
```

### Utilities

```haskell
collectEvents :: Music -> [Event]
-- Flatten music to event list

duration :: Music -> Duration
-- Total duration of music
```

### Pitch Constants

All pitches: `c0`-`c8`, `d0`-`d8`, etc. with sharps `cs0`-`cs8`, `ds0`-`ds8`, etc.

Middle C (MIDI 60) is `c4`.

Flat aliases: `db = cs`, `eb = ds`, `gb = fs`, `ab = gs`, `bb = as`

Pitch class (semitone offset): `c = 0`, `cs = 1`, ..., `b = 11`

### Duration Constants

| Constant | Value | Note |
|----------|-------|------|
| `whole` | 2000 | Whole note |
| `half` | 1000 | Half note |
| `quarter` | 500 | Quarter note |
| `eighth` | 250 | Eighth note |
| `sixteenth` | 125 | Sixteenth note |

```haskell
dotted :: Duration -> Duration  -- 1.5x duration
bpm :: Int -> Duration          -- quarter note duration for tempo
```

### Velocity Constants

| Constant | Value | Dynamic |
|----------|-------|---------|
| `ppp` | 16 | Pianississimo |
| `pp` | 33 | Pianissimo |
| `p` | 49 | Piano |
| `mp` | 64 | Mezzo-piano |
| `mf` | 80 | Mezzo-forte |
| `ff` | 96 | Fortissimo |
| `fff` | 112 | Fortississimo |

### Scale Constants

55 scales available as `scale*` constants:

**Diatonic modes:** `scaleMajor`, `scaleMinor`, `scaleDorian`, `scalePhrygian`, `scaleLydian`, `scaleMixolydian`, `scaleLocrian`

**Minor variants:** `scaleHarmonicMinor`, `scaleMelodicMinor`, `scaleHarmonicMajor`

**Pentatonic & Blues:** `scalePentatonic`, `scalePentatonicMinor`, `scaleBlues`, `scaleBluesMajor`

**Symmetric:** `scaleWholeTone`, `scaleChromatic`, `scaleDiminished`, `scaleAugmented`

**Bebop:** `scaleBebopDominant`, `scaleBebopMajor`, `scaleBebopMinor`

**World:** `scaleHungarianMinor`, `scaleDoubleHarmonic`, `scaleHirajoshi`, `scalePersian`, etc.

**Arabic Maqamat (12-TET):** `scaleMaqamHijaz`, `scaleMaqamNahawand`, etc.

**Indian Ragas (12-TET):** `scaleRagaBhairav`, `scaleRagaTodi`, etc.

### Scale Functions

```haskell
buildScale :: Pitch -> Scale -> [Pitch]
-- Build scale pitches from root

scaleDegree :: Pitch -> Scale -> Int -> Pitch
-- Get nth degree (1-based, supports extensions like 9, 11, 13)

inScale :: Pitch -> Pitch -> Scale -> Bool
-- Check if pitch belongs to scale

quantize :: Pitch -> Pitch -> Scale -> Pitch
-- Snap pitch to nearest scale tone
```

### Pure Generative Functions

Music.hs includes pure generative algorithms that work with explicit seeds for reproducibility.

#### Pure PRNG

```haskell
type Seed = Int

nextRandom :: Seed -> (Int, Seed)
-- Linear Congruential Generator, returns (value, nextSeed)

randomRange :: Seed -> Int -> Int -> (Int, Seed)
-- Random Int in range [lo, hi]

randomList :: Seed -> Int -> Int -> Int -> ([Int], Seed)
-- Generate n random Ints in range [lo, hi]
```

#### Deterministic Algorithms

```haskell
euclideanRhythm :: Int -> Int -> [Bool]
-- Bjorklund algorithm: euclideanRhythm 3 8 = [T,F,F,T,F,F,T,F]

arpUp :: [a] -> [a]
-- Ascending pattern (identity)

arpDown :: [a] -> [a]
-- Descending pattern (reverse)

arpUpDown :: [a] -> [a]
-- Up-down pattern (no repeated top)

retrograde :: Music -> Music
-- Reverse music in time

invert :: Pitch -> Music -> Music
-- Melodic inversion around axis pitch
```

#### Seed-based Random

```haskell
shuffle :: Seed -> [a] -> [a]
-- Fisher-Yates shuffle

pick :: Seed -> [a] -> a
-- Pick one element

pickN :: Seed -> Int -> [a] -> [a]
-- Pick n elements (with replacement)

randomWalk :: Seed -> Pitch -> Int -> Int -> [Pitch]
-- Random walk: seed, start, maxStep, count

drunkWalk :: Seed -> Pitch -> [Pitch] -> Int -> Int -> [Pitch]
-- Scale-constrained walk: seed, start, scale, maxDegrees, count
```

### Microtonal Scale Constants

Cents-based scales for quarter-tones:

`scaleMaqamBayatiCents`, `scaleMaqamRastCents`, `scaleMaqamSabaCents`, `scaleMakamUssakCents`, `scaleShrutiCents`, etc.

```haskell
centsToNote :: Pitch -> Int -> (Pitch, Int)
-- Convert cents interval to (MIDI note, bend cents)
```

---

## Midi Module (FFI)

```haskell
import Midi
```

Low-level FFI bindings. Most users should use MidiPerform or MusicPerform instead.

### Port Management

```haskell
midiInit :: IO Bool
midiCleanup :: IO ()
midiListPorts :: IO Int
midiPortName :: Int -> IO String
midiOpen :: Int -> IO Bool
midiOpenVirtual :: String -> IO Bool
midiClose :: IO ()
midiIsOpen :: IO Bool
```

### MIDI Messages

```haskell
midiNoteOn :: Int -> Int -> Int -> IO ()
midiNoteOff :: Int -> Int -> IO ()
midiCC :: Int -> Int -> Int -> IO ()
midiProgram :: Int -> Int -> IO ()
midiPitchBend :: Int -> Int -> IO ()
midiSend :: Int -> Int -> Int -> IO ()
```

### Utilities

```haskell
midiSleep :: Int -> IO ()
midiPanic :: IO ()
midiCentsToBend :: Int -> IO Int
```

### Random

```haskell
midiSeedRandom :: Int -> IO ()
midiRandom :: IO Int
midiRandomRange :: Int -> Int -> IO Int
```

### MIDI Recording

Record MIDI events for replay or export. Records note-on, note-off, and CC events with timestamps.

```haskell
midiRecordStart :: Int -> IO Bool
-- Start recording at given BPM

midiRecordStop :: IO Int
-- Stop recording, returns event count

midiSaveToFile :: String -> IO Bool
-- Save recorded events to standard MIDI file (.mid)

midiSaveToSource :: String -> IO Bool
-- Save recorded events as Haskell source file (.hs)

midiRecordCount :: IO Int
-- Get current event count

midiRecordActive :: IO Bool
-- Check if recording is active

midiReadMid :: String -> IO Bool
-- Read and display MIDI file info
```

#### Recording Example

```haskell
import Midi
import MidiPerform

main :: IO ()
main = do
    _ <- midiOpenVirtual "MhsMidi"
    _ <- midiRecordStart 120

    note c4
    note e4
    chord (major g4)

    _ <- midiRecordStop
    _ <- midiSaveToFile "melody.mid"        -- Standard MIDI file
    _ <- midiSaveToSource "melody.hs"    -- Haskell source file

    midiClose
```

The MIDI file can be opened in any DAW or MIDI player. The Haskell source file contains a `replay` function to reproduce the performance.

#### Reading MIDI Files

```haskell
import Midi

main :: IO ()
main = do
    -- Read and display MIDI file info and events
    _ <- midiReadMid "song.mid"
    return ()
```

This prints:
- File metadata (format, tracks, PPQN, tempo, duration)
- All events with tick, channel, type, and data bytes
