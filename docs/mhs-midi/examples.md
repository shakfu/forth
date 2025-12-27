# mhs-midi Examples

## MidiRepl Examples

### Hello MIDI with MidiRepl

Play a C major scale using `mapM_`:

```haskell
module HelloMidi(main) where
import MidiRepl

main :: IO ()
main = do
    open
    mapM_ n [c4, d4, e4, f4, g4, a4, b4, c5]
    close
```

### Partial Application

Define reusable note functions:

```haskell
module PartialApp(main) where
import MidiRepl

-- Pitch-last allows partial application
loud = note 1 fff quarter
soft = note 1 pp half
fast = note 1 mf sixteenth
bass = note 2 ff quarter

main :: IO ()
main = do
    open

    -- Ascending loud
    mapM_ loud [c4, e4, g4]

    -- Descending soft
    mapM_ soft [c5, g4, e4, c4]

    -- Fast scale
    mapM_ fast [c4, d4, e4, f4, g4, a4, b4, c5]

    -- Bass line on channel 2
    mapM_ bass [c2, g2, c2, g2]

    close
```

### Chords with MidiRepl

```haskell
module ChordsPrelude(main) where
import MidiRepl

-- Custom chord function
bigChord = notes 1 ff whole

main :: IO ()
main = do
    open

    -- Default chord (quarter, mf)
    ch [c4, e4, g4]
    ch [f4, a4, c5]
    ch [g4, b4, d5]

    -- Big finale chord
    bigChord [c3, g3, c4, e4, g4, c5]

    close
```

---

## Basic Examples (using Midi module)

### Hello MIDI

Play a C major scale:

```haskell
module HelloMidi(main) where
import Midi

main :: IO ()
main = do
    midiOpenVirtual "HelloMidi"

    playNote c4 quarter
    playNote d4 quarter
    playNote e4 quarter
    playNote f4 quarter
    playNote g4 quarter
    playNote a4 quarter
    playNote b4 quarter
    playNote c5 half

    midiClose
```

### Chord Progression

Play a I-IV-V-I progression:

```haskell
module Chords(main) where
import Midi

main :: IO ()
main = do
    midiOpenVirtual "Chords"

    -- I - C major
    chord [c4, e4, g4] half

    -- IV - F major
    chord [f4, a4, c5] half

    -- V - G major
    chord [g4, b4, d5] half

    -- I - C major
    chord [c4, e4, g4] whole

    midiClose
```

### Melody with Dynamics

```haskell
module DynamicMelody(main) where
import Midi

main :: IO ()
main = do
    midiOpenVirtual "Dynamics"

    -- Start soft, get louder
    play c4 pp quarter
    play d4 p quarter
    play e4 mp quarter
    play f4 mf quarter
    play g4 ff quarter
    play a4 fff half

    -- Back down
    play g4 ff quarter
    play f4 mf quarter
    play e4 mp quarter
    play d4 p quarter
    play c4 pp whole

    midiClose
```

---

## Intermediate Examples

### Arpeggiated Chords

```haskell
module Arpeggio(main) where
import Midi

main :: IO ()
main = do
    midiOpenVirtual "Arpeggio"

    -- Arpeggiate C major up and down
    arpeggio [c4, e4, g4, c5] eighth mf
    arpeggio [c5, g4, e4, c4] eighth mf

    rest quarter

    -- Arpeggiate A minor
    arpeggio [a4, c5, e5, a5] sixteenth ff
    arpeggio [a5, e5, c5, a4] sixteenth ff

    midiClose
```

### Using the melody Function

```haskell
module TwinkleTwinkle(main) where
import Midi

twinkle :: [(Pitch, Duration)]
twinkle =
    [ (c4, quarter), (c4, quarter), (g4, quarter), (g4, quarter)
    , (a4, quarter), (a4, quarter), (g4, half)
    , (f4, quarter), (f4, quarter), (e4, quarter), (e4, quarter)
    , (d4, quarter), (d4, quarter), (c4, half)
    ]

main :: IO ()
main = do
    midiOpenVirtual "Twinkle"
    melody twinkle mf
    midiClose
```

### Repeating Patterns

```haskell
module Patterns(main) where
import Midi

-- A simple 4-note pattern
pattern1 :: IO ()
pattern1 = do
    playNote c4 eighth
    playNote e4 eighth
    playNote g4 eighth
    playNote e4 eighth

main :: IO ()
main = do
    midiOpenVirtual "Patterns"

    -- Repeat the pattern 4 times
    times 4 pattern1

    -- Different pattern
    times 4 (arpeggio [c4, e4, g4] sixteenth mf)

    midiClose
```

---

## Advanced Examples

### Multi-Channel Composition

```haskell
module MultiChannel(main) where
import Midi

-- Play bass on channel 2
bass :: IO ()
bass = do
    midiNoteOn 2 c2 100
    midiSleep 500
    midiNoteOff 2 c2

-- Play melody on channel 1
mel :: IO ()
mel = do
    midiNoteOn 1 c4 80
    midiSleep 250
    midiNoteOff 1 c4
    midiNoteOn 1 e4 80
    midiSleep 250
    midiNoteOff 1 e4

main :: IO ()
main = do
    midiOpenVirtual "MultiChannel"

    -- Set different instruments per channel
    midiProgram 1 0   -- Piano on channel 1
    midiProgram 2 32  -- Bass on channel 2

    -- Play both parts (sequentially in this simple example)
    times 4 (bass >> mel)

    midiClose
```

### Control Changes

```haskell
module ControlChanges(main) where
import Midi

main :: IO ()
main = do
    midiOpenVirtual "CC"

    -- Set volume to 100
    midiCC 1 7 100

    -- Enable sustain pedal
    midiCC 1 64 127

    -- Play some notes with sustain
    playNote c4 quarter
    playNote e4 quarter
    playNote g4 quarter

    -- Release sustain
    midiCC 1 64 0

    rest quarter

    -- Modulation wheel sweep
    midiNoteOn 1 c4 80
    midiCC 1 1 0
    midiSleep 100
    midiCC 1 1 40
    midiSleep 100
    midiCC 1 1 80
    midiSleep 100
    midiCC 1 1 127
    midiSleep 100
    midiCC 1 1 80
    midiSleep 100
    midiCC 1 1 40
    midiSleep 100
    midiCC 1 1 0
    midiNoteOff 1 c4

    midiClose
```

### Pitch Bend

```haskell
module PitchBend(main) where
import Midi

-- Bend up smoothly
bendUp :: IO ()
bendUp = do
    midiPitchBend 1 0
    midiSleep 50
    midiPitchBend 1 2000
    midiSleep 50
    midiPitchBend 1 4000
    midiSleep 50
    midiPitchBend 1 6000
    midiSleep 50
    midiPitchBend 1 8191

-- Bend back to center
bendCenter :: IO ()
bendCenter = do
    midiPitchBend 1 6000
    midiSleep 50
    midiPitchBend 1 4000
    midiSleep 50
    midiPitchBend 1 2000
    midiSleep 50
    midiPitchBend 1 0

main :: IO ()
main = do
    midiOpenVirtual "PitchBend"

    midiNoteOn 1 c4 100
    midiSleep 200
    bendUp
    midiSleep 200
    bendCenter
    midiSleep 200
    midiNoteOff 1 c4

    midiClose
```

### Hardware Port Selection

```haskell
module HardwarePort(main) where
import Midi

main :: IO ()
main = do
    -- List available ports
    n <- midiListPorts
    putStrLn $ "Found " ++ show n ++ " MIDI ports:"

    -- Print port names
    printPorts 0 n

    -- Open first port (if available)
    if n > 0
        then do
            ok <- midiOpen 0
            if ok
                then do
                    putStrLn "Opened port 0"
                    playNote c4 quarter
                    midiClose
                else putStrLn "Failed to open port"
        else putStrLn "No MIDI ports available"

printPorts :: Int -> Int -> IO ()
printPorts i n
    | i >= n    = return ()
    | otherwise = do
        name <- midiPortName i
        putStrLn $ "  " ++ show i ++ ": " ++ name
        printPorts (i + 1) n
```

---

## REPL Session Examples

Start the REPL:

```bash
./scripts/mhs-midi-repl
```

### Quick Note Test (MidiRepl)

```haskell
> import MidiRepl
> open
MIDI open
> n c4
> mapM_ n [c4, e4, g4]
> close
```

### Partial Application in REPL

```haskell
> import MidiRepl
> open
MIDI open

-- Define custom note functions
> let loud = note 1 fff quarter
> let soft = note 1 pp half

-- Use them
> loud c4
> mapM_ loud [c4, e4, g4]
> mapM_ soft [c5, g4, e4]

> close
```

### Interactive Chord Exploration

```haskell
> import MidiRepl
> open
MIDI open

-- Major chords using ch
> ch [c4, e4, g4]
> ch [f4, a4, c5]
> ch [g4, b4, d5]

-- Custom chord function
> let bigChord = notes 1 ff whole
> bigChord [c3, g3, c4, e4, g4]

> close
```

### List MIDI Ports

```haskell
> import MidiRepl
> ports
2 MIDI port(s):
  0: IAC Driver Bus 1
  1: USB MIDI Device
```

### Low-level Midi Module

For non-REPL usage or when you need return values:

```haskell
> import Midi
> midiOpenVirtual "Test" >>= print
True
> play c4 mf quarter >> return ()
> midiClose >> return ()
```
