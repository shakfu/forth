# Midi.hs API Reference

## Module Import

```haskell
import Midi
```

---

## Initialization & Port Management

### midiInit

```haskell
midiInit :: IO Bool
```

Initialize the MIDI system. Returns `True` on success.

### midiCleanup

```haskell
midiCleanup :: IO ()
```

Cleanup the MIDI system and release resources.

### midiListPorts

```haskell
midiListPorts :: IO Int
```

List available MIDI output ports. Returns the count of available ports.

### midiPortName

```haskell
midiPortName :: Int -> IO String
```

Get the name of a MIDI port by index.

```haskell
n <- midiListPorts
names <- mapM midiPortName [0..n-1]
```

### midiOpen

```haskell
midiOpen :: Int -> IO Bool
```

Open a hardware MIDI port by index. Returns `True` on success.

### midiOpenVirtual

```haskell
midiOpenVirtual :: String -> IO Bool
```

Create and open a virtual MIDI port with the given name. Other applications (DAWs, synths) can connect to this port.

```haskell
midiOpenVirtual "MyApp"
```

### midiClose

```haskell
midiClose :: IO ()
```

Close the current MIDI port.

### midiIsOpen

```haskell
midiIsOpen :: IO Bool
```

Check if a MIDI port is currently open.

---

## Low-Level MIDI Operations

### midiNoteOn

```haskell
midiNoteOn :: Int -> Int -> Int -> IO ()
midiNoteOn channel pitch velocity
```

Send a Note On message.

- `channel`: 1-16
- `pitch`: 0-127 (60 = middle C)
- `velocity`: 0-127

### midiNoteOff

```haskell
midiNoteOff :: Int -> Int -> IO ()
midiNoteOff channel pitch
```

Send a Note Off message.

### midiCC

```haskell
midiCC :: Int -> Int -> Int -> IO ()
midiCC channel controller value
```

Send a Control Change message.

- `controller`: 0-127 (e.g., 1 = mod wheel, 7 = volume, 64 = sustain)
- `value`: 0-127

### midiProgram

```haskell
midiProgram :: Int -> Int -> IO ()
midiProgram channel program
```

Send a Program Change message.

- `program`: 0-127

### midiPitchBend

```haskell
midiPitchBend :: Int -> Int -> IO ()
midiPitchBend channel value
```

Send a Pitch Bend message.

- `value`: -8192 to 8191 (0 = center)

### midiSend

```haskell
midiSend :: Int -> Int -> Int -> IO ()
midiSend status data1 data2
```

Send a raw 3-byte MIDI message.

### midiSleep

```haskell
midiSleep :: Int -> IO ()
```

Sleep for the given number of milliseconds.

### midiPanic

```haskell
midiPanic :: IO ()
```

Send All Notes Off on all channels. Use to silence stuck notes.

---

## Types

```haskell
type Pitch    = Int   -- MIDI note number (0-127)
type Duration = Int   -- milliseconds
type Velocity = Int   -- 0-127
type Channel  = Int   -- 1-16
```

---

## Pitch Names

### Pitch Constants

All pitches follow the pattern `<note><octave>` where:
- Note: `c`, `d`, `e`, `f`, `g`, `a`, `b`
- Sharps: `cs`, `ds`, `fs`, `gs`, `as`
- Octave: 0-8

Middle C (MIDI 60) is `c4`.

| Pitch | MIDI | Pitch | MIDI | Pitch | MIDI |
|-------|------|-------|------|-------|------|
| c4    | 60   | d4    | 62   | e4    | 64   |
| f4    | 65   | g4    | 67   | a4    | 69   |
| b4    | 71   | c5    | 72   | cs4   | 61   |

### Flat Aliases

```haskell
db = cs   -- D flat = C sharp
eb = ds   -- E flat = D sharp
gb = fs   -- G flat = F sharp
ab = gs   -- A flat = G sharp
bb = as   -- B flat = A sharp
```

### Pitch Class (Semitone Offset)

For building chords and scales:

```haskell
c, cs, d, ds, e, f, fs, g, gs, a, as, b :: Int
-- c = 0, cs = 1, d = 2, ... b = 11
```

---

## Durations

Based on 120 BPM by default.

| Constant    | Milliseconds | Musical Value |
|-------------|--------------|---------------|
| `whole`     | 2000         | Whole note    |
| `half`      | 1000         | Half note     |
| `quarter`   | 500          | Quarter note  |
| `eighth`    | 250          | Eighth note   |
| `sixteenth` | 125          | 16th note     |

### dotted

```haskell
dotted :: Duration -> Duration
```

Returns 1.5x the given duration.

```haskell
dotted quarter  -- 750ms
dotted half     -- 1500ms
```

### bpm

```haskell
bpm :: Int -> Duration
```

Calculate quarter note duration for a given tempo.

```haskell
bpm 120  -- 500ms
bpm 60   -- 1000ms
bpm 140  -- ~428ms
```

---

## Velocities (Dynamics)

| Constant | Value | Dynamic        |
|----------|-------|----------------|
| `ppp`    | 16    | Pianississimo  |
| `pp`     | 33    | Pianissimo     |
| `p`      | 49    | Piano          |
| `mp`     | 64    | Mezzo-piano    |
| `mf`     | 80    | Mezzo-forte    |
| `ff`     | 96    | Fortissimo     |
| `fff`    | 112   | Fortississimo  |

---

## High-Level Note Playing

### play

```haskell
play :: Pitch -> Velocity -> Duration -> IO ()
```

Play a single note with specified velocity and duration on the default channel.

```haskell
play c4 mf quarter
play g4 ff half
```

### playNote

```haskell
playNote :: Pitch -> Duration -> IO ()
```

Play a note with default velocity (`mf`).

```haskell
playNote c4 quarter
playNote e4 eighth
```

### playChord

```haskell
playChord :: [Pitch] -> Velocity -> Duration -> IO ()
```

Play multiple notes simultaneously.

```haskell
playChord [c4, e4, g4] mf half      -- C major chord
playChord [a4, c5, e5] ff quarter   -- A minor chord
```

### rest

```haskell
rest :: Duration -> IO ()
```

Silent pause for the given duration.

```haskell
rest quarter
rest (dotted half)
```

---

## Sequences

### melody

```haskell
melody :: [(Pitch, Duration)] -> Velocity -> IO ()
```

Play a sequence of notes.

```haskell
melody [(c4, quarter), (d4, quarter), (e4, half)] mf
```

### chord

```haskell
chord :: [Pitch] -> Duration -> IO ()
```

Play a chord with default velocity (`mf`).

```haskell
chord [c4, e4, g4] half
```

### arpeggio

```haskell
arpeggio :: [Pitch] -> Duration -> Velocity -> IO ()
```

Play notes one after another (arpeggiated).

```haskell
arpeggio [c4, e4, g4, c5] eighth mf
```

### times

```haskell
times :: Int -> IO () -> IO ()
```

Repeat an action n times.

```haskell
times 4 (playNote c4 quarter)
times 2 (chord [c4, e4, g4] half)
```

---

## Channel Management

### defaultChannel

```haskell
defaultChannel :: Channel  -- = 1
```

The default MIDI channel used by high-level functions.

### withChannel

```haskell
withChannel :: Channel -> (Channel -> IO ()) -> IO ()
```

Execute an action with a specific channel.

```haskell
withChannel 2 (\ch -> midiNoteOn ch 60 100)
```
