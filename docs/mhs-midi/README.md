# mhs-midi

A Haskell MIDI library for MicroHs, providing both pure functional composition and immediate MIDI playback with generative music functions.

## Features

- **Pure Music DSL**: Compose and transform music functionally before performance
- **Immediate Playback**: Direct MIDI output for REPL-style interaction
- **Async Scheduler**: Concurrent voice playback using native Haskell threads
- **Generative Music**: Random selection, walks, euclidean rhythms, probability
- **55 Built-in Scales**: Modes, pentatonics, world scales, ragas, maqamat
- **Microtonal Support**: 10 cents-based scales with quarter-tones
- **Interactive REPL**: Fast startup with caching

## Module Structure

| Module | Purpose |
|--------|---------|
| `Music.hs` | Pure music theory + DSL (no IO) |
| `Midi.hs` | FFI bindings |
| `Async.hs` | Concurrent scheduler using forkIO |
| `MusicPerform.hs` | `perform` bridge for pure Music (re-exports Async) |
| `MidiPerform.hs` | Immediate IO + generative functions |

## Quick Start

### 1. Build

```bash
make
```

This builds the default `mhs-midi-standalone` with embedded `.hs` source files.

### 2. Start the REPL

```bash
./scripts/mhs-midi-repl
```

### 3. Immediate Playback (MidiPerform)

```haskell
> import MidiPerform
> open
MIDI open
> note c4
> chord [c4, e4, g4]
> melody [c4, d4, e4, f4, g4]
> drunk 16 c4 (major c4) 2    -- generative walk
> close
```

### 4. Pure Composition (MusicPerform)

```haskell
> import MusicPerform
> midiOpenVirtual "test"
> let m = line [c4, e4, g4] mf quarter
> perform m
> perform (transpose 7 m)
> midiClose
```

## Two Approaches

### MidiPerform - Immediate IO

For REPL interaction and generative music. Musical terms are IO actions:

```haskell
import MidiPerform

main = do
    open
    note c4                           -- play immediately
    chord [c4, e4, g4]                -- play chord
    times 4 (pick (pentatonic c4))    -- random notes
    drunk 16 c4 (major c4) 2          -- drunk walk
    euclidean 5 8 (note c4)           -- euclidean rhythm
    close
```

### MusicPerform - Pure Composition

For composing music as pure data, then performing:

```haskell
import MusicPerform

melody = line [c4, e4, g4] mf quarter
piece = melody +:+ transpose 7 melody

main = do
    midiOpenVirtual "MyApp"
    perform piece
    perform (louder 20 (stretch 2 piece))
    midiClose
```

### Async - Concurrent Voices

Play multiple independent voices simultaneously using Haskell threads:

```haskell
import MusicPerform

main = do
    midiOpenVirtual "MyApp"

    -- Spawn concurrent voices
    spawn "melody" $ perform (line [c4, e4, g4, c5] mf quarter)
    spawn "bass" $ perform (note c2 ff whole)

    run  -- Wait for all voices to complete
    midiClose
```

Each voice runs in its own thread. The scheduler provides:

```haskell
spawn :: String -> IO () -> IO VoiceId  -- Spawn a named voice
run :: IO ()                             -- Wait for all voices
stop :: VoiceId -> IO Bool              -- Stop a specific voice
stopAll :: IO ()                         -- Stop all voices
voices :: IO Int                         -- Get count of active voices
status :: IO (Bool, Int, [String])      -- (running, count, names)
```

Async note helpers for quick voice creation:

```haskell
asyncNote :: Channel -> Pitch -> Velocity -> Duration -> IO VoiceId
asyncChord :: Channel -> [Pitch] -> Velocity -> Duration -> IO VoiceId
asyncPerform :: Music -> IO VoiceId
```

## Generative Functions (MidiPerform)

### Random Selection

```haskell
pick [c4, e4, g4]           -- random note from list
chance 75 (note c4)         -- 75% probability
oneOf [note c4, chord [c4, e4, g4]]  -- random action
maybeDo (note c4)           -- 50% chance
```

### Random Sequences

```haskell
scramble [c4, e4, g4, c5]   -- random order
randomNote c4 c5            -- random pitch in range
randomMelody 8 c4 c5        -- 8 random notes
```

### Algorithmic Patterns

```haskell
walk 16 c4 3                -- random walk, max 3 semitone steps
drunk 16 c4 (major c4) 2    -- walk constrained to scale
euclidean 5 8 (note c4)     -- 5 hits over 8 steps
```

### Scales for Generative Use

```haskell
major c4        -- C major scale (multiple octaves)
minor a4        -- A minor
pentatonic c4   -- pentatonic
blues c4        -- blues
dorian d4       -- dorian mode
```

## Pure Music DSL (Music.hs)

### Constructors

```haskell
note :: Pitch -> Velocity -> Duration -> Music
rest :: Duration -> Music
chord :: [Pitch] -> Velocity -> Duration -> Music
line :: [Pitch] -> Velocity -> Duration -> Music
```

### Pure Generative Functions

```haskell
-- Deterministic algorithms
euclideanRhythm 3 8         -- [T,F,F,T,F,F,T,F]
arpUp, arpDown, arpUpDown   -- arpeggio patterns
retrograde                   -- reverse in time
invert c4                    -- melodic inversion

-- Seed-based random (reproducible)
shuffle 42 [c4,e4,g4]       -- same seed = same result
randomWalk 42 c4 3 16       -- seed, start, maxStep, count
drunkWalk 42 c4 scale 2 16  -- constrained to scale
```

### Combinators

```haskell
(+:+) :: Music -> Music -> Music   -- sequential
(|||) :: Music -> Music -> Music   -- parallel
timesM :: Int -> Music -> Music    -- repeat
```

### Transformations

```haskell
transpose :: Int -> Music -> Music
louder :: Int -> Music -> Music
softer :: Int -> Music -> Music
stretch :: Int -> Music -> Music   -- 2 = twice as slow
compress :: Int -> Music -> Music  -- 2 = twice as fast
```

### Example

```haskell
import MusicPerform

melody = line [c4, e4, g4] mf quarter
bass = line [c3, g3] ff half
piece = melody ||| bass                    -- parallel
full = piece +:+ transpose 5 piece         -- sequential

main = do
    midiOpenVirtual "Composition"
    perform full
    perform (stretch 2 (softer 20 full))   -- slow and quiet
    midiClose
```

## Documentation

- [API Reference](api-reference.md) - Complete function documentation
- [Examples](examples.md) - More code examples
- [Architecture](architecture.md) - How the FFI works
- [Package Build](mhs-pkg-build.md) - Technical details on `.pkg` embedding

## Standalone Build Options

The `mhs-midi-standalone` binary can be built with different embedding strategies. Choose based on your priorities:

### Build Option Selection Guide

| Priority | Recommended Build | Command |
|----------|-------------------|---------|
| Fast first run | `MHS_USE_PKG` | `cmake -B build -DMHS_USE_PKG=ON` |
| Smallest binary | `MHS_USE_ZSTD` | `cmake -B build -DMHS_USE_ZSTD=ON` |
| Fast + small | Both | `cmake -B build -DMHS_USE_PKG=ON -DMHS_USE_ZSTD=ON` |
| Simplest build | Default | `cmake -B build` |

### Performance Comparison

| Build Mode | Cold Start | Warm Cache | Binary Size |
|------------|------------|------------|-------------|
| Default | ~20s | ~0.5s | ~3.2 MB |
| `MHS_USE_PKG` | ~1s | ~0.95s | ~5.5 MB |
| `MHS_USE_ZSTD` | ~20s | ~0.5s | ~1.3 MB |
| Both | ~1s | ~0.95s | ~2.5 MB |

**Key insight**: After the first run, `.mhscache` is created and all modes have similar warm-start times (~0.5-1s). The main difference is:
- **PKG mode**: Eliminates the 20-second cold-start penalty (first run or fresh machine)
- **ZSTD mode**: Reduces binary size by 60% (useful for distribution)

### Build Commands

The build options are:

| Mode         | What's embedded           | Compression |
|--------------|---------------------------|-------------|
| Default      | .hs source files          | None        |
| MHS_USE_ZSTD | .hs source files          | zstd        |
| MHS_USE_PKG  | .pkg precompiled packages | None        |
| Both         | .pkg precompiled packages | zstd        |

They can be used as follows:

```bash
# Default: embed .hs source files
cmake -B build
cmake --build build --target mhs-midi-standalone

# Package mode: fast cold start (~1s vs ~20s)
# Requires: make install in thirdparty/MicroHs first
cmake -B build -DMHS_USE_PKG=ON
cmake --build build --target mhs-midi-standalone

# Compressed mode: smallest binary (~1.3 MB)
cmake -B build -DMHS_USE_ZSTD=ON
cmake --build build --target mhs-midi-standalone

# Combined: fast start + small binary
cmake -B build -DMHS_USE_PKG=ON -DMHS_USE_ZSTD=ON
cmake --build build --target mhs-midi-standalone
```

### Prerequisites for MHS_USE_PKG

Package mode requires MicroHs to be installed with packages:

```bash
cd thirdparty/MicroHs
make
make install   # Creates ~/.mcabal/mhs-0.15.2.0/ with base.pkg
```

## Requirements

- Built project (`make` must complete successfully)
- macOS (CoreMIDI), Linux (ALSA), or Windows (WinMM)
