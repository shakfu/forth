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

## Build Variants

Multiple build variants are available with different trade-offs:

| Target | Binary Size | Cold Start | Description |
|--------|-------------|------------|-------------|
| `mhs-midi-src` | 3.3 MB | ~20s | Source embedding (default) |
| `mhs-midi-src-zstd` | 1.3 MB | ~20s | Compressed source (smallest) |
| `mhs-midi-pkg` | 4.8 MB | ~1s | Package embedding (fastest startup) |
| `mhs-midi-pkg-zstd` | 3.0 MB | ~1s | Compressed packages (best balance) |

**Key insight**: After the first run, `.mhscache` is created and all variants have similar warm-start times (~0.5-1s). The main differences are:
- **pkg variants**: Eliminate the 20-second cold-start penalty (first run or fresh machine)
- **zstd variants**: Reduce binary size significantly (useful for distribution)

### Selection Guide

| Priority | Recommended Variant | Command |
|----------|---------------------|---------|
| Fastest startup | `mhs-midi-pkg` | `cmake --build build --target mhs-midi-pkg` |
| Smallest binary | `mhs-midi-src-zstd` | `cmake --build build --target mhs-midi-src-zstd` |
| Best balance | `mhs-midi-pkg-zstd` | `cmake --build build --target mhs-midi-pkg-zstd` |
| Simplest build | `mhs-midi-src` | `cmake --build build --target mhs-midi-src` |

### Build Commands

```bash
# Build all variants
cmake -B build && cmake --build build --target mhs-midi-all

# Or build individual variants:
cmake --build build --target mhs-midi-src        # Source embedding
cmake --build build --target mhs-midi-src-zstd   # Compressed source
cmake --build build --target mhs-midi-pkg        # Package embedding
cmake --build build --target mhs-midi-pkg-zstd   # Compressed packages
```

### Variant Details

| Variant | What's Embedded | Compression | Use Case |
|---------|-----------------|-------------|----------|
| `mhs-midi-src` | .hs source files | None | Development, debugging |
| `mhs-midi-src-zstd` | .hs source files | zstd | Size-constrained distribution |
| `mhs-midi-pkg` | .pkg precompiled packages | None | Fast cold start, CI/CD |
| `mhs-midi-pkg-zstd` | .pkg precompiled packages | zstd | Best for end-user distribution |

### Prerequisites for Package Variants

Package variants (`mhs-midi-pkg`, `mhs-midi-pkg-zstd`) require MicroHs packages to be built first. The build system handles this automatically, but you can also build manually:

```bash
cd thirdparty/MicroHs
make
make install   # Creates ~/.mcabal/mhs-0.15.2.0/ with base.pkg
```

### Aliases

For convenience, the following aliases are available:
- `mhs-midi` - Same as `mhs-midi-src` (default, requires MHSDIR auto-detection)
- `mhs-midi-standalone` - Alias for `mhs-midi-src` (backward compatibility)

## Requirements

- Built project (`make` must complete successfully)
- macOS (CoreMIDI), Linux (ALSA), or Windows (WinMM)
