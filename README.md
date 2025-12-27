# midi-langs

A couple of mini MIDI-capable languages for generating and transforming MIDI sequences:

- **midi_forth** - A Forth-like interpreter with concise musical notation
- **mhs-midi** - A Haskell-based MIDI language using the marvelous [MicroHs](https://github.com/augustss/MicroHs)

## Building

```bash
make              # Build everything
make test         # Run tests
make clean        # Remove build directory
```

Requires GCC/Clang and CMake 3.16+. First build compiles libremidi automatically.

## Running

```bash
./build/midi_forth      # Start MIDI Forth REPL
```

Type `help` for commands, `quit` to exit.

---

## Quick Start

```forth
midi-virtual            \ Create virtual MIDI port
500 dur!                \ Set note duration to 500ms
c4, e4, g4,             \ Play C, E, G
(c4 e4 g4),             \ Play C major chord
midi-close
```

---

## Concise Notation

The primary way to play notes. Use pitch names with octave numbers, trigger with comma.

### Single Notes

```forth
c4,                     \ Play middle C with defaults
c#4, db4,               \ Sharps and flats
60,                     \ MIDI note number works too
```

### Default Parameters

```forth
2 ch!                   \ Set default channel (1-16)
100 vel!                \ Set default velocity (0-127)
250 dur!                \ Set default duration in ms
```

### Explicit Parameters

```forth
1 c4 100 500,           \ ch pitch vel dur - play C4 on ch1, vel 100, 500ms
```

### Chords

```forth
(c4 e4 g4),             \ Play notes simultaneously
(c4 e4 g4) 1 80 500,    \ Chord with explicit ch, vel, dur
```

### Rests

```forth
r,                      \ Rest for default duration
r 1000,                 \ Rest for 1000ms
```

---

## Dynamics

Set velocity with dynamic markings:

```forth
ppp                     \ velocity 16
pp                      \ velocity 32
p                       \ velocity 48
mp                      \ velocity 64
mf                      \ velocity 80 (default)
f                       \ velocity 96
ff                      \ velocity 112
fff                     \ velocity 127

mf c4, ff g4,           \ Change dynamics inline
```

---

## Articulation

Add suffixes to pitch names:

```forth
c4.,                    \ Staccato - 50% duration
c4>,                    \ Accent - +20 velocity
c4-,                    \ Tenuto - full duration
```

---

## Relative Intervals

Move relative to last played pitch:

```forth
c4, +2, +2, +1,         \ Plays C D E F (up by semitones)
g4, -2, -2, -1,         \ Plays G F E D (down by semitones)
c4, ^,                  \ C4 then C5 (octave up)
c4, v,                  \ C4 then C3 (octave down)
```

---

## Alternatives and Probability

### Random Selection

```forth
c4|e4|g4,               \ Randomly play one of C, E, or G
c4|r,                   \ 50% note, 50% silence
```

### Probability

```forth
c4 75%,                 \ 75% chance to play, 25% silent
```

---

## Word Definitions

Define reusable patterns:

```forth
: melody c4, e4, g4, ;  \ Define a word
melody                  \ Execute it
melody 4 times          \ Execute 4 times total
```

---

## Anonymous Blocks

```forth
{ c4, e4, g4, } 4 *     \ Execute block 4 times
```

---

## Conditionals

```forth
1 if c4, then                       \ Play if true (non-zero)
0 if c4, else e4, then              \ Play else branch if false

random 50 > if c4, else e4, then    \ Random choice
```

---

## File Operations

### Load

```forth
load mysong.4th         \ Load and execute a file
```

### Record Commands

```forth
rec                     \ Start recording input
: melody c4, e4, g4, ;
melody
stop                    \ Stop recording
save mysong.4th         \ Save to file
```

### Capture MIDI

Record actual MIDI output with timing:

```forth
midi-virtual
capture                 \ Start capturing MIDI events
c4, e4, g4,
(c4 e4 g4),
stop                    \ Stop capturing
save-midi melody.4th    \ Save as sequence file
midi-close
```

The saved file contains sequence commands that can be loaded and replayed:

```forth
load melody.4th         \ Load the captured sequence
midi-virtual
seq-play                \ Play it back
midi-close
```

---

## MIDI Output

### Virtual Port

```forth
midi-virtual            \ Create "MidiForth" virtual port
```

### Hardware Ports

```forth
midi-list               \ List available ports
0 midi-open             \ Open port by index
midi-close              \ Close current port
```

### Control Messages

```forth
1 1 64 cc               \ CC: channel, cc#, value
1 25 pc                 \ Program change: channel, program
1 8192 pb               \ Pitch bend: channel, value (0-16383, center=8192)
panic                   \ All notes off on all channels
```

---

## Sequences

Store and manipulate MIDI sequences:

```forth
seq-new                 \ Create new sequence (returns ID)
0 seq                   \ Select sequence by ID

\ Add notes: time pitch vel dur
0 60 100 480 seq-note
480 62 100 480 seq-note
960 64 100 480 seq-note

seq-show                \ Display sequence events
seq-play                \ Play through MIDI output

\ Transformations
5 seq-transpose         \ Transpose by semitones
seq-reverse             \ Reverse timing
200 seq-stretch         \ Stretch timing (100 = normal)
seq-clear               \ Clear all events
```

### Sequence with Channels

```forth
\ time channel pitch vel dur
0 1 60 100 480 seq-note-ch      \ Channel 1
0 2 48 80 480 seq-note-ch       \ Channel 2 (bass)
```

---

## Packed Notes

Combine pitch, velocity, channel, duration into one value:

```forth
60 100 1 480 note       \ pitch vel ch dur -> packed note
note!                   \ Play packed note

\ Extract components
pitch@ vel@ ch@ dur@

\ Transform
12 transpose            \ Transpose by semitones
note.                   \ Print note info
```

---

## Chord Builders

```forth
c4 major .s             \ -> 60 64 67 (C E G)
a4 minor .s             \ -> 69 72 76 (A C E)
c4 dim .s               \ Diminished triad
c4 aug .s               \ Augmented triad
g4 dom7 .s              \ Dominant 7th
c4 maj7 .s              \ Major 7th
d4 min7 .s              \ Minor 7th
```

### Playing Chords

```forth
\ play-chord: pitches vel dur count
c4 major 100 half 3 play-chord

\ chord>seq: pitches vel dur time count
c4 major 100 half 0 3 chord>seq

\ arp>seq: pitches vel note-dur spacing time count
c4 major 100 eighth eighth 0 3 arp>seq
```

---

## Duration Constants

Tick values (480 ticks = quarter note):

```forth
whole .         \ 1920
half .          \ 960
quarter .       \ 480
eighth .        \ 240
sixteenth .     \ 120
```

---

## Tempo

```forth
140 bpm!                \ Set tempo
bpm@ .                  \ Get tempo (default 120)
500 ms                  \ Sleep for 500 milliseconds
```

---

## Stack Operations

```forth
.                       \ Print and pop top
.s                      \ Show stack
dup drop swap over rot  \ Stack manipulation
+ - * /                 \ Arithmetic
= < >                   \ Comparison (true=-1, false=0)
and or xor not          \ Bitwise
clear                   \ Clear stack
random                  \ Push random 0-99
```

---

## Complete Examples

### Melody with Dynamics

```forth
midi-virtual
250 dur!

mf c4, d4, e4, f4,
f g4, a4,
ff b4,
p c5,

midi-close
```

### Chord Progression

```forth
midi-virtual
100 bpm!
seq-new drop

c4 major 100 half 0 3 chord>seq
f4 major 100 half 960 3 chord>seq
g4 major 100 half 1920 3 chord>seq
c4 major 100 whole 2880 3 chord>seq

seq-play
midi-close
```

### Generative Pattern

```forth
midi-virtual
200 dur!

: note c4|e4|g4 80%, ;
note 16 times

midi-close
```

### Record and Replay

```forth
midi-virtual
capture
250 dur!
c4, e4, g4, c5,
stop
save-midi phrase.4th
midi-close

\ Later:
load phrase.4th
midi-virtual
seq-play
7 seq-transpose
seq-play
midi-close
```

---

## mhs-midi: Haskell MIDI Language

A functional approach to MIDI using MicroHs (a lightweight Haskell implementation).

### Building mhs-midi Examples

```bash
make                    # Builds all including mhs-midi
./build/hello_midi      # Run C major scale example
./build/list_ports      # List available MIDI ports
```

### Interactive REPL

The `mhs-midi` binary provides an interactive Haskell REPL with MIDI FFI support:

```bash
./scripts/mhs-midi-repl
```

This wrapper sets up paths and enables caching for fast startup.

In the REPL (IO actions need `>>= print` or `>> return ()`):
```haskell
> import Midi
> midiOpenVirtual "TestPort" >>= print
True
> midiNoteOn 1 60 100 >> return ()
> midiSleep 500 >> return ()
> midiNoteOff 1 60 >> return ()
> midiClose >> return ()
```

Run Haskell files (interpreted):
```bash
./scripts/mhs-midi-repl -r MyMidiProgram.hs
```

Or compile to a standalone executable:
```bash
./scripts/mhs-midi-compile MyMidiProgram.hs -o my_program
./my_program
```

### Example: Playing Notes

```haskell
-- HelloMidi.hs
module HelloMidi(main) where
import Midi

main :: IO ()
main = do
    midiOpenVirtual "MhsMidi"

    -- Play C major scale
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

### Example: Chords

```haskell
module Chords(main) where
import Midi

main :: IO ()
main = do
    midiOpenVirtual "MhsMidi"

    -- I-IV-V-I progression
    chord [c4, e4, g4] half       -- C major
    chord [f4, a4, c5] half       -- F major
    chord [g4, b4, d5] half       -- G major
    chord [c4, e4, g4] whole      -- C major

    midiClose
```

### Midi.hs API

| Function | Description |
|----------|-------------|
| `midiOpenVirtual "name"` | Create virtual MIDI port |
| `midiOpen n` | Open hardware port by index |
| `midiClose` | Close MIDI port |
| `playNote pitch dur` | Play note with default velocity |
| `play pitch vel dur` | Play note with velocity |
| `playChord [pitches] vel dur` | Play chord |
| `chord [pitches] dur` | Play chord (default velocity) |
| `arpeggio [pitches] dur vel` | Arpeggiate notes |
| `melody [(pitch,dur)] vel` | Play melody sequence |
| `rest dur` | Silent pause |
| `times n action` | Repeat action n times |

### Pitches

```haskell
c0..c8, d0..d8, e0..e8, f0..f8, g0..g8, a0..a8, b0..b8  -- Natural notes
cs0..cs8, ds0..ds8, fs0..fs8, gs0..gs8, as0..as8       -- Sharps
db, eb, gb, ab, bb                                      -- Flat aliases
```

### Durations

```haskell
whole      -- 2000ms (at 120 BPM)
half       -- 1000ms
quarter    -- 500ms
eighth     -- 250ms
sixteenth  -- 125ms
dotted d   -- 1.5x duration
bpm n      -- Quarter note duration at tempo n
```

### Dynamics

```haskell
ppp, pp, p, mp, mf, ff, fff  -- Velocity values 16..112
```

---

## Architecture

- **projects/midi_forth/midi_forth.c** (~2700 lines)
  - Concise notation system with comma trigger
  - 32-bit packed note format
  - Sequence storage (256 events/seq, 64 sequences)
  - Recording and MIDI capture systems
  - Conditionals, blocks, word definitions

- **projects/forth/forth.c** - Basic Forth interpreter

- **projects/mhs_midi/** - MicroHs MIDI language
  - `midi_ffi.c/h` - C FFI bindings for libremidi
  - `lib/Midi.hs` - High-level Haskell MIDI library
  - `examples/` - Example programs

- **Dependencies**:
  - libremidi v5.3.1 (auto-built from `thirdparty/libremidi/`)
  - [MicroHs](https://github.com/augustss/MicroHs) (in `thirdparty/MicroHs/`)
