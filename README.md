# midi-langs

A collection of mini MIDI-capable languages for generating and transforming MIDI sequences:

- **midi_forth** - A Forth-like interpreter with concise musical notation
- **mhs-midi** - A Haskell-based MIDI language using [MicroHs](https://github.com/augustss/MicroHs)
- **pktpy_midi** - A Python-based MIDI language using [PocketPy](https://pocketpy.dev)
- **s7_midi** - A Scheme-based MIDI language using [s7](https://ccrma.stanford.edu/software/snd/snd/s7.html)
- **lua_midi** - A Lua-based MIDI language using [Lua 5.5](https://www.lua.org/)

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

## pktpy_midi: Python MIDI Language

A Pythonic approach to MIDI using PocketPy (a lightweight embeddable Python interpreter).

### Running

```bash
./build/pktpy_midi              # Start Python REPL with MIDI support
./build/pktpy_midi script.py    # Run a Python script
```

### Quick Example

```python
import midi

with midi.open() as m:
    m.note("C4")                    # Play middle C (500ms, velocity 80)
    m.note("E4")
    m.note("G4")
    m.note("C4", 100, 1000)         # Louder, longer
```

### API Reference

```python
import midi

# --- Port management ---
ports = midi.list_ports()       # [(0, "Port Name"), ...]
m = midi.open()                 # Virtual port (default name "pktpyMIDI")
m = midi.open("CustomName")     # Virtual port with custom name
m = midi.open(0)                # Hardware port by index

# --- Pitch helpers ---
midi.note("C4")                 # -> 60 (parse note name)
midi.c4, midi.cs4, midi.d4      # Pitch constants (c0-b8, sharps: cs, ds, fs, gs, as)
midi.transpose("C4", 2)         # -> 62 (transpose by semitones)
midi.octave_up(60)              # -> 72
midi.octave_down(60)            # -> 48

# --- Dynamics (velocity values) ---
midi.ppp    # 16
midi.pp     # 33
midi.p      # 49
midi.mp     # 64
midi.mf     # 80 (default)
midi.f      # 96
midi.ff     # 112
midi.fff    # 127

# --- Durations (milliseconds at 120 BPM) ---
midi.whole      # 2000
midi.half       # 1000
midi.quarter    # 500
midi.eighth     # 250
midi.sixteenth  # 125
midi.dotted(midi.quarter)  # 750 (1.5x)

# --- Tempo ---
midi.set_tempo(140)         # Set BPM (updates duration constants)
midi.get_tempo()            # Get current BPM
midi.bpm(60)                # -> 1000 (quarter note ms at 60 BPM)

# --- Chord builders (return pitch lists) ---
midi.major("C4")            # [60, 64, 67]
midi.minor("C4")            # [60, 63, 67]
midi.dim("C4")              # [60, 63, 66]
midi.aug("C4")              # [60, 64, 68]
midi.dom7("C4")             # [60, 64, 67, 70]
midi.maj7("C4")             # [60, 64, 67, 71]
midi.min7("C4")             # [60, 63, 67, 70]

# --- Timing ---
midi.sleep(500)             # Sleep 500ms
midi.rest()                 # Rest for quarter note duration
midi.rest(midi.half)        # Rest for half note

# --- MidiOut methods ---
m.note(pitch, velocity=80, duration=500, channel=1)
m.note("C4")                # Pitch can be string or int
m.note(midi.c4, midi.f, midi.quarter)  # Using constants

m.chord(pitches, velocity=80, duration=500, channel=1)
m.chord(midi.major("C4"))   # Using chord builder

m.arpeggio(pitches, velocity=80, note_duration=eighth, spacing=None, channel=1)
m.arpeggio(midi.dom7("G3"), midi.mf, midi.sixteenth)

# --- CC helpers ---
m.modulation(64)            # CC 1
m.volume(100)               # CC 7
m.pan(64)                   # CC 10 (0=left, 64=center, 127=right)
m.sustain(True)             # CC 64
m.sustain(False)

# --- Low-level methods ---
m.note_on(pitch, velocity=80, channel=1)
m.note_off(pitch, velocity=0, channel=1)
m.cc(control, value, channel=1)
m.program_change(program, channel=1)
m.all_notes_off(channel=None)
m.close()
m.is_open                   # Property

# --- Context manager ---
with midi.open() as m:
    m.note("C4")
```

### Example: Simple Melody

```python
import midi

with midi.open() as m:
    for n in [midi.c4, midi.d4, midi.e4, midi.f4, midi.g4]:
        m.note(n, midi.mf, midi.quarter)
```

### Example: Chord Progression

```python
import midi

with midi.open() as m:
    m.chord(midi.major("C4"), midi.mf, midi.half)
    m.chord(midi.major("F4"), midi.mf, midi.half)
    m.chord(midi.major("G4"), midi.f, midi.half)
    m.chord(midi.major("C4"), midi.mf, midi.whole)
```

### Example: Arpeggio with Dynamics

```python
import midi

midi.set_tempo(100)
with midi.open() as m:
    m.arpeggio(midi.maj7("C4"), midi.mp, midi.sixteenth)
    m.arpeggio(midi.min7("A3"), midi.mf, midi.sixteenth)
    m.arpeggio(midi.dom7("G3"), midi.f, midi.sixteenth)
    m.chord(midi.major("C4"), midi.ff, midi.whole)
```

---

## s7_midi: Scheme MIDI Language

A Lisp/Scheme approach to MIDI using s7 (a lightweight embeddable Scheme interpreter from the Snd project).

### Running

```bash
./build/s7_midi              # Start Scheme REPL with MIDI support
./build/s7_midi script.scm   # Run a Scheme file
./build/s7_midi -e '(+ 1 2)' # Evaluate expression
```

### Quick Example

Using convenience functions:

```scheme
(open)                          ; Open virtual MIDI port
(n c4)                          ; Play middle C
(ch (major c4))                 ; Play C major chord
(arp (min7 a3) mf sixteenth)    ; Arpeggiate A minor 7
(close)
```

Or with explicit port management:

```scheme
(define m (midi-open))
(midi-note m c4 mf quarter)
(midi-chord m (major c4) mf half)
(midi-close m)
```

### API Reference

```scheme
;; --- Port management ---
(midi-list-ports)           ; -> ((0 "Port Name") ...)
(midi-open)                 ; Virtual port (default name "s7MIDI")
(midi-open "CustomName")    ; Virtual port with custom name
(midi-open 0)               ; Hardware port by index
(midi-close m)              ; Close port
(midi-open? m)              ; Check if open
(midi-out? x)               ; Type predicate

;; --- Pitch helpers ---
(note "C4")                 ; -> 60 (parse note name)
(note 'c4)                  ; -> 60 (symbol form)
c4 cs4 d4 ...               ; Pitch constants (c0-c8, sharps: cs, ds, fs, gs, as)
(transpose c4 2)            ; -> 62 (transpose by semitones)
(octave-up c4)              ; -> 72
(octave-down c4)            ; -> 48

;; --- Dynamics (velocity values) ---
ppp    ; 16
pp     ; 33
p      ; 49
mp     ; 64
mf     ; 80 (default)
f      ; 96
ff     ; 112
fff    ; 127

;; --- Durations (milliseconds at 120 BPM) ---
whole      ; 2000
half       ; 1000
quarter    ; 500
eighth     ; 250
sixteenth  ; 125
(dotted quarter)  ; 750 (1.5x)

;; --- Tempo ---
(set-tempo! 140)            ; Set BPM (updates duration constants)
(get-tempo)                 ; Get current BPM
(bpm 60)                    ; -> 1000 (quarter note ms at 60 BPM)

;; --- Chord builders (return pitch lists) ---
(major c4)                  ; (60 64 67)
(minor c4)                  ; (60 63 67)
(dim c4)                    ; (60 63 66)
(aug c4)                    ; (60 64 68)
(dom7 c4)                   ; (60 64 67 70)
(maj7 c4)                   ; (60 64 67 71)
(min7 c4)                   ; (60 63 67 70)

;; --- Note playing ---
(midi-note m pitch [vel] [dur] [ch])
(midi-note m c4)            ; Defaults: vel=80, dur=500, ch=1
(midi-note m c4 mf quarter)

(midi-chord m pitches [vel] [dur] [ch])
(midi-chord m (major c4) mf half)

(midi-arpeggio m pitches [vel] [dur] [ch])
(midi-arpeggio m (dom7 g3) f sixteenth)

;; --- Low-level ---
(midi-note-on m pitch [vel] [ch])
(midi-note-off m pitch [vel] [ch])
(midi-cc m control value [ch])
(midi-program m program [ch])
(midi-all-notes-off m [ch])

;; --- Timing ---
(midi-sleep 500)            ; Sleep 500ms
(rest)                      ; Rest for quarter note
(rest half)                 ; Rest for half note

;; --- Utilities ---
(help)                      ; Show available functions

;; --- REPL convenience functions (use global *midi* port) ---
*midi*                      ; Global MIDI port variable
(open)                      ; Open virtual port, set *midi*
(open "name")               ; Open named virtual port
(open 0)                    ; Open hardware port by index
(close)                     ; Close *midi* port
(n c4)                      ; Play note on *midi*
(n c4 mf quarter)           ; With velocity and duration
(ch (major c4))             ; Play chord on *midi*
(arp (min7 a3) mf sixteenth) ; Arpeggiate on *midi*
```

### Example: Simple Melody

```scheme
(define m (midi-open))
(for-each (lambda (p) (midi-note m p mf quarter))
          (list c4 d4 e4 f4 g4))
(midi-close m)
```

### Example: Chord Progression

```scheme
(define m (midi-open))
(midi-chord m (major c4) mf half)    ; I
(midi-chord m (major f4) mf half)    ; IV
(midi-chord m (major g4) f half)     ; V
(midi-chord m (major c4) mf whole)   ; I
(midi-close m)
```

### Example: Generative Pattern

```scheme
(define m (midi-open))
(set-tempo! 140)

;; Random note from chord
(define (random-note chord)
  (list-ref chord (random (length chord))))

;; Play 16 random notes from C major 7
(do ((i 0 (+ i 1)))
    ((= i 16))
  (midi-note m (random-note (maj7 c4)) mf sixteenth))

(midi-close m)
```

---

## lua_midi: Lua MIDI Language

A Lua-based MIDI language using Lua 5.5, providing a simple and expressive approach to MIDI programming with concise syntax.

### Running

```bash
./build/lua_midi              # Start Lua REPL with MIDI support
./build/lua_midi script.lua   # Run a Lua script
./build/lua_midi -e 'print(c4)'  # Evaluate expression
```

### Quick Example

Using convenience functions (recommended for REPL):

```lua
> open()                        -- Open virtual MIDI port
> n(c4)                         -- Play middle C
> ch(major(c4))                 -- Play C major chord
> arp(min7(a3), mf, sixteenth)  -- Arpeggiate
> close()
```

Or with explicit port management (use global, not `local`):

```lua
> m = midi.open()               -- Global variable (no 'local'!)
> m:note(c4, mf, quarter)
> m:chord(major(c4), mf, half)
> m:close()
```

**Note:** In the REPL, each line is a separate chunk, so `local` variables don't persist. Use global assignment (`m = ...`) or the convenience functions.

### Global Constants

All constants are available as globals for concise syntax:

```lua
-- Pitches: c0-b8, cs0-cs8 (sharps), db0-bb8 (flats)
c4, e4, g4, cs4, db4, etc.

-- Dynamics: ppp, pp, p, mp, mf, f, ff, fff
-- Durations: whole, half, quarter, eighth, sixteenth
-- Chord builders: major, minor, dim, aug, dom7, maj7, min7
-- Utilities: transpose, octave_up, octave_down, dotted, rest, sleep
```

The `midi.` prefix also works: `midi.c4`, `midi.mf`, etc.

### API Reference

```lua
-- Port management
midi.list_ports()           -- List available MIDI ports
midi.open()                 -- Virtual port "luaMIDI"
midi.open("Name")           -- Named virtual port
midi.open(0)                -- Hardware port by index
m:close()                   -- Close port
m:is_open()                 -- Check if open

-- Note playing
m:note(pitch, [vel], [dur], [ch])     -- Play single note
m:chord(pitches, [vel], [dur], [ch])  -- Play chord
m:arpeggio(pitches, [vel], [dur], [ch]) -- Arpeggiate
m:note_on(pitch, [vel], [ch])         -- Send note on
m:note_off(pitch, [vel], [ch])        -- Send note off

-- Control messages
m:cc(control, value, [ch])  -- Control change
m:program(prog, [ch])       -- Program change
m:all_notes_off([ch])       -- All notes off

-- Tempo
midi.set_tempo(bpm)         -- Set tempo (updates duration constants)
midi.get_tempo()            -- Get current tempo

-- REPL convenience (use global port)
open(), close()             -- Open/close default port
n(pitch, [vel], [dur], [ch])   -- Play note
ch(pitches, [vel], [dur], [ch]) -- Play chord
arp(pitches, [vel], [dur], [ch]) -- Arpeggiate
```

### Example: Simple Melody (script)

```lua
-- melody.lua
local m = midi.open()
for _, p in ipairs({c4, d4, e4, f4, g4}) do
    m:note(p, mf, quarter)
end
m:close()
```

### Example: Chord Progression (REPL)

```lua
> open()
> ch(major(c4), mf, half)   -- I
> ch(major(f3), mf, half)   -- IV
> ch(major(g3), f, half)    -- V
> ch(major(c4), mf, whole)  -- I
> close()
```

### Example: Generative Pattern (script)

```lua
-- generative.lua
local m = midi.open()
midi.set_tempo(140)

-- Random note from chord
local function random_note(chord)
    return chord[math.random(#chord)]
end

-- Play 16 random notes from C major 7
local chord = maj7(c4)
for i = 1, 16 do
    m:note(random_note(chord), mf, sixteenth)
end

m:close()
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

- **projects/pktpy_midi/** - PocketPy MIDI language
  - `midi_module.c` - C bindings for libremidi with Pythonic API
  - `pocketpy.c/h` - PocketPy v2.1.6 interpreter
  - Context manager support, note name parsing

- **projects/s7_midi/** - s7 Scheme MIDI language
  - `midi_module.c` - s7 FFI bindings for libremidi
  - Scheme prelude with pitch constants, chord builders, tempo
  - Full Scheme programming (closures, macros, etc.)

- **projects/lua_midi/** - Lua 5.5 MIDI language
  - `midi_module.c` - Lua FFI bindings for libremidi
  - Lua prelude with pitch constants, chord builders, tempo
  - Full Lua programming with tables, closures, metatables

- **Dependencies**:
  - libremidi v5.3.1 (auto-built from `thirdparty/libremidi/`)
  - [MicroHs](https://github.com/augustss/MicroHs) (in `thirdparty/MicroHs/`)
  - [PocketPy](https://pocketpy.dev) v2.1.6 (embedded in `projects/pktpy_midi/`)
  - [s7 Scheme](https://ccrma.stanford.edu/software/snd/snd/s7.html) (in `thirdparty/s7/`)
  - [Lua 5.5](https://www.lua.org/) (in `thirdparty/lua-5.5.0/`)
