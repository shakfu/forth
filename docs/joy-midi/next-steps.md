# Joy-MIDI: Next Steps

This document tracks what's implemented and outlines future directions for Joy-MIDI.

## Current Implementation Status

### Implemented

| Feature | Syntax | Notes |
|---------|--------|-------|
| Note literals | `c d e f g a b` | Parse-time conversion to MIDI integers |
| Octave suffix | `c4 c5 c6` | Explicit octave (default 4) |
| Accidentals | `c+ c- d++ e--` | Sharp/flat modifiers |
| Rests | `r` | Rest marker (-1) |
| Sequential play | `[c e g] play` | Play notes one after another |
| Chord play | `[c e g] chord` | Play notes simultaneously |
| Dynamics | `pp p mp mf f ff` | Runtime velocity control |
| Tempo | `120 tempo` | Set BPM |
| Quantization | `80 quant` | Gate time percentage |
| Volume | `75 vol` | Velocity percentage |
| Chord builders | `c major`, `d minor` | Build triads from root |
| 7th chords | `c dom7`, `d maj7`, `e min7` | Build 7th chords |
| Transpose | `60 7 transpose` | Transpose by semitones |
| User definitions | `def name == body .` | DEFINE/def syntax |
| Virtual MIDI | Auto-created in REPL | No explicit `midi-virtual` needed |

### Design Principle

Notes are converted to MIDI integers at **parse time**. This means:
- `c` becomes `60`, `c5` becomes `72`
- `[c d e]` is literally `[60 62 64]`
- All Joy combinators work naturally: `[c d e] [7 +] map` transposes

This is fundamentally different from the original Alda-like proposal where notes would auto-play. The current design is more algebraic and Joy-like.

---

## Phase 1: Standard Library (prelude.joy)

Create a prelude file with common musical patterns:

```joy
\ prelude.joy - Standard Joy-MIDI library

\ --- Selection ---
def pick == dup size rand swap rem at .
def shuffle == dup size [swap dup size rand swap rem at swap] times pop .

\ --- Repetition ---
def repeat == swap [dup] swap times pop .  \ [c e g] 4 repeat -> [c e g c e g c e g c e g]

\ --- Transposition ---
def up == [+] cons map .       \ [c d e] 7 up -> [g a b]
def down == [swap -] cons map . \ [c d e] 7 down -> [f g a]
def octave-up == 12 up .
def octave-down == 12 down .

\ --- Intervals ---
def unison == 0 .
def min2 == 1 .
def maj2 == 2 .
def min3 == 3 .
def maj3 == 4 .
def p4 == 5 .
def tritone == 6 .
def p5 == 7 .
def min6 == 8 .
def maj6 == 9 .
def min7 == 10 .
def maj7 == 11 .
def octave == 12 .

\ --- Scales (as interval patterns) ---
def major-scale == [0 2 4 5 7 9 11 12] .
def minor-scale == [0 2 3 5 7 8 10 12] .
def pentatonic == [0 2 4 7 9] .
def blues == [0 3 5 6 7 10] .
def chromatic == [0 1 2 3 4 5 6 7 8 9 10 11] .

\ Build scale from root: c major-scale scale -> [60 62 64 65 67 69 71 72]
def scale == swap [[+] cons] cons map i .

\ --- Arpeggiation ---
def arp == [play] step .
def arp-down == reverse arp .
def arp-up-down == dup reverse rest concat arp .

\ --- Rhythm helpers ---
def staccato == 50 quant .
def legato == 100 quant .
def normal == 90 quant .

\ --- Random/generative ---
def maybe == rand 100 < [i] [pop] ifte .  \ [c play] 50 maybe
def one-of == pick i .                     \ [[c play] [e play] [g play]] one-of
```

**Implementation**: Add `joy_load_file()` call in main.c to load prelude if it exists.

---

## Phase 2: Extended MIDI Primitives

### Control Change Messages

```joy
\ midi-cc ( cc-num value -- )
64 127 midi-cc    \ Sustain pedal on
64 0 midi-cc      \ Sustain pedal off
1 64 midi-cc      \ Mod wheel half

\ Named CC helpers (in prelude)
def sustain-on == 64 127 midi-cc .
def sustain-off == 64 0 midi-cc .
def mod == 1 swap midi-cc .       \ 64 mod
def expression == 11 swap midi-cc .
def pan == 10 swap midi-cc .      \ 0=left, 64=center, 127=right
```

### Program Change

```joy
\ midi-program ( program -- )
0 midi-program    \ Acoustic Grand Piano
24 midi-program   \ Nylon Guitar
73 midi-program   \ Flute

\ Named programs (General MIDI, in prelude)
def piano == 0 midi-program .
def epiano == 4 midi-program .
def organ == 19 midi-program .
def guitar == 24 midi-program .
def bass == 32 midi-program .
def strings == 48 midi-program .
def brass == 61 midi-program .
def flute == 73 midi-program .
def synth-lead == 80 midi-program .
def synth-pad == 88 midi-program .
```

### Channel Selection

```joy
\ midi-channel ( ch -- )
1 midi-channel    \ Switch to channel 1 (0-15)
9 midi-channel    \ Drums (channel 10 in 1-indexed)

\ Per-note channel (alternative)
60 80 500 1 midi-note-ch   \ Note on specific channel
```

### Pitch Bend

```joy
\ midi-bend ( value -- )  value: -8192 to 8191, 0 = center
0 midi-bend       \ Center
4096 midi-bend    \ Bend up
-4096 midi-bend   \ Bend down

\ Cents-based (in prelude)
def bend-cents == 8192 * 100 / midi-bend .  \ 50 bend-cents = quarter tone up
```

---

## Phase 3: Timing and Sequencing

### Non-blocking Note Playback

```joy
\ Fire-and-forget notes (current play is blocking)
c play&           \ Start note, don't wait
500 midi-sleep    \ Do other things
                  \ Note auto-stops after duration

\ Or explicit on/off
c 80 midi-note-on
500 midi-sleep
c midi-note-off
```

### Sequence Builder

```joy
\ Build sequences for parallel playback
seq-new
  c 500 seq-add
  e 500 seq-add
  g 1000 seq-add
seq-play          \ Play sequence (blocking)
seq-play&         \ Play sequence (non-blocking)

\ Or list-based
[[c 500] [e 500] [g 1000]] seq-from-list seq-play
```

### Parallel Voices

```joy
\ Play two sequences simultaneously
[c d e f g] [play] step    \ Melody (blocking)

\ With parallel construct
[ [c d e f g] arp ]
[ [c3 e3 g3] chord 2000 midi-sleep [f3 a3 c] chord ]
parallel                    \ Both run concurrently, wait for longest
```

---

## Phase 4: Duration Notation

The current design uses `tempo` and fixed durations. We could add rhythmic notation:

### Option A: Duration Words

```joy
\ Duration modifiers (set state like dynamics)
q       \ Quarter note duration
h       \ Half note
w       \ Whole
e       \ Eighth
s       \ Sixteenth

\ Usage
q [c d e f] arp    \ Quarter notes
e [c d e f] arp    \ Eighth notes
```

### Option B: Duration Suffixes (Parse-time)

Extend the note parser to handle duration:

```joy
c4 d4 e4 f4       \ Quarter notes (c4 = C quarter, not C octave 4)
c8 d8 e8 f8       \ Eighth notes
```

**Conflict**: This conflicts with octave notation (`c4` = octave 4). Options:
1. Use `c:4` for duration, `c4` for octave
2. Use `c/4` for duration
3. Context-dependent (if followed by note, it's duration; otherwise octave)

### Option C: Explicit Duration Parameter

```joy
\ play-with-dur ( notes duration -- )
[c d e f] 250 play-dur    \ Each note 250ms
[c d e f] q play-dur      \ Each note = quarter at current tempo
```

**Recommendation**: Start with Option C (explicit duration) as it's most compatible with the current algebraic design.

---

## Phase 5: Pattern Language

### Euclidean Rhythms

```joy
\ euclidean ( hits total -- pattern )
3 8 euclidean     \ -> [1 0 0 1 0 0 1 0] (tresillo)
5 8 euclidean     \ -> [1 0 1 1 0 1 1 0] (cinquillo)

\ Apply to notes
[c e g] 3 8 euclidean apply-rhythm play
```

### Markov Chains

```joy
\ Transition matrix for melody generation
[
  [0.1 0.3 0.2 0.2 0.1 0.1]   \ From C: probabilities to C D E F G A
  [0.2 0.1 0.3 0.2 0.1 0.1]   \ From D
  \ ...
] markov-matrix

[c d e f g a] 16 markov-generate play   \ Generate 16-note melody
```

### L-Systems

```joy
\ Musical L-system
"A" axiom
[ ["A" "AB"] ["B" "A"] ] rules
5 iterate         \ Apply rules 5 times
[c e] interpret   \ A=c, B=e
play
```

---

## Phase 6: MIDI File I/O

### Export

```joy
\ Record to MIDI file
midi-record-start
[c d e f g] arp
[c e g] chord
midi-record-stop
"melody.mid" midi-save
```

### Import

```joy
\ Load and analyze MIDI file
"song.mid" midi-load    \ -> [[note vel dur] ...]
dup size .              \ Number of events
[first] map             \ Extract pitches
```

---

## Phase 7: Audio Synthesis (Future)

If we add audio output alongside MIDI:

```joy
\ Simple oscillators
440 sine play-audio       \ 440Hz sine wave
[440 550 660] sines mix play-audio   \ Chord

\ Envelope
440 sine [0.1 0.2 0.5 0.3] adsr apply play-audio

\ Effects
440 sine 1000 lowpass 0.3 reverb play-audio
```

This would require integrating an audio library (miniaudio, PortAudio, etc.).

---

## Implementation Priority

### High Priority (Core Usability)
1. **prelude.joy** - Standard library with common patterns
2. **midi-cc** - Control change for expression
3. **midi-program** - Instrument selection
4. **Documentation** - Complete reference for all primitives

### Medium Priority (Enhanced Expression)
5. **midi-channel** - Multi-channel support
6. **Non-blocking play** - `play&` for concurrent notes
7. **Sequence builder** - Complex timing patterns
8. **Duration helpers** - `q h w e s` duration words

### Lower Priority (Advanced Features)
9. **Euclidean rhythms** - Generative patterns
10. **MIDI file export** - Save compositions
11. **Parallel voices** - True polyphony
12. **Audio synthesis** - Direct sound generation

---

## File Organization

```
projects/joy-midi/
  main.c
  joy_midi.c/h
  midi_primitives.c/h
  music_notation.c/h
  music_context.c/h
  prelude.joy           # NEW: Standard library

docs/joy-midi/
  README.md             # User guide
  next-steps.md         # This file
  reference.md          # NEW: Complete primitive reference
```

---

## References

- [Alda Language](https://alda.io/) - Notation inspiration
- [Joy Language](https://hypercubed.github.io/joy/html/j00rat.html) - Concatenative programming
- [TidalCycles](https://tidalcycles.org/) - Pattern language inspiration
- [Sonic Pi](https://sonic-pi.net/) - Live coding reference
- [General MIDI](https://en.wikipedia.org/wiki/General_MIDI) - Program numbers
