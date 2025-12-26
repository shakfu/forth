# MIDI Forth Syntax Reference

## Overview

MIDI Forth is a Forth-like language for MIDI sequencing and live performance. This document covers both implemented features and planned extensions.

---

# Implemented Features

## Note Notation

### Pitch Names

Format: `[note][accidental][octave]`

| Component | Values |
|-----------|--------|
| Note | `c`, `d`, `e`, `f`, `g`, `a`, `b` (case insensitive) |
| Accidental | `#` (sharp), `b` (flat), or omitted (natural) |
| Octave | `0` - `9` (middle C = C4 = MIDI 60) |

Examples:
- `c4` = 60 (middle C)
- `a4` = 69 (A440)
- `c#4` = 61
- `db4` = 61 (enharmonic)
- `c5` = 72
- `c3` = 48

MIDI numbers (0-127) can also be used directly: `60` = middle C.

### Playing Notes

The comma `,` is the universal play trigger:

```forth
c4,                     \ Play C4 with defaults (ch=1, vel=80, dur=500ms)
60,                     \ Same note using MIDI number
c#4,                    \ C sharp 4
db4,                    \ D flat 4
```

### Sequential Notes

Notes separated by commas play one after another (blocking):

```forth
c4, d4, e4,             \ Play C, D, E in sequence
```

### Chords (Concurrent Notes)

Parentheses group notes to play simultaneously:

```forth
(c4 e4 g4),             \ C major chord
(c4 eb4 g4),            \ C minor chord
(60 64 67),             \ Same as C major, using MIDI numbers
```

Inside parentheses:
- Whitespace separates pitches
- No commas needed (all notes share same parameters)
- `)` closes the group, `,` triggers playback

### Explicit Parameters

Override channel, velocity, and duration:

```forth
\ Single note: ch pitch vel dur
1 c4 100 500,           \ Channel 1, C4, velocity 100, duration 500ms
2 60 80 250,            \ Channel 2, MIDI 60, vel 80, dur 250ms

\ Chord: pitches then ch vel dur
(c4 e4 g4) 1 80 500,    \ C major, channel 1, vel 80, dur 500ms
(c4 e4 g4) 2 100 1000,  \ On channel 2, louder, longer
```

### Context Variables

Set defaults to avoid repetition:

```forth
2 ch!                   \ Set default channel to 2
100 vel!                \ Set default velocity to 100
250 dur!                \ Set default duration to 250ms

c4,                     \ Now plays on ch 2, vel 100, dur 250ms
```

## MIDI Setup

```forth
midi-list               \ List available MIDI output ports
midi-open ( n -- )      \ Open port by index
midi-virtual            \ Create virtual port "MidiForth"
midi-close              \ Close current port
panic                   \ All notes off (emergency stop)
```

## Control Messages

```forth
\ Control Change: ch cc val
1 64 127 cc             \ Sustain pedal on (CC 64)
1 64 0 cc               \ Sustain pedal off
1 1 64 cc               \ Mod wheel to 64
```

## Stack Operations

Standard Forth stack words:

```forth
dup                     \ Duplicate top
drop                    \ Discard top
swap                    \ Swap top two
over                    \ Copy second to top
rot                     \ Rotate top three
.s                      \ Show stack contents
```

## Arithmetic

```forth
+  -  *  /              \ Basic math
and  or  xor  not       \ Bitwise
=  <  >                  \ Comparison (returns -1 true, 0 false)
```

## Timing

```forth
500 ms                  \ Sleep 500 milliseconds
120 bpm!                \ Set tempo to 120 BPM
bpm@                    \ Get current BPM
```

---

# Planned Extensions

## Priority 1: Core

Foundation for composition and reuse.

### Word Definitions

Standard Forth colon definitions for reusable patterns:

```forth
: cmaj (c4 e4 g4), ;
: cmin (c4 eb4 g4), ;
: melody c4, e4, g4, c5, ;

cmaj                    \ Play C major chord
melody                  \ Play the melody
```

**Implementation notes:**
- Requires compile mode vs interpret mode
- Store word body as token list
- Execute by interpreting stored tokens
- Warn if stack not empty after `;` (catches missing trailing comma)

```forth
: broken c4, e4, g4, c5 ;
Warning: definition 'broken' ended with 1 item(s) on stack
```

### Rests

Silence for the default (or explicit) duration:

```forth
r,                      \ Rest with default duration
r 250,                  \ Rest with explicit duration (ms)
c4, r, e4, r, g4,       \ Notes with rests between
```

**Open question**: Should `r` push a sentinel value, or should `,` detect rest mode?

### Loops

Repeat patterns using defined words:

```forth
: phrase c4, d4, e4, ;
4 0 do phrase loop      \ Standard Forth loop

\ Or simpler syntax:
phrase 4 times          \ Custom "times" word
```

## Priority 2: Generative / Expression

Enable generative music and expressive notation.

### Dynamics

Velocity presets using standard musical terms:

```forth
ff                      \ Set velocity to 112 (fortissimo)
mf                      \ Set velocity to 80 (mezzo-forte)
pp                      \ Set velocity to 32 (pianissimo)

mf c4, e4, ff g4,       \ Dynamics change mid-phrase
```

| Symbol | Velocity | Meaning |
|--------|----------|---------|
| `ppp`  | 16       | pianississimo |
| `pp`   | 32       | pianissimo |
| `p`    | 48       | piano |
| `mp`   | 64       | mezzo-piano |
| `mf`   | 80       | mezzo-forte |
| `f`    | 96       | forte |
| `ff`   | 112      | fortissimo |
| `fff`  | 127      | fortississimo |

### Probability (Generative Music)

Two complementary syntaxes for probabilistic note selection:

#### Weighted play/silence with `%`

The `%` modifier specifies probability of playing vs. silence:

```forth
c4 75%,                 \ 75% chance to play, 25% silence
c4 90%,                 \ 90% chance to play
c4 50%,                 \ Coin flip
c4 100%,                \ Always (same as c4,)
c4 0%,                  \ Never (rest)

(c4 e4 g4) 80%,         \ 80% chance to play chord

c4, e4 50%, g4,         \ C always, E maybe, G always
```

#### Equal-probability alternatives with `|`

The `|` separates alternatives with equal probability:

```forth
c4|e4,                  \ 50% C4, 50% E4
c4|e4|g4,               \ 33.33% each
c4|e4|g4|a4,            \ 25% each
c4|r,                   \ 50% C4, 50% silence (equivalent to c4 50%,)
(c4 e4 g4)|(f4 a4 c5),  \ 50% C major, 50% F major
```

#### Summary

| Syntax | Meaning |
|--------|---------|
| `c4,` | Always play |
| `c4 75%,` | 75% play, 25% silence |
| `c4\|e4,` | 50/50 between C4 and E4 |
| `c4\|e4\|g4,` | Equal chance of each |
| `c4\|r,` | Same as `c4 50%,` |

Use cases:
- Generative/algorithmic composition
- Humanization (slight randomness)
- Evolving patterns

**Implementation**:
- `%` pops probability (0-100), plays if `random() < probability`, else silence
- `|` requires tokenizer change to treat `|` as separator; picks one alternative at random

### Square Brackets for Explicit Form

Explicit grouping to avoid stack-depth heuristics:

```forth
[1 c4 100 500],         \ Explicit single note
[(c4 e4 g4) 1 80 500],  \ Explicit chord
```

**Rationale**: The current heuristic (count stack items) is fragile if the stack has leftover values. Brackets provide explicit grouping. Could also enable partial overrides like `[c4 100],` (pitch + velocity only).

## Priority 3: Convenience

Quality of life improvements.

### Relative Intervals

Move by semitones from last pitch:

```forth
c4, +2, +2, +1,         \ C D E F (whole, whole, half step)
c4, +7, +5,             \ C G C (up fifth, up fourth)
g4, -2, -2,             \ G F Eb (down whole steps)
```

**Requires**: Tracking "current pitch" state.

### Octave Shifts

```forth
c4, ^,                  \ C4 then C5 (up octave)
c4, v,                  \ C4 then C3 (down octave)
```

### Program Change

```forth
1 0 pc                  \ Channel 1, program 0 (Acoustic Grand Piano)
1 25 pc                 \ Channel 1, program 25 (Acoustic Guitar)
10 0 pc                 \ Channel 10 (drums), program 0
```

## Priority 4: Advanced

Extended features for complex compositions.

### Articulation

```forth
c4.,                    \ Staccato (50% duration)
c4-,                    \ Tenuto (full duration, slight emphasis)
c4>,                    \ Accent (higher velocity)
```

**Concern**: Suffixes complicate tokenization. May need different approach.

### Pitch Bend

```forth
1 8192 pb               \ Center (no bend)
1 0 pb                  \ Full bend down
1 16383 pb              \ Full bend up
```

### Anonymous Blocks

If word definitions feel too heavy for one-off patterns:

```forth
{ c4, e4, g4, } 4 *     \ Play phrase 4 times
```

**Concern**: Requires deferred execution. Less idiomatic than named words.

### Conditionals

```forth
: maybe-play
    random 50 > if
        c4,
    else
        r,
    then
;
```

**Requires**: `if`/`else`/`then` and comparison operators.

---

# Implementation Notes

## Tokenizer

The characters `,`, `(`, `)` are treated as word boundaries:
- `c4,` tokenizes as `c4` `,`
- `(c4 e4)` tokenizes as `(` `c4` `e4` `)`

Future: Add `[` `]` `{` `}` as word boundaries.

## Pitch Parsing

When a token is not in the dictionary and not a plain integer, parse as pitch name:

```c
int parse_pitch(const char* token) {
    int note, accidental = 0, octave;
    int i = 0;

    switch (tolower(token[i++])) {
        case 'c': note = 0; break;
        case 'd': note = 2; break;
        case 'e': note = 4; break;
        case 'f': note = 5; break;
        case 'g': note = 7; break;
        case 'a': note = 9; break;
        case 'b': note = 11; break;
        default: return -1;
    }

    if (token[i] == '#') { accidental = 1; i++; }
    else if (token[i] == 'b') { accidental = -1; i++; }

    if (!isdigit(token[i])) return -1;
    octave = token[i++] - '0';

    if (token[i] != '\0') return -1;

    return (octave + 1) * 12 + note + accidental;
}
```

## Chord Marker

A sentinel value marks the start of a chord on the stack:

```c
#define CHORD_MARKER 0x7FFFFFFF

void op_chord_open(Stack* stack) {
    push(stack, CHORD_MARKER);
}
```

## Comma Behavior

The `,` word inspects the stack to determine mode:

1. Find chord marker (if any)
2. Count items since marker (or stack bottom)
3. Dispatch:
   - Chord marker + N pitches: play chord
   - Chord marker + N pitches + 3 params: play chord with params
   - 1 item: play note with defaults
   - 4 items: play note with explicit params
   - Other: error

## Defaults

| Parameter | Default | Range |
|-----------|---------|-------|
| Channel   | 1       | 1-16 |
| Velocity  | 80      | 0-127 |
| Duration  | 500ms   | 1-60000 |

---

# Examples

## Simple Melody

```forth
midi-virtual
250 dur!
c4, d4, e4, f4, g4, a4, b4, c5,
```

## Chord Progression

```forth
midi-virtual
500 dur!
(c4 e4 g4),             \ C major
(f4 a4 c5),             \ F major
(g4 b4 d5),             \ G major
(c4 e4 g4),             \ C major
```

## With Dynamics (future)

```forth
midi-virtual
mf
: verse c4, e4, g4, c5, ;
: chorus ff (c4 e4 g4 c5), ;
verse verse chorus verse
```

## Arpeggio Pattern (future)

```forth
midi-virtual
100 dur!
: arp c4, e4, g4, c5, g4, e4, ;
arp 4 times
```

---

# Complex Composition (future)

This section demonstrates advanced composition techniques using nested definitions and all bracket types.

## Bracket Reference

| Bracket | Purpose | Example |
|---------|---------|---------|
| `( )` | Chord grouping | `(c4 e4 g4),` |
| `[ ]` | Explicit parameters | `[1 c4 100 500],` |
| `{ }` | Anonymous blocks | `{ c4, e4, } 4 *` |
| `: ;` | Word definitions | `: melody c4, e4, ;` |

## Building Blocks: Atomic Phrases

Start with simple, reusable building blocks:

```forth
\ Individual chords
: Cmaj  (c4 e4 g4), ;
: Dm    (d4 f4 a4), ;
: Em    (e4 g4 b4), ;
: Fmaj  (f4 a4 c5), ;
: Gmaj  (g4 b4 d5), ;
: Am    (a4 c5 e5), ;

\ Arpeggios
: Cmaj-arp  c4, e4, g4, c5, ;
: Cmaj-arp-down  c5, g4, e4, c4, ;

\ Melodic fragments
: motif-a  c4, d4, e4, c4, ;
: motif-b  e4, f4, g4, ;
```

## Nested Composition

Words can call other words to build larger structures:

```forth
\ Combine motifs into phrases
: phrase-1  motif-a motif-b ;
: phrase-2  motif-b motif-a ;

\ Combine phrases into sections
: verse    phrase-1 phrase-1 phrase-2 ;
: chorus   ff phrase-2 phrase-2 mf ;

\ Combine sections into song structure
: song     verse verse chorus verse chorus chorus ;
```

## Layering with Channels

Use explicit parameters for multi-channel composition:

```forth
\ Define instruments on different channels
: piano    1 ch! ;
: bass     2 ch! ;
: strings  3 ch! ;

\ Bass line (channel 2, lower octave, longer notes)
: bassline
    bass 500 dur!
    c2, c2, f2, g2,
;

\ Chord pad (channel 3, sustained)
: pad
    strings 1000 dur! 60 vel!
    (c4 e4 g4), (f4 a4 c5), (g4 b4 d5), (c4 e4 g4),
;

\ Melody (channel 1)
: lead
    piano 250 dur! 100 vel!
    c5, d5, e5, g5, e5, d5, c5, r,
;
```

## Explicit Parameters with Brackets

Use `[ ]` for precise control within compositions:

```forth
\ Mix of defaults and explicit
: dynamic-phrase
    c4,                       \ Use defaults
    [1 e4 120 100],           \ Accent: louder, shorter
    [1 g4 60 400],            \ Soft, sustained
    c5,                       \ Back to defaults
;

\ Chord with explicit parameters
: dramatic-chord
    [(c3 c4 e4 g4 c5) 1 127 2000],   \ Full, loud, long
;
```

## Generative Patterns with Probability

Use `%` and `|` for evolving, non-repetitive music:

```forth
\ Maybe play a grace note
: maybe-grace  d4 30%, ;

\ Melody with variations
: gen-melody
    c4,
    maybe-grace e4,           \ Sometimes has grace note
    g4|a4,                    \ Random choice
    c5 80%,                   \ Usually play, sometimes skip
;

\ Chord with random voicing
: gen-chord
    (c4 e4 g4)|(c4 e4 g4 c5)|(e4 g4 c5),   \ Different voicings
;

\ Evolving progression
: gen-progression
    Cmaj|Am,                  \ I or vi
    Fmaj|Dm,                  \ IV or ii
    Gmaj,                     \ V (always)
    Cmaj 80%,                 \ Usually resolve, sometimes don't
;
```

## Combining Everything

A complete composition using all features:

```forth
midi-virtual
120 bpm!

\ === Sound palette ===
: piano   1 ch! ;
: bass    2 ch! ;
: drums  10 ch! ;

\ === Rhythm section ===
: kick    [10 36 100 100], ;   \ Bass drum
: snare   [10 38 100 100], ;   \ Snare
: hihat   [10 42 60 50], ;     \ Closed hi-hat

: beat
    kick hihat, snare hihat,
    kick hihat, kick snare hihat,
;

\ === Bass patterns ===
: bass-a
    bass 200 dur!
    c2, r, c2, r, g2, r, g2, r,
;

: bass-b
    bass 200 dur!
    f2, r, f2, r, g2, r, g2, a2,
;

\ === Chord voicings ===
: pad-C   piano 70 vel! 800 dur! (c4 e4 g4), ;
: pad-F   piano 70 vel! 800 dur! (f4 a4 c5), ;
: pad-G   piano 70 vel! 800 dur! (g4 b4 d5), ;
: pad-Am  piano 70 vel! 800 dur! (a4 c5 e5), ;

\ === Melodic fragments ===
: riff-1
    piano 100 vel! 100 dur!
    c5, d5, e5, g5,
    e5 80%, d5, c5|d5, r,
;

: riff-2
    piano 100 vel! 100 dur!
    g5, e5, d5, c5,
    c5|d5|e5, r, r, r,
;

\ === Sections ===
: intro
    pad-C pad-C pad-F pad-G
;

: verse
    bass-a riff-1
    bass-b riff-2
;

: chorus
    ff
    pad-C riff-1 pad-Am riff-2
    pad-F riff-1 pad-G riff-1
    mf
;

\ === Song structure ===
: song
    intro
    verse verse
    chorus
    verse
    chorus chorus
    intro           \ Outro
;

\ Play it!
song
```

## Pattern Variations with Anonymous Blocks

Use `{ }` for quick variations without defining words:

```forth
\ Repeat a phrase
{ c4, e4, g4, } 4 *

\ Repeat with probability
{ c4 80%, e4, g4 50%, } 8 *

\ Nested repetition
: ostinato
    {
        { c4, e4, } 2 *
        { g4, e4, } 2 *
    } 4 *
;
```

## Summary: Composition Hierarchy

```
Song
 +-- Section (intro, verse, chorus)
      +-- Phrase
           +-- Motif / Riff
                +-- Notes, Chords, Rests
                     +-- Pitch, Velocity, Duration, Channel
```

Each level can be defined as a word and reused, creating maintainable, readable compositions.
