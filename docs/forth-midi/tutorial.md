# MIDI Forth Tutorial

A getting started guide for MIDI Forth - a Forth-like language for MIDI sequencing.

## Setup

Build and run:

```bash
make
./build/forth_midi
```

You'll see a prompt: `>`. Type `help` for a command reference, `quit` to exit.

## Lesson 1: Playing Notes

First, create a virtual MIDI output that other apps can receive from:

```forth
midi-virtual
```

Now play a note using pitch name and comma:

```forth
c4,
```

The comma `,` is the play trigger. Try different notes:

```forth
d4, e4, f4, g4,
```

Sharps use `#`, flats use `b`:

```forth
c#4, db4, f#4,
```

You can also use MIDI note numbers directly:

```forth
60, 64, 67,
```

## Lesson 2: Chords

Wrap notes in parentheses to play them together:

```forth
(c4 e4 g4),
```

This plays a C major chord. Try others:

```forth
(d4 f4 a4),
(e4 g4 b4),
```

## Lesson 3: Context Variables

Set defaults for channel, velocity, and duration:

```forth
2 ch!           \ Use MIDI channel 2
100 vel!        \ Velocity 100 (louder)
250 dur!        \ 250ms duration (shorter)
c4, e4, g4,     \ Plays with new defaults
```

Reset to defaults:

```forth
1 ch! 80 vel! 500 dur!
```

## Lesson 4: Dynamics

Use dynamic markings to set velocity:

```forth
pp c4,          \ Very soft (velocity 32)
mf e4,          \ Medium (velocity 80)
ff g4,          \ Very loud (velocity 112)
```

Available dynamics: `ppp` `pp` `p` `mp` `mf` `f` `ff` `fff`

## Lesson 5: Rests

Use `r` for silence:

```forth
c4, r, e4, r, g4,
```

Specify rest duration:

```forth
c4, r 1000,     \ 1 second rest
e4,
```

## Lesson 6: Word Definitions

Define reusable patterns with `: name ... ;`

```forth
: cmaj (c4 e4 g4), ;
: gmaj (g3 b3 d4), ;
cmaj gmaj cmaj
```

Define melodies:

```forth
: melody c4, e4, g4, c5, g4, e4, c4, ;
melody
```

## Lesson 7: Loops

Repeat a word with `times`:

```forth
: phrase c4, e4, g4, ;
phrase 4 times
```

This plays the phrase 4 times total (1 initial + 3 repeats).

## Lesson 8: Generative Music

### Probability

Add chance with `%`:

```forth
c4 75%,         \ 75% chance to play
c4 50%,         \ Coin flip
```

### Alternatives

Choose randomly between options with `|`:

```forth
c4|e4,          \ 50% C4, 50% E4
c4|e4|g4,       \ 33% each
c4|r,           \ 50% play, 50% silence
```

Combine for generative patterns:

```forth
: gen c4|e4|g4 75%, ;
gen 16 times
```

## Lesson 9: Relative Movement

Use `+N` and `-N` for semitone intervals:

```forth
c4, +2, +2, +1, +2,    \ C D E F G (whole, whole, half, whole)
```

Octave shifts with `^` and `v`:

```forth
c4, ^, ^, v, v,        \ C4, C5, C6, C5, C4
```

## Lesson 10: Articulation

Add suffixes to pitch names:

```forth
c4.,            \ Staccato (50% duration)
c4>,            \ Accent (+20 velocity)
c4-,            \ Tenuto (full duration)
```

Combine with dynamics:

```forth
ff c4., mf e4>, p g4-,
```

## Lesson 11: Anonymous Blocks

Use `{ ... }` for inline patterns with `*` to repeat:

```forth
{ c4, e4, g4, } 4 *
```

Useful for quick experiments without naming:

```forth
{ c4., r, e4., r, } 8 *
```

## Lesson 12: Conditionals

Use `if`/`else`/`then` with `random`:

```forth
random 50 > if c4, else e4, then
```

In a word:

```forth
: coin random 50 > if c4, else e4, then ;
coin coin coin coin
```

Without else:

```forth
: maybe random 75 < if c4, then ;
maybe maybe maybe maybe
```

## Lesson 13: Control Messages

Program change (instrument):

```forth
1 0 pc          \ Channel 1, program 0 (piano)
1 25 pc         \ Channel 1, program 25 (acoustic guitar)
```

Control change:

```forth
1 7 100 cc      \ Channel 1, CC7 (volume), value 100
1 1 64 cc       \ Channel 1, CC1 (mod wheel), value 64
```

Pitch bend:

```forth
1 8192 pb       \ Center (no bend)
1 16383 pb      \ Full bend up
1 0 pb          \ Full bend down
```

## Lesson 14: Explicit Parameters

Override all defaults with `[ch pitch vel dur]`:

```forth
[1 60 127 1000],        \ Channel 1, C4, max velocity, 1 second
[2 48 64 250],          \ Channel 2, C3, medium velocity, 250ms
```

For chords with explicit params:

```forth
(c4 e4 g4) 1 100 750,   \ Chord on ch1, vel 100, 750ms
```

## Example: Complete Piece

```forth
midi-virtual

\ Define chord progressions
: I   (c4 e4 g4), ;
: IV  (f4 a4 c5), ;
: V   (g4 b4 d5), ;
: vi  (a4 c5 e5), ;

\ Define a melody fragment
: m1 mf c5, e5, g5, e5, ;
: m2 f5, a5, f5, ;

\ Play progression
500 dur!
I I IV IV I I V V

\ Play with dynamics
pp I ff I pp I ff I

\ Generative ending
: ending c5|e5|g5 75%, ;
ending 8 times
I
```

## Lesson 15: Scales

MIDI Forth includes 49 built-in scales. Type `scales` to see them all.

Build a scale from a root pitch:

```forth
c4 scale-major scale .s    \ Pushes: 60 62 64 65 67 69 71 7
```

Get specific scale degrees (1-based):

```forth
c4 scale-major 1 degree .  \ 60 (root)
c4 scale-major 3 degree .  \ 64 (third)
c4 scale-major 5 degree .  \ 67 (fifth)
```

Check if a note belongs to a scale:

```forth
e4 c4 scale-major in-scale? .   \ -1 (true)
c#4 c4 scale-major in-scale? .  \ 0 (false)
```

Quantize to nearest scale tone:

```forth
c#4 c4 scale-major quantize .   \ 60 (snaps to C)
```

Use scales for generative music:

```forth
midi-virtual

\ Random pentatonic melody
: gen-note
    random 127 * 100 /
    c4 scale-pentatonic quantize
    75%,
;
gen-note 16 times
```

See `docs/scales.md` for the complete scale reference.

## Next Steps

- Type `help` in the REPL for the full command reference
- See `docs/syntax.md` for complete syntax documentation
- See `docs/scales.md` for the 49 built-in scales
- Explore the sequence system for non-blocking playback (`seq-new`, `seq-play`)
