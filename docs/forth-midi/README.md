# forth-midi

A Forth-like MIDI interpreter with concise musical notation for generating and transforming MIDI sequences.

## Features

- Concise pitch notation: `c4,` plays middle C
- Chord syntax: `(c4 e4 g4),` plays a chord
- Dynamics: `ppp` to `fff`
- Articulation: staccato `.`, accent `>`, tenuto `-`
- Word definitions: `: melody c4, e4, g4, ;`
- Generative: probability `75%`, alternatives `c4|e4|g4`
- 49 built-in scales with `scale`, `degree`, `quantize`
- Sequences for non-blocking playback
- Recording and MIDI capture
- Virtual and hardware MIDI port support

## Quick Start

```bash
./build/forth_midi
```

```forth
midi-open            \ Create virtual MIDI port
c4, e4, g4,             \ Play C, E, G sequentially
(c4 e4 g4),             \ Play C major chord
: cmaj (c4 e4 g4), ;    \ Define a word
cmaj 4 times            \ Play it 4 times
midi-close
```

Type `help` for command reference, `quit` to exit.

## Documentation

- [Tutorial](tutorial.md) - Step-by-step introduction
- [Syntax Reference](syntax.md) - Complete syntax documentation
- [Scales](scales.md) - 49 built-in scales and scale operations
- [Sequences](sequences.md) - Non-blocking sequence playback
- [Data Structures](data_structures.md) - Packed notes, sequences
- [New Features](new_features.md) - Recent additions

## Example: Generative Pattern

```forth
midi-open
200 dur!

\ Random note from pentatonic scale, 75% chance to play
: gen-note
    random 127 * 100 /
    c4 scale-pentatonic quantize
    75%,
;

gen-note 16 times
midi-close
```

## Example: Chord Progression with Sequences

```forth
midi-open
seq-new drop

c4 major 100 half 0 3 chord>seq
f4 major 100 half 960 3 chord>seq
g4 major 100 half 1920 3 chord>seq
c4 major 100 whole 2880 3 chord>seq

seq-play
midi-close
```
