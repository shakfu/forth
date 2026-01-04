# Alda Examples

This document showcases examples from the `examples/` directory.

## Getting Started

### Hello World

The simplest Alda program:

```alda
piano: c8 d e f g f e d c2.
```

Run with:

```bash
./build/alda_midi docs/alda-midi/examples/hello_world.alda
```

### Twinkle Twinkle Little Star

A familiar melody with tempo and octave settings:

```alda
piano:
  (tempo 100)
  o4
  c4 c g g | a a g2 |
  f4 f e e | d d c2 |
  g4 g f f | e e d2 |
  g4 g f f | e e d2 |
  c4 c g g | a a g2 |
  f4 f e e | d d c2
```

---

## Chords

### Basic Chord Progression

I-V-vi-IV progression in C major:

```alda
piano:
  (tempo 80)
  o3
  # C major
  c1/e/g |
  # G major
  g/b/>d |
  # A minor
  <a/>c/e |
  # F major
  <f/a/>c |
  # Resolve to C
  <c/e/g
```

Chords use `/` to combine notes at the same time.

---

## Dynamics

### Crescendo and Decrescendo

Dynamic markings from pianissimo to fortissimo:

```alda
piano:
  c8
  (ppp) d
  (pp) e
  (p) f
  (mp) g
  (mf) a
  (f) b
  (ff) > c
  (fff) < b a g f e d c4
```

Available dynamics: `ppp`, `pp`, `p`, `mp`, `mf`, `f`, `ff`, `fff`

---

## Multiple Instruments

### Duet

Violin and cello playing together:

```alda
(tempo! 100)

violin:
  o4 (mf)
  g4 a b > c | d2 e | d4 c < b a | g1 |
  g4 a b > c | d2 e | f4 e d c | < b1

cello:
  o2 (mf)
  g2 d | g2 c | d2 g | g1 |
  g2 d | g2 c | d2 d | g1
```

### Orchestra

Full orchestral arrangement with strings, winds, and brass:

```alda
(tempo! 90)

violin:
  o4 (mf)
  g2 a | b2 > c | d2 c | < b1

viola:
  o3 (mp)
  d2 f | g2 a | b2 a | g1

cello:
  o2 (mp)
  g2 d | g2 e | g2 f | g1

flute:
  o5 (p)
  r1 | r | d2 e | d1

french-horn:
  o3 (p)
  r1 | r | r | (mf) g1
```

Each instrument is assigned its own MIDI channel automatically.

---

## Voices (Polyphony)

### Three-Voice Polyphony

Independent melodic lines within a single instrument:

```alda
piano:
  (tempo 80)
  o4

  V1: c1 | d | e | f
  V2: e1 | f | g | a
  V3: g1 | a | b | > c
  V0:

  # After V0, all voices merge
  < c1/e/g
```

- `V1:`, `V2:`, etc. declare independent voices
- Each voice has its own timing
- `V0:` merges all voices, continuing from the latest position

---

## Repeats

### Repeat Patterns

```alda
piano:
  (tempo 140)
  o4

  # Repeat a single note
  c8*4 d*4 e*4 f*4 |

  # Repeat a sequence
  [c8 d e]*4 |

  # Repeat with octave changes
  [c4 e g > c <]*2
```

---

## Complete Example List

The `examples/` directory contains 40 Alda files:

### Basic

| File | Description |
|------|-------------|
| `hello_world.alda` | Simple scale |
| `simple.alda` | Basic note sequences |
| `twinkle.alda` | Twinkle Twinkle Little Star |

### Notation Features

| File | Description |
|------|-------------|
| `chords.alda` | Chord notation with `/` |
| `dynamics.alda` | Dynamic markings (ppp-fff) |
| `repeats.alda` | Repeat syntax |
| `voices.alda` | Polyphonic voices (V1, V2, V0) |
| `panning.alda` | Stereo panning |
| `track-volume.alda` | Volume control |

### Multiple Instruments

| File | Description |
|------|-------------|
| `duet.alda` | Violin and cello duet |
| `orchestra.alda` | Full orchestral arrangement |
| `all-instruments.alda` | All 128 GM instruments |
| `percussion.alda` | Drum patterns |

### Classical Pieces

| File | Description |
|------|-------------|
| `bach_cello_suite_no_1.alda` | Bach Cello Suite No. 1 |
| `bach-prelude.alda` | Bach prelude |
| `debussy_quartet.alda` | Debussy string quartet excerpt |
| `rachmaninoff_piano_concerto_2_mvmt_2.alda` | Rachmaninoff excerpt |
| `jimenez-divertimento.alda` | Jimenez divertimento |

### Advanced Features

| File | Description |
|------|-------------|
| `poly.alda` | Polyphonic writing |
| `multi-poly.alda` | Multi-voice polyphony |
| `modes.alda` | Musical modes |
| `phase.alda` | Phase music patterns |
| `variables.alda` | Variable definitions |
| `variables-2.alda` | More variable examples |
| `markers.alda` | Marker definitions |
| `cram.alda` | Cram expressions |
| `nesting.alda` | Nested structures |

### MIDI Features

| File | Description |
|------|-------------|
| `midi-channel-management.alda` | Channel assignment |
| `midi-channel-management-2.alda` | Advanced channel control |

---

## Running Examples

Play any example file:

```bash
./build/alda_midi docs/alda-midi/examples/FILE.alda
```

With verbose output:

```bash
./build/alda_midi -v docs/alda-midi/examples/orchestra.alda
```

In the REPL, copy and paste example code directly:

```bash
./build/alda_midi
alda> piano: c d e f g
```
