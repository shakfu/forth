# Shared Alda Test Suite

This directory contains a language-agnostic test suite for validating Alda implementations.
Each test file is a standalone `.alda` file with a corresponding `.expected` file
describing the expected MIDI output.

## Structure

```
shared_suite/
  README.md              # This file
  01_notes_basic.alda    # Basic note tests
  01_notes_basic.expected # Expected MIDI output
  02_notes_accidentals.alda
  02_notes_accidentals.expected
  ...
```

## Test Format

Each `.alda` file contains:
1. A comment header describing what is being tested
2. Simple, isolated test cases
3. Comments indicating expected behavior

## Expected Output Format (.expected files)

The `.expected` files use a simple line-based format that's easy to parse in any language:

```
# Comments start with #
# Format: TYPE field1 field2 ...

PROGRAM program channel time
TEMPO bpm time
CC control value channel time
NOTE pitch start duration velocity channel
```

Example:
```
# Expected output for 01_notes_basic.alda
PROGRAM 0 0 0.0000
TEMPO 120.0 0.0000
NOTE 60 0.0000 0.4500 80 0
NOTE 62 0.5000 0.4500 80 0
NOTE 64 1.0000 0.4500 80 0
```

### Record Types

| Type | Fields | Description |
|------|--------|-------------|
| `NOTE` | pitch start duration velocity channel | MIDI note event |
| `PROGRAM` | program channel time | Program change |
| `CC` | control value channel time | Control change |
| `TEMPO` | bpm time | Tempo change |

All times are in seconds. Pitch is MIDI note number (0-127).

## MIDI Conventions

- **Tempo**: Default 120 BPM unless specified
- **Velocity**: Default 80 (mf) unless dynamics specified
- **Channel**: 0-15, assigned per part (channel 9 reserved for percussion)
- **Timing**: All times in seconds at specified tempo
- **Duration**: Note duration in seconds (affected by quantization, default 90%)

## Pitch Reference

| Note | MIDI Pitch | Note | MIDI Pitch |
|------|------------|------|------------|
| C4   | 60         | G4   | 67         |
| D4   | 62         | A4   | 69         |
| E4   | 64         | B4   | 71         |
| F4   | 65         | C5   | 72         |

## Duration Reference (at 120 BPM)

| Duration | Beats | Seconds |
|----------|-------|---------|
| 1 (whole) | 4.0 | 2.0 |
| 2 (half) | 2.0 | 1.0 |
| 4 (quarter) | 1.0 | 0.5 |
| 8 (eighth) | 0.5 | 0.25 |
| 16 (sixteenth) | 0.25 | 0.125 |
| 4. (dotted quarter) | 1.5 | 0.75 |

## Parsing .expected Files

### C

```c
FILE *f = fopen("01_notes_basic.expected", "r");
char line[256];
while (fgets(line, sizeof(line), f)) {
    if (line[0] == '#' || line[0] == '\n') continue;

    char type[16];
    if (sscanf(line, "%15s", type) != 1) continue;

    if (strcmp(type, "NOTE") == 0) {
        int pitch, velocity, channel;
        float start, duration;
        sscanf(line, "NOTE %d %f %f %d %d",
               &pitch, &start, &duration, &velocity, &channel);
        // Use note data...
    }
}
```

### Python

```python
for line in open("01_notes_basic.expected"):
    line = line.strip()
    if not line or line.startswith("#"):
        continue
    parts = line.split()
    if parts[0] == "NOTE":
        pitch, start, duration, velocity, channel = (
            int(parts[1]), float(parts[2]), float(parts[3]),
            int(parts[4]), int(parts[5])
        )
```

## Test Categories

### 1. Notes (01-03)
- Basic pitches (c, d, e, f, g, a, b)
- Accidentals (+, -, _, ++, --)
- Durations (1, 2, 4, 8, 16, 32)
- Dotted notes (4., 4..)
- Millisecond/second durations (500ms, 2s)

### 2. Octaves (04)
- Octave set (o0 through o9)
- Octave up (>)
- Octave down (<)
- Default octave (o4)

### 3. Rests (05)
- Basic rests with durations
- Rests don't produce MIDI notes

### 4. Chords (06)
- Simultaneous notes (c/e/g)
- Chord with duration (c1/e/g)
- Chord with octave changes (c/e/g/>c)

### 5. Ties (07)
- Duration ties (c1~1 = 2 whole notes duration)
- Note ties/slurs (c~d~e)

### 6. Tempo (08)
- Tempo attribute (tempo 120)
- Global tempo (tempo! 120)
- Tempo changes mid-score

### 7. Volume (09)
- Volume attribute (vol 80)
- Volume changes

### 8. Dynamics (10)
- pp, p, mp, mf, f, ff
- ppp, fff (extended)

### 9. Parts (11)
- Single instrument (piano:)
- Part alias (piano "p1":)
- Multiple instruments (violin/viola:)
- Channel assignment

### 10. Variables (12)
- Definition (theme = c d e)
- Reference (piano: theme)
- No sound on definition

### 11. Markers (13)
- Marker definition (%verse)
- Marker jump (@verse)
- Multi-part synchronization

### 12. Voices (14)
- Voice declaration (V1:, V2:)
- Voice merge (V0:)
- Parallel voices

### 13. Repeats (15)
- Simple repeat (c*4)
- Bracketed repeat ([c d e]*2)
- On-repetitions ([c d'1 e'2]*2)

### 14. Cram/Tuplets (16)
- Triplets ({c d e}4)
- Quintuplets ({c d e f g}2)
- Nested cram

### 15. Key Signature (17)
- Major keys (key-sig '(c major))
- Minor keys (key-sig '(a minor))
- Accidental override with natural (_)

### 16. Transposition (18)
- Transpose up (transpose 5)
- Transpose down (transpose -7)
- Transpose reset (transpose 0)

### 17. Quantization (19)
- Quantization/legato (quant 90)
- Staccato (quant 50)

### 18. Panning (20)
- Pan left (panning 0)
- Pan center (panning 50)
- Pan right (panning 100)
