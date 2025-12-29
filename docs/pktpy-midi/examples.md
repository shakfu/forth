# pktpy-midi Examples

## Basic Examples

### Hello MIDI

Play a C major scale:

```python
import midi

with midi.open() as m:
    for note in [midi.c4, midi.d4, midi.e4, midi.f4,
                 midi.g4, midi.a4, midi.b4, midi.c5]:
        m.note(note, midi.mf, midi.quarter)
```

### Using Note Names

Play notes using string notation:

```python
import midi

with midi.open() as m:
    m.note("C4")
    m.note("D4")
    m.note("E4")
    m.note("F4")
    m.note("G4", midi.f, midi.half)  # Louder, longer
```

### Chord Progression

Play a I-IV-V-I progression:

```python
import midi

with midi.open() as m:
    # I - C major
    m.chord(midi.major("C4"), midi.mf, midi.half)

    # IV - F major
    m.chord(midi.major("F3"), midi.mf, midi.half)

    # V - G major
    m.chord(midi.major("G3"), midi.mf, midi.half)

    # I - C major
    m.chord(midi.major("C4"), midi.f, midi.whole)
```

### Melody with Dynamics

```python
import midi

with midi.open() as m:
    # Start soft, get louder
    m.note(midi.c4, midi.pp, midi.quarter)
    m.note(midi.d4, midi.p, midi.quarter)
    m.note(midi.e4, midi.mp, midi.quarter)
    m.note(midi.f4, midi.mf, midi.quarter)
    m.note(midi.g4, midi.f, midi.quarter)
    m.note(midi.a4, midi.ff, midi.half)

    # Back down
    m.note(midi.g4, midi.f, midi.quarter)
    m.note(midi.f4, midi.mf, midi.quarter)
    m.note(midi.e4, midi.mp, midi.quarter)
    m.note(midi.d4, midi.p, midi.quarter)
    m.note(midi.c4, midi.pp, midi.whole)
```

---

## Intermediate Examples

### Arpeggiated Chords

```python
import midi

with midi.open() as m:
    # Arpeggiate C major up and down
    m.arpeggio([midi.c4, midi.e4, midi.g4, midi.c5], midi.mf, midi.eighth)
    m.arpeggio([midi.c5, midi.g4, midi.e4, midi.c4], midi.mf, midi.eighth)

    midi.rest(midi.quarter)

    # Arpeggiate A minor
    m.arpeggio(midi.minor("A4"), midi.ff, midi.sixteenth)
    m.arpeggio(list(reversed(midi.minor("A4"))), midi.ff, midi.sixteenth)
```

### Tempo Changes

```python
import midi

# Start slow
midi.set_tempo(60)

with midi.open() as m:
    m.note(midi.c4, midi.mf, midi.quarter)  # 1000ms at 60 BPM
    m.note(midi.e4)
    m.note(midi.g4)

    # Speed up
    midi.set_tempo(120)
    m.note(midi.c5, midi.mf, midi.quarter)  # 500ms at 120 BPM
    m.note(midi.e5)
    m.note(midi.g5)

    # Even faster
    midi.set_tempo(180)
    m.note(midi.c6, midi.mf, midi.quarter)  # ~333ms at 180 BPM
    m.note(midi.e6)
    m.note(midi.g6)
```

### Dotted Rhythms

```python
import midi

with midi.open() as m:
    # Dotted quarter - eighth pattern
    m.note(midi.c4, midi.mf, midi.dotted(midi.quarter))
    m.note(midi.d4, midi.mf, midi.eighth)
    m.note(midi.e4, midi.mf, midi.dotted(midi.quarter))
    m.note(midi.f4, midi.mf, midi.eighth)
    m.note(midi.g4, midi.f, midi.half)
```

### All Chord Types

```python
import midi

with midi.open() as m:
    root = "C4"

    # Triads
    m.chord(midi.major(root), midi.mf, midi.half)
    m.chord(midi.minor(root), midi.mf, midi.half)
    m.chord(midi.dim(root), midi.mf, midi.half)
    m.chord(midi.aug(root), midi.mf, midi.half)

    midi.rest(midi.quarter)

    # Seventh chords
    m.chord(midi.dom7(root), midi.mf, midi.half)
    m.chord(midi.maj7(root), midi.mf, midi.half)
    m.chord(midi.min7(root), midi.mf, midi.half)
```

---

## Scale Examples

### Playing Scales

```python
import midi

with midi.open() as m:
    # Play C major scale
    for pitch in midi.scale(60, "major"):
        m.note(pitch, midi.mf, midi.eighth)

    midi.rest(midi.quarter)

    # Play D dorian scale
    for pitch in midi.scale("D4", "dorian"):
        m.note(pitch, midi.mf, midi.eighth)
```

### Modal Exploration

Play through all diatonic modes starting on C:

```python
import midi

modes = ["major", "dorian", "phrygian", "lydian",
         "mixolydian", "minor", "locrian"]

with midi.open() as m:
    for mode in modes:
        print(mode)
        for pitch in midi.scale(60, mode):
            m.note(pitch, midi.mf, midi.sixteenth)
        midi.rest(midi.quarter)
```

### Using Scale Degrees

Build chords from scale degrees:

```python
import midi

with midi.open() as m:
    root = 60

    # I chord (1, 3, 5)
    m.chord([midi.degree(root, "major", 1),
             midi.degree(root, "major", 3),
             midi.degree(root, "major", 5)], midi.mf, midi.half)

    # IV chord (4, 6, 8)
    m.chord([midi.degree(root, "major", 4),
             midi.degree(root, "major", 6),
             midi.degree(root, "major", 8)], midi.mf, midi.half)

    # V chord (5, 7, 9)
    m.chord([midi.degree(root, "major", 5),
             midi.degree(root, "major", 7),
             midi.degree(root, "major", 9)], midi.mf, midi.half)

    # I chord
    m.chord([midi.degree(root, "major", 1),
             midi.degree(root, "major", 3),
             midi.degree(root, "major", 5)], midi.f, midi.whole)
```

### Scale-Constrained Melody

Quantize random pitches to a scale:

```python
import midi
import random

with midi.open() as m:
    root = 60

    # Generate random pitches and quantize to C pentatonic
    for _ in range(16):
        random_pitch = midi.c4 + random.randint(-12, 12)
        quantized = midi.quantize(random_pitch, root, "pentatonic")
        m.note(quantized, midi.mf, midi.sixteenth)
```

### Blues Scale Improvisation

```python
import midi
import random

midi.set_tempo(100)

with midi.open() as m:
    blues = midi.scale(60, "blues")
    # Add octave above for more range
    full_blues = list(blues) + [p + 12 for p in blues]

    # Random blues licks
    for bar in range(4):
        for beat in range(4):
            note_count = random.randint(1, 3)
            for n in range(note_count):
                pitch = random.choice(full_blues)
                vel = 60 + random.randint(0, 40)
                m.note(pitch, vel, midi.sixteenth)

    # End on the root
    m.note(60, midi.f, midi.whole)
```

### Indian Raga

Play Raga Bhairav:

```python
import midi

with midi.open() as m:
    bhairav = midi.scale(60, "raga_bhairav")

    # Aroha (ascending)
    for pitch in bhairav:
        m.note(pitch, midi.mp, midi.quarter)
    m.note(72, midi.mf, midi.half)  # Octave

    midi.rest(midi.quarter)

    # Avaroha (descending)
    for pitch in reversed(bhairav):
        m.note(pitch, midi.mf, midi.quarter)
```

### Microtonal Maqam with Quarter Tones

Play authentic Maqam Bayati with quarter tones:

```python
import midi

with midi.open() as m:
    root = 60

    # Play scale with pitch bend for quarter tones
    for cents in midi.SCALE_MAQAM_BAYATI_CENTS:
        note, bend = midi.cents_to_note(root, cents)
        m.pitch_bend(bend)
        m.note(note, midi.mf, midi.quarter)

    # Add octave
    m.pitch_bend(0)
    m.note(root + 12, midi.mf, midi.half)
```

### Arabic Maqam

Play Maqam Hijaz (12-TET approximation):

```python
import midi

with midi.open() as m:
    hijaz = midi.scale(62, "maqam_hijaz")  # D is traditional root

    # Ascending
    for pitch in hijaz:
        m.note(pitch, midi.mf, midi.quarter)

    # Add the octave
    m.note(74, midi.mf, midi.half)

    # Descending
    for pitch in reversed(hijaz):
        m.note(pitch, midi.mf, midi.quarter)
```

---

## Advanced Examples

### Control Changes

```python
import midi

with midi.open() as m:
    # Set volume
    m.volume(100)

    # Enable sustain pedal
    m.sustain(True)

    # Play notes with sustain
    m.note(midi.c4, midi.mf, midi.quarter)
    m.note(midi.e4, midi.mf, midi.quarter)
    m.note(midi.g4, midi.mf, midi.quarter)

    # Release sustain
    m.sustain(False)

    midi.rest(midi.quarter)

    # Modulation sweep
    m.note_on(midi.c4)
    for i in range(0, 128, 8):
        m.modulation(i)
        midi.sleep(50)
    for i in range(127, -1, -8):
        m.modulation(i)
        midi.sleep(50)
    m.note_off(midi.c4)
```

### Program Changes

```python
import midi

with midi.open() as m:
    # Piano (program 0)
    m.program_change(0)
    m.note(midi.c4, midi.mf, midi.quarter)
    m.note(midi.e4)
    m.note(midi.g4)

    # Strings (program 48)
    m.program_change(48)
    m.chord(midi.major("C4"), midi.mp, midi.whole)

    # Brass (program 61)
    m.program_change(61)
    m.note(midi.c5, midi.ff, midi.half)
```

### Transpose a Melody

```python
import midi

melody = [midi.c4, midi.d4, midi.e4, midi.f4, midi.g4]

with midi.open() as m:
    # Original
    for note in melody:
        m.note(note, midi.mf, midi.eighth)

    midi.rest(midi.quarter)

    # Up a perfect fifth
    for note in melody:
        m.note(midi.transpose(note, 7), midi.mf, midi.eighth)

    midi.rest(midi.quarter)

    # Down an octave
    for note in melody:
        m.note(midi.octave_down(note), midi.mf, midi.eighth)
```

### Hardware Port Selection

```python
import midi

# List available ports
ports = midi.list_ports()
print(f"Found {len(ports)} MIDI ports:")
for idx, name in ports:
    print(f"  {idx}: {name}")

# Open specific hardware port
if ports:
    with midi.open(0) as m:
        m.note(midi.c4, midi.mf, midi.quarter)
```

### Complex Rhythm Pattern

```python
import midi

midi.set_tempo(100)

with midi.open() as m:
    # Syncopated pattern
    pattern = [
        (midi.c4, midi.eighth),
        (midi.c4, midi.sixteenth),
        (midi.c4, midi.sixteenth),
        (midi.e4, midi.eighth),
        (midi.g4, midi.dotted(midi.quarter)),
        (midi.e4, midi.eighth),
    ]

    # Play pattern 4 times
    for _ in range(4):
        for pitch, duration in pattern:
            m.note(pitch, midi.mf, duration)
```

---

## REPL Session Examples

Start the REPL:

```bash
./build/pktpy_midi
```

### Quick Note Test

```python
>>> import midi
>>> m = midi.open()
>>> m.note("C4")
>>> m.note("E4")
>>> m.note("G4")
>>> m.close()
```

### Interactive Chord Exploration

```python
>>> import midi
>>> m = midi.open()
>>> m.chord(midi.major("C4"))
>>> m.chord(midi.minor("A3"))
>>> m.chord(midi.dom7("G3"))
>>> m.close()
```

### Testing Dynamics

```python
>>> import midi
>>> m = midi.open()
>>> m.note(midi.c4, midi.ppp)  # Very soft
>>> m.note(midi.c4, midi.mf)   # Medium
>>> m.note(midi.c4, midi.fff)  # Very loud
>>> m.close()
```

### Using Context Manager

```python
>>> import midi
>>> with midi.open() as m:
...     m.note("C4")
...     m.chord(midi.major("C4"))
...
>>> # Port automatically closed
```
