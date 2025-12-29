# Scales in MIDI Forth

MIDI Forth includes 49 built-in scales from the common music theory library, plus functions to build scales, get scale degrees, check scale membership, and quantize pitches.

## Scale Constants

Scale constants are words that push a scale ID onto the stack. Use them with the `scale`, `degree`, `in-scale?`, and `quantize` words.

### Listing All Scales

```forth
scales
```

Outputs all 49 available scales with their IDs.

### Diatonic Modes

| Word | Scale |
|------|-------|
| `scale-major` | Major (Ionian) |
| `scale-dorian` | Dorian mode |
| `scale-phrygian` | Phrygian mode |
| `scale-lydian` | Lydian mode |
| `scale-mixolydian` | Mixolydian mode |
| `scale-minor` | Natural minor (Aeolian) |
| `scale-locrian` | Locrian mode |
| `scale-harmonic-minor` | Harmonic minor |
| `scale-melodic-minor` | Melodic minor (ascending) |

### Pentatonic & Blues

| Word | Scale |
|------|-------|
| `scale-pentatonic` | Major pentatonic |
| `scale-pentatonic-minor` | Minor pentatonic |
| `scale-blues` | Blues scale |

### Symmetric Scales

| Word | Scale |
|------|-------|
| `scale-whole-tone` | Whole tone |
| `scale-chromatic` | Chromatic (all 12 notes) |
| `scale-diminished-hw` | Diminished (half-whole) |
| `scale-diminished-wh` | Diminished (whole-half) |
| `scale-augmented` | Augmented scale |

### Bebop Scales

| Word | Scale |
|------|-------|
| `scale-bebop-dominant` | Bebop dominant |
| `scale-bebop-major` | Bebop major |
| `scale-bebop-minor` | Bebop minor |

### World Scales

| Word | Scale |
|------|-------|
| `scale-hungarian-minor` | Hungarian minor |
| `scale-double-harmonic` | Byzantine/Arabic |
| `scale-neapolitan-major` | Neapolitan major |
| `scale-neapolitan-minor` | Neapolitan minor |
| `scale-phrygian-dominant` | Spanish/Jewish |
| `scale-persian` | Persian |
| `scale-altered` | Super Locrian |
| `scale-hirajoshi` | Japanese Hirajoshi |
| `scale-in-sen` | Japanese In-Sen |
| `scale-iwato` | Japanese Iwato |
| `scale-kumoi` | Japanese Kumoi |
| `scale-egyptian` | Egyptian |
| `scale-romanian-minor` | Romanian minor |
| `scale-spanish-8-tone` | Spanish 8-tone |
| `scale-enigmatic` | Enigmatic |

### Arabic Maqamat (12-TET)

| Word | Scale |
|------|-------|
| `scale-maqam-hijaz` | Maqam Hijaz |
| `scale-maqam-nahawand` | Maqam Nahawand |
| `scale-maqam-nikriz` | Maqam Nikriz |
| `scale-maqam-athar-kurd` | Maqam Athar Kurd |
| `scale-maqam-shawq-afza` | Maqam Shawq Afza |
| `scale-maqam-jiharkah` | Maqam Jiharkah |

### Indian Ragas (12-TET)

| Word | Scale |
|------|-------|
| `scale-raga-bhairav` | Raga Bhairav |
| `scale-raga-todi` | Raga Todi |
| `scale-raga-marwa` | Raga Marwa |
| `scale-raga-purvi` | Raga Purvi |
| `scale-raga-charukeshi` | Raga Charukeshi |
| `scale-raga-khamaj` | Raga Khamaj |
| `scale-raga-bhimpalasi` | Raga Bhimpalasi |
| `scale-raga-darbari` | Raga Darbari |

## Scale Operations

### scale

```forth
( root scale-id -- p1 p2 ... pN N )
```

Build a scale from root pitch and scale ID. Pushes all scale pitches followed by the count.

```forth
c4 scale-major scale .s
\ Output: <8> 60 62 64 65 67 69 71 7
```

The example shows C major scale: C4(60), D4(62), E4(64), F4(65), G4(67), A4(69), B4(71), and count 7.

### degree

```forth
( root scale-id degree -- pitch )
```

Get a specific scale degree (1-based). Supports extended degrees beyond the octave.

```forth
c4 scale-major 1 degree .    \ 60 (C4 - root)
c4 scale-major 3 degree .    \ 64 (E4 - third)
c4 scale-major 5 degree .    \ 67 (G4 - fifth)
c4 scale-major 9 degree .    \ 74 (D5 - ninth)
```

### in-scale?

```forth
( pitch root scale-id -- flag )
```

Check if a pitch belongs to a scale (in any octave). Returns -1 (true) or 0 (false).

```forth
e4 c4 scale-major in-scale? .    \ -1 (E is in C major)
c#4 c4 scale-major in-scale? .   \ 0 (C# is not in C major)
```

### quantize

```forth
( pitch root scale-id -- quantized-pitch )
```

Snap a pitch to the nearest note in the scale.

```forth
c#4 c4 scale-major quantize .    \ 60 (C# snaps to C)
f#4 c4 scale-major quantize .    \ 67 (F# snaps to G)
```

## Microtonal Support

### cents>bend

```forth
( cents -- bend )
```

Convert a cents offset to a MIDI pitch bend value. Center is 8192, range is 0-16383.

```forth
0 cents>bend .      \ 8192 (center)
50 cents>bend .     \ 10240 (quarter-tone up)
-50 cents>bend .    \ 6144 (quarter-tone down)
```

### pb-cents

```forth
( cents channel -- )
```

Send a pitch bend in cents on a specific channel.

```forth
50 1 pb-cents       \ Quarter-tone up on channel 1
0 1 pb-cents        \ Reset to center
```

## Examples

### Play a Scale

```forth
midi-open

\ Play C major scale ascending
: play-scale
    c4 scale-major scale   \ Build scale, get pitches + count
    drop                   \ Drop count
    ( now 7 pitches on stack )
    ,,,,,,,                \ Play all 7
;

play-scale
```

### Build Triads from Scale Degrees

```forth
midi-open

\ I chord (1-3-5)
: I-chord
    c4 scale-major 1 degree
    c4 scale-major 3 degree
    c4 scale-major 5 degree
    ( now 3 pitches on stack )
;

\ IV chord (4-6-8)
: IV-chord
    c4 scale-major 4 degree
    c4 scale-major 6 degree
    c4 scale-major 8 degree
;

\ V chord (5-7-9)
: V-chord
    c4 scale-major 5 degree
    c4 scale-major 7 degree
    c4 scale-major 9 degree
;

\ Play progression
(I-chord), (IV-chord), (V-chord), (I-chord),
```

### Quantize Random Notes

```forth
midi-open

\ Generate random note and quantize to scale
: random-note
    random 127 * 100 /     \ Random 0-127
    c4 scale-pentatonic quantize
    ,
;

random-note 16 times
```

### Modal Exploration

```forth
midi-open

\ Play same notes through different modes
: play-mode ( scale-id -- )
    c4 swap scale
    drop ,,,,,,,
;

scale-major play-mode
500 ms
scale-dorian play-mode
500 ms
scale-phrygian play-mode
```

### Blues Improvisation

```forth
midi-open

\ Random blues phrase
: blues-note
    random 127 * 100 /
    c4 scale-blues quantize
    75%,
;

: blues-phrase blues-note 4 times r, ;

blues-phrase 8 times
```
