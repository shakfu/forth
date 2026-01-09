# alda-midi TODO

## Unimplemented Features

Features listed by priority, based on frequency of use in real Alda scores.

### Priority 1: High Impact

(All high priority features implemented)

---

### Priority 2: Medium Impact

#### On-Repetitions (ON_REPS)
**Why:** Enables alternate endings and variations in repeated sections. Common in structured music.

**Syntax:**
```alda
[ c d e f [g4]'1-3 [g a b > c]'4 ]*4
```

**Implementation notes:**
- Parser creates `ALDA_NODE_ON_REPS` with repetition spec string
- Need to parse the spec: `'1,3` means "on repeats 1 and 3", `'1-3` means "on repeats 1 through 3"
- During repeat execution, track current iteration count
- Only visit ON_REPS children when current iteration matches the spec

**Estimated complexity:** Medium

---

#### Markers (MARKER, AT_MARKER)
**Why:** Enables synchronization between parts in multi-instrument scores.

**Syntax:**
```alda
guitar: r1 %verse
piano: @verse c d e f
```

**Implementation notes:**
- Parser creates `ALDA_NODE_MARKER` and `ALDA_NODE_AT_MARKER` nodes
- Need a marker table in `AldaContext`: name -> tick position
- On MARKER: store current tick position with the marker name
- On AT_MARKER: set current tick position to the stored value
- Error if AT_MARKER references undefined marker

**Estimated complexity:** Low-Medium

---

### Priority 3: Low Impact

#### Key Signature
**Why:** Convenience feature - accidentals can be written explicitly.

**Syntax:**
```alda
(key-sig "f+ c+")   # F# and C# in key signature
(key-sig '(f :sharp c :sharp))
```

**Implementation notes:**
- Store active accidentals in part state
- When parsing note letters, apply key signature accidentals unless explicitly overridden
- Would require changes to note interpretation, not just a new visitor

**Estimated complexity:** Medium (touches note parsing)

---

#### Transposition
**Why:** Convenience feature - can transpose manually or in post-processing.

**Syntax:**
```alda
(transpose 2)   # transpose up 2 semitones
```

**Implementation notes:**
- Store transposition offset in part state
- Add offset to all note pitches during scheduling

**Estimated complexity:** Low

---

## Implemented Features

- Notes, rests, chords (`c/e/g`)
- Durations (`c4`, `c8.`, ties `c4~4`)
- Octaves (`o4`, `>`, `<`)
- Dynamics (`(ppp)` through `(fff)`)
- Tempo (`(tempo 120)`)
- Volume/Quantization (`(volume 80)`, `(quant 90)`)
- Panning (`(panning 64)`)
- Parts/Instruments (128 GM instruments with aliases)
- Voices (`V1:`, `V2:`, `V0:`)
- Simple repeats (`[c d e]*4`)
- Barlines (`|`) - visual only
- Comments (`# ...`)
- Program changes (instrument switching)
- Part aliases (`piano "melody":`)
- Variables (`motif = c d e f`, `piano: motif`)
- Cram expressions (`{c d e f g}2` - fit notes into duration)

## Testing Notes

When implementing new features, test with these example files:
- `variables.alda` - Variable definitions
- `cram.alda` - Cram expressions
- `alternate-endings.alda` - On-repetitions
- `markers.alda` - Marker synchronization
- `bach-prelude.alda` - Complex cram usage
- `nicechord-alda-demo.alda` - Mix of features
