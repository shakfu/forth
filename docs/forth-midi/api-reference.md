# API Reference

## MIDI Setup

### Port Management

| Word | Stack | Description |
|------|-------|-------------|
| `midi-list` | `( -- )` | List available MIDI output ports |
| `midi-apis` | `( -- )` | List available MIDI APIs |
| `midi-open` | `( -- )` | Create virtual port "ForthMIDI" |
| `midi-open-as` | `name ( -- )` | Create virtual port with custom name |
| `midi-open-port` | `( n -- )` | Open hardware port by index |
| `midi-close` | `( -- )` | Close current MIDI port |
| `panic` | `( -- )` | All notes off (emergency stop) |

```forth
midi-list               \ See available ports
midi-open               \ Create virtual port
midi-open-as MySynth    \ Named virtual port
0 midi-open-port        \ Open first hardware port
midi-close              \ Close when done
```

---

## Note Notation

### Pitch Names

Format: `[note][accidental][octave]`

| Component | Values |
|-----------|--------|
| Note | `c`, `d`, `e`, `f`, `g`, `a`, `b` (case insensitive) |
| Accidental | `#` (sharp), `b` (flat), or omitted (natural) |
| Octave | `0` - `9` (middle C = C4 = MIDI 60) |

```forth
c4      \ 60 (middle C)
a4      \ 69 (A440)
c#4     \ 61 (C sharp)
db4     \ 61 (D flat, enharmonic)
```

MIDI numbers (0-127) work directly: `60` = middle C.

### Playing Notes

The comma `,` triggers playback:

```forth
c4,                     \ Play C4 with defaults
60,                     \ Same note using MIDI number
c4, d4, e4,             \ Sequential notes
```

### Chords

Parentheses group notes for simultaneous playback:

```forth
(c4 e4 g4),             \ C major chord
(c4 eb4 g4),            \ C minor chord
(60 64 67),             \ Using MIDI numbers
```

### Explicit Parameters

Override channel, velocity, and duration:

```forth
\ Single note: ch pitch vel dur
1 c4 100 500,           \ Channel 1, C4, velocity 100, 500ms

\ Chord with params: pitches then ch vel dur
(c4 e4 g4) 1 80 500,    \ C major, ch 1, vel 80, 500ms
```

### Bracket Sequences

Use `[ ]` to create sequences as first-class values:

```forth
[ c4 e4 g4 ],           \ Create and play sequence
[ c4 e4 g4 ]            \ Create sequence (on stack, not played)
dup,                    \ Play it
shuffle,                \ Shuffle and play

\ Sequences can contain:
[ c4 e4 g4 ]            \ Pitches
[ (c4 e4 g4) (f4 a4) ]  \ Chords
[ c4 r e4 r g4 ]        \ Rests
[ mf c4 ff e4 p g4 ]    \ Dynamics
[ quarter c4 eighth e4 ]\ Durations
[ 1 2 3 4 5 ]           \ Plain numbers (for generative ops)
```

Sequences support generative operations like `shuffle`, `reverse`, `pick`, and more. See [Generative Music](#generative-music).

---

## Context Variables

Set defaults to avoid repetition:

| Word | Stack | Description |
|------|-------|-------------|
| `ch!` | `( n -- )` | Set default channel (1-16) |
| `vel!` | `( n -- )` | Set default velocity (0-127) |
| `dur!` | `( n -- )` | Set default duration (ms) |
| `gate!` | `( n -- )` | Set default gate (1-100, percentage of duration to sound) |
| `ch@` | `( -- n )` | Get current channel |
| `vel@` | `( -- n )` | Get current velocity |
| `dur@` | `( -- n )` | Get current duration |
| `gate@` | `( -- n )` | Get current gate |

```forth
2 ch!                   \ Default to channel 2
100 vel!                \ Default velocity 100
250 dur!                \ Default duration 250ms
50 gate!                \ 50% gate (staccato-like)

c4,                     \ Uses ch=2, vel=100, dur=250, gate=50
```

### Named Parameter Syntax

Use `=` for one-shot (next note only) or `:=` for persistent (change default):

| Syntax | Effect |
|--------|--------|
| `vel=100` | Set velocity for next note only |
| `vel:=100` | Change default velocity |
| `ch=2` | Set channel for next note only |
| `ch:=2` | Change default channel |
| `dur=500` | Set duration for next note only |
| `dur:=500` | Change default duration |
| `gate=80` | Set gate for next note only |
| `gate:=80` | Change default gate |
| `bpm:=120` | Change tempo (persistent only) |
| `prog:=25` | Send program change (persistent only) |
| `pan:=64` | Send pan CC (persistent only) |

```forth
vel=100 c4,             \ C4 at velocity 100, then back to default
vel:=100 c4, e4, g4,    \ All notes at velocity 100
ch=2 dur=250 e4,        \ E4 on channel 2, 250ms duration
```

---

## Dynamics

Velocity presets using standard musical terms:

| Word | Velocity | Dynamic |
|------|----------|---------|
| `ppp` | 16 | Pianississimo |
| `pp` | 32 | Pianissimo |
| `p` | 48 | Piano |
| `mp` | 64 | Mezzo-piano |
| `mf` | 80 | Mezzo-forte (default) |
| `f` | 96 | Forte |
| `ff` | 112 | Fortissimo |
| `fff` | 127 | Fortississimo |

```forth
mf c4, e4, ff g4,       \ Dynamics change mid-phrase
pp (c4 e4 g4),          \ Soft chord
```

---

## Articulation

Articulation suffixes modify note playback:

| Suffix | Effect | Description |
|--------|--------|-------------|
| `.` | Staccato | 50% duration |
| `>` | Accent | +20 velocity |
| `-` | Tenuto | Full duration |

```forth
c4.,                    \ Staccato C4
c4>,                    \ Accented C4
c4-,                    \ Tenuto C4
mf c4., ff e4>,         \ Combined with dynamics
```

---

## Rests

| Word | Stack | Description |
|------|-------|-------------|
| `r` | `( -- )` | Rest marker (silence for default duration) |

```forth
c4, r, e4, r, g4,       \ Notes with rests between
r 250,                  \ Rest with explicit duration (ms)
```

---

## Stack Operations

| Word | Stack | Description |
|------|-------|-------------|
| `dup` | `( a -- a a )` | Duplicate top |
| `drop` | `( a -- )` | Discard top |
| `swap` | `( a b -- b a )` | Swap top two |
| `over` | `( a b -- a b a )` | Copy second to top |
| `rot` | `( a b c -- b c a )` | Rotate top three |
| `clear` | `( ... -- )` | Clear entire stack |
| `.s` | `( -- )` | Show stack contents |

---

## Arithmetic

| Word | Stack | Description |
|------|-------|-------------|
| `+` | `( a b -- a+b )` | Addition |
| `-` | `( a b -- a-b )` | Subtraction |
| `*` | `( a b -- a*b )` | Multiplication (also block repeat) |
| `/` | `( a b -- a/b )` | Division |

---

## Bitwise Operations

| Word | Stack | Description |
|------|-------|-------------|
| `and` | `( a b -- a&b )` | Bitwise AND |
| `or` | `( a b -- a\|b )` | Bitwise OR |
| `xor` | `( a b -- a^b )` | Bitwise XOR |
| `not` | `( a -- ~a )` | Bitwise NOT |

---

## Comparison

| Word | Stack | Description |
|------|-------|-------------|
| `=` | `( a b -- flag )` | Equal (-1 true, 0 false) |
| `<` | `( a b -- flag )` | Less than |
| `>` | `( a b -- flag )` | Greater than |

---

## Output

| Word | Stack | Description |
|------|-------|-------------|
| `.` | `( n -- )` | Print number |
| `cr` | `( -- )` | Print newline |
| `space` | `( -- )` | Print space |

---

## Timing & Tempo

| Word | Stack | Description |
|------|-------|-------------|
| `ms` | `( n -- )` | Sleep for n milliseconds |
| `bpm!` | `( n -- )` | Set tempo (20-300 BPM) |
| `bpm@` | `( -- n )` | Get current tempo |

### Duration Constants

Based on 480 ticks per quarter note:

| Word | Ticks | Description |
|------|-------|-------------|
| `quarter` | 480 | Quarter note |
| `half` | 960 | Half note |
| `whole` | 1920 | Whole note |
| `eighth` | 240 | Eighth note |
| `sixteenth` | 120 | Sixteenth note |

```forth
120 bpm!                \ Set tempo to 120 BPM
500 ms                  \ Sleep 500 milliseconds
```

---

## Control Messages

### Control Change

| Word | Stack | Description |
|------|-------|-------------|
| `cc` | `( ch cc val -- )` | Send Control Change |

Common CC numbers:
- 1: Modulation wheel
- 7: Channel volume
- 10: Pan
- 11: Expression
- 64: Sustain pedal
- 91: Reverb
- 93: Chorus

```forth
1 64 127 cc             \ Sustain pedal on
1 64 0 cc               \ Sustain pedal off
1 1 64 cc               \ Mod wheel to 64
```

### Program Change

| Word | Stack | Description |
|------|-------|-------------|
| `pc` | `( ch program -- )` | Send Program Change |

```forth
1 0 pc                  \ Channel 1, program 0 (Piano)
1 25 pc                 \ Channel 1, program 25 (Guitar)
```

### Pitch Bend

| Word | Stack | Description |
|------|-------|-------------|
| `pb` | `( ch bend -- )` | Send pitch bend (0-16383, center=8192) |

```forth
1 8192 pb               \ Center (no bend)
1 0 pb                  \ Full bend down
1 16383 pb              \ Full bend up
```

---

## Chord Builders

All take a root pitch and push the chord notes onto the stack:

| Word | Stack | Description |
|------|-------|-------------|
| `major` | `( root -- p1 p2 p3 )` | Major triad |
| `minor` | `( root -- p1 p2 p3 )` | Minor triad |
| `dim` | `( root -- p1 p2 p3 )` | Diminished triad |
| `aug` | `( root -- p1 p2 p3 )` | Augmented triad |
| `dom7` | `( root -- p1 p2 p3 p4 )` | Dominant 7th |
| `maj7` | `( root -- p1 p2 p3 p4 )` | Major 7th |
| `min7` | `( root -- p1 p2 p3 p4 )` | Minor 7th |

```forth
c4 major                \ Pushes 60 64 67 (C E G)
a3 minor                \ Pushes 57 60 64 (A C E)
g4 dom7                 \ Pushes 67 71 74 77 (G B D F)
```

### Playing Chords

| Word | Stack | Description |
|------|-------|-------------|
| `play-chord` | `( p1...pN N vel dur -- )` | Play N notes as chord |

```forth
c4 major 3 80 500 play-chord    \ Play C major, 3 notes, vel 80, 500ms
```

---

## Octave Shifts

| Word | Stack | Description |
|------|-------|-------------|
| `^` | `( -- pitch )` | Push current pitch up one octave |
| `v` | `( -- pitch )` | Push current pitch down one octave |

```forth
c4, ^,                  \ C4 then C5
c4, v,                  \ C4 then C3
```

---

## Relative Intervals

Use `+N` or `-N` to move by semitones from last pitch:

```forth
c4, +2, +2, +1,         \ C D E F (whole, whole, half)
c4, +7, +5,             \ C G C (up fifth, up fourth)
g4, -2, -2,             \ G F Eb (down)
```

---

## Transpose

| Word | Stack | Description |
|------|-------|-------------|
| `transpose` | `( pitch semitones -- new-pitch )` | Transpose a pitch |

```forth
c4 7 transpose .        \ 67 (G4)
```

---

## Packed Notes

Encode pitch, velocity, channel, and duration in a single 32-bit value:

| Word | Stack | Description |
|------|-------|-------------|
| `note` | `( pitch vel ch dur -- packed )` | Create packed note |
| `pitch@` | `( packed -- pitch )` | Extract pitch |
| `vel@` | `( packed -- vel )` | Extract velocity |
| `ch@` | `( packed -- ch )` | Extract channel |
| `dur@` | `( packed -- dur )` | Extract duration |
| `note.` | `( packed -- )` | Print packed note |
| `note!` | `( packed -- )` | Play packed note |

```forth
60 80 1 quarter note    \ Create packed C4
dup pitch@ .            \ Print pitch (60)
note!                   \ Play it
```

---

## Scale Constants

49 built-in scales. Use `scales` to list all.

### Diatonic Modes

| Word | Scale |
|------|-------|
| `scale-major` | Major (Ionian) |
| `scale-dorian` | Dorian |
| `scale-phrygian` | Phrygian |
| `scale-lydian` | Lydian |
| `scale-mixolydian` | Mixolydian |
| `scale-minor` | Natural minor (Aeolian) |
| `scale-locrian` | Locrian |
| `scale-harmonic-minor` | Harmonic minor |
| `scale-melodic-minor` | Melodic minor |

### Pentatonic & Blues

| Word | Scale |
|------|-------|
| `scale-pentatonic` | Major pentatonic |
| `scale-pentatonic-minor` | Minor pentatonic |
| `scale-blues` | Blues |

### Symmetric Scales

| Word | Scale |
|------|-------|
| `scale-whole-tone` | Whole tone |
| `scale-chromatic` | Chromatic |
| `scale-diminished-hw` | Diminished half-whole |
| `scale-diminished-wh` | Diminished whole-half |
| `scale-augmented` | Augmented |

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

---

## Scale Operations

| Word | Stack | Description |
|------|-------|-------------|
| `scale` | `( root scale-id -- p1...pN N )` | Build scale, push pitches and count |
| `degree` | `( root scale-id degree -- pitch )` | Get specific scale degree (1-based) |
| `in-scale?` | `( pitch root scale-id -- flag )` | Check if pitch is in scale |
| `quantize` | `( pitch root scale-id -- quantized )` | Snap pitch to nearest scale tone |
| `scales` | `( -- )` | List all available scales |

```forth
c4 scale-major scale .s     \ Build C major: 60 62 64 65 67 69 71 7
c4 scale-major 3 degree .   \ 64 (E4, third)
e4 c4 scale-major in-scale? .   \ -1 (true)
c#4 c4 scale-major quantize .   \ 60 or 62 (snapped)
```

---

## Microtonal Support

| Word | Stack | Description |
|------|-------|-------------|
| `cents>bend` | `( cents -- bend )` | Convert cents to pitch bend value |
| `pb-cents` | `( cents ch -- )` | Send pitch bend in cents |

```forth
0 cents>bend .          \ 8192 (center)
50 cents>bend .         \ Quarter-tone up
50 1 pb-cents           \ Quarter-tone up on channel 1
0 1 pb-cents            \ Reset bend
```

---

## Sequences

Non-blocking MIDI patterns with timing.

### Sequence Management

| Word | Stack | Description |
|------|-------|-------------|
| `seq-new` | `( -- id )` | Create new sequence, returns id |
| `seq` | `( id -- )` | Select sequence by id |
| `seq@` | `( -- id )` | Get current sequence id |
| `seq-clear` | `( -- )` | Clear all events |
| `seq-length` | `( -- n )` | Get event count |

### Adding Events

| Word | Stack | Description |
|------|-------|-------------|
| `seq-note` | `( time pitch vel dur -- )` | Add note (default channel) |
| `seq-note-ch` | `( time ch pitch vel dur -- )` | Add note with channel |
| `seq-add` | `( packed time -- )` | Add packed note |

### Playback & Display

| Word | Stack | Description |
|------|-------|-------------|
| `seq-play` | `( -- )` | Play sequence (blocking) |
| `seq-show` | `( -- )` | Print all events |

### Transformations

| Word | Stack | Description |
|------|-------|-------------|
| `seq-transpose` | `( semitones -- )` | Transpose all notes |
| `seq-reverse` | `( -- )` | Reverse timing |
| `seq-stretch` | `( factor -- )` | Scale timing (100=normal) |

### Chord/Arp to Sequence

| Word | Stack | Description |
|------|-------|-------------|
| `chord>seq` | `( p1...pN vel dur time N -- )` | Add N-note chord at time |
| `arp>seq` | `( p1...pN vel note-dur spacing start N -- )` | Add arpeggio |

```forth
seq-new drop
0 60 80 quarter seq-note        \ C4 at tick 0
quarter 64 80 quarter seq-note  \ E4 at tick 480
seq-play

\ Using chord builders
c4 major 80 half 0 3 chord>seq  \ C major chord at time 0
seq-play
```

---

## Generative Music

### Probability

| Word | Stack | Description |
|------|-------|-------------|
| `%` | `( pitch probability -- pitch\|rest )` | Play with probability |
| `random` | `( -- n )` | Random number 0-99 (system rand) |
| `chance` | `( probability -- flag )` | Probability gate using seed |

```forth
c4 75%,                 \ 75% chance to play
c4 50%,                 \ Coin flip
random 50 > if c4, else e4, then   \ Conditional
```

### Alternatives

| Word | Stack | Description |
|------|-------|-------------|
| `\|` | (syntax) | Separate equal-probability alternatives |

```forth
c4|e4,                  \ 50% C4, 50% E4
c4|e4|g4,               \ 33% each
c4|r,                   \ 50% C4, 50% rest
(c4 e4 g4)|(f4 a4 c5),  \ 50% C major, 50% F major
```

### Seeded Random (Reproducible)

| Word | Stack | Description |
|------|-------|-------------|
| `seed!` | `( n -- )` | Set PRNG seed |
| `seed@` | `( -- n )` | Get current seed |
| `next-random` | `( -- n )` | Get next random number |
| `srand-range` | `( lo hi -- n )` | Random int in range [lo, hi] |

```forth
42 seed!                \ Set seed for reproducibility
next-random .           \ Deterministic random number
0 127 srand-range .     \ Random in range
```

### Sequence Operations

These operations work on bracket sequences `[ ... ]`:

| Word | Stack | Description |
|------|-------|-------------|
| `shuffle` | `( seq -- seq )` | Fisher-Yates shuffle in place |
| `reverse` | `( seq -- seq )` | Reverse sequence in place |
| `pick` | `( seq -- value )` | Pick one random element |
| `pick-n` | `( seq n -- seq )` | Pick n random elements (new seq) |
| `invert` | `( seq axis -- seq )` | Invert pitches around axis |
| `arp-up` | `( seq -- seq )` | No change (ascending) |
| `arp-down` | `( seq -- seq )` | Reverse (descending) |
| `arp-up-down` | `( seq -- seq )` | Original + reversed middle |
| `concat` | `( seq1 seq2 -- seq )` | Concatenate two sequences |
| `btranspose` | `( seq semitones -- seq )` | Transpose all pitches |

```forth
[ c4 e4 g4 ] shuffle,       \ Play shuffled
[ c4 e4 g4 ] reverse,       \ Play reversed (g4 e4 c4)
[ c4 e4 g4 ] pick .         \ Print random pitch
[ c4 e4 g4 b4 ] 2 pick-n,   \ Pick 2 random, play as sequence
[ c4 e4 g4 ] 64 invert,     \ Invert around E4
[ c4 e4 g4 ] arp-up-down,   \ c4 e4 g4 e4
[ c4 e4 ] [ g4 b4 ] concat, \ Play c4 e4 g4 b4
[ c4 e4 g4 ] 7 btranspose,  \ Play transposed up a fifth
```

### Random Walks

| Word | Stack | Description |
|------|-------|-------------|
| `random-walk` | `( start max-step n -- seq )` | Generate random walk sequence |
| `drunk-walk` | `( scale-seq start max-degrees n -- seq )` | Drunk walk within scale |

```forth
60 3 8 random-walk,         \ 8 notes starting at C4, max 3 semitone steps
[ c4 d4 e4 f4 g4 a4 b4 ] c4 2 8 drunk-walk,  \ 8 notes in C major scale
```

### Weighted Selection

| Word | Stack | Description |
|------|-------|-------------|
| `weighted-pick` | `( seq -- value )` | Pick from value/weight pairs |

```forth
\ Sequence of value/weight pairs: c4 has weight 50, e4 has weight 30, g4 has weight 20
[ c4 50 e4 30 g4 20 ] weighted-pick,  \ Play weighted random pick
```

### Euclidean Rhythms

| Word | Stack | Description |
|------|-------|-------------|
| `euclidean` | `( hits steps -- b1...bn )` | Bjorklund's algorithm |

```forth
3 8 euclidean .s            \ 1 0 0 1 0 0 1 0 (3 hits in 8 steps)
```

---

## Control Flow

### Conditionals

```forth
flag if ... then
flag if ... else ... then
```

```forth
random 50 > if c4, else e4, then
: coin random 50 > if c4, else e4, then ;
```

### Word Definitions

```forth
: name ... ;
```

```forth
: cmaj (c4 e4 g4), ;
: melody c4, e4, g4, c5, ;
cmaj                    \ Play C major
melody                  \ Play melody
```

### Loops

| Word | Stack | Description |
|------|-------|-------------|
| `times` | `( n -- )` | Repeat last word n times |

```forth
: phrase c4, d4, e4, ;
phrase 4 times          \ Play phrase 4 times
```

### Anonymous Blocks

Use `{ }` for deferred execution:

| Word | Stack | Description |
|------|-------|-------------|
| `{ ... }` | `( -- block-ref )` | Capture block |
| `*` | `( block n -- )` | Execute block n times |

```forth
{ c4, e4, g4, } 4 *     \ Play phrase 4 times
{ c4 80%, e4, } 8 *     \ Generative pattern
```

---

## Recording & File I/O

### Command Recording

Record input commands for replay:

| Command | Description |
|---------|-------------|
| `rec` | Start recording input commands |
| `stop` | Stop recording (and MIDI capture) |
| `save filename` | Save recorded commands to .4th file |
| `load filename` | Load and execute a .4th file |

```forth
rec                     \ Start recording
c4, e4, g4,             \ Commands are recorded
stop                    \ Stop recording
save melody.4th         \ Save to file
load melody.4th         \ Load and execute later
```

### MIDI Event Recording

Record MIDI events with timing for export:

| Command | Description |
|---------|-------------|
| `rec-midi` | Start recording MIDI events |
| `stop` | Stop recording |
| `save-midi filename` | Save as Forth sequence file |
| `write-mid filename` | Save as standard MIDI file |
| `read-mid filename` | Read and display .mid file info |

```forth
midi-open
rec-midi                \ Start MIDI recording
c4, e4, g4,             \ Play notes
stop                    \ Stop recording
write-mid song.mid      \ Save as standard MIDI file
```

---

## Utility Commands

| Command | Description |
|---------|-------------|
| `help` | Display command reference |
| `quit` | Exit interpreter |
| `words` | List all defined words |

---

## Examples

### Simple Melody

```forth
midi-open
250 dur!
c4, d4, e4, f4, g4, a4, b4, c5,
midi-close
```

### Chord Progression

```forth
midi-open
500 dur!
(c4 e4 g4),             \ C major
(f4 a4 c5),             \ F major
(g4 b4 d5),             \ G major
(c4 e4 g4),             \ C major
midi-close
```

### Generative Pattern

```forth
midi-open
200 dur!

: gen-note
    random 127 * 100 /
    c4 scale-pentatonic quantize
    75%,
;

gen-note 16 times
midi-close
```

### With Dynamics and Articulation

```forth
midi-open
: phrase
    mf c4., d4., e4.,   \ Staccato
    ff g4>,             \ Accent
    p (c5 e5 g5),       \ Soft chord
;
phrase 4 times
midi-close
```

### Sequence with Transformations

```forth
midi-open
seq-new drop

0 60 80 quarter seq-note
quarter 64 80 quarter seq-note
half 67 80 quarter seq-note

seq-play                \ Play original
5 seq-transpose         \ Up a fourth
seq-play                \ Play transposed
seq-reverse             \ Reverse
seq-play                \ Play backwards
midi-close
```
