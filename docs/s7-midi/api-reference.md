# API Reference

## Port Management

### midi-list-ports

```scheme
(midi-list-ports) -> list
```

List available MIDI output ports. Returns list of `(index name)` pairs.

```scheme
(midi-list-ports)
; => ((0 "IAC Driver Bus 1") (1 "USB MIDI Device"))
```

### midi-open

```scheme
(midi-open) -> midi-out
(midi-open name) -> midi-out
(midi-open index) -> midi-out
```

Open a MIDI output port.

- `(midi-open)` - Create virtual port named "s7MIDI"
- `(midi-open "MyApp")` - Create virtual port with custom name
- `(midi-open 0)` - Open hardware port by index

```scheme
(define m (midi-open))                ; Virtual port
(define m (midi-open "MySynth"))      ; Named virtual port
(define m (midi-open 0))              ; First hardware port
```

### midi-close

```scheme
(midi-close m)
```

Close the MIDI port. Automatically sends all-notes-off on all channels.

```scheme
(midi-close m)
```

### midi-open?

```scheme
(midi-open? m) -> boolean
```

Check if the MIDI port is open.

```scheme
(define m (midi-open))
(midi-open? m)  ; => #t
(midi-close m)
(midi-open? m)  ; => #f
```

### midi-out?

```scheme
(midi-out? x) -> boolean
```

Type predicate for midi-out objects.

```scheme
(midi-out? (midi-open))  ; => #t
(midi-out? 42)           ; => #f
```

---

## Note Playing

### midi-note

```scheme
(midi-note m pitch [velocity] [duration] [channel])
```

Play a single note (blocking).

- `m` - midi-out object
- `pitch` - MIDI number (0-127) or note name symbol/string
- `velocity` - Note velocity (0-127), default 80
- `duration` - Duration in milliseconds, default 500
- `channel` - MIDI channel (1-16), default 1

```scheme
(midi-note m c4)                    ; Defaults
(midi-note m 60)                    ; By MIDI number
(midi-note m c4 mf)                 ; With velocity
(midi-note m c4 mf quarter)         ; With duration
(midi-note m c4 80 500 2)           ; On channel 2
```

### midi-chord

```scheme
(midi-chord m pitches [velocity] [duration] [channel])
```

Play multiple notes simultaneously.

- `m` - midi-out object
- `pitches` - List of MIDI numbers or note names
- `velocity` - Note velocity (0-127), default 80
- `duration` - Duration in milliseconds, default 500
- `channel` - MIDI channel (1-16), default 1

```scheme
(midi-chord m '(60 64 67))                    ; C major by numbers
(midi-chord m (list c4 e4 g4))                ; C major by constants
(midi-chord m (major c4) mf half)             ; Using chord builder
(midi-chord m (dom7 g3) f quarter 2)          ; On channel 2
```

### midi-arpeggio

```scheme
(midi-arpeggio m pitches [velocity] [duration] [channel])
```

Play notes sequentially (arpeggiated).

- `m` - midi-out object
- `pitches` - List of pitches to arpeggiate
- `velocity` - Note velocity (0-127), default mf (80)
- `duration` - Duration of each note, default eighth (250)
- `channel` - MIDI channel (1-16), default 1

```scheme
(midi-arpeggio m (major c4))                  ; Defaults
(midi-arpeggio m (min7 a3) mf sixteenth)      ; Fast arpeggio
```

### midi-note-on

```scheme
(midi-note-on m pitch [velocity] [channel])
```

Send Note On message (non-blocking).

```scheme
(midi-note-on m 60)           ; Note on, default velocity
(midi-note-on m c4 100)       ; With velocity
(midi-note-on m c4 100 2)     ; On channel 2
```

### midi-note-off

```scheme
(midi-note-off m pitch [velocity] [channel])
```

Send Note Off message.

```scheme
(midi-note-off m 60)
(midi-note-off m c4 64 2)     ; With release velocity on channel 2
```

---

## Control Messages

### midi-cc

```scheme
(midi-cc m control value [channel])
```

Send Control Change message.

- `control` - CC number (0-127)
- `value` - CC value (0-127)
- `channel` - MIDI channel (1-16), default 1

```scheme
(midi-cc m 1 64)        ; Modulation wheel to middle
(midi-cc m 7 100)       ; Volume
(midi-cc m 64 127)      ; Sustain pedal on
(midi-cc m 64 0)        ; Sustain pedal off
```

Common CC numbers:

- 1: Modulation wheel
- 7: Channel volume
- 10: Pan
- 11: Expression
- 64: Sustain pedal
- 91: Reverb
- 93: Chorus

### midi-program

```scheme
(midi-program m program [channel])
```

Send Program Change message.

- `program` - Program number (0-127)
- `channel` - MIDI channel (1-16), default 1

```scheme
(midi-program m 0)      ; Piano
(midi-program m 25)     ; Acoustic guitar
(midi-program m 48 2)   ; Strings on channel 2
```

### midi-all-notes-off

```scheme
(midi-all-notes-off m [channel])
```

Send All Notes Off. If channel is omitted, sends on all channels (1-16).

```scheme
(midi-all-notes-off m)      ; All channels
(midi-all-notes-off m 1)    ; Channel 1 only
```

---

## Pitch Helpers

### note

```scheme
(note name) -> integer
```

Parse note name to MIDI number.

- Supports: C, D, E, F, G, A, B (case insensitive)
- Accidentals: # or s (sharp), b (flat)
- Octaves: -1 to 9

```scheme
(note "C4")    ; => 60
(note "C#4")   ; => 61
(note "Db4")   ; => 61
(note 'c4)     ; => 60 (symbol form)
(note 'cs4)    ; => 61
```

### Pitch Constants

All pitches from C0 to B8:

```scheme
c0 cs0 d0 ds0 e0 f0 fs0 g0 gs0 a0 as0 b0   ; Octave 0
c1 cs1 d1 ds1 e1 f1 fs1 g1 gs1 a1 as1 b1   ; Octave 1
...
c4 cs4 d4 ds4 e4 f4 fs4 g4 gs4 a4 as4 b4   ; Octave 4 (middle C = c4 = 60)
...
c8 cs8 d8 ds8 e8 f8 fs8 g8 gs8 a8 as8 b8   ; Octave 8
```

Flat aliases:

```scheme
db0 eb0 gb0 ab0 bb0   ; D-flat, E-flat, G-flat, A-flat, B-flat
; ... through octave 8
```

### transpose

```scheme
(transpose pitch semitones) -> integer
```

Transpose pitch by semitones.

```scheme
(transpose c4 2)      ; => 62 (D4)
(transpose 60 -12)    ; => 48 (C3)
(transpose "C4" 7)    ; => 67 (G4)
```

### octave-up

```scheme
(octave-up pitch) -> integer
```

Transpose up one octave (+12 semitones).

```scheme
(octave-up c4)    ; => 72 (C5)
(octave-up 60)    ; => 72
```

### octave-down

```scheme
(octave-down pitch) -> integer
```

Transpose down one octave (-12 semitones).

```scheme
(octave-down c4)   ; => 48 (C3)
(octave-down 60)   ; => 48
```

---

## Chord Builders

All chord builders accept a root pitch (integer or symbol) and return a list of MIDI numbers.

### major

```scheme
(major root) -> list
```

Build major triad: root, major 3rd, perfect 5th.

```scheme
(major c4)     ; => (60 64 67)
(major 60)     ; => (60 64 67)
```

### minor

```scheme
(minor root) -> list
```

Build minor triad: root, minor 3rd, perfect 5th.

```scheme
(minor c4)     ; => (60 63 67)
```

### dim

```scheme
(dim root) -> list
```

Build diminished triad: root, minor 3rd, diminished 5th.

```scheme
(dim c4)       ; => (60 63 66)
```

### aug

```scheme
(aug root) -> list
```

Build augmented triad: root, major 3rd, augmented 5th.

```scheme
(aug c4)       ; => (60 64 68)
```

### dom7

```scheme
(dom7 root) -> list
```

Build dominant 7th chord.

```scheme
(dom7 c4)      ; => (60 64 67 70)
```

### maj7

```scheme
(maj7 root) -> list
```

Build major 7th chord.

```scheme
(maj7 c4)      ; => (60 64 67 71)
```

### min7

```scheme
(min7 root) -> list
```

Build minor 7th chord.

```scheme
(min7 c4)      ; => (60 63 67 70)
```

---

## Duration Constants

Based on 120 BPM by default. Use `set-tempo!` to change.

| Constant    | Milliseconds | Musical Value |
|-------------|--------------|---------------|
| `whole`     | 2000         | Whole note    |
| `half`      | 1000         | Half note     |
| `quarter`   | 500          | Quarter note  |
| `eighth`    | 250          | Eighth note   |
| `sixteenth` | 125          | 16th note     |

### dotted

```scheme
(dotted duration) -> integer
```

Returns 1.5x the given duration.

```scheme
(dotted quarter)   ; => 750
(dotted half)      ; => 1500
```

---

## Tempo

### set-tempo

```scheme
(set-tempo! bpm)
```

Set tempo and update all duration constants.

```scheme
(set-tempo! 120)    ; 120 BPM (default)
(set-tempo! 60)     ; 60 BPM - durations double
quarter             ; => 1000 at 60 BPM
```

### get-tempo

```scheme
(get-tempo) -> integer
```

Get current tempo in BPM.

```scheme
(get-tempo)    ; => 120
```

### bpm

```scheme
(bpm tempo) -> integer
```

Calculate quarter note duration for a given tempo (without changing global tempo).

```scheme
(bpm 120)    ; => 500
(bpm 60)     ; => 1000
(bpm 140)    ; => 428
```

---

## Velocity Constants (Dynamics)

| Constant | Value | Dynamic        |
|----------|-------|----------------|
| `ppp`    | 16    | Pianississimo  |
| `pp`     | 33    | Pianissimo     |
| `p`      | 49    | Piano          |
| `mp`     | 64    | Mezzo-piano    |
| `mf`     | 80    | Mezzo-forte    |
| `f`      | 96    | Forte          |
| `ff`     | 112   | Fortissimo     |
| `fff`    | 127   | Fortississimo  |

---

## Timing

### midi-sleep

```scheme
(midi-sleep ms)
```

Sleep for given milliseconds.

```scheme
(midi-sleep 500)    ; Wait half second
(midi-sleep 1000)   ; Wait one second
```

### rest

```scheme
(rest [duration])
```

Rest (silence) for given duration. Default is quarter note.

```scheme
(rest)              ; Quarter note rest
(rest half)         ; Half note rest
(rest 1000)         ; 1 second rest
```

---

## Utilities

### help

```scheme
(help)
```

Display available functions and usage information.

```scheme
> (help)
s7_midi - Scheme MIDI language
...
```

---

## REPL Convenience Functions

These functions use a global `*midi*` variable for simpler interactive sessions.

### *midi*

Global variable holding the default MIDI output port. Initially `#f`.

```scheme
*midi*       ; => #f (before open)
(open)
*midi*       ; => #<midi-out virtual "s7MIDI">
```

### open

```scheme
(open) -> midi-out
(open name) -> midi-out
(open index) -> midi-out
```

Open a MIDI port and set it as the default. Closes any previously open port.

```scheme
(open)              ; Create virtual port
(open "MyApp")      ; Named virtual port
(open 0)            ; Hardware port by index
```

### close

```scheme
(close)
```

Close the default MIDI port.

```scheme
(close)
```

### n

```scheme
(n pitch [velocity] [duration] [channel])
```

Play a note on the default MIDI port.

```scheme
(n c4)                  ; Play C4 with defaults
(n c4 mf)               ; With velocity
(n c4 mf quarter)       ; With duration
(n c4 80 500 2)         ; On channel 2
```

### ch

```scheme
(ch pitches [velocity] [duration] [channel])
```

Play a chord on the default MIDI port.

```scheme
(ch (major c4))             ; C major chord
(ch (minor a3) mf half)     ; A minor, half note
```

### arp

```scheme
(arp pitches [velocity] [duration] [channel])
```

Arpeggiate notes on the default MIDI port.

```scheme
(arp (major c4))                ; Arpeggiate C major
(arp (min7 a3) mf sixteenth)    ; Fast A minor 7 arpeggio
```
