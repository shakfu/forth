# s7-midi Examples

## Basic Examples

### Hello MIDI

Play a C major scale:

```scheme
(define m (midi-open))

(for-each (lambda (p) (midi-note m p mf quarter))
          (list c4 d4 e4 f4 g4 a4 b4 c5))

(midi-close m)
```

### Using Note Names

Play notes using string notation:

```scheme
(define m (midi-open))

(midi-note m (note "C4"))
(midi-note m (note "D4"))
(midi-note m (note "E4"))
(midi-note m (note "F4"))
(midi-note m (note "G4") f half)  ; Louder, longer

(midi-close m)
```

### Chord Progression

Play a I-IV-V-I progression:

```scheme
(define m (midi-open))

;; I - C major
(midi-chord m (major c4) mf half)

;; IV - F major
(midi-chord m (major f3) mf half)

;; V - G major
(midi-chord m (major g3) mf half)

;; I - C major
(midi-chord m (major c4) f whole)

(midi-close m)
```

### Melody with Dynamics

```scheme
(define m (midi-open))

;; Crescendo
(midi-note m c4 pp quarter)
(midi-note m d4 p quarter)
(midi-note m e4 mp quarter)
(midi-note m f4 mf quarter)
(midi-note m g4 f quarter)
(midi-note m a4 ff half)

;; Decrescendo
(midi-note m g4 f quarter)
(midi-note m f4 mf quarter)
(midi-note m e4 mp quarter)
(midi-note m d4 p quarter)
(midi-note m c4 pp whole)

(midi-close m)
```

---

## Intermediate Examples

### Arpeggiated Chords

```scheme
(define m (midi-open))

;; Arpeggiate C major up and down
(midi-arpeggio m (list c4 e4 g4 c5) mf eighth)
(midi-arpeggio m (list c5 g4 e4 c4) mf eighth)

(rest quarter)

;; Arpeggiate A minor
(midi-arpeggio m (minor a4) ff sixteenth)
(midi-arpeggio m (reverse (minor a4)) ff sixteenth)

(midi-close m)
```

### Tempo Changes

```scheme
;; Start slow
(set-tempo! 60)

(define m (midi-open))

(midi-note m c4 mf quarter)  ; 1000ms at 60 BPM
(midi-note m e4 mf quarter)
(midi-note m g4 mf quarter)

;; Speed up
(set-tempo! 120)
(midi-note m c5 mf quarter)  ; 500ms at 120 BPM
(midi-note m e5 mf quarter)
(midi-note m g5 mf quarter)

;; Even faster
(set-tempo! 180)
(midi-note m c6 mf quarter)  ; ~333ms at 180 BPM
(midi-note m e6 mf quarter)
(midi-note m g6 mf quarter)

(midi-close m)
```

### Dotted Rhythms

```scheme
(define m (midi-open))

;; Dotted quarter - eighth pattern
(midi-note m c4 mf (dotted quarter))
(midi-note m d4 mf eighth)
(midi-note m e4 mf (dotted quarter))
(midi-note m f4 mf eighth)
(midi-note m g4 f half)

(midi-close m)
```

### All Chord Types

```scheme
(define m (midi-open))
(define root c4)

;; Triads
(midi-chord m (major root) mf half)
(midi-chord m (minor root) mf half)
(midi-chord m (dim root) mf half)
(midi-chord m (aug root) mf half)

(rest quarter)

;; Seventh chords
(midi-chord m (dom7 root) mf half)
(midi-chord m (maj7 root) mf half)
(midi-chord m (min7 root) mf half)

(midi-close m)
```

---

## Advanced Examples

### Higher-Order Functions

Use Scheme's functional features for musical patterns:

```scheme
(define m (midi-open))

;; Create a note-playing function with fixed parameters
(define (make-player velocity duration)
  (lambda (pitch)
    (midi-note m pitch velocity duration)))

;; Define different "instruments"
(define loud-short (make-player fff sixteenth))
(define soft-long (make-player pp half))

;; Use them
(for-each loud-short (list c4 e4 g4 c5))
(for-each soft-long (list c5 g4 e4 c4))

(midi-close m)
```

### Pattern Repetition

```scheme
(define m (midi-open))

;; Define a pattern as a procedure
(define (pattern1)
  (midi-note m c4 mf eighth)
  (midi-note m e4 mf eighth)
  (midi-note m g4 mf eighth)
  (midi-note m e4 mf eighth))

;; Repeat it 4 times
(do ((i 0 (+ i 1)))
    ((= i 4))
  (pattern1))

;; Or use a helper
(define (times n proc)
  (do ((i 0 (+ i 1)))
      ((= i n))
    (proc)))

(times 4 pattern1)

(midi-close m)
```

### Transpose a Melody

```scheme
(define m (midi-open))

(define melody (list c4 d4 e4 f4 g4))

;; Original
(for-each (lambda (p) (midi-note m p mf eighth)) melody)
(rest quarter)

;; Up a perfect fifth
(for-each (lambda (p) (midi-note m (transpose p 7) mf eighth)) melody)
(rest quarter)

;; Down an octave
(for-each (lambda (p) (midi-note m (octave-down p) mf eighth)) melody)

(midi-close m)
```

### Control Changes

```scheme
(define m (midi-open))

;; Set volume
(midi-cc m 7 100)

;; Enable sustain pedal
(midi-cc m 64 127)

;; Play notes with sustain
(midi-note m c4 mf quarter)
(midi-note m e4 mf quarter)
(midi-note m g4 mf quarter)

;; Release sustain
(midi-cc m 64 0)

(rest quarter)

;; Modulation sweep
(midi-note-on m c4 80)
(do ((i 0 (+ i 8)))
    ((> i 127))
  (midi-cc m 1 i)
  (midi-sleep 50))
(do ((i 127 (- i 8)))
    ((< i 0))
  (midi-cc m 1 i)
  (midi-sleep 50))
(midi-note-off m c4)

(midi-close m)
```

### Program Changes

```scheme
(define m (midi-open))

;; Piano (program 0)
(midi-program m 0)
(midi-note m c4 mf quarter)
(midi-note m e4 mf quarter)
(midi-note m g4 mf quarter)

;; Strings (program 48)
(midi-program m 48)
(midi-chord m (major c4) mp whole)

;; Brass (program 61)
(midi-program m 61)
(midi-note m c5 ff half)

(midi-close m)
```

### Generative Music

```scheme
(define m (midi-open))
(set-tempo! 140)

;; Random note from a list
(define (random-element lst)
  (list-ref lst (random (length lst))))

;; Random note from chord
(define (random-note chord)
  (random-element chord))

;; Random velocity in range
(define (random-velocity min max)
  (+ min (random (- max min))))

;; Play 32 random notes from C major 7
(define chord (maj7 c4))
(do ((i 0 (+ i 1)))
    ((= i 32))
  (midi-note m
             (random-note chord)
             (random-velocity 60 100)
             sixteenth))

(midi-close m)
```

### Probability-Based Patterns

```scheme
(define m (midi-open))

;; Play note with probability
(define (maybe-play pitch prob)
  (if (< (random 100) prob)
      (midi-note m pitch mf sixteenth)
      (rest sixteenth)))

;; Sparse texture
(do ((i 0 (+ i 1)))
    ((= i 32))
  (maybe-play c4 30)   ; 30% chance
  (maybe-play e4 50)   ; 50% chance
  (maybe-play g4 70))  ; 70% chance

(midi-close m)
```

### Multi-Channel Composition

```scheme
(define m (midi-open))

;; Set up instruments
(midi-program m 0 1)    ; Piano on channel 1
(midi-program m 32 2)   ; Bass on channel 2

;; Helper for channel-specific playing
(define (play-on-channel ch pitch vel dur)
  (midi-note m pitch vel dur ch))

;; Simple bass line and melody
(define (bar)
  ;; Bass note (channel 2)
  (play-on-channel 2 c2 100 half)
  ;; Melody notes (channel 1)
  (play-on-channel 1 c4 80 quarter)
  (play-on-channel 1 e4 80 quarter))

;; Play 4 bars
(do ((i 0 (+ i 1)))
    ((= i 4))
  (bar))

(midi-close m)
```

---

## REPL Session Examples

Start the REPL:

```bash
./build/s7_midi
```

### Using Convenience Functions (Recommended)

The simplest way to use s7-midi interactively:

```scheme
> (open)
#<midi-out virtual "s7MIDI">
> (n c4)
> (n e4)
> (n g4)
> (ch (major c4))
> (arp (min7 a3) mf sixteenth)
> (close)
```

### Quick Note Test (Explicit Port)

```scheme
> (define m (midi-open))
#<midi-out virtual "s7MIDI">
> (midi-note m c4)
> (midi-note m e4)
> (midi-note m g4)
> (midi-close m)
```

### Interactive Chord Exploration

```scheme
> (define m (midi-open))
> (midi-chord m (major c4))
> (midi-chord m (minor a3))
> (midi-chord m (dom7 g3))
> (midi-close m)
```

### Testing Dynamics

```scheme
> (define m (midi-open))
> (midi-note m c4 ppp)   ; Very soft
> (midi-note m c4 mf)    ; Medium
> (midi-note m c4 fff)   ; Very loud
> (midi-close m)
```

### List MIDI Ports

```scheme
> (midi-list-ports)
((0 "IAC Driver Bus 1") (1 "USB MIDI Device"))
```

### Checking Values

```scheme
> c4
60
> (major c4)
(60 64 67)
> mf
80
> quarter
500
> (bpm 60)
1000
```
