# s7-midi

A Scheme-based MIDI language using [s7](https://ccrma.stanford.edu/software/snd/snd/s7.html), providing a functional/Lisp approach to MIDI programming.

## Features

- Full Scheme programming language (closures, macros, first-class functions)
- Musical abstractions: pitches, durations, velocities, chords
- **55 built-in scales** (modes, pentatonics, blues, exotic, Arabic Maqamat, Indian Ragas)
- **10 microtonal scales** with quarter-tone support via pitch bend
- Scale functions: build scales, get degrees, quantize pitches
- Low-level MIDI control: note on/off, CC, program change, pitch bend
- Virtual and hardware MIDI port support
- Chord builders and transpose helpers
- Tempo-aware duration constants
- **Async scheduler**: concurrent voice playback using thunk-based cooperative multitasking

## Quick Start

### 1. Build

```bash
make
```

### 2. Run interactively

```bash
./build/s7_midi
```

### 3. Play some notes

Using convenience functions:

```scheme
> (open)
#<midi-out virtual "s7MIDI">
> (n c4)
> (ch (major c4))
> (close)
```

Or using explicit port management:

```scheme
> (define m (midi-open))
#<midi-out virtual "s7MIDI">
> (midi-note m c4 mf quarter)
> (midi-chord m (major c4) mf half)
> (midi-close m)
```

## Usage Modes

### Interactive REPL

```bash
./build/s7_midi
```

Type `(help)` for available functions, `(quit)` to exit.

### Evaluate Expression

```bash
./build/s7_midi -e '(+ 1 2)'
./build/s7_midi -e '(major c4)'
```

### Run Scheme File

```bash
./build/s7_midi script.scm
```

## Example Program

```scheme
;; melody.scm - Simple melody with chords

(define m (midi-open))
(set-tempo! 120)

;; Play a C major scale
(for-each (lambda (p) (midi-note m p mf quarter))
          (list c4 d4 e4 f4 g4 a4 b4 c5))

;; Play chord progression
(midi-chord m (major c4) mf half)    ; I
(midi-chord m (major f3) mf half)    ; IV
(midi-chord m (major g3) f half)     ; V
(midi-chord m (major c4) mf whole)   ; I

(midi-close m)
```

Run with:

```bash
./build/s7_midi melody.scm
```

## Async Playback

Play multiple voices concurrently using the thunk-based scheduler:

```scheme
(open)

;; Voice 1: Bass pattern (repeating)
(spawn (make-repeat-voice
         (lambda () (midi-note *midi* c2 f sixteenth))
         8
         eighth)
       "bass")

;; Voice 2: Chord progression
(spawn (make-sequence-voice
         (list
           (cons (lambda () (midi-chord *midi* (major c4) mf half)) half)
           (cons (lambda () (midi-chord *midi* (major f3) mf half)) half)
           (cons (lambda () (midi-chord *midi* (major g3) f half)) half)
           (cons (lambda () (midi-chord *midi* (major c4) mf whole)) 0)))
       "chords")

(run)   ; Both voices play simultaneously
(close)
```

Each voice is a closure that returns ms-to-wait or `#f` when done. See [API Reference](api-reference.md#async-scheduler) for details.

## Why Scheme for MIDI?

Scheme's functional nature makes it ideal for musical programming:

- **First-class functions**: Pass note-playing functions as arguments
- **Closures**: Create parameterized musical patterns
- **Lists**: Natural representation for chords and sequences
- **Macros**: Define custom musical notation
- **REPL**: Interactive exploration and live coding

## Documentation

- [API Reference](api-reference.md) - Complete function documentation
- [Examples](examples.md) - More code examples
- [Architecture](architecture.md) - How the FFI works

## Requirements

- Built project (`make` must complete successfully)
- macOS (CoreMIDI), Linux (ALSA), or Windows (WinMM)
