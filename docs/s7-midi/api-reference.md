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

## Scale Functions

### build-scale

```scheme
(build-scale root intervals) -> list
```

Build a scale from a root pitch and list of semitone intervals.

```scheme
(build-scale c4 '(0 2 4 5 7 9 11))  ; => (60 62 64 65 67 69 71) - C major
(build-scale c4 scale-minor)        ; => (60 62 63 65 67 68 70) - C minor
```

### scale (helper)

```scheme
(scale root name) -> list
```

Build a scale using a scale name symbol. Looks up intervals from `*scales*`.

```scheme
(scale c4 'major)         ; C major scale
(scale a4 'minor)         ; A natural minor
(scale c4 'blues)         ; C blues scale
(scale d4 'maqam-hijaz)   ; D Hijaz (Arabic)
(scale c4 'raga-bhairav)  ; C Bhairav (Indian)
```

### scale-degree

```scheme
(scale-degree root intervals degree) -> integer
```

Get a specific scale degree. Degrees are 1-based (1 = root). Supports extended degrees beyond the octave.

```scheme
(scale-degree c4 scale-major 1)   ; => 60 (root)
(scale-degree c4 scale-major 3)   ; => 64 (third)
(scale-degree c4 scale-major 5)   ; => 67 (fifth)
(scale-degree c4 scale-major 9)   ; => 74 (ninth = 2nd + octave)
```

### degree (helper)

```scheme
(degree root name n) -> integer
```

Get a scale degree using a scale name symbol.

```scheme
(degree c4 'major 3)    ; => 64 (E4, third of C major)
(degree c4 'major 5)    ; => 67 (G4, fifth of C major)
(degree c4 'minor 7)    ; => 70 (Bb4, seventh of C minor)
(degree c4 'major 9)    ; => 74 (D5, ninth)
```

### in-scale?

```scheme
(in-scale? pitch root intervals) -> boolean
```

Check if a pitch belongs to a scale (in any octave).

```scheme
(in-scale? e4 c4 scale-major)   ; => #t (E is in C major)
(in-scale? fs4 c4 scale-major)  ; => #f (F# is not in C major)
(in-scale? 76 60 scale-major)   ; => #t (E5 is in C major)
```

### in-scale-named? (helper)

```scheme
(in-scale-named? pitch root name) -> boolean
```

Check if a pitch is in a named scale.

```scheme
(in-scale-named? e4 c4 'major)    ; => #t
(in-scale-named? fs4 c4 'major)   ; => #f
```

### quantize-to-scale

```scheme
(quantize-to-scale pitch root intervals) -> integer
```

Quantize (snap) a pitch to the nearest tone in a scale.

```scheme
(quantize-to-scale fs4 c4 scale-major)   ; => 65 (F# -> F or G)
```

### quantize (helper)

```scheme
(quantize pitch root name) -> integer
```

Quantize to a named scale.

```scheme
(quantize fs4 c4 'major)       ; Snap F# to C major
(quantize 61 c4 'pentatonic)   ; Snap C# to C pentatonic
```

### Scale Constants

All scales are available as variables with the `scale-` prefix:

```scheme
;; Diatonic modes
scale-major          ; (0 2 4 5 7 9 11)
scale-dorian         ; (0 2 3 5 7 9 10)
scale-phrygian       ; (0 1 3 5 7 8 10)
scale-lydian         ; (0 2 4 6 7 9 11)
scale-mixolydian     ; (0 2 4 5 7 9 10)
scale-minor          ; (0 2 3 5 7 8 10)
scale-locrian        ; (0 1 3 5 6 8 10)

;; Other scales
scale-harmonic-minor ; (0 2 3 5 7 8 11)
scale-melodic-minor  ; (0 2 3 5 7 9 11)
scale-pentatonic     ; (0 2 4 7 9)
scale-blues          ; (0 3 5 6 7 10)
scale-whole-tone     ; (0 2 4 6 8 10)

;; Exotic scales
scale-hungarian-minor
scale-double-harmonic
scale-phrygian-dominant
scale-hirajoshi      ; Japanese
scale-in-sen         ; Japanese

;; Arabic Maqamat (12-TET approximations)
scale-maqam-hijaz
scale-maqam-nahawand
scale-maqam-nikriz

;; Indian Ragas (12-TET approximations)
scale-raga-bhairav
scale-raga-todi
scale-raga-marwa
```

See `*scales*` alist for all 55 available scales by name.

---

## Microtonal / Pitch Bend

### midi-pitch-bend

```scheme
(midi-pitch-bend m cents [channel])
```

Send a pitch bend message. Cents are relative to the current note.

```scheme
(midi-pitch-bend m 0)       ; Center (no bend)
(midi-pitch-bend m 100)     ; Bend up 1 semitone
(midi-pitch-bend m -50)     ; Bend down quarter tone
(midi-pitch-bend m 50 2)    ; Quarter tone up on channel 2
```

### cents-to-note

```scheme
(cents-to-note root cents) -> pair
```

Convert a cents interval to a `(note . bend-cents)` pair.

```scheme
(cents-to-note c4 150)    ; => (61 . 50) - C#4 + 50 cents
(cents-to-note c4 350)    ; => (63 . 50) - Eb4 + 50 cents
```

### Microtonal Scale Constants

For scales with quarter tones, use the `-cents` variants:

```scheme
;; Arabic Maqamat with quarter tones
scale-maqam-bayati-cents   ; (0 150 300 500 700 800 1000)
scale-maqam-rast-cents     ; (0 200 350 500 700 900 1050)
scale-maqam-saba-cents     ; (0 150 300 400 500 700 800)
scale-maqam-sikah-cents    ; (0 150 350 500 650 850 1000)

;; Turkish Makamlar
scale-makam-ussak-cents    ; (0 150 300 500 700 800 1000)
scale-makam-huseyni-cents  ; (0 150 300 500 700 900 1000)

;; Indian 22-Shruti scale
scale-shruti-cents         ; 22 intervals in cents
```

### Playing Microtonal Scales

```scheme
(define m (midi-open))

;; Play Maqam Bayati with quarter tones
(for-each
  (lambda (cents)
    (let ((result (cents-to-note d4 cents)))
      (midi-pitch-bend m (cdr result))
      (midi-note m (car result) mf quarter)))
  scale-maqam-bayati-cents)

(midi-pitch-bend m 0)  ; Reset bend
(midi-close m)
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

---

## MIDI Recording

Record MIDI events for replay or export. Records note-on, note-off, and CC events with timestamps.

### record-midi

```scheme
(record-midi [bpm])
```

Start recording MIDI events. Optional BPM parameter (default 120) is stored with the recording for reference.

```scheme
(record-midi)        ; Start at 120 BPM
(record-midi 140)    ; Start at 140 BPM
```

### record-stop

```scheme
(record-stop)
```

Stop recording MIDI events. Prints the number of events recorded.

```scheme
(record-stop)
; => MIDI recording stopped. 42 events recorded.
```

### save-midi

```scheme
(save-midi filename)
```

Save recorded events to a Scheme replay script. The generated file can be loaded to replay the recorded performance.

```scheme
(save-midi "my_song.scm")
; => Saved 42 events to my_song.scm
```

The generated file contains:
- Event data as a Scheme list
- Replay code that recreates the timing

### record-status

```scheme
(record-status) -> (active count bpm)
```

Get current recording status. Returns a list of three values: whether recording is active, event count, and BPM.

```scheme
(record-status)   ; => (#t 42 120)
```

### Recording Example

```scheme
(define m (midi-open))

(record-midi 120)

(midi-note m c4 mf quarter)
(midi-note m e4 mf quarter)
(midi-chord m (major g4) f half)

(record-stop)
(save-midi "melody.scm")

(midi-close m)
```

---

## MIDI File I/O

Read and write standard MIDI files (.mid format).

### write-mid

```scheme
(write-mid filename [ppqn])
```

Write recorded events to a standard MIDI file. Optional PPQN parameter (default 480).

```scheme
(define m (midi-open))
(record-midi 120)

(midi-note m c4 mf quarter)
(midi-note m e4 mf quarter)
(midi-chord m (major g4) f half)

(record-stop)
(write-mid "melody.mid")   ; Standard MIDI file

(midi-close m)
```

### read-mid

```scheme
(read-mid filename) -> alist
```

Read a standard MIDI file and return its contents as an association list with metadata and events.

```scheme
(define data (read-mid "song.mid"))

;; Metadata
(cdr (assq 'num-tracks data))  ; Number of tracks
(cdr (assq 'ppqn data))        ; Pulses per quarter note
(cdr (assq 'tempo data))       ; Tempo in microseconds per beat
(cdr (assq 'duration data))    ; Duration in milliseconds
(cdr (assq 'format data))      ; MIDI format (0, 1, or 2)

;; Events - list of (track tick channel type data1 data2)
(define events (cdr (assq 'events data)))
(for-each
  (lambda (event)
    (display (format #f "t=~A ch=~A type=~A d1=~A d2=~A~%"
                     (cadr event)   ; tick
                     (caddr event)  ; channel
                     (cadddr event) ; type
                     (car (cddddr event))   ; data1
                     (cadr (cddddr event))))) ; data2
  events)
```

Event types (in the type field):
- `144` (`#x90`) = Note On
- `128` (`#x80`) = Note Off
- `176` (`#xB0`) = Control Change
- `192` (`#xC0`) = Program Change
- `224` (`#xE0`) = Pitch Bend

---

## Async Scheduler

The scheduler provides non-blocking concurrent playback using a thunk-based cooperative multitasking model. Each "voice" is a Scheme procedure that returns either a number (ms to wait) or `#f` (done).

### spawn

```scheme
(spawn thunk [name]) -> voice-id
```

Create a new voice from a procedure. The procedure takes no arguments and should return:
- A number (milliseconds to wait before next call)
- `#f` (voice is complete)

```scheme
;; Simple voice that runs 3 times
(define count 0)
(spawn (lambda ()
         (set! count (+ count 1))
         (display count)
         (newline)
         (if (<= count 3) 100 #f))
       "counter")
```

### run

```scheme
(run)
```

Run the scheduler until all voices complete. This is a blocking call that processes timer events.

```scheme
(spawn (lambda () (display "Hello!\n") #f))
(run)   ; Prints "Hello!" then returns
```

### stop

```scheme
(stop) -> unspecified
(stop voice-id) -> boolean
```

Stop all voices or a specific voice by ID.

```scheme
(define v (spawn (lambda () 1000)))  ; Would run forever
(stop v)        ; Stop specific voice, returns #t
(stop)          ; Stop all voices
```

### voices

```scheme
(voices) -> integer
```

Return the count of active voices.

```scheme
(spawn (lambda () #f))
(spawn (lambda () #f))
(voices)   ; => 2
```

### scheduler-status

```scheme
(scheduler-status) -> alist
```

Return scheduler status as an association list.

```scheme
(spawn (lambda () #f) "test")
(scheduler-status)
; => ((voices (1 "test" #f)) (active . 1) (running . #f))
```

The alist contains:
- `voices` - List of `(id name waiting?)` for each active voice
- `active` - Count of active voices
- `running` - Whether the scheduler is currently running

---

## Voice Builders

Helper functions to create common voice patterns. All use closures to maintain state.

### make-sequence-voice

```scheme
(make-sequence-voice steps) -> procedure
```

Create a voice from a list of `(action . delay)` pairs.

```scheme
(spawn (make-sequence-voice
         (list
           (cons (lambda () (display "one\n")) 100)
           (cons (lambda () (display "two\n")) 100)
           (cons (lambda () (display "three\n")) 0))))
(run)
; Prints: one, two, three with 100ms delays
```

### make-note-voice

```scheme
(make-note-voice pitch vel dur) -> procedure
```

Create a voice that plays a single note on the default port.

```scheme
(open)
(spawn (make-note-voice c4 mf quarter))
(run)  ; Plays C4 for 500ms
```

### make-melody-voice

```scheme
(make-melody-voice pitches vel dur) -> procedure
```

Create a voice that plays a sequence of notes.

```scheme
(open)
(spawn (make-melody-voice '(60 62 64 65 67) mf eighth))
(run)  ; Plays C-D-E-F-G with 250ms each
```

### make-chord-voice

```scheme
(make-chord-voice pitches vel dur) -> procedure
```

Create a voice that plays a chord.

```scheme
(open)
(spawn (make-chord-voice (major c4) mf half))
(run)  ; Plays C major chord for 1 second
```

### make-repeat-voice

```scheme
(make-repeat-voice thunk n delay) -> procedure
```

Create a voice that calls a thunk n times with delay between.

```scheme
(spawn (make-repeat-voice
         (lambda () (display "tick\n"))
         5    ; repeat 5 times
         200) ; 200ms between
       "ticker")
(run)
```

### make-loop-voice

```scheme
(make-loop-voice thunk delay) -> procedure
```

Create a voice that loops forever (until stopped).

```scheme
(define beat-id
  (spawn (make-loop-voice
           (lambda () (midi-note-on *midi* 36 100))
           500)
         "beat"))
; ... later ...
(stop beat-id)  ; Stop the loop
```

---

## Async Convenience Functions

These functions spawn voices automatically using the global `*midi*` port.

### async-note

```scheme
(async-note pitch [vel] [dur]) -> voice-id
```

Spawn a voice to play a single note.

```scheme
(open)
(async-note c4)
(async-note e4 mf)
(async-note g4 f half)
(run)  ; All three notes play
```

### async-chord

```scheme
(async-chord pitches [vel] [dur]) -> voice-id
```

Spawn a voice to play a chord.

```scheme
(open)
(async-chord (major c4) mf quarter)
(run)
```

### async-melody

```scheme
(async-melody pitches [vel] [dur]) -> voice-id
```

Spawn a voice to play a melody.

```scheme
(open)
(async-melody '(60 64 67 72) mf eighth)
(run)
```

---

## Concurrent Playback Examples

### Multiple Voices

```scheme
(open)

;; Bass voice
(spawn (make-repeat-voice
         (lambda () (midi-note *midi* c2 f sixteenth))
         8
         eighth)
       "bass")

;; Lead melody
(spawn (make-melody-voice (scale c4 'pentatonic) mf quarter) "melody")

(run)  ; Both play simultaneously
(close)
```

### Polyrhythmic Pattern

```scheme
(open)

;; 3 beats
(spawn (make-repeat-voice
         (lambda () (midi-note *midi* 60 mf 100))
         3
         400)
       "3-beat")

;; 4 beats (different rate)
(spawn (make-repeat-voice
         (lambda () (midi-note *midi* 67 mf 100))
         4
         300)
       "4-beat")

(run)
(close)
```

### State Machine Voice

```scheme
(open)

(define (make-random-walker)
  (let ((pitch 60)
        (steps 20))
    (lambda ()
      (if (<= steps 0)
          #f
          (begin
            (midi-note *midi* pitch mf sixteenth)
            (set! pitch (+ pitch (- (random 5) 2)))  ; random walk
            (set! pitch (max 48 (min 84 pitch)))    ; clamp
            (set! steps (- steps 1))
            eighth)))))

(spawn (make-random-walker) "walker")
(run)
(close)
```
