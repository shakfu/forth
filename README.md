# Forth

A Forth-like interpreter with MIDI capabilities for generating and transforming MIDI sequences.

## Building

```bash
make              # Build both interpreters
make midi_forth   # Build MIDI interpreter only
make clean        # Remove binaries and build artifacts
```

Requires GCC/Clang and CMake 3.22+. First build compiles libremidi automatically.

## Running

```bash
./midi_forth      # Start MIDI Forth REPL
```

Type `help` for commands, `quit` to exit.

---

## MIDI Output

### midi-virtual

Create a virtual MIDI port named "MidiForth". Connect a DAW or synth to this port.

```forth
midi-virtual
\ Output: Created virtual MIDI output: MidiForth
```

### midi-list

List available hardware MIDI output ports.

```forth
midi-list
\ Output:
\ MIDI output ports:
\   0: IAC Driver Bus 1
\   1: USB MIDI Interface
```

### midi-open ( n -- )

Open a hardware port by index (from `midi-list`).

```forth
0 midi-open
\ Output: Opened MIDI output: IAC Driver Bus 1
```

### midi-close

Close the current MIDI output.

```forth
midi-close
\ Output: MIDI output closed
```

### note-on ( channel pitch velocity -- )

Send a Note On message. Channel is 1-16, pitch is 0-127, velocity is 0-127.

```forth
midi-virtual
1 60 100 note-on      \ channel 1, middle C, velocity 100
1 64 80 note-on       \ channel 1, E, velocity 80
1 67 90 note-on       \ channel 1, G, velocity 90
```

### note-off ( channel pitch velocity -- )

Send a Note Off message.

```forth
1 60 0 note-off       \ turn off middle C on channel 1
1 64 0 note-off
1 67 0 note-off
```

### cc ( channel cc-number value -- )

Send a Control Change message.

```forth
midi-virtual
1 1 64 cc             \ channel 1, CC 1 (mod wheel), value 64
1 7 100 cc            \ channel 1, CC 7 (volume), value 100
1 10 0 cc             \ channel 1, CC 10 (pan), value 0 (left)
1 64 127 cc           \ channel 1, CC 64 (sustain pedal), on
```

### panic

Send All Notes Off on all 16 channels. Use when notes get stuck.

```forth
panic
```

---

## Timing

### ms ( n -- )

Sleep for n milliseconds.

```forth
midi-virtual
1 60 100 note-on
500 ms                \ wait half a second
1 60 0 note-off
midi-close
```

### bpm! ( n -- )

Set tempo in beats per minute (default 120). Affects sequence playback and `note!`.

```forth
140 bpm!              \ set tempo to 140 BPM
```

### bpm@ ( -- n )

Get current tempo.

```forth
bpm@ .
\ Output: 120
```

---

## Packed Notes

Pack pitch, velocity, channel, and duration into a single value for easy manipulation.

### note ( pitch velocity channel duration -- packed-note )

Create a packed note. Duration is in ticks (480 = quarter note).

```forth
60 100 1 480 note     \ middle C, velocity 100, channel 1, quarter note
.s
\ Output: <1> 126058556
```

### note! ( packed-note -- )

Play a packed note immediately (blocking - waits for duration).

```forth
midi-virtual
60 100 1 480 note note!     \ play middle C for one quarter note
62 100 1 480 note note!     \ play D
64 100 1 480 note note!     \ play E
midi-close
```

### note. ( packed-note -- )

Print note details.

```forth
60 100 1 480 note note.
\ Output: note: pitch=60 vel=100 ch=1 dur=480
```

### pitch@ ( packed-note -- pitch )

Extract pitch from packed note.

```forth
60 100 1 480 note pitch@ .
\ Output: 60
```

### vel@ ( packed-note -- velocity )

Extract velocity.

```forth
60 100 1 480 note vel@ .
\ Output: 100
```

### ch@ ( packed-note -- channel )

Extract channel (returns 1-16).

```forth
60 100 1 480 note ch@ .
\ Output: 1
```

### dur@ ( packed-note -- duration )

Extract duration in ticks.

```forth
60 100 1 480 note dur@ .
\ Output: 480
```

### transpose ( packed-note semitones -- packed-note )

Transpose a packed note by semitones.

```forth
60 100 1 480 note       \ middle C
12 transpose            \ up one octave
note.
\ Output: note: pitch=72 vel=100 ch=1 dur=480

60 100 1 480 note
-5 transpose            \ down a fourth
note.
\ Output: note: pitch=55 vel=100 ch=1 dur=480
```

---

## Sequences

Store multiple events with timing for playback and transformation.

### seq-new ( -- id )

Create a new sequence and select it. Returns sequence ID.

```forth
seq-new .
\ Output: 0

seq-new .
\ Output: 1
```

### seq ( id -- )

Select an existing sequence by ID.

```forth
0 seq                 \ select sequence 0
```

### seq@ ( -- id )

Get current sequence ID.

```forth
seq@ .
\ Output: 0
```

### seq-note ( time pitch velocity duration -- )

Add a note to the current sequence. Time and duration are in ticks.

```forth
seq-new
0 60 100 480 seq-note       \ C at tick 0, quarter note
480 62 100 480 seq-note     \ D at tick 480
960 64 100 480 seq-note     \ E at tick 960
1440 65 100 480 seq-note    \ F at tick 1440
```

### seq-note-ch ( time channel pitch velocity duration -- )

Add a note with explicit channel.

```forth
seq-new
0 1 60 100 480 seq-note-ch    \ channel 1, C
0 2 48 80 480 seq-note-ch     \ channel 2, bass note (C2)
```

### seq-add ( packed-note time -- )

Add a packed note at specified time.

```forth
seq-new
60 100 1 480 note 0 seq-add
64 100 1 480 note 480 seq-add
67 100 1 480 note 960 seq-add
```

### seq-show

Display all events in current sequence.

```forth
seq-new
0 60 100 480 seq-note
480 64 100 480 seq-note
seq-show
\ Output:
\ Sequence 0: 4 events, bpm=120
\   t=   0 ON  ch=1 d1= 60 d2=100
\   t= 480 OFF ch=1 d1= 60 d2=  0
\   t= 480 ON  ch=1 d1= 64 d2=100
\   t= 960 OFF ch=1 d1= 64 d2=  0
```

### seq-length ( -- n )

Get number of events in current sequence.

```forth
seq-length .
\ Output: 4
```

### seq-play

Play the current sequence through MIDI output.

```forth
midi-virtual
seq-new
0 60 100 480 seq-note
480 62 100 480 seq-note
960 64 100 480 seq-note
seq-play
midi-close
```

### seq-clear

Remove all events from current sequence.

```forth
seq-clear
seq-length .
\ Output: 0
```

### seq-transpose ( semitones -- )

Transpose all notes in current sequence.

```forth
seq-new
0 60 100 480 seq-note
480 62 100 480 seq-note

5 seq-transpose           \ up a fourth
seq-show
\ Notes are now 65 and 67 instead of 60 and 62

-12 seq-transpose         \ down an octave
```

### seq-reverse

Reverse the timing of the sequence (retrograde).

```forth
seq-new
0 60 100 480 seq-note       \ C first
480 64 100 480 seq-note     \ E second
960 67 100 480 seq-note     \ G third

seq-reverse                 \ now plays G, E, C
seq-play
```

### seq-stretch ( percentage -- )

Stretch or compress timing. 100 = normal, 200 = double length, 50 = half length.

```forth
seq-new
0 60 100 480 seq-note
480 64 100 480 seq-note

200 seq-stretch             \ double the duration
seq-show
\ Times are now 0 and 960 instead of 0 and 480

50 seq-stretch              \ back to original (half of doubled)
```

---

## Note Names

Push MIDI note numbers. Default octave is 4 (middle C).

```forth
C .     \ Output: 60
D .     \ Output: 62
E .     \ Output: 64
F .     \ Output: 65
G .     \ Output: 67
A .     \ Output: 69
B .     \ Output: 71
```

### octave ( note octave-number -- note )

Change the octave. Octave 4 is middle C, octave 0 is the lowest.

```forth
C 3 octave .    \ Output: 48  (C3)
C 4 octave .    \ Output: 60  (C4, middle C)
C 5 octave .    \ Output: 72  (C5)
A 2 octave .    \ Output: 45  (A2)
```

---

## Duration Constants

Tick values for common note durations (480 ticks = 1 quarter note).

```forth
whole .         \ Output: 1920
half .          \ Output: 960
quarter .       \ Output: 480
eighth .        \ Output: 240
sixteenth .     \ Output: 120
```

Use with packed notes:

```forth
midi-virtual
C 100 1 quarter note note!      \ quarter note C
D 100 1 eighth note note!       \ eighth note D
E 100 1 eighth note note!       \ eighth note E
F 100 1 half note note!         \ half note F
midi-close
```

---

## Chord Builders

Build chords from a root note. Push 3 or 4 pitches onto the stack.

### major ( root -- p1 p2 p3 )

Major triad (root, major third, perfect fifth).

```forth
C major .s
\ Output: <3> 60 64 67

60 major .s
\ Output: <3> 60 64 67
```

### minor ( root -- p1 p2 p3 )

Minor triad (root, minor third, perfect fifth).

```forth
A minor .s
\ Output: <3> 69 72 76
```

### dim ( root -- p1 p2 p3 )

Diminished triad (root, minor third, diminished fifth).

```forth
B dim .s
\ Output: <3> 71 74 77
```

### aug ( root -- p1 p2 p3 )

Augmented triad (root, major third, augmented fifth).

```forth
C aug .s
\ Output: <3> 60 64 68
```

### dom7 ( root -- p1 p2 p3 p4 )

Dominant 7th chord (root, major third, perfect fifth, minor seventh).

```forth
G dom7 .s
\ Output: <4> 67 71 74 77
```

### maj7 ( root -- p1 p2 p3 p4 )

Major 7th chord (root, major third, perfect fifth, major seventh).

```forth
C maj7 .s
\ Output: <4> 60 64 67 71
```

### min7 ( root -- p1 p2 p3 p4 )

Minor 7th chord (root, minor third, perfect fifth, minor seventh).

```forth
D min7 .s
\ Output: <4> 62 65 69 72
```

---

## Chord Playback

### play-chord ( p1 p2 ... pn velocity duration count -- )

Play multiple notes simultaneously (blocking).

```forth
midi-virtual

\ Play C major chord for a half note
C major 100 half 3 play-chord

\ Play G7 chord for a quarter note
G dom7 80 quarter 4 play-chord

midi-close
```

### chord>seq ( p1 p2 ... pn velocity duration time count -- )

Add a chord to the current sequence.

```forth
seq-new

\ C major at tick 0
C major 100 half 0 3 chord>seq

\ F major at tick 960
F major 100 half 960 3 chord>seq

\ G major at tick 1920
G major 100 half 1920 3 chord>seq

\ C major at tick 2880
C major 100 whole 2880 3 chord>seq

midi-virtual
seq-play
midi-close
```

### arp>seq ( p1 p2 ... pn velocity note-dur spacing start-time count -- )

Add an arpeggio (notes played one after another) to the sequence.

```forth
seq-new

\ Arpeggiate C major: each note is an eighth, spaced by eighths
C major 100 eighth eighth 0 3 arp>seq

seq-show
\ Output:
\ Sequence 0: 6 events, bpm=120
\   t=   0 ON  ch=1 d1= 60 d2=100
\   t= 240 OFF ch=1 d1= 60 d2=  0
\   t= 240 ON  ch=1 d1= 64 d2=100
\   t= 480 OFF ch=1 d1= 64 d2=  0
\   t= 480 ON  ch=1 d1= 67 d2=100
\   t= 720 OFF ch=1 d1= 67 d2=  0

midi-virtual
seq-play
midi-close
```

---

## Stack Operations

```forth
5 3 + .           \ Output: 8
10 4 - .          \ Output: 6
6 7 * .           \ Output: 42
20 4 / .          \ Output: 5

1 2 3 .s          \ Output: <3> 1 2 3
dup .s            \ Output: <4> 1 2 3 3
drop .s           \ Output: <3> 1 2 3
swap .s           \ Output: <3> 1 3 2
over .s           \ Output: <4> 1 3 2 3
rot .s            \ Output: <4> 3 2 3 1
```

---

## Complete Examples

### Simple Melody

```forth
midi-virtual
120 bpm!

60 100 1 quarter note note!     \ C
62 100 1 quarter note note!     \ D
64 100 1 quarter note note!     \ E
60 100 1 half note note!        \ C (held longer)

midi-close
```

### Chord Progression (I-IV-V-I)

```forth
midi-virtual
100 bpm!
seq-new

C major 100 half 0 3 chord>seq
F major 100 half 960 3 chord>seq
G major 100 half 1920 3 chord>seq
C major 100 whole 2880 3 chord>seq

seq-play
midi-close
```

### Arpeggiated Chords

```forth
midi-virtual
140 bpm!
seq-new

C maj7 90 eighth eighth 0 4 arp>seq
A min7 90 eighth eighth 960 4 arp>seq
D min7 90 eighth eighth 1920 4 arp>seq
G dom7 90 eighth eighth 2880 4 arp>seq

seq-play
midi-close
```

### Bass Line with Chords

```forth
midi-virtual
seq-new

\ Bass notes on channel 2
0 2 36 100 quarter seq-note-ch      \ C2
480 2 36 100 quarter seq-note-ch
960 2 41 100 quarter seq-note-ch    \ F2
1440 2 41 100 quarter seq-note-ch
1920 2 43 100 quarter seq-note-ch   \ G2
2400 2 43 100 quarter seq-note-ch

\ Chords on channel 1 (added to same sequence)
C major 80 half 0 3 chord>seq
F major 80 half 960 3 chord>seq
G major 80 half 1920 3 chord>seq

seq-play
midi-close
```

### Transform and Replay

```forth
midi-virtual
seq-new

\ Original phrase
0 60 100 quarter seq-note
480 62 100 quarter seq-note
960 64 100 quarter seq-note
1440 67 100 quarter seq-note

seq-play                    \ play original
1000 ms

7 seq-transpose             \ up a fifth
seq-play                    \ play transposed
1000 ms

seq-reverse                 \ backwards
seq-play                    \ play reversed
1000 ms

midi-close
```

---

## Architecture

- **midi_forth.c** (~1300 lines)
  - Layer 1: libremidi C API integration
  - Layer 2: 32-bit packed note format
  - Layer 3: Array-based sequence storage (256 events/seq, 64 sequences)
  - Layer 4: Pattern DSL (note names, durations, chord builders)

- **Dependencies**: libremidi v5.3.1 (auto-built from `thirdparty/libremidi/`)
