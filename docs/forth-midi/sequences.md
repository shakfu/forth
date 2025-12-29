# Sequence System

The sequence system allows building MIDI patterns that can be played back, transformed, and manipulated before playback. Unlike the immediate-play notation (`c4,`), sequences store events with timing information.

## Overview

Sequences store MIDI events (note-on, note-off, CC) with tick-based timing. They can be:

- Built up note by note
- Transformed (transpose, reverse, stretch)
- Played back at any tempo

## Basic Usage

### Creating and Playing a Sequence

```forth
seq-new             \ Create new sequence, returns id (e.g., 0)
drop                \ Discard the id (it's auto-selected)

\ Add notes: time pitch velocity duration
0 60 80 480 seq-note        \ C4 at tick 0
480 64 80 480 seq-note      \ E4 at tick 480
960 67 80 480 seq-note      \ G4 at tick 960

seq-play            \ Play the sequence
```

### Timing

Timing uses ticks, not milliseconds. Constants are provided:

| Word | Ticks | Description |
|------|-------|-------------|
| `quarter` | 480 | Quarter note |
| `half` | 960 | Half note |
| `whole` | 1920 | Whole note |
| `eighth` | 240 | Eighth note |
| `sixteenth` | 120 | Sixteenth note |

The default tempo is 120 BPM. Change with:

```forth
140 bpm!            \ Set tempo to 140 BPM
bpm@                \ Get current tempo
```

## Word Reference

### Sequence Management

| Word | Stack | Description |
|------|-------|-------------|
| `seq-new` | `( -- id )` | Create new sequence, select it, push id |
| `seq` | `( id -- )` | Select sequence by id |
| `seq@` | `( -- id )` | Get current sequence id |
| `seq-clear` | `( -- )` | Clear all events from current sequence |
| `seq-length` | `( -- n )` | Get number of events in sequence |

### Adding Events

| Word | Stack | Description |
|------|-------|-------------|
| `seq-note` | `( time pitch vel dur -- )` | Add note at time (uses default channel) |
| `seq-note-ch` | `( time ch pitch vel dur -- )` | Add note with specific channel |
| `seq-add` | `( packed-note time -- )` | Add packed note at time |

### Playback and Display

| Word | Stack | Description |
|------|-------|-------------|
| `seq-play` | `( -- )` | Play current sequence (blocking) |
| `seq-show` | `( -- )` | Print all events in sequence |

### Transformations

| Word | Stack | Description |
|------|-------|-------------|
| `seq-transpose` | `( semitones -- )` | Transpose all notes |
| `seq-reverse` | `( -- )` | Reverse timing (play backwards) |
| `seq-stretch` | `( factor -- )` | Scale timing (100=normal, 200=double, 50=half) |

## Examples

### Simple Melody

```forth
midi-open
seq-new drop

\ Build a scale
0 60 80 quarter seq-note
quarter 62 80 quarter seq-note
quarter 2 * 64 80 quarter seq-note
quarter 3 * 65 80 quarter seq-note
quarter 4 * 67 80 half seq-note

seq-play
```

### Using Note Duration Constants

```forth
seq-new drop

\ Using stack calculations for timing
0 60 80 quarter seq-note
quarter dup 62 80 quarter seq-note
2 * dup 64 80 quarter seq-note
drop quarter 3 * 65 80 half seq-note

seq-play
```

### Transpose and Replay

```forth
seq-play            \ Play original
5 seq-transpose     \ Transpose up a fourth
seq-play            \ Play transposed
-5 seq-transpose    \ Transpose back
```

### Time Stretching

```forth
seq-play            \ Normal speed
200 seq-stretch     \ Double all times (half speed)
seq-play            \ Play slower
50 seq-stretch      \ Halve all times (back to normal, then double speed)
seq-play            \ Play faster
```

### Reverse Playback

```forth
seq-play            \ Play forward
seq-reverse         \ Reverse timing
seq-play            \ Play backward
seq-reverse         \ Reverse again (back to original)
```

### Inspect Sequence

```forth
seq-show
```

Output:

```text
Sequence 0: 6 events, bpm=120
  t=   0 ON  ch=1 d1= 60 d2= 80
  t= 480 OFF ch=1 d1= 60 d2=  0
  t= 480 ON  ch=1 d1= 64 d2= 80
  t= 960 OFF ch=1 d1= 64 d2=  0
  ...
```

### Using Chord Builders with Sequences

```forth
seq-new drop

\ Build chord notes
60 major            \ Pushes 60 64 67
80 quarter 0 3 chord>seq    \ Add 3-note chord at time 0

67 minor            \ Pushes 67 70 74
80 quarter quarter 3 chord>seq  \ Add at time=quarter

seq-play
```

### Arpeggios

```forth
seq-new drop

60 major                    \ C major triad
80 eighth eighth 0 3 arp>seq    \ Arpeggiate: vel, note-dur, spacing, start-time, count

seq-play
```

## Packed Notes

For more control, use the packed note system:

```forth
\ Create packed note: pitch vel ch dur -> packed
60 80 1 quarter note        \ C4, vel 80, ch 1, quarter note

\ Add to sequence
dup 0 seq-add               \ Add at time 0
quarter seq-add             \ Add another at time=quarter (using dup)
```

Extract components:

```forth
60 80 1 quarter note        \ Create packed note
dup pitch@ .                \ Print pitch (60)
dup vel@ .                  \ Print velocity (80)
dup ch@ .                   \ Print channel (1)
dur@ .                      \ Print duration (480)
```

## Multiple Sequences

```forth
\ Create two sequences
seq-new                     \ Returns 0
seq-new                     \ Returns 1 (now selected)

\ Add to sequence 1
0 60 80 quarter seq-note

\ Switch to sequence 0
0 seq
0 72 80 quarter seq-note

\ Play sequence 0
seq-play

\ Play sequence 1
1 seq seq-play
```

## Tips

1. **Blocking playback**: `seq-play` blocks until done. For the REPL this is fine, but for complex arrangements you may want to use the immediate notation instead.

2. **Events are sorted**: Events are automatically sorted by time before playback.

3. **Note-off handling**: `seq-note` automatically creates both note-on and note-off events.

4. **Tempo affects playback**: Changing `bpm!` affects how tick durations translate to real time.

5. **Transform order matters**: `seq-reverse` followed by `seq-transpose` gives different results than the reverse order.
