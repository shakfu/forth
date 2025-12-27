# Sequence Data Structures

Analysis of possible data structures for representing MIDI sequences in MIDI Forth.

## Design Considerations

- How to represent individual events (notes, CCs, etc.)
- How to store sequences of events
- How to enable transformations (transpose, invert, retrograde, etc.)
- How to support polyphony and multiple tracks
- Memory model: stack vs heap allocation
- Live coding: ability to modify sequences while playing

---

## Option 1: Event as Packed Integer

Encode a note event in a single 32-bit value:

```text
Bits 0-6:   pitch (0-127)
Bits 7-13:  velocity (0-127)
Bits 14-17: channel (0-15)
Bits 18-31: duration in ticks (0-16383)
```

**Proposed words:**

```forth
60 100 1 480 note>packed    \ pitch vel ch dur -- packed
dup pitch@ .                \ extract pitch
12 transpose                \ shift pitch up an octave
```

**Pros:**

- Fits on stack naturally
- Simple transformations (bit manipulation)
- Cache-friendly
- No memory management

**Cons:**

- Limited duration range (max ~16k ticks)
- No CC or other event types
- Rigid format, hard to extend

---

## Option 2: Heap-Allocated Event Structs

Store events in a memory region, push handles/pointers to stack:

```c
typedef struct {
    uint16_t time;      // tick offset
    uint8_t  type;      // note-on, note-off, cc, etc.
    uint8_t  channel;
    uint8_t  data1;     // pitch or cc number
    uint8_t  data2;     // velocity or cc value
} MidiEvent;  // 6 bytes
```

**Proposed words:**

```forth
event-new ( time type ch d1 d2 -- event-handle )
event-time@ ( event -- time )
event-pitch@ ( event -- pitch )
event-free ( event -- )

\ Example: create note-on at tick 0
0 NOTE-ON 1 60 100 event-new
```

**Pros:**

- Flexible, supports all MIDI message types
- Extensible to MIDI 2.0, sysex, etc.
- Natural event representation

**Cons:**

- Requires memory management
- Indirection overhead
- Need to track allocations

---

## Option 3: Sequence as Linked List

Each event contains pointer to next event:

```c
typedef struct Event {
    uint16_t time;
    uint8_t  type, channel, data1, data2;
    struct Event* next;
} Event;
```

**Proposed words:**

```forth
seq-new ( -- seq )
seq-add ( seq event -- seq )
seq-head ( seq -- event )
seq-tail ( seq -- seq )
seq-cons ( event seq -- seq )
```

**Pros:**

- Easy insert/delete at any position
- Natural recursive structure for Forth
- No reallocation needed

**Cons:**

- Poor cache locality
- O(n) to access by index
- More memory per event (pointer overhead)

---

## Option 4: Sequence as Array/Vector

Contiguous block of events with length:

```c
typedef struct {
    int capacity;
    int length;
    MidiEvent* events;  // sorted by time
} Sequence;
```

**Proposed words:**

```forth
seq-new ( -- seq )
seq-append ( seq time type ch d1 d2 -- )
seq-length ( seq -- n )
seq-get ( seq index -- time type ch d1 d2 )
seq-play ( seq -- )
seq-transpose ( seq semitones -- seq )
seq-reverse ( seq -- seq )
```

**Pros:**

- O(1) random access
- Good cache locality for playback
- Easy to sort by time
- Simple iteration

**Cons:**

- O(n) insert in middle
- Needs reallocation when growing
- Fixed event size

---

## Option 5: Pattern-Based (Live Coding Friendly)

Higher-level abstraction where patterns are generators that produce events:

```forth
\ Define patterns as words that push note data
: C-MAJ   60 64 67 ;           \ pitches for C major chord
: ARPEG   0 , 240 , 480 , ;    \ timing offsets

\ Play chord: all notes at once, duration 480
1 100 480 C-MAJ chord

\ Play arpeggio: notes at different times
1 100 240 C-MAJ ARPEG arpeggio
```

**Alternative syntax:**

```forth
\ Pattern literal syntax
pat[ 0 60 100 480 | 0 64 100 480 | 0 67 100 480 ]pat
\ Each entry: time pitch velocity duration
```

**Pros:**

- Composable and reusable
- Musical abstractions (chords, scales, arpeggios)
- Good for live coding - modify pattern, hear changes
- Compact representation

**Cons:**

- More complex implementation
- Patterns need interpretation/compilation step
- Less direct control over individual events

---

## Option 6: Hybrid - Event Pool + Sequence Index

Separate storage from ordering:

```c
MidiEvent event_pool[MAX_EVENTS];   // flat array of all events
int free_list;                       // head of free event list

typedef struct {
    int* indices;    // indices into event_pool, sorted by time
    int length;
    int capacity;
} Sequence;
```

**Proposed words:**

```forth
pool-alloc ( -- event-index )
pool-free ( event-index -- )
seq-new ( -- seq )
seq-add-event ( seq event-index -- )
seq-clone ( seq -- seq2 )           \ shallow copy, shares events
seq-deep-clone ( seq -- seq2 )      \ copies events too
```

**Pros:**

- Events can belong to multiple sequences
- Easy to create variations without duplicating data
- Efficient cloning for live manipulation
- Pool allocation is fast

**Cons:**

- Two-level indirection
- More bookkeeping
- Need to track reference counts or accept leaks

---

## Comparison Matrix

| Approach | Stack-friendly | Polyphony | Transforms | Live Coding | Complexity |
|----------|---------------|-----------|------------|-------------|------------|
| Packed Integer | Yes | Limited | Easy | Medium | Low |
| Heap Events | No | Yes | Medium | Medium | Medium |
| Linked List | Partial | Yes | Medium | Good | Medium |
| Array/Vector | No | Yes | Easy | Medium | Low |
| Pattern-Based | Yes | Yes | Easy | Excellent | High |
| Hybrid Pool | No | Yes | Easy | Excellent | High |

---

## Recommended Approach

Given the goals of polyphonic sequences, live coding, and transformations:

### Phase 1: Packed Integers (Notes Only)

Start simple with packed note representation:

- Works with existing stack
- Sufficient for basic melodies and chords
- Easy transformations via bit manipulation

```forth
\ Create and transform notes
60 100 1 480 pack-note    \ C4, vel 100, ch 1, dur 480
12 transpose              \ becomes C5
```

### Phase 2: Heap-Allocated Sequences (Array-Based)

Add sequence storage in a memory region:

- Sequence handles on stack
- Array storage for events
- Basic operations: append, play, transform

```forth
seq-new                   \ create empty sequence
0 60 100 480 seq-note     \ add C4 at tick 0
0 64 100 480 seq-note     \ add E4 at tick 0
0 67 100 480 seq-note     \ add G4 at tick 0
seq-play                  \ play the chord
```

### Phase 3: Pattern DSL

Build musical abstractions on top:

- User-defined words generate sequences
- Chord/scale/arpeggio helpers
- Live-friendly pattern manipulation

```forth
: C-MAJOR ( seq -- seq ) 60 note 64 note 67 note ;
: QUARTER ( -- dur ) 480 ;

seq-new C-MAJOR QUARTER seq-play
```

---

## Implementation Notes

### Memory Layout

For Phase 2, suggested memory organization:

```c
#define MAX_EVENTS 4096
#define MAX_SEQUENCES 64

MidiEvent event_heap[MAX_EVENTS];
int event_heap_top = 0;

Sequence sequences[MAX_SEQUENCES];
int sequence_count = 0;
```

### Tick Resolution

Standard MIDI files use 480 or 960 ticks per quarter note (PPQ). Suggest:

- Default: 480 PPQ
- Tempo stored separately (microseconds per quarter note)
- Conversion words: `ms>ticks`, `ticks>ms`, `bpm!`

### Playback Model

For live coding, maintain:

- Current tick position
- Tempo/BPM
- Playing flag
- Event index (next event to play)

Playback runs in background, checks current time against next event time.
