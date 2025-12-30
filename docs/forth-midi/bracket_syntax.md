# Bracket Sequence Syntax

**Status:** Implemented
**Author:** team-forth-midi
**Date:** 2025-12-30

## Summary

The bracket syntax `[ ... ]` creates sequences of notes as first-class values. The closing bracket `]` creates a sequence object; the comma `,` plays it. This replaces the need for comma-per-note notation while enabling sequence manipulation before playback.

Sequences can hold both musical elements (pitches, chords, rests, dynamics, durations) and plain numbers for use with generative operations.

## Motivation

### Current Syntax Pain Points

1. **Trailing comma requirement:**
   ```forth
   c4, e4, g4,           \ Trailing comma feels redundant
   ```

2. **Visual noise for melodies:**
   ```forth
   c4, +2, +2, +1, +2, +2, +2, -12,    \ Hard to read
   ```

3. **Inconsistent mental model:**
   - `,` triggers playback
   - But you need it after every note, even the last one
   - Easy to forget, causes silent failures

### Proposed Solution

```forth
[ c4 +2 +2 +1 +2 +2 +2 -12 ],   \ Clean, readable sequence
```

The bracket clearly delineates a musical phrase. `]` creates the sequence object, `,` plays it.

## Current Bracket Usage

Currently `[ ]` is used for explicit parameter grouping:

```forth
[1 c4 100 500],        \ channel=1, pitch=c4, velocity=100, duration=500
[(c4 e4 g4) 1 80 500], \ chord with explicit params
```

This proposal repurposes `[ ]` for sequences. See [Backwards Compatibility](#backwards-compatibility) for migration path.

## Proposed Syntax

### Basic Sequences

```forth
[ c4 e4 g4 ],          \ Play C4, E4, G4 sequentially
[ c4 d4 e4 f4 g4 ],    \ Five-note melody
```

- `[ ... ]` creates a sequence object (pushed to stack)
- `,` plays the sequence

Each element plays with current default duration, velocity, and channel.

### Sequences as Values

Sequences are first-class values that can be manipulated before playback:

```forth
[ c4 e4 g4 ]           \ Create sequence (on stack, not played)
: arpeggio ;           \ Assign to word

arpeggio,              \ Play it
arpeggio reverse,      \ Reverse and play
arpeggio 12 transpose, \ Transpose up an octave and play

[ c4 e4 ] [ g4 c5 ] concat,   \ Concatenate and play
```

### Relative Intervals

```forth
[ c4 +2 +2 +1 +2 +2 +2 +1 ],   \ C major scale ascending
[ g4 -2 -2 -1 -2 -2 -2 -1 ],   \ G major scale descending
```

Intervals are calculated from the previous pitch in the sequence.

### Dynamics Within Sequences

Dynamics apply to subsequent notes until changed:

```forth
[ mf c4 e4 ff g4 p c5 ],
```

Equivalent to:
- `mf` sets velocity to 80
- `c4` plays at velocity 80
- `e4` plays at velocity 80
- `ff` sets velocity to 112
- `g4` plays at velocity 112
- `p` sets velocity to 49
- `c5` plays at velocity 49

### Rests

The `r` word inserts a rest (silence for default duration):

```forth
[ c4 r e4 r g4 ],      \ C4, rest, E4, rest, G4
[ c4 e4 r r g4 ],      \ C4, E4, two rests, G4
```

### Chords Within Sequences

Parentheses still group simultaneous notes:

```forth
[ c4 (e4 g4) c5 ],     \ C4 melody, E4+G4 chord, C5 melody
[ (c4 e4 g4) (f4 a4 c5) (g4 b4 d5) ],  \ Three chords in sequence
```

### Chord Builders Within Sequences

```forth
[ c4 major (f4 a4 c5) g4 dom7 ],
```

Interpretation:
- `c4 major` pushes C4, E4, G4 onto stack, plays as chord
- `(f4 a4 c5)` plays as chord
- `g4 dom7` pushes G4, B4, D5, F5, plays as chord

### Articulation

Articulation suffixes work as expected:

```forth
[ c4. e4. g4> c5- ],   \ Staccato, staccato, accent, tenuto
```

### Nested Brackets

Nested brackets are **not allowed**. Use word definitions for complex structures:

```forth
\ Not allowed:
[ c4 [ e4 g4 ] c5 ],   \ ERROR

\ Instead:
: phrase e4 g4 ;
[ c4 phrase c5 ],      \ OK - word expands inline
```

### Duration Override

To change duration mid-sequence, use `dur!`:

```forth
[ 250 dur! c4 e4 500 dur! g4 c5 ],
```

Or use named durations:

```forth
[ eighth c4 e4 quarter g4 half c5 ],
```

Where duration words set the default for subsequent notes.

## Sequences vs Single Notes

| Syntax | Meaning |
|--------|---------|
| `[ c4 e4 g4 ]` | Create sequence object (on stack) |
| `[ c4 e4 g4 ],` | Create sequence and play it |
| `c4,` | Play single note |
| `c4 vel=100,` | Play single note with params |

Both sequences and single notes use `,` to trigger playback.

### Explicit Params Replacement: Named Parameters

The old explicit params syntax `[1 c4 100 500],` is deprecated. Use named parameters instead:

```forth
\ Old explicit params (deprecated)
[1 c4 100 500],
[(c4 e4 g4) 1 80 500],

\ New named parameters
c4 ch=1 vel=100 dur=500,
(c4 e4 g4) ch=1 vel=80 dur=500,
```

**Named Parameter Syntax:**

| Parameter | Syntax | Example |
|-----------|--------|---------|
| Channel | `ch=N` | `ch=2` |
| Velocity | `vel=N` | `vel=100` |
| Duration | `dur=N` | `dur=500` |

**Benefits:**
- Self-documenting (no memorizing parameter order)
- Partial specification (only override what you need)
- Order-independent
- Familiar `name=value` syntax

**Examples:**

```forth
c4 vel=100,                     \ Just velocity
c4 dur=250,                     \ Just duration
c4 vel=100 dur=250,             \ Velocity and duration
c4 ch=2 vel=100 dur=500,        \ All three
(c4 e4 g4) vel=80 dur=half,     \ Chord with params
mf c4. vel=110,                 \ Combines with dynamics and articulation
```

**Context variables** use `:=` for persistent assignment:

```forth
ch:=2 vel:=100 dur:=250         \ Set defaults (persistent)
c4, e4, g4,                     \ Uses defaults
c4 vel=127,                     \ Override just this note (one-shot)
```

**Comparison:**

| Syntax | Scope | Example |
|--------|-------|---------|
| `vel=100` | One-shot (next note only) | `c4 vel=100,` |
| `vel:=100` | Persistent (until changed) | `vel:=100 c4, e4,` |

### Extended Parameters

The `=` and `:=` syntax extends to all musical parameters:

**Note Parameters:**

| Parameter | One-shot | Persistent | Range | Description |
|-----------|----------|------------|-------|-------------|
| Channel | `ch=2` | `ch:=2` | 1-16 | MIDI channel |
| Velocity | `vel=100` | `vel:=100` | 0-127 | Note velocity |
| Duration | `dur=500` | `dur:=500` | ms | Note duration |
| Gate | `gate=80` | `gate:=80` | 1-100 | Note length as % of duration |

**Global Parameters (persistent only):**

| Parameter | Syntax | Old Syntax | Description |
|-----------|--------|------------|-------------|
| Tempo | `bpm:=120` | `120 bpm!` | Beats per minute |
| Program | `prog:=25` | `1 25 pc` | Instrument (0-127) |
| Pan | `pan:=64` | `1 10 64 cc` | Stereo position (0-127, 64=center) |

**Generic CC:**

| Syntax | Description |
|--------|-------------|
| `cc1:=64` | Set CC 1 (mod wheel) to 64 |
| `cc10:=0` | Set CC 10 (pan) to 0 (left) |
| `cc64:=127` | Set CC 64 (sustain) to 127 (on) |
| `cc7:=100` | Set CC 7 (volume) to 100 |

Common CC numbers:
- `cc1` - Modulation wheel
- `cc7` - Volume
- `cc10` - Pan
- `cc11` - Expression
- `cc64` - Sustain pedal

**Examples:**

```forth
\ Full setup
bpm:=120 prog:=25 ch:=1
vel:=80 dur:=quarter gate:=85
pan:=64 cc1:=0

\ Play melody
[ c4 e4 g4 c5 ],

\ Change instrument and pan
prog:=0 pan:=32                  \ Piano, pan left
[ c5 e5 g5 ],

\ One-shot overrides
c4 vel=127 gate=50,              \ Loud staccato
e4,                              \ Back to defaults

\ Sustain pedal
cc64:=127                        \ Pedal down
[ c4 e4 g4 ],
cc64:=0                          \ Pedal up
```

**Gate behavior:**

```forth
gate:=100                        \ Full legato
[ c4 e4 g4 ],                    \ Notes connect seamlessly

gate:=50                         \ Staccato
[ c4 e4 g4 ],                    \ Notes play 50% of duration

gate:=75                         \ Slight separation
[ c4 e4 g4 ],                    \ Natural phrasing
```

## Grammar

```ebnf
sequence      = "[" seq-element* "]" ;
seq-element   = pitch | interval | chord | rest | dynamic | duration | articulated-pitch ;

pitch         = note-name octave ;
interval      = ("+" | "-") number ;
chord         = "(" pitch+ ")" | pitch chord-builder ;
rest          = "r" ;
dynamic       = "ppp" | "pp" | "p" | "mp" | "mf" | "f" | "ff" | "fff" ;
duration      = "whole" | "half" | "quarter" | "eighth" | "sixteenth" | number "dur!" ;
articulated-pitch = pitch ("." | ">" | "-") ;
chord-builder = "major" | "minor" | "dim" | "aug" | "dom7" | "maj7" | "min7" ;

(* Named parameters for note playback - one-shot *)
named-param   = param-name "=" param-value ;
param-name    = "ch" | "vel" | "dur" | "gate" ;
param-value   = number | duration ;

(* Persistent context assignment *)
context-assign = context-name ":=" param-value ;
context-name  = "ch" | "vel" | "dur" | "gate"            (* note params *)
              | "bpm" | "prog" | "pan"                   (* global params *)
              | "cc" number ;                            (* generic CC *)

(* Note with optional named params *)
note-play     = (pitch | chord) named-param* "," ;
```

## Semantics

### Execution Model

**When `]` is encountered:**
1. Collect all elements since matching `[`
2. Create sequence object containing these elements
3. Push sequence object onto stack

**When `,` is applied to a sequence:**
1. Pop sequence from stack
2. For each element in order:
   - If dynamic: update current velocity
   - If duration word: update current duration
   - If rest: sleep for current duration
   - If pitch/interval: play note with current params, sleep for duration
   - If chord: play all notes simultaneously, sleep for duration

### State Within Sequences

Sequences have their own local state for:
- Current pitch (for relative intervals)
- Current velocity (for dynamics)
- Current duration (for duration words)

Channel is inherited from global state and not locally modifiable within sequences.

### Sequences Are Values

Sequences are first-class values on the stack:

```forth
[ c4 e4 g4 ]               \ Create sequence (on stack)
dup,                       \ Play it
,                          \ Play it again

[ c4 e4 g4 ] 4 times,      \ Repeat sequence 4 times

[ c4 e4 g4 ]
: melody ;                 \ Save to word
melody,                    \ Play anytime
```

## Examples

### Before and After

**Simple melody:**
```forth
\ Before
c4, d4, e4, f4, g4, a4, b4, c5,

\ After
[ c4 d4 e4 f4 g4 a4 b4 c5 ],
```

**Melody with dynamics:**
```forth
\ Before
mf c4, e4, ff g4, p c5,

\ After
[ mf c4 e4 ff g4 p c5 ],
```

**Scale using intervals:**
```forth
\ Before
c4, +2, +2, +1, +2, +2, +2, +1,

\ After
[ c4 +2 +2 +1 +2 +2 +2 +1 ],
```

**Mixed melody and chords:**
```forth
\ Before
c4, e4, (g4 c5),

\ After
[ c4 e4 (g4 c5) ],
```

**With rests:**
```forth
\ Before
c4, r, e4, r, g4,

\ After
[ c4 r e4 r g4 ],
```

**Explicit parameters:**
```forth
\ Before
[1 c4 100 500],
[(c4 e4 g4) 2 80 1000],

\ After
c4 ch=1 vel=100 dur=500,
(c4 e4 g4) ch=2 vel=80 dur=1000,
```

**Partial parameter override:**
```forth
\ Before (not possible - had to specify all 4)
[1 c4 100 500],

\ After (just override what you need)
c4 vel=100,
c4 dur=250,
c4 ch=2,
```

### Complex Example

```forth
midi-open
120 bpm!

\ Define sequences (stored as values)
[ mf (c4 e4 g4) (f4 a4 c5) (g4 b4 d5) (c4 e4 g4) ]
: verse ;

[ p c5 d5 e5 ff g5 p e5 d5 c5 r ]
: melody ;

\ Play using sequences
: song
    verse,
    melody,
    verse,
    ff [ (c4 e4 g4 c5) ],   \ Final chord
;

song
midi-close
```

## Implementation Status

### Implemented Features

1. **Bracket sequences**: `[ ... ]` creates sequence objects as first-class values
2. **Sequence playback**: `,` plays sequences when on top of stack
3. **Musical elements in sequences**: pitches, chords `( )`, rests `r`, dynamics, durations
4. **Plain numbers in sequences**: for use with generative operations
5. **Generative operations on sequences**:
   - `shuffle` - Fisher-Yates shuffle on sequence elements
   - `reverse` - reverse sequence in place
   - `pick` - pick random element from sequence
   - `pick-n` - pick n random elements, returns new sequence
   - `invert` - invert pitches around axis
   - `arp-up-down` - create sequence with middle reversed appended
   - `random-walk` - outputs sequence instead of stack values
   - `drunk-walk` - takes scale sequence as input, outputs sequence
   - `weighted-pick` - pick from sequence with value/weight pairs

### Not Yet Implemented

1. Named parameters (`vel=100`, `ch:=2`) - still use Forth-style `100 vel!`
2. Gate parameter
3. Sequence concatenation (`concat`)
4. Sequence transpose operation (use `seq-transpose` on traditional sequences)

### Backwards Compatibility

The old explicit params syntax `[1 c4 100 500],` has been replaced by the sequence syntax. Use standard Forth-style parameter setting:

```forth
\ Set defaults
1 ch! 100 vel! 500 dur!

\ Play notes with those defaults
c4, e4, g4,

\ Or use sequences
[ c4 e4 g4 ],
```

## Implementation Notes

### Tokenizer Changes

- `[` starts sequence capture mode
- Tokens accumulated until `]`
- `]` creates sequence object, pushes to stack
- `,` pops sequence (or note) from stack and plays it
- `=` is treated as part of the token (not a separator)
  - `ch=2` is one token, not three

### Named Parameter Parsing

```c
// Parameter types
#define PARAM_CHANNEL  1
#define PARAM_VELOCITY 2
#define PARAM_DURATION 3
#define PARAM_GATE     4
#define PARAM_BPM      5
#define PARAM_PROGRAM  6
#define PARAM_PAN      7
#define PARAM_CC       8   // Generic CC, cc_number stored separately

#define SCOPE_ONESHOT    0   // vel=100  (next note only)
#define SCOPE_PERSISTENT 1   // vel:=100 (until changed)

// Parse parameter name, returns param type or 0 if unknown
// For CC, also sets cc_number (e.g., "cc64" -> PARAM_CC, cc_number=64)
int parse_param_name(const char* name, int name_len, int* cc_number) {
    *cc_number = -1;

    if (name_len == 2 && strncmp(name, "ch", 2) == 0) return PARAM_CHANNEL;
    if (name_len == 3 && strncmp(name, "vel", 3) == 0) return PARAM_VELOCITY;
    if (name_len == 3 && strncmp(name, "dur", 3) == 0) return PARAM_DURATION;
    if (name_len == 4 && strncmp(name, "gate", 4) == 0) return PARAM_GATE;
    if (name_len == 3 && strncmp(name, "bpm", 3) == 0) return PARAM_BPM;
    if (name_len == 4 && strncmp(name, "prog", 4) == 0) return PARAM_PROGRAM;
    if (name_len == 3 && strncmp(name, "pan", 3) == 0) return PARAM_PAN;

    // Check for ccN pattern (e.g., cc64, cc1, cc127)
    if (name_len >= 3 && strncmp(name, "cc", 2) == 0) {
        *cc_number = atoi(name + 2);
        if (*cc_number >= 0 && *cc_number <= 127) {
            return PARAM_CC;
        }
    }

    return 0;  // Unknown parameter
}

// Parse parameter assignment (both = and :=)
int parse_param_assign(const char* token, int* param_type, int* value,
                       int* scope, int* cc_number) {
    *cc_number = -1;

    // Check for := (persistent) first
    char* assign = strstr(token, ":=");
    if (assign) {
        *scope = SCOPE_PERSISTENT;
        int name_len = assign - token;
        const char* val_str = assign + 2;

        *param_type = parse_param_name(token, name_len, cc_number);
        if (*param_type == 0) return 0;

        *value = (*param_type == PARAM_DURATION)
            ? parse_duration(val_str)
            : atoi(val_str);
        return 1;
    }

    // Check for = (one-shot)
    char* eq = strchr(token, '=');
    if (eq) {
        *scope = SCOPE_ONESHOT;
        int name_len = eq - token;
        const char* val_str = eq + 1;

        *param_type = parse_param_name(token, name_len, cc_number);
        if (*param_type == 0) return 0;

        // Global params (bpm, prog, pan, cc) don't support one-shot
        if (*param_type >= PARAM_BPM) {
            printf("Warning: %.*s only supports := (persistent)\n",
                   name_len, token);
            *scope = SCOPE_PERSISTENT;
        }

        *value = (*param_type == PARAM_DURATION)
            ? parse_duration(val_str)
            : atoi(val_str);
        return 1;
    }

    return 0;  // Not a parameter assignment
}
```

### New Interpreter State

```c
// Sequence state
int in_sequence = 0;           // Currently inside [ ]
int sequence_start = -1;       // Stack position at [
int sequence_pitch = -1;       // Last pitch (for intervals)

// Persistent context (set with :=)
int ctx_channel = 1;           // ch:=N
int ctx_velocity = 80;         // vel:=N
int ctx_duration = 500;        // dur:=N
int ctx_gate = 100;            // gate:=N (percentage)
int ctx_bpm = 120;             // bpm:=N
int ctx_program = 0;           // prog:=N

// Pending one-shot params (set with =, cleared after use)
int pending_channel = -1;      // -1 = use context
int pending_velocity = -1;
int pending_duration = -1;
int pending_gate = -1;

// Helper to get effective value (pending if set, else context)
int effective_channel()  { return pending_channel >= 0 ? pending_channel : ctx_channel; }
int effective_velocity() { return pending_velocity >= 0 ? pending_velocity : ctx_velocity; }
int effective_duration() { return pending_duration >= 0 ? pending_duration : ctx_duration; }
int effective_gate()     { return pending_gate >= 0 ? pending_gate : ctx_gate; }

// Clear pending params after note plays
void clear_pending() {
    pending_channel = -1;
    pending_velocity = -1;
    pending_duration = -1;
    pending_gate = -1;
}
```

### Sequence Execution

```c
void execute_sequence(Token* tokens, int count) {
    int vel = default_velocity;
    int dur = default_duration;
    int last_pitch = -1;

    for (int i = 0; i < count; i++) {
        Token* t = &tokens[i];

        if (is_dynamic(t)) {
            vel = dynamic_to_velocity(t);
        } else if (is_duration(t)) {
            dur = parse_duration(t);
        } else if (is_rest(t)) {
            sleep_ms(dur);
        } else if (is_pitch(t)) {
            int pitch = parse_pitch(t);
            play_note(pitch, vel, dur);
            last_pitch = pitch;
        } else if (is_interval(t)) {
            int interval = parse_interval(t);
            int pitch = last_pitch + interval;
            play_note(pitch, vel, dur);
            last_pitch = pitch;
        } else if (is_chord(t)) {
            int* pitches = parse_chord(t);
            int count = chord_count(t);
            play_chord(pitches, vel, dur);
            last_pitch = pitches[count - 1];  // highest (top voice)
        }
    }
}
```

## Design Decisions

1. **Sequences are first-class values**
   - `[ c4 e4 g4 ]` creates a sequence object (pushed to stack)
   - `[ c4 e4 g4 ],` creates and plays the sequence
   - Can be assigned to words, transformed, and manipulated

2. **Interval base after chords: highest note (top voice)**
   - `[ (c4 e4 g4) +2 ],` plays chord then A4 (G4 + 2 semitones)
   - Rationale: Top voice typically carries melody; follows melodic contour naturally

3. **Empty sequences are valid no-ops**
   - `[ ],` is valid and does nothing when played

4. **Duration words set context directly (sticky)**
   - `[ quarter c4 e4 eighth g4 a4 ],` - c4/e4 as quarters, g4/a4 as eighths
   - Consistent with dynamics behavior (`mf c4 e4 ff g4`)
   - Reads naturally like sheet music notation

5. **Probability applies to next element**
   - `[ c4 e4 75% g4 ],` - 75% chance to play g4
   - Consistent with existing probability syntax

## Future Considerations

1. **Octave transposition parameter**
   - Potential syntax: `oct:=5` to set default octave for pitches without explicit octave
   - Would require syntax for octave-less pitches (e.g., `c, e, g,`)
   - Deferred; requires further design work

## Alternatives Considered

### Alternative A: Angle Brackets

```forth
< c4 e4 g4 >
```

Pros: No conflict with existing syntax
Cons: Less common, harder to type

### Alternative B: Implicit Sequence (EOL triggers)

```forth
c4, e4, g4     \ No trailing comma needed, EOL triggers last note
```

Pros: Minimal change
Cons: Doesn't help with visual grouping

### Alternative C: Sequence Word

```forth
seq{ c4 e4 g4 }
```

Pros: Explicit, no ambiguity
Cons: More verbose

### Alternative D: Bar Notation

```forth
| c4 e4 g4 |
```

Pros: Musical connotation (bar lines)
Cons: `|` already used for alternatives

## Conclusion

The bracket sequence syntax `[ ... ]` provides a cleaner, more readable way to express melodies and sequences. Sequences are first-class values that can be manipulated before playback with `,`.

Key benefits:
- Reduces visual noise (no comma per note inside sequences)
- Enables sequence manipulation (transpose, reverse, concat)
- Consistent model: `,` always means "play"
- Named parameters replace cryptic positional syntax

The main trade-off is deprecating the old explicit params syntax `[1 c4 100 500],`, replaced by the more readable `c4 ch=1 vel=100 dur=500,`.

## References

- Current forth-midi syntax: [syntax.md](syntax.md)
- API reference: [api-reference.md](api-reference.md)
