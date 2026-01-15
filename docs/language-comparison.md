# Language Comparison Guide

This guide helps you choose the right MIDI language implementation for your needs.

## Quick Comparison

| Feature | alda-midi | forth-midi | joy-midi | lua-midi | s7-midi | pktpy-midi | mhs-midi |
| --------- | ----------- | ------------ | ---------- | ---------- | --------- | ------------ | ---------- |
| **Language** | Alda | Forth-like | Joy | Lua 5.5 | Scheme (s7) | Python (PocketPy) | Haskell (MicroHs) |
| **Paradigm** | Declarative | Stack-based | Concatenative | Imperative | Functional/Lisp | Object-oriented | Pure functional |
| **Binary size** | ~300KB | ~200KB | ~150KB | ~400KB | ~350KB | ~500KB | ~1.5MB |
| **Startup time** | Instant | Instant | Instant | Instant | Instant | Instant | ~0.5s |
| **Learning curve** | Easy | Medium | Medium | Easy | Medium | Easy | Hard |
| **Notation** | Music notation | Concise DSL | Algebraic | Method calls | S-expressions | Method calls | DSL + combinators |

## Language Profiles

### alda-midi

**Best for:** Traditional music notation, quick sketching, musicians

```alda
piano:
(tempo 120)
c4 d e f | g2 g | a4 a a a | g2 r

violin:
o3 c1~1
```

**Strengths:**

- Familiar music notation (`c4` = C quarter note, `g2` = G half note)
- Part-based composition (piano:, violin:, etc.)
- 128 GM instruments with natural names
- Voices for polyphony within parts (V1:, V2:)
- Concurrent mode for layering parts in REPL
- Always non-blocking - REPL stays responsive
- Ties (`~`), dotted notes (`.`), chords (`c/e/g`)

**Weaknesses:**

- Less programmable than other languages
- No general-purpose programming features
- Limited to Alda language constructs

**Choose if:** You think in traditional music notation, want to sketch ideas quickly, or are a musician first and programmer second.

---

### forth-midi

**Best for:** Live coding, minimal syntax, experimentation

```forth
midi-open
c4, e4, g4,           \ Play C, E, G
(c4 e4 g4),           \ Play chord
: melody c4, e4, g4, ;
melody 4 times
```

**Strengths:**

- Most concise notation (`c4,` plays a note)
- Immediate feedback - every line executes instantly
- Stack-based operations for algorithmic composition
- Built-in probability (`75%`) and alternatives (`c4|e4`)
- Articulation suffixes (`.` staccato, `>` accent, `-` tenuto)
- Anonymous blocks with `{ ... } N *`

**Weaknesses:**

- Unfamiliar syntax for most programmers
- Stack manipulation can be confusing
- Limited data structures

**Choose if:** You want the most direct path from idea to sound, or you're comfortable with Forth/stack-based languages.

---

### joy-midi

**Best for:** Algebraic composition, functional transformations, concatenative programming

```joy
\ Notes are integers at parse time
[c d e f g] play              \ Play melody
[c e g] chord                 \ Play C major chord
[c d e] [7 +] map play        \ Transpose up a fifth

def pick == dup size rand swap rem at .
[c d e f g a b] pick play     \ Random note
```

**Strengths:**

- Notes become MIDI integers at parse time (`c` = 60, `c5` = 72)
- All Joy combinators work naturally with notes (`map`, `filter`, `fold`)
- Pure functional transformations on note lists
- User definitions with `def name == body .`
- Stack-based but more algebraic than Forth
- Minimal syntax - composition through concatenation

**Weaknesses:**

- No async/concurrent voices yet
- Smaller ecosystem than other languages
- Concatenative paradigm unfamiliar to most

**Choose if:** You appreciate functional programming, want algebraic note manipulation, or enjoy concatenative languages like Factor or Forth.

---

### lua-midi

**Best for:** Scripting, familiar syntax, rapid prototyping

```lua
local m = midi.open()
m:note(c4, mf, quarter)
m:chord(midi.major(c4), f, half)
m:arpeggio(midi.dom7(g4), mp, eighth)
m:close()
```

**Strengths:**

- Familiar syntax for most programmers
- Full Lua 5.5 standard library
- Object-oriented API with methods
- Easy string manipulation and tables
- Coroutines for concurrent patterns
- REPL with convenience functions (`n()`, `ch()`, `arp()`)

**Weaknesses:**

- More verbose than forth-midi
- No built-in probability syntax

**Choose if:** You know Lua or want a mainstream scripting language with good documentation.

---

### s7-midi

**Best for:** Algorithmic composition, Lisp enthusiasts, macro programming

```scheme
(define m (midi-open))
(midi-note m c4 mf quarter)
(midi-chord m (major c4) f half)
(times 4 (lambda () (midi-note m c4 mf quarter)))
(midi-close m)
```

**Strengths:**

- Full Scheme with macros and continuations
- Excellent for algorithmic/generative music
- First-class functions everywhere
- Pattern matching and list processing
- s7's C FFI for extending
- Minimal, elegant syntax once learned

**Weaknesses:**

- Parentheses-heavy syntax
- Steeper learning curve for non-Lispers
- Less mainstream documentation

**Choose if:** You appreciate Lisp/Scheme, want powerful macros, or do heavy algorithmic composition.

---

### pktpy-midi

**Best for:** Python users, data processing, ML integration

```python
import midi

with midi.open() as m:
    m.note(midi.c4, midi.mf, midi.quarter)
    m.chord(midi.major(midi.c4), midi.f, midi.half)
    for i in range(4):
        m.note(midi.c4 + i, midi.mf, midi.eighth)
```

**Strengths:**

- Python 3 syntax (subset via PocketPy)
- Context managers for resource handling
- List comprehensions and generators
- Familiar to data scientists
- Easy integration with algorithms

**Weaknesses:**

- PocketPy is a subset, not full CPython
- Slightly more verbose
- No NumPy/SciPy (embedded interpreter)

**Choose if:** You know Python and want familiar syntax, or you're prototyping ideas for a larger Python project.

---

### mhs-midi

**Best for:** Pure functional programming, type safety, composition

```haskell
import MidiPerform

main = do
    open
    note c4
    chord (major c4)
    melody [c4, e4, g4]
    times 4 (note c4)
    close
```

**Strengths:**

- Pure functional with IO separation
- Strong types catch errors at compile time
- Powerful Music DSL with combinators (`+:+`, `|||`)
- Deterministic generative functions
- Mathematical approach to music
- 55 built-in scales including microtonal

**Weaknesses:**

- Slowest startup (MicroHs compilation)
- Steepest learning curve
- Largest binary size
- Requires understanding monads for IO

**Choose if:** You love Haskell, want type safety, or prefer mathematical/compositional approaches.

---

## Feature Comparison

### Pitch Notation

| Language | Example | Notes |
| ---------- | --------- | ------- |
| alda-midi | `c4` `c#4` `db4` | Duration suffix (4=quarter) |
| forth-midi | `c4,` `C#4,` `Db4,` | Comma triggers, case-insensitive |
| joy-midi | `c` `c5` `c+` `c-` | Parse-time integers, octave/accidentals |
| lua-midi | `midi.c4` or `midi.note("C4")` | Constants or string parsing |
| s7-midi | `c4` `cs4` | Scheme symbols |
| pktpy-midi | `midi.c4` or `midi.note("C4")` | Constants or string parsing |
| mhs-midi | `c4` `cs4` | Haskell constants |

### Chord Building

| Language | Major Triad | Dominant 7th |
| ---------- | ------------- | -------------- |
| alda-midi | `c/e/g` | `c/e/g/b-` |
| forth-midi | `(c4 e4 g4),` | `(c4 e4 g4 bb4),` |
| joy-midi | `c major chord` or `[c e g] chord` | `c dom7 chord` |
| lua-midi | `midi.major(c4)` | `midi.dom7(c4)` |
| s7-midi | `(major c4)` | `(dom7 c4)` |
| pktpy-midi | `midi.major(c4)` | `midi.dom7(c4)` |
| mhs-midi | `major c4` | `dom7 c4` |

### Repetition

| Language | Play 4 times |
| ---------- | -------------- |
| alda-midi | `c4 c c c` or `[c4 c]*2` (limited) |
| forth-midi | `melody 4 times` or `{ c4, } 4 *` |
| joy-midi | `4 [[c] play] times` or `[c c c c] play` |
| lua-midi | `for i=1,4 do m:note(c4,mf,quarter) end` |
| s7-midi | `(times 4 (lambda () (midi-note m c4 mf quarter)))` |
| pktpy-midi | `for i in range(4): m.note(c4, mf, quarter)` |
| mhs-midi | `times 4 (note c4)` |

### Probability/Randomness

| Language | 50% chance | Random selection |
| ---------- | ------------ | ------------------ |
| alda-midi | N/A | N/A |
| forth-midi | `c4 50%,` | `c4\|e4\|g4,` |
| joy-midi | `rand 50 < [[c] play] [] ifte` | `[c e g] dup size rand swap rem at` |
| lua-midi | `if math.random() < 0.5 then ... end` | `midi.pick({c4,e4,g4})` |
| s7-midi | `(chance 50 ...)` | `(pick (list c4 e4 g4))` |
| pktpy-midi | `if random.random() < 0.5: ...` | `random.choice([c4,e4,g4])` |
| mhs-midi | `chance 50 (note c4)` | `pick seed [c4,e4,g4]` |

### Recording

All implementations support MIDI event recording:

| Language | Start | Stop | Save |
| ---------- | ------- | ------ | ------ |
| alda-midi | N/A | N/A | N/A (file-based) |
| forth-midi | `rec-midi` | `stop` | `save-midi file.4th` |
| joy-midi | N/A | N/A | N/A (planned) |
| lua-midi | `record_midi()` | `record_stop()` | `save_midi("file.lua")` |
| s7-midi | `(record-midi)` | `(record-stop)` | `(save-midi "file.scm")` |
| pktpy-midi | `midi.record_midi()` | `midi.record_stop()` | `midi.save_midi("file.py")` |
| mhs-midi | `midiRecordStart 120` | `midiRecordStop` | `midiRecordSave "file.hs"` |

---

## Use Case Recommendations

### Musicians / Quick Sketching

**Recommended:** alda-midi

Traditional music notation is instantly familiar. Write `c4 d e f | g2` and hear a melody without learning programming concepts.

### Live Performance

**Recommended:** forth-midi or alda-midi

Both offer immediate feedback. forth-midi for programmers who want stack manipulation; alda-midi for musicians who think in notes.

### Teaching/Learning

**Recommended:** lua-midi, pktpy-midi, or alda-midi

lua-midi and pktpy-midi use familiar syntax for programmers. alda-midi is ideal for teaching music concepts without programming overhead.

### Algorithmic Composition

**Recommended:** s7-midi, mhs-midi, or joy-midi

Functional programming excels at algorithmic music. s7 for Lisp macros, mhs-midi for type-safe pure functions, joy-midi for concatenative transformations on note lists.

### Quick Prototyping

**Recommended:** lua-midi or alda-midi

lua-midi for programmers; alda-midi for musicians. Both offer fast iteration.

### Production Code

**Recommended:** mhs-midi

Type safety catches errors early. Pure functional style makes code easier to reason about and test.

### Integration with DAWs

**All implementations** create virtual MIDI ports that DAWs can receive. Choose based on your comfort with the language.

---

## Performance Comparison

| Metric | alda-midi | forth-midi | joy-midi | lua-midi | s7-midi | pktpy-midi | mhs-midi |
| -------- | ----------- | ------------ | ---------- | ---------- | --------- | ------------ | ---------- |
| Startup | <1ms | <1ms | <1ms | <1ms | <1ms | <1ms | ~500ms |
| Note latency | <1ms | <1ms | <1ms | <1ms | <1ms | <1ms | <1ms |
| Memory usage | ~3MB | ~2MB | ~2MB | ~4MB | ~3MB | ~5MB | ~10MB |
| Test count | 3 | 82 | 14 | 26 | 33 | 22 | 149 |

Note: MicroHs compiles Haskell to C at startup, which adds latency. Once running, performance is equivalent.

---

## Multi-Voice and Async Capabilities

This section analyzes each implementation's ability to create complex multi-voice MIDI compositions and launch sequences asynchronously.

### Capability Matrix

| Feature | alda-midi | forth-midi | joy-midi | lua-midi | s7-midi | pktpy-midi | mhs-midi |
| --------- | ----------- | ------------ | ---------- | ---------- | --------- | ------------ | ---------- |
| **MIDI Channels** | 16 | 16 | 16 | 16 | 16 | 16 | 16 |
| **Simultaneous Notes** | Chords/Voices | Chords | Chords | Chords/Arpeggio | Chords/Arpeggio | Chords/Arpeggio | Chords/Melody |
| **Sequence System** | Tick-based events | Yes (64 seq, 256 events) | No | No | No | No | No |
| **Async Launch** | Yes (always) | Yes (seq-play&) | No (planned) | Yes (spawn/run) | Yes (spawn/run) | Yes (spawn/run) | Yes (spawn/run) |
| **Host Concurrency** | libuv thread | libuv thread | Synchronous | libuv + coroutines | libuv + thunks | libuv + generators | Native threads |

### Multi-Voice Support

All implementations support 16 MIDI channels, enabling multi-voice compositions. However, the approaches differ:

**alda-midi** uses parts and voices for natural multi-voice composition:

```alda
piano:
V1: c4 d e f     # Voice 1: melody
V2: o3 c1        # Voice 2: bass (plays simultaneously)
V0:              # Merge voices

violin:          # Different instrument, auto-assigned channel
c4 d e f
```

**forth-midi** has structured sequence support:

```forth
\ Explicit channel per note in sequences
0 1 60 100 480 seq-note-ch   \ time=0, ch=1, C4, vel=100, dur=480
0 2 64 100 480 seq-note-ch   \ time=0, ch=2, E4 on different channel
480 1 62 100 480 seq-note-ch \ time=480, ch=1, D4
seq-play
```

**joy-midi** uses chord and list operations:

```joy
\ Play chord - all notes simultaneously
[c e g] chord

\ Channel support via midi-note primitive
60 80 500 1 midi-note-ch    \ C4 on channel 1
64 80 500 2 midi-note-ch    \ E4 on channel 2
```

**lua-midi**, **pktpy-midi**, **s7-midi** use method calls with channel parameters:

```lua
-- Lua
m:note_on(60, 100, 1)  -- C4 on channel 1
m:note_on(64, 100, 2)  -- E4 on channel 2
```

**mhs-midi** uses Haskell's type system:

```haskell
-- Haskell
melody [c4, e4, g4]  -- Sequential
chord (major c4)     -- Simultaneous on same channel
```

### Asynchronous Playback

All implementations now support non-blocking async playback:

- **alda-midi**: Always async - REPL stays responsive, concurrent mode for layering parts
- **forth-midi**: `seq-play&` and `seq-loop&` for background sequence playback
- **lua-midi/pktpy-midi/s7-midi**: `spawn()` and `run()` for voice-based async; `poll()` for non-blocking checks
- **mhs-midi**: Native Haskell threads via `forkIO`; `spawn` and `run` functions

### libremidi Recommendations

The underlying libremidi library recommends two approaches for non-blocking MIDI (see [queue.md](https://celtera.github.io/libremidi/queue.html)):

1. **Callback-based queue**: Build event processing on the callback mechanism, integrating with the application's event loop. Example in `thirdparty/libremidi/examples/qmidiin.cpp`:

   ```cpp
   // Ring buffer queue with callback
   conf.on_message = [this](libremidi::message m) {
       queue.push(std::move(m));
   };
   // Main loop polls queue non-blocking
   while (!done) {
       auto msg = midiin.get_message();
       if (!msg.empty()) process(msg);
       std::this_thread::sleep_for(10ms);
   }
   ```

2. **Async runtime with coroutines**: Use C++20 coroutines for imperative-style non-blocking code. Example in `thirdparty/libremidi/examples/coroutines.cpp`:

   ```cpp
   // Boost.Cobalt coroutines
   cobalt::channel<libremidi::message> channel_impl{64};
   libremidi::midi_in midiin{{.on_message = channel}};
   for (;;) {
       auto msg = co_await channel_impl.read();  // Non-blocking await
       process(msg);
   }
   ```

### Implementation Notes

All implementations now use one or more of these patterns:

1. **Event scheduler**: alda-midi uses tick-based event scheduling with libuv timers
2. **Thread-per-voice**: mhs-midi uses native Haskell threads (`forkIO`)
3. **Coroutine integration**: lua-midi, pktpy-midi, s7-midi use host language concurrency primitives with libuv timers
4. **Background event loop**: forth-midi uses a dedicated libuv thread for sequence playback

### Practical Guidance

For complex multi-voice compositions:

| Approach | Recommended Implementation |
| ---------- | --------------------------- |
| **Multi-part scores** | alda-midi with parts (piano:, violin:) and voices (V1:, V2:) |
| **Interleaved timeline** | forth-midi sequences with manual time offsets |
| **Polyphonic chords** | Any implementation using chord functions |
| **Algorithmic voices** | mhs-midi, s7-midi, or joy-midi with functional composition |
| **Live layering** | alda-midi concurrent mode, or DAW receiving from multiple instances |
| **List-based transformations** | joy-midi with `map`, `filter`, combinators |

---

## Getting Started

All implementations are built with:

```bash
make
```

Run any implementation:

```bash
./build/alda_midi     # Alda
./build/forth_midi    # Forth
./build/joy_midi      # Joy
./build/lua_midi      # Lua
./build/s7_midi       # Scheme
./build/pktpy_midi    # Python
./build/mhs-midi      # Haskell
```

See individual documentation in `docs/<lang>-midi/` for detailed API references and tutorials.
