# Language Comparison Guide

This guide helps you choose the right MIDI language implementation for your needs.

## Quick Comparison

| Feature | forth-midi | lua-midi | s7-midi | pktpy-midi | mhs-midi |
|---------|------------|----------|---------|------------|----------|
| **Language** | Forth-like | Lua 5.5 | Scheme (s7) | Python (PocketPy) | Haskell (MicroHs) |
| **Paradigm** | Stack-based | Imperative | Functional/Lisp | Object-oriented | Pure functional |
| **Binary size** | ~200KB | ~400KB | ~350KB | ~500KB | ~1.5MB |
| **Startup time** | Instant | Instant | Instant | Instant | ~0.5s |
| **Learning curve** | Medium | Easy | Medium | Easy | Hard |
| **Notation** | Concise DSL | Method calls | S-expressions | Method calls | DSL + combinators |

## Language Profiles

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
|----------|---------|-------|
| forth-midi | `c4,` `C#4,` `Db4,` | Comma triggers, case-insensitive |
| lua-midi | `midi.c4` or `midi.note("C4")` | Constants or string parsing |
| s7-midi | `c4` `cs4` | Scheme symbols |
| pktpy-midi | `midi.c4` or `midi.note("C4")` | Constants or string parsing |
| mhs-midi | `c4` `cs4` | Haskell constants |

### Chord Building

| Language | Major Triad | Dominant 7th |
|----------|-------------|--------------|
| forth-midi | `(c4 e4 g4),` | `(c4 e4 g4 bb4),` |
| lua-midi | `midi.major(c4)` | `midi.dom7(c4)` |
| s7-midi | `(major c4)` | `(dom7 c4)` |
| pktpy-midi | `midi.major(c4)` | `midi.dom7(c4)` |
| mhs-midi | `major c4` | `dom7 c4` |

### Repetition

| Language | Play 4 times |
|----------|--------------|
| forth-midi | `melody 4 times` or `{ c4, } 4 *` |
| lua-midi | `for i=1,4 do m:note(c4,mf,quarter) end` |
| s7-midi | `(times 4 (lambda () (midi-note m c4 mf quarter)))` |
| pktpy-midi | `for i in range(4): m.note(c4, mf, quarter)` |
| mhs-midi | `times 4 (note c4)` |

### Probability/Randomness

| Language | 50% chance | Random selection |
|----------|------------|------------------|
| forth-midi | `c4 50%,` | `c4\|e4\|g4,` |
| lua-midi | `if math.random() < 0.5 then ... end` | `midi.pick({c4,e4,g4})` |
| s7-midi | `(chance 50 ...)` | `(pick (list c4 e4 g4))` |
| pktpy-midi | `if random.random() < 0.5: ...` | `random.choice([c4,e4,g4])` |
| mhs-midi | `chance 50 (note c4)` | `pick seed [c4,e4,g4]` |

### Recording

All implementations support MIDI event recording:

| Language | Start | Stop | Save |
|----------|-------|------|------|
| forth-midi | `rec-midi` | `stop` | `save-midi file.4th` |
| lua-midi | `record_midi()` | `record_stop()` | `save_midi("file.lua")` |
| s7-midi | `(record-midi)` | `(record-stop)` | `(save-midi "file.scm")` |
| pktpy-midi | `midi.record_midi()` | `midi.record_stop()` | `midi.save_midi("file.py")` |
| mhs-midi | `midiRecordStart 120` | `midiRecordStop` | `midiRecordSave "file.hs"` |

---

## Use Case Recommendations

### Live Performance
**Recommended:** forth-midi

The concise notation and immediate execution make it ideal for live coding. Type `c4,` and hear it instantly.

### Teaching/Learning
**Recommended:** lua-midi or pktpy-midi

Familiar syntax makes it easy for students. Good error messages and conventional programming concepts.

### Algorithmic Composition
**Recommended:** s7-midi or mhs-midi

Functional programming excels at algorithmic music. s7 for Lisp macros, mhs-midi for type-safe pure functions.

### Quick Prototyping
**Recommended:** lua-midi

Full standard library, familiar syntax, fast iteration. Easy to translate ideas to other languages later.

### Production Code
**Recommended:** mhs-midi

Type safety catches errors early. Pure functional style makes code easier to reason about and test.

### Integration with DAWs
**All implementations** create virtual MIDI ports that DAWs can receive. Choose based on your comfort with the language.

---

## Performance Comparison

| Metric | forth-midi | lua-midi | s7-midi | pktpy-midi | mhs-midi |
|--------|------------|----------|---------|------------|----------|
| Startup | <1ms | <1ms | <1ms | <1ms | ~500ms |
| Note latency | <1ms | <1ms | <1ms | <1ms | <1ms |
| Memory usage | ~2MB | ~4MB | ~3MB | ~5MB | ~10MB |
| Test count | 82 | 26 | 33 | 22 | 149 |

Note: MicroHs compiles Haskell to C at startup, which adds latency. Once running, performance is equivalent.

---

## Multi-Voice and Async Capabilities

This section analyzes each implementation's ability to create complex multi-voice MIDI compositions and launch sequences asynchronously.

### Capability Matrix

| Feature | forth-midi | lua-midi | s7-midi | pktpy-midi | mhs-midi |
|---------|------------|----------|---------|------------|----------|
| **MIDI Channels** | 16 | 16 | 16 | 16 | 16 |
| **Simultaneous Notes** | Chords | Chords/Arpeggio | Chords/Arpeggio | Chords/Arpeggio | Chords/Melody |
| **Sequence System** | Yes (64 seq, 256 events) | No | No | No | No |
| **Async Launch** | No (blocking) | No | No | No | No |
| **Host Concurrency** | None | Coroutines (unused) | None | Threading (unused) | Lazy evaluation |

### Multi-Voice Support

All implementations support 16 MIDI channels, enabling multi-voice compositions. However, the approaches differ:

**forth-midi** has the most structured support:
```forth
\ Explicit channel per note in sequences
0 1 60 100 480 seq-note-ch   \ time=0, ch=1, C4, vel=100, dur=480
0 2 64 100 480 seq-note-ch   \ time=0, ch=2, E4 on different channel
480 1 62 100 480 seq-note-ch \ time=480, ch=1, D4
seq-play
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

### Asynchronous Limitations

**Current State:** No implementation supports truly asynchronous sequence playback. All use blocking sleep calls for timing:

- **forth-midi**: `seq-play` blocks until complete (uses `midi_sleep_ms()` between events)
- **lua-midi/pktpy-midi/s7-midi**: Direct note methods block for duration via `usleep()`
- **mhs-midi**: Relies on MicroHs host for timing control

**Workaround for multi-voice:** Interleave notes from different voices manually with calculated timings, or use chord notation for vertically aligned events.

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

### Architectural Gap

To enable complex multi-voice songs with independent timing, future implementations could:

1. **Event scheduler**: Timestamp-based event queue processed by a timer/callback
2. **Thread-per-voice**: Each voice runs in its own thread (requires thread-safe MIDI output)
3. **Coroutine integration**: Leverage host language coroutines (Lua, Python) for cooperative multitasking
4. **Lock-free queue**: As libremidi suggests, use a proper lock-free queue (e.g., `atomic_queue` or `readerwriterqueue`)

### Practical Guidance

For complex multi-voice compositions today:

| Approach | Recommended Implementation |
|----------|---------------------------|
| **Interleaved timeline** | forth-midi sequences with manual time offsets |
| **Polyphonic chords** | Any implementation using chord functions |
| **Algorithmic voices** | mhs-midi or s7-midi with functional composition |
| **Live layering** | External DAW receiving from multiple instances |

---

## Getting Started

All implementations are built with:
```bash
make
```

Run any implementation:
```bash
./build/forth_midi    # Forth
./build/lua_midi      # Lua
./build/s7_midi       # Scheme
./build/pktpy_midi    # Python
./build/mhs-midi      # Haskell
```

See individual documentation in `docs/<lang>-midi/` for detailed API references and tutorials.
