# Async Models Across Languages

This document compares the concurrent playback implementations across all five midi-langs interpreters.

## Overview

All five languages now support concurrent voice playback, but each uses a different underlying model suited to the language's strengths:

| Language | Model | Threading | Blocking | Non-blocking |
|----------|-------|-----------|----------|--------------|
| **forth-midi** | libuv event loop | Background thread | N/A | `seq-play&` (always async) |
| **lua-midi** | libuv + Lua coroutines | Background thread | `run()` | `poll()` |
| **pktpy-midi** | libuv + Python generators | Background thread | `run()` | `poll()` |
| **s7-midi** | libuv + Scheme thunks | Main thread | `(run)` | `(poll)` |
| **mhs-midi** | Native Haskell threads | Multiple threads | `run` | N/A |

## forth-midi

**Model**: Sequence-based async with libuv event loop

Forth-midi uses a dedicated libuv thread for timing. Sequences can be played asynchronously while the REPL remains responsive.

```forth
\ Create and play sequences asynchronously
seq-new
c4, e4, g4,
seq-play&          \ Play async (REPL stays responsive)

seq-new
c2, g2, c3,
seq-loop&          \ Loop async

seq-active .       \ Shows count of playing sequences
seq-stop-all       \ Stop everything
```

**Key characteristics**:
- Non-blocking: REPL stays responsive during playback
- Up to 8 concurrent sequences
- Sequence-oriented (not voice/function-oriented)
- No explicit `run()` needed - playback happens automatically

## lua-midi

**Model**: Lua coroutines with libuv timer thread

Lua-midi combines Lua's native coroutines with a background libuv event loop. Voices are coroutines that yield to pause.

```lua
spawn(function()
    play(c4, mf, quarter)    -- Non-blocking note
    yield_ms(500)            -- Pause 500ms
    play(e4, mf, quarter)
end, "melody")

spawn(function()
    play(c2, ff, whole)
end, "bass")

run()  -- Blocks until all voices complete
```

**Non-blocking mode with `poll()`**:
```lua
spawn(melody_voice)
spawn(bass_voice)
-- REPL stays responsive
while poll() do
    -- Can check for user input, display status, etc.
end
```

**Key characteristics**:
- `yield_ms(n)` for explicit timing control
- `play()`, `play_chord()`, `play_arp()` are non-blocking within voices
- Up to 16 concurrent voices
- `run()` blocks, `poll()` is non-blocking

## pktpy-midi

**Model**: Python generators with libuv timer thread

PocketPy-midi uses Python generators (functions with `yield`) as voices. Each yield returns milliseconds to wait.

```python
def melody():
    out = midi.open()
    for ms in midi.play(out, midi.c4, midi.mf, midi.quarter):
        yield ms  # Yields wait time
    for ms in midi.play(out, midi.e4, midi.mf, midi.quarter):
        yield ms

def bass():
    out = midi.open()
    for ms in midi.play(out, midi.c2, midi.ff, midi.whole):
        yield ms

midi.spawn(melody, "melody")
midi.spawn(bass, "bass")
midi.run()  # Blocks until complete
```

**Non-blocking mode with `poll()`**:
```python
midi.spawn(melody)
midi.spawn(bass)
# REPL stays responsive
while midi.poll():
    pass  # Can do other work here
```

**Key characteristics**:
- Generator-based: voice functions must contain `yield`
- Use `for ms in midi.play(...): yield ms` pattern (not `yield from`)
- Up to 16 concurrent voices
- `run()` blocks, `poll()` is non-blocking
- Known issue: sequential `run()` calls hang

## s7-midi

**Model**: Thunk-based cooperative multitasking with libuv

S7-midi uses a unique thunk-based model. Each voice is a closure that returns either:
- A number (milliseconds to wait before next call)
- `#f` (voice is complete)

```scheme
;; Voice builder: returns a thunk
(define (make-melody-voice pitches vel dur)
  (let ((remaining pitches))
    (lambda ()
      (if (null? remaining)
          #f  ; Done
          (begin
            (midi-note *midi* (car remaining) vel dur)
            (set! remaining (cdr remaining))
            dur)))))  ; Wait dur ms

(spawn (make-melody-voice '(60 64 67) mf quarter) "melody")
(spawn (make-note-voice c2 ff whole) "bass")
(run)  ; Blocks until complete
```

**Non-blocking mode with `(poll)`**:
```scheme
(spawn melody-voice "melody")
(spawn bass-voice "bass")
;; REPL stays responsive
(let loop ()
  (when (poll)
    (loop)))
```

**Key characteristics**:
- Thunk-based: voice is a procedure called repeatedly
- Return value controls timing (number = wait, `#f` = done)
- Prelude provides voice builders: `make-sequence-voice`, `make-repeat-voice`, etc.
- Up to 16 concurrent voices
- `(run)` blocks, `(poll)` is non-blocking

## mhs-midi

**Model**: Native Haskell threads (forkIO)

MicroHs provides full `Control.Concurrent` support, so mhs-midi uses native Haskell threads. Each voice runs in its own lightweight thread.

```haskell
import MusicPerform

main = do
    midiOpenVirtual "MyApp"

    -- Spawn concurrent voices (run in separate threads)
    spawn "melody" $ perform (line [c4, e4, g4] mf quarter)
    spawn "bass" $ perform (note c2 ff whole)

    run  -- Wait for all voices
    midiClose
```

**Key characteristics**:
- True parallelism via `forkIO`
- No special yield/thunk pattern needed - use normal Haskell IO
- Thread-safe via `MVar` synchronization
- Voices clean up automatically when done
- `run` blocks until all voices complete

## Comparison

### Ease of Use

| Simplest | Medium | Most Complex |
|----------|--------|--------------|
| mhs-midi (normal IO) | lua-midi (yield_ms) | s7-midi (thunks) |
| forth-midi (seq-play&) | pktpy-midi (generators) | |

### Performance

| Most Efficient | Medium | Least Efficient |
|----------------|--------|-----------------|
| mhs-midi (native threads) | forth-midi (libuv) | s7-midi (main thread) |
| | lua-midi/pktpy-midi | |

### REPL Responsiveness

| Always Non-blocking | Non-blocking via `poll()` | Blocking only |
|---------------------|---------------------------|---------------|
| forth-midi | lua-midi | mhs-midi |
| | pktpy-midi | |
| | s7-midi | |

## Cross-Language Example

Here's the same musical idea in all five languages:

### forth-midi
```forth
midi-virtual
seq-new
mf c4, e4, g4, c5,
seq-play&
seq-new
ff c2,
seq-play&
```

### lua-midi
```lua
open()
spawn(function()
    for _, p in ipairs({c4, e4, g4, c5}) do
        play(p, mf, quarter)
    end
end, "melody")
spawn(function()
    play(c2, ff, whole)
end, "bass")
run()
close()
```

### pktpy-midi
```python
out = midi.open()
def melody():
    for p in [midi.c4, midi.e4, midi.g4, midi.c5]:
        for ms in midi.play(out, p, midi.mf, midi.quarter):
            yield ms
def bass():
    for ms in midi.play(out, midi.c2, midi.ff, midi.whole):
        yield ms
midi.spawn(melody, "melody")
midi.spawn(bass, "bass")
midi.run()
out.close()
```

### s7-midi
```scheme
(open)
(spawn (make-melody-voice (list c4 e4 g4 c5) mf quarter) "melody")
(spawn (make-note-voice c2 ff whole) "bass")
(run)
(close)
```

### mhs-midi
```haskell
import MusicPerform

main = do
    midiOpenVirtual "example"
    spawn "melody" $ perform (line [c4, e4, g4, c5] mf quarter)
    spawn "bass" $ perform (note c2 ff whole)
    run
    midiClose
```

## When to Use Which

- **forth-midi**: Best for live coding and interactive exploration (REPL stays responsive)
- **lua-midi**: Best for scripted compositions with clear timing control
- **pktpy-midi**: Best for Python programmers, generator-based patterns
- **s7-midi**: Best for functional/Lisp approach, complex voice transformations
- **mhs-midi**: Best for type-safe composition, pure functional patterns
