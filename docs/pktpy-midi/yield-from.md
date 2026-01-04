# PocketPy `yield from` Support

This document describes `yield from` support in PocketPy as used by pktpy-midi.

## Status: Working

As of PocketPy 2.1.6, `yield from` works correctly and can be used with the async scheduler.

## Usage

You can use `yield from` to delegate to sub-generators:

```python
import midi

def play_note(out, pitch, vel, dur):
    yield from midi.play(out, pitch, vel, dur)

def melody():
    out = midi.open()
    yield from play_note(out, midi.c4, midi.mf, 200)
    yield from play_note(out, midi.e4, midi.mf, 200)
    yield from play_note(out, midi.g4, midi.mf, 200)
    out.close()

midi.spawn(melody)
midi.run()
```

## Example: Multi-Voice with yield from

```python
import midi

def voice1():
    out = midi.open()
    for note in [midi.c4, midi.e4, midi.g4]:
        yield from midi.play(out, note, midi.mf, 200)
    out.close()

def voice2():
    out = midi.open()
    for note in [midi.c3, midi.g3]:
        yield from midi.play(out, note, midi.f, 300)
    out.close()

midi.spawn(voice1)
midi.spawn(voice2)
midi.run()
```

## How It Works

PocketPy's `yield from` implementation:

1. Calls `iter()` on the expression to get an iterator
2. Repeatedly calls `next()` on the iterator
3. Each value from the inner iterator is yielded to the outer caller
4. When `StopIteration` is raised, `yield from` completes and returns the value (if any)

This matches Python's PEP 380 semantics for basic generator delegation.

## Historical Note

Earlier versions of this document described a bug where `yield from` would skip yielded values. This issue has been resolved in PocketPy 2.1.6. The explicit `for` loop workaround is no longer necessary, though it remains functionally equivalent:

```python
# Both of these work correctly:

# Using yield from (recommended)
yield from midi.play(out, note, vel, dur)

# Using explicit for loop (also works)
for ms in midi.play(out, note, vel, dur):
    yield ms
```
