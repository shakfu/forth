# PocketPy `yield from` Issue

This document describes a limitation discovered in PocketPy's `yield from` implementation that affects the async scheduler in pktpy-midi.

## Summary

PocketPy's `yield from` statement does not correctly delegate to sub-generators when used with the async scheduler. The yielded values from the inner generator are not passed through to the outer generator.

## Expected Behavior (CPython)

In standard Python, `yield from` delegates iteration to a sub-generator:

```python
def inner():
    yield 100
    yield 200

def outer():
    print("start")
    yield from inner()  # Should yield 100, then 200
    print("end")

gen = outer()
print(next(gen))  # start, 100
print(next(gen))  # 200
print(next(gen))  # end, StopIteration
```

The `yield from inner()` statement should:
1. Call `inner()` to get a generator
2. Iterate the inner generator, yielding each value to the caller
3. Resume after all values are exhausted

## Observed Behavior (PocketPy)

In PocketPy, `yield from` appears to consume the inner generator but does not yield its values to the caller:

```python
def inner():
    print("inner: yielding 100")
    yield 100
    print("inner: yielding 200")
    yield 200

def outer():
    print("outer: start")
    yield from inner()
    print("outer: after inner")
    yield 300
    print("outer: done")

gen = outer()
# First next() prints:
#   outer: start
#   outer: after inner  <-- inner's yields were skipped!
# Returns 300
```

The inner generator's `yield 100` and `yield 200` are never seen by the caller.

## Workaround

Use explicit iteration instead of `yield from`:

```python
# Instead of:
yield from inner()

# Use:
for value in inner():
    yield value
```

This works correctly because it explicitly yields each value from the inner generator.

## Impact on pktpy-midi

The async playback helpers (`midi.play`, `midi.play_chord`, etc.) are generators that yield wait times. Users must use the explicit iteration pattern:

```python
# This does NOT work:
def voice():
    out = midi.open()
    yield from midi.play(out, 60, 80, 500)  # BUG: yields are lost
    out.close()

# This WORKS:
def voice():
    out = midi.open()
    for ms in midi.play(out, 60, 80, 500):
        yield ms
    out.close()
```

## Test Case for PocketPy

The following test case demonstrates the issue:

```python
# test_yield_from.py
def inner():
    yield 1
    yield 2
    yield 3

def outer_yield_from():
    yield from inner()

def outer_explicit():
    for x in inner():
        yield x

# Test yield from
print("Testing yield from:")
gen = outer_yield_from()
results = list(gen)
print(f"  Results: {results}")
print(f"  Expected: [1, 2, 3]")
print(f"  Pass: {results == [1, 2, 3]}")

# Test explicit iteration
print("\nTesting explicit for loop:")
gen = outer_explicit()
results = list(gen)
print(f"  Results: {results}")
print(f"  Expected: [1, 2, 3]")
print(f"  Pass: {results == [1, 2, 3]}")
```

Expected output (CPython):
```
Testing yield from:
  Results: [1, 2, 3]
  Expected: [1, 2, 3]
  Pass: True

Testing explicit for loop:
  Results: [1, 2, 3]
  Expected: [1, 2, 3]
  Pass: True
```

Actual output (PocketPy):
```
Testing yield from:
  Results: []
  Expected: [1, 2, 3]
  Pass: False

Testing explicit for loop:
  Results: [1, 2, 3]
  Expected: [1, 2, 3]
  Pass: True
```

## Environment

- PocketPy version: v2.1.6 (embedded in pktpy-midi)
- Discovered: December 2024
- pktpy-midi project: https://github.com/your-repo/midi-langs

## Related

- PocketPy repository: https://github.com/pocketpy/pocketpy
- Python PEP 380 (yield from): https://peps.python.org/pep-0380/
