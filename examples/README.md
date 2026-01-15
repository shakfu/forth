# Example Compositions

Runnable examples demonstrating each language's unique strengths.

## Running Examples

```bash
# Forth - concise stack-based notation
./build/forth_midi examples/melody_forth.4th

# Joy - algebraic composition with combinators
./build/joy_midi examples/algebraic_joy.joy

# Lua - coroutine-based concurrent voices
./build/lua_midi examples/concurrent_lua.lua

# PocketPy - generator-based patterns
./build/pktpy_midi examples/generative_pktpy.py

# s7 Scheme - functional thunk-based composition
./build/s7_midi examples/functional_s7.scm

# MicroHs - pure functional (requires MicroHs setup)
./scripts/mhs-midi-repl -r examples/pure_mhs.hs
```

## What Each Example Demonstrates

| File | Language | Key Features |
| ------ | ---------- | -------------- |
| `melody_forth.4th` | Forth | Concise notation, word definitions, dynamics |
| `algebraic_joy.joy` | Joy | Parse-time notes, combinators, `def`, `play`/`chord` |
| `concurrent_lua.lua` | Lua | Coroutines, `spawn`/`run`, `play()` helpers |
| `generative_pktpy.py` | PocketPy | Generators, `yield` pattern, concurrent voices |
| `functional_s7.scm` | s7 Scheme | Thunks, closures, functional voice builders |
| `pure_mhs.hs` | MicroHs | Pure `Music` type, `perform`, native threads |

## Tips

- Examples use fast tempos (480 BPM) for quick execution
- Connect a MIDI synthesizer or DAW to hear output
- Use `midi-list` (Forth) or equivalent to find hardware ports
