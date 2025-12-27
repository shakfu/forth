# mhs-midi Architecture

This document explains how mhs-midi integrates MicroHs with MIDI functionality.

## Overview

```
+------------------+     +------------------+     +------------------+
|    Midi.hs       | --> |  midi_ffi.c      | --> |   libremidi      |
|  (Haskell API)   |     | (C FFI bindings) |     | (MIDI backend)   |
+------------------+     +------------------+     +------------------+
        |                        |
        v                        v
+------------------+     +------------------+
| MicroHs Compiler |     | midi_ffi_wrappers|
|   (mhs)          |     |  (MHS FFI glue)  |
+------------------+     +------------------+
        |                        |
        v                        v
+------------------+     +------------------+
|  mhs-midi REPL   | <-- |   eval.c         |
|  (executable)    |     | (MicroHs runtime)|
+------------------+     +------------------+
```

## Components

### 1. Midi.hs - Haskell API

Location: `projects/mhs_midi/lib/Midi.hs`

The high-level Haskell module that users import. Contains:

- FFI declarations (`foreign import ccall`)
- Type definitions (`Pitch`, `Duration`, `Velocity`)
- Musical constants (note names, durations, dynamics)
- High-level functions (`play`, `playNote`, `chord`, etc.)

```haskell
foreign import ccall "midi_ffi.h midi_note_on"
    c_midi_note_on :: CInt -> CInt -> CInt -> IO CInt
```

### 2. midi_ffi.c - C FFI Bindings

Location: `projects/mhs_midi/midi_ffi.c`

C wrapper around libremidi providing a simple interface:

```c
int midi_note_on(int channel, int pitch, int velocity);
int midi_open_virtual(const char* name);
void midi_close(void);
```

### 3. libremidi - MIDI Backend

Location: `thirdparty/libremidi/`

Cross-platform MIDI library supporting:
- macOS: CoreMIDI
- Linux: ALSA
- Windows: WinMM

### 4. midi_ffi_wrappers.c - MicroHs FFI Glue

Location: `projects/mhs_midi/midi_ffi_wrappers.c`

Bridges between MicroHs runtime and midi_ffi.c. MicroHs FFI uses a specific calling convention:

```c
// MicroHs FFI wrapper pattern
from_t mhs_midi_note_on(int s) {
    return mhs_from_Int(s, 3, midi_note_on(
        mhs_to_Int(s, 0),   // channel
        mhs_to_Int(s, 1),   // pitch
        mhs_to_Int(s, 2))); // velocity
}
```

Key concepts:
- `s` is the stack pointer
- `mhs_to_Int(s, n)` extracts the nth argument
- `mhs_from_Int(s, arity, result)` returns the result
- Arity must match the number of arguments

### 5. xffi_table - FFI Registration

MicroHs has two FFI tables:

1. **ffi_table** (built-in): Standard functions in `eval.c`
2. **xffi_table** (external): Custom FFI functions

mhs-midi populates `xffi_table` with MIDI functions:

```c
const struct ffi_entry midi_ffi_table[] = {
    { "midi_init",       0, mhs_midi_init },
    { "midi_note_on",    3, mhs_midi_note_on },
    { "midi_close",      0, mhs_midi_close },
    // ...
    { 0, 0, 0 }  // sentinel
};

const struct ffi_entry *xffi_table = midi_ffi_table;
```

## Build Process

### REPL Build (`mhs-midi`)

```
generated/mhs.c (MicroHs compiler)
         +
     eval.c (runtime)
         +
midi_ffi_wrappers.c (FFI glue)
         +
    midi_ffi.c (C bindings)
         +
   libremidi.a (MIDI backend)
         |
         v
    mhs-midi (executable)
```

The build strips the default `xffi_table` from `generated/mhs.c` and substitutes our MIDI-enabled table.

### Compiled Program Build

```
MyProgram.hs + Midi.hs
         |
         v (mhs -C)
    MyProgram.c (generated)
         +
     eval.c
         +
    midi_ffi.c
         +
   libremidi.a
         |
         v
   my_program (executable)
```

When MicroHs compiles Haskell to C, it generates its own FFI wrappers and `xffi_table` in the output.

## FFI Function Signature Reference

### Argument Extraction

```c
mhs_to_Int(s, n)   // Extract Int from position n
mhs_to_Ptr(s, n)   // Extract pointer (for strings)
```

### Return Value

```c
mhs_from_Int(s, arity, value)   // Return Int
mhs_from_Ptr(s, arity, ptr)     // Return pointer
mhs_from_Unit(s, arity)         // Return () for void functions
```

### Arity

The arity parameter must equal the number of arguments consumed:

| Function          | Args | Arity |
|-------------------|------|-------|
| `midi_init`       | 0    | 0     |
| `midi_open`       | 1    | 1     |
| `midi_note_off`   | 2    | 2     |
| `midi_note_on`    | 3    | 3     |

## Caching

MicroHs supports compilation caching via `.mhscache`:

- `-C` flag enables read/write caching
- First run builds the cache
- Subsequent runs are faster

The `mhs-midi-repl` script enables caching by default.

## File Locations

```
projects/mhs_midi/
  lib/
    Midi.hs              # Haskell API
  examples/
    HelloMidi.hs         # Example programs
    Chords.hs
    ...
  midi_ffi.h             # C API header
  midi_ffi.c             # C implementation
  midi_ffi_wrappers.c    # MicroHs FFI wrappers
  mhs_midi_main.c        # REPL entry point
  CMakeLists.txt         # Build configuration

scripts/
  mhs-midi-repl          # REPL launcher
  mhs-midi-compile       # Compilation script

thirdparty/
  MicroHs/               # MicroHs compiler
  libremidi/             # MIDI backend
```
