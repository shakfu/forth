# pktpy-midi Architecture

This document explains how pktpy-midi integrates PocketPy with MIDI functionality.

## Overview

```
+------------------+     +------------------+     +------------------+
|   Python Code    | --> |  midi_module.c   | --> |   libremidi      |
| (user scripts)   |     | (C bindings)     |     | (MIDI backend)   |
+------------------+     +------------------+     +------------------+
        |                        |
        v                        v
+------------------+     +------------------+
|   pocketpy.c     |     | Python Prelude   |
| (interpreter)    |     | (helper funcs)   |
+------------------+     +------------------+
        |                        |
        v                        v
+------------------+     +------------------+
|    main.c        | <-- | MidiOut class    |
|  (entry point)   |     | (type + methods) |
+------------------+     +------------------+
```

## Components

### 1. PocketPy - Python Interpreter

Location: `projects/pktpy_midi/pocketpy.c`, `pocketpy.h`

PocketPy v2.1.6 is an embeddable Python interpreter written in C. Key features:
- Small footprint (~250KB compiled)
- CPython-compatible subset
- Easy C API for extensions

### 2. midi_module.c - C Bindings

Location: `projects/pktpy_midi/midi_module.c`

The core C module providing MIDI functionality. Contains:

- Module functions (`midi.open`, `midi.list_ports`, `midi.note`)
- `MidiOut` class definition and methods
- Python prelude (helper functions as embedded Python code)

#### MidiOut Type

```c
typedef struct {
    libremidi_midi_out_handle* handle;
    int is_virtual;
} MidiOutData;
```

The `MidiOut` type wraps a libremidi output handle with:
- Destructor for cleanup (`MidiOut_dtor`)
- Methods bound via `py_bindmethod`
- Properties via `py_bindproperty`
- Magic methods for context manager and repr

### 3. libremidi - MIDI Backend

Location: `thirdparty/libremidi/`

Cross-platform MIDI library supporting:
- macOS: CoreMIDI
- Linux: ALSA
- Windows: WinMM

### 4. Python Prelude

The module includes an embedded Python prelude that runs at initialization. This adds:

- Constants (dynamics, durations, pitches)
- Helper functions (chord builders, transpose)
- Method extensions (arpeggio, CC helpers)

This approach allows adding Python-level features without C code:

```c
static const char* prelude =
    "import midi\n"
    "midi.ppp = 16\n"
    "midi.pp = 33\n"
    // ... more Python code
    ;

py_exec(prelude, "<midi_prelude>", EXEC_MODE, midi_mod);
```

## Module Initialization

The initialization sequence in `pk_midi_module_init()`:

1. Create the `midi` module
2. Bind module-level functions
3. Create and configure `MidiOut` type
4. Execute Python prelude

```c
void pk_midi_module_init(void) {
    // 1. Create module
    py_GlobalRef mod = py_newmodule("midi");

    // 2. Bind functions
    py_bindfunc(mod, "list_ports", midi_list_ports);
    py_bindfunc(mod, "open", midi_open);
    py_bindfunc(mod, "note", midi_note);

    // 3. Create MidiOut type
    tp_MidiOut = py_newtype("MidiOut", tp_object, mod, MidiOut_dtor);
    py_bindmethod(tp_MidiOut, "note", MidiOut_note);
    // ... more methods

    // 4. Execute prelude
    py_exec(prelude, "<midi_prelude>", EXEC_MODE, midi_mod);
}
```

## PocketPy C API Patterns

### Creating Functions

```c
static bool my_function(int argc, py_StackRef argv) {
    PY_CHECK_ARGC(1);                    // Verify argument count
    PY_CHECK_ARG_TYPE(0, tp_int);        // Type check

    int value = (int)py_toint(py_arg(0)); // Get argument

    // Do work...

    py_newint(py_retval(), result);      // Set return value
    return true;                          // Success
}
```

### Creating Types

```c
// Define type with destructor
tp_MidiOut = py_newtype("MidiOut", tp_object, mod, MidiOut_dtor);

// Create instance with userdata
MidiOutData* data = py_newobject(py_retval(), tp_MidiOut, 0, sizeof(MidiOutData));
data->handle = handle;
```

### Binding Methods

```c
py_bindmethod(tp_MidiOut, "note", MidiOut_note);
py_bindproperty(tp_MidiOut, "is_open", MidiOut_is_open_getter, NULL);
py_bindmagic(tp_MidiOut, py_name("__enter__"), MidiOut__enter__);
```

### Error Handling

```c
if (error_condition) {
    return py_exception(tp_ValueError, "Error message: %s", details);
}
```

## MIDI Message Format

MIDI messages are sent as byte arrays:

```c
// Note On: 0x90 + channel, pitch, velocity
uint8_t msg[3] = {
    0x90 | ((channel - 1) & 0x0F),
    pitch & 0x7F,
    velocity & 0x7F
};
libremidi_midi_out_send_message(data->handle, msg, 3);

// Note Off: 0x80 + channel, pitch, velocity
msg[0] = 0x80 | ((channel - 1) & 0x0F);

// Control Change: 0xB0 + channel, control, value
msg[0] = 0xB0 | ((channel - 1) & 0x0F);

// Program Change: 0xC0 + channel, program (2 bytes only)
uint8_t pc[2] = {
    0xC0 | ((channel - 1) & 0x0F),
    program & 0x7F
};
```

## File Structure

```
projects/pktpy_midi/
  main.c              # Entry point, calls py_initialize and module init
  midi_module.c       # MIDI module implementation
  pocketpy.c          # PocketPy interpreter
  pocketpy.h          # PocketPy API header
  CMakeLists.txt      # Build configuration

thirdparty/
  libremidi/          # MIDI backend library

tests/
  test_pktpy_midi.sh  # Test suite (23 tests)

docs/pktpy-midi/
  README.md           # Overview
  api-reference.md    # Complete API docs
  examples.md         # Code examples
  architecture.md     # This file
```

## Build Process

```
pocketpy.c + midi_module.c + main.c
                +
           libremidi.a
                |
                v
          pktpy_midi (executable)
```

CMake configuration:

```cmake
add_executable(pktpy_midi main.c midi_module.c pocketpy.c)
target_link_libraries(pktpy_midi PRIVATE libremidi)
target_compile_definitions(pktpy_midi PRIVATE PK_IS_PUBLIC_INCLUDE)
```

## Extending the Module

### Adding a New C Function

1. Write the C function:

```c
static bool midi_new_func(int argc, py_StackRef argv) {
    PY_CHECK_ARGC(1);
    // Implementation...
    py_newnone(py_retval());
    return true;
}
```

2. Bind it in `pk_midi_module_init`:

```c
py_bindfunc(mod, "new_func", midi_new_func);
```

### Adding a New Method to MidiOut

1. Write the method (first arg is self):

```c
static bool MidiOut_new_method(int argc, py_StackRef argv) {
    MidiOutData* data = MidiOut_get(py_arg(0));
    if (!data) return false;
    // Implementation...
    return true;
}
```

2. Bind it:

```c
py_bindmethod(tp_MidiOut, "new_method", MidiOut_new_method);
```

### Adding via Python Prelude

For simpler additions, extend the prelude:

```c
static const char* prelude =
    // ... existing code ...
    "def _new_helper(x):\n"
    "    return x * 2\n"
    "midi.new_helper = _new_helper\n"
    ;
```

This is preferred for:
- Pure Python logic
- Helper functions
- Constants
- Method wrappers
