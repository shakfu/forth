# s7-midi Architecture

This document explains how s7-midi integrates the s7 Scheme interpreter with MIDI functionality.

## Overview

```text
+------------------+     +------------------+     +------------------+
|   Scheme Code    | --> |  midi_module.c   | --> |   libremidi      |
| (user scripts)   |     | (s7 FFI bindings)|     | (MIDI backend)   |
+------------------+     +------------------+     +------------------+
        |                        |
        v                        v
+------------------+     +------------------+
|      s7.c        |     | Scheme Prelude   |
|  (interpreter)   |     | (helper funcs)   |
+------------------+     +------------------+
        |                        |
        v                        v
+------------------+     +------------------+
|     main.c       | <-- | midi-out type    |
|  (entry point)   |     | (C object)       |
+------------------+     +------------------+
```

## Components

### 1. s7 Scheme - Interpreter

Location: `thirdparty/s7/s7.c`, `thirdparty/s7/s7.h`

s7 is a Scheme interpreter designed for embedding in C applications. Originally created for the Snd sound editor, it's a mature, well-documented implementation.

Key features:

- Full R5RS Scheme with extensions
- Easy C FFI via `s7_define_function`
- Custom C types via `s7_make_c_type`
- Garbage collection with marking support
- ~103K lines, single-file implementation

### 2. midi_module.c - FFI Bindings

Location: `projects/s7_midi/midi_module.c`

The core C module providing MIDI functionality to Scheme:

- Module initialization (`s7_midi_init`)
- C functions exposed to Scheme
- Custom `midi-out` type with destructor
- Scheme prelude (embedded Scheme code)

#### midi-out Type

```c
typedef struct {
    libremidi_midi_out_handle* handle;
    int is_virtual;
    char *port_name;
} MidiOutData;

static s7_int midi_out_tag = 0;
```

The `midi-out` type wraps a libremidi output handle with:

- Automatic cleanup via `free_midi_out`
- String representation via `midi_out_to_string`
- Type predicate via `is_midi_out`

### 3. libremidi - MIDI Backend

Location: `thirdparty/libremidi/`

Cross-platform MIDI library supporting:

- macOS: CoreMIDI
- Linux: ALSA
- Windows: WinMM

### 4. Scheme Prelude

The module includes an embedded Scheme prelude that defines:

- Pitch constants (c0-c8, cs0-cs8, etc.)
- Duration constants (whole, half, quarter, etc.)
- Velocity constants (ppp through fff)
- Chord builders (major, minor, dim, aug, dom7, maj7, min7)
- Tempo functions (set-tempo!, get-tempo, bpm)
- Helper functions (transpose, octave-up, octave-down)
- Arpeggio and rest functions

```c
static const char *scheme_prelude =
    ";; Dynamics (velocity values)\n"
    "(define ppp 16)\n"
    "(define pp 33)\n"
    // ... more Scheme code
    ;

// Loaded via:
s7_load_c_string(sc, scheme_prelude, strlen(scheme_prelude));
```

## s7 FFI Patterns

### Defining Functions

All C functions exposed to Scheme follow this pattern:

```c
static s7_pointer g_my_function(s7_scheme *sc, s7_pointer args) {
    // Extract arguments from args list
    s7_pointer arg1 = s7_car(args);
    s7_pointer arg2 = s7_cadr(args);

    // Type checking
    if (!s7_is_integer(arg1)) {
        return s7_wrong_type_error(sc, s7_make_symbol(sc, "my-function"),
                                   1, arg1, s7_make_string(sc, "integer"));
    }

    // Convert to C types
    int value = (int)s7_integer(arg1);

    // Do work...

    // Return result
    return s7_make_integer(sc, result);
    // Or for void functions:
    return s7_unspecified(sc);
}

// Registration:
s7_define_function(sc, "my-function", g_my_function,
                   2,      // required args
                   1,      // optional args
                   false,  // rest args?
                   "(my-function arg1 arg2 [opt]) documentation");
```

### Creating Custom Types

```c
// 1. Define type tag
static s7_int midi_out_tag = 0;

// 2. Define destructor
static void free_midi_out(void *val) {
    MidiOutData *data = (MidiOutData *)val;
    if (data) {
        if (data->handle) {
            libremidi_midi_out_free(data->handle);
        }
        free(data);
    }
}

// 3. Define to-string
static s7_pointer midi_out_to_string(s7_scheme *sc, s7_pointer args) {
    MidiOutData *data = (MidiOutData *)s7_c_object_value(s7_car(args));
    char buf[128];
    snprintf(buf, sizeof(buf), "#<midi-out %s>",
             data->handle ? "open" : "closed");
    return s7_make_string(sc, buf);
}

// 4. Register type in init
midi_out_tag = s7_make_c_type(sc, "midi-out");
s7_c_type_set_free(sc, midi_out_tag, free_midi_out);
s7_c_type_set_to_string(sc, midi_out_tag, midi_out_to_string);

// 5. Create instances
MidiOutData *data = malloc(sizeof(MidiOutData));
data->handle = handle;
return s7_make_c_object(sc, midi_out_tag, (void *)data);

// 6. Extract from Scheme
MidiOutData *data = (MidiOutData *)s7_c_object_value(s7_car(args));
```

### Error Handling

```c
// Wrong type error
return s7_wrong_type_error(sc, s7_make_symbol(sc, "func-name"),
                           1, arg, s7_make_string(sc, "expected type"));

// General error
return s7_error(sc, s7_make_symbol(sc, "error-type"),
                s7_list(sc, 1, s7_make_string(sc, "Error message")));
```

### Argument Extraction

```c
// Get pitch from integer, string, or symbol
static int get_pitch(s7_scheme *sc, s7_pointer arg) {
    if (s7_is_integer(arg)) {
        return (int)s7_integer(arg);
    } else if (s7_is_string(arg)) {
        return parse_pitch(s7_string(arg));
    } else if (s7_is_symbol(arg)) {
        return parse_pitch(s7_symbol_name(arg));
    }
    return -1;
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

## Module Initialization

The initialization sequence in `s7_midi_init()`:

1. Create `midi-out` custom type with destructor
2. Register all C functions with s7
3. Load Scheme prelude

```c
void s7_midi_init(s7_scheme *sc) {
    // 1. Create midi-out type
    midi_out_tag = s7_make_c_type(sc, "midi-out");
    s7_c_type_set_free(sc, midi_out_tag, free_midi_out);
    s7_c_type_set_to_string(sc, midi_out_tag, midi_out_to_string);

    // 2. Register functions
    s7_define_function(sc, "midi-open", g_midi_open, 0, 1, false, "...");
    s7_define_function(sc, "midi-close", g_midi_close, 1, 0, false, "...");
    s7_define_function(sc, "midi-note", g_midi_note, 2, 3, false, "...");
    // ... more functions

    // 3. Load prelude
    s7_load_c_string(sc, scheme_prelude, strlen(scheme_prelude));
}
```

## File Structure

```text
projects/s7_midi/
  main.c              # Entry point, REPL
  midi_module.c       # s7 FFI bindings (~870 lines)
  CMakeLists.txt      # Build configuration

thirdparty/s7/
  s7.c                # s7 interpreter (103K lines)
  s7.h                # s7 API header
  s7.html             # Documentation
  s7-ffi.html         # FFI documentation

tests/
  test_s7_midi.sh     # Test suite (21 tests)

docs/s7-midi/
  README.md           # Overview
  api-reference.md    # Complete API docs
  examples.md         # Code examples
  architecture.md     # This file
```

## Build Process

```text
s7.c + midi_module.c + main.c
           +
      libremidi.a
           |
           v
     s7_midi (executable)
```

CMake configuration:

```cmake
set(S7_DIR ${CMAKE_SOURCE_DIR}/thirdparty/s7)

add_executable(s7_midi
    main.c
    midi_module.c
    ${S7_DIR}/s7.c
)

target_include_directories(s7_midi PRIVATE ${S7_DIR})
target_link_libraries(s7_midi PRIVATE libremidi m)
```

## Extending the Module

### Adding a New C Function

1. Write the C function:

```c
static s7_pointer g_new_func(s7_scheme *sc, s7_pointer args) {
    // Implementation...
    return s7_unspecified(sc);
}
```

1. Register it in `s7_midi_init`:

```c
s7_define_function(sc, "new-func", g_new_func, 1, 0, false,
                   "(new-func arg) documentation");
```

### Adding via Scheme Prelude

For simpler additions, extend the prelude:

```c
static const char *scheme_prelude =
    // ... existing code ...
    "(define (new-helper x)\n"
    "  (* x 2))\n"
    ;
```

This is preferred for:

- Pure Scheme logic
- Helper functions
- Constants
- Higher-order functions

## Comparison with Other Implementations

| Aspect | s7_midi | pktpy_midi | midi_forth |
|--------|---------|------------|------------|
| Language | Scheme | Python | Forth |
| Paradigm | Functional | Object-oriented | Stack-based |
| FFI Style | s7_define_function | py_bindfunc | Direct C calls |
| Custom Types | s7_make_c_type | py_newtype | None |
| Prelude | Scheme code | Python code | Forth words |
| REPL | Built-in | Built-in | Built-in |
| Macros | Yes | No | No |
| Closures | Yes | Yes | No |
