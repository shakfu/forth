# lua-midi Architecture

This document explains how lua-midi integrates the Lua 5.5 interpreter with MIDI functionality.

## Overview

```text
+------------------+     +------------------+     +------------------+
|    Lua Code      | --> |  midi_module.c   | --> |   libremidi      |
| (user scripts)   |     | (Lua C bindings) |     | (MIDI backend)   |
+------------------+     +------------------+     +------------------+
        |                        |
        v                        v
+------------------+     +------------------+
|    Lua 5.5       |     |   Lua Prelude    |
|  (interpreter)   |     | (helper funcs)   |
+------------------+     +------------------+
        |                        |
        v                        v
+------------------+     +------------------+
|     main.c       | <-- |  MidiOut type    |
|  (entry point)   |     | (C userdata)     |
+------------------+     +------------------+
```

## Components

### 1. Lua 5.5 - Interpreter

Location: `thirdparty/lua-5.5.0/src/`

Lua 5.5 is the latest version of the Lua scripting language, designed for embedding in C applications.

Key features:

- Compact, efficient implementation (~25K lines)
- Clean C API for extension
- Userdata with metatables for custom types
- Garbage collection with __gc metamethod
- Coroutines for concurrent patterns

### 2. midi_module.c - Lua C Bindings

Location: `projects/lua-midi/midi_module.c`

The core C module providing MIDI functionality to Lua:

- Module initialization (`luaopen_midi`)
- C functions exposed to Lua
- `MidiOut` userdata type with metatable
- Lua prelude (embedded Lua code)

#### MidiOut Userdata

```c
typedef struct {
    libremidi_midi_out_handle* handle;
    int is_virtual;
    char *port_name;
} MidiOutData;

#define MIDI_OUT_MT "MidiOut"
```

The `MidiOut` type wraps a libremidi output handle with:

- Automatic cleanup via `__gc` metamethod
- String representation via `__tostring` metamethod
- Method dispatch via `__index` metamethod

### 3. libremidi - MIDI Backend

Location: `thirdparty/libremidi/`

Cross-platform MIDI library supporting:

- macOS: CoreMIDI
- Linux: ALSA
- Windows: WinMM

### 4. Lua Prelude

The module includes an embedded Lua prelude that defines:

- Pitch constants (midi.c0-c8, midi.cs0-cs8, etc.)
- Duration constants (midi.whole, half, quarter, etc.)
- Velocity constants (midi.ppp through midi.fff)
- Tempo functions (midi.set_tempo, midi.get_tempo, midi.bpm)
- Helper functions (midi.dotted, midi.rest)
- REPL convenience functions (open, close, n, ch, arp)

```c
static const char *lua_prelude =
    "-- Pitch constants\n"
    "for oct = 0, 8 do\n"
    "  midi['c'..oct] = 12 + oct * 12\n"
    // ... more Lua code
    ;

// Loaded via:
luaL_dostring(L, lua_prelude);
```

## Lua C API Patterns

### Defining Functions

All C functions exposed to Lua follow this pattern:

```c
static int l_my_function(lua_State *L) {
    // Get arguments from stack (1-indexed)
    int arg1 = luaL_checkinteger(L, 1);
    const char *arg2 = luaL_checkstring(L, 2);
    int opt_arg = luaL_optinteger(L, 3, 42);  // Optional with default

    // Type checking is done by luaL_check* functions
    // They raise an error if type doesn't match

    // Do work...

    // Push result onto stack
    lua_pushinteger(L, result);

    // Return number of values on stack
    return 1;  // or 0 for no return value
}

// Registration:
static const luaL_Reg my_funcs[] = {
    {"my_function", l_my_function},
    {NULL, NULL}  // Sentinel
};

luaL_newlib(L, my_funcs);
```

### Creating Userdata Types

```c
// 1. Define metatable name
#define MY_TYPE_MT "MyType"

// 2. Create userdata instance
MyData *data = (MyData *)lua_newuserdata(L, sizeof(MyData));
data->field = value;

// 3. Set metatable
luaL_getmetatable(L, MY_TYPE_MT);
lua_setmetatable(L, -2);

// 4. Define destructor
static int my_type_gc(lua_State *L) {
    MyData *data = (MyData *)luaL_checkudata(L, 1, MY_TYPE_MT);
    if (data->resource) {
        free_resource(data->resource);
        data->resource = NULL;
    }
    return 0;
}

// 5. Define methods
static int my_type_method(lua_State *L) {
    MyData *data = (MyData *)luaL_checkudata(L, 1, MY_TYPE_MT);
    // Use data...
    return 0;
}

// 6. Register in module init
static const luaL_Reg my_type_methods[] = {
    {"method", my_type_method},
    {NULL, NULL}
};

luaL_newmetatable(L, MY_TYPE_MT);
lua_pushvalue(L, -1);
lua_setfield(L, -2, "__index");  // mt.__index = mt
lua_pushcfunction(L, my_type_gc);
lua_setfield(L, -2, "__gc");
luaL_setfuncs(L, my_type_methods, 0);
lua_pop(L, 1);
```

### Error Handling

```c
// Raise error with message
return luaL_error(L, "Invalid value: %d", value);

// Type error
return luaL_argerror(L, 1, "expected integer");

// Protected call for Lua code
if (luaL_dostring(L, code) != LUA_OK) {
    const char *err = lua_tostring(L, -1);
    fprintf(stderr, "Error: %s\n", err);
    lua_pop(L, 1);
}
```

### Argument Extraction

```c
// Get pitch from integer or string
static int get_pitch(lua_State *L, int idx) {
    if (lua_isinteger(L, idx)) {
        return (int)lua_tointeger(L, idx);
    } else if (lua_isnumber(L, idx)) {
        return (int)lua_tonumber(L, idx);
    } else if (lua_isstring(L, idx)) {
        return parse_pitch(lua_tostring(L, idx));
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

The initialization sequence in `luaopen_midi()`:

1. Create `MidiOut` metatable with methods and __gc
2. Create module table with functions
3. Set module as global `midi`
4. Load Lua prelude
5. Return module table

```c
int luaopen_midi(lua_State *L) {
    // 1. Create MidiOut metatable
    luaL_newmetatable(L, MIDI_OUT_MT);
    lua_pushvalue(L, -1);
    lua_setfield(L, -2, "__index");
    lua_pushcfunction(L, midi_out_gc);
    lua_setfield(L, -2, "__gc");
    lua_pushcfunction(L, midi_out_tostring);
    lua_setfield(L, -2, "__tostring");
    luaL_setfuncs(L, midi_methods, 0);
    lua_pop(L, 1);

    // 2. Create module table
    luaL_newlib(L, midi_funcs);

    // 3. Set as global for prelude
    lua_setglobal(L, "midi");

    // 4. Load prelude
    luaL_dostring(L, lua_prelude);

    // 5. Return module
    lua_getglobal(L, "midi");
    return 1;
}
```

## File Structure

```text
projects/lua-midi/
  main.c              # Entry point, REPL with readline
  midi_module.c       # Lua C bindings (~900 lines)
  CMakeLists.txt      # Build configuration

thirdparty/lua-5.5.0/src/
  lua.h               # Lua C API
  lauxlib.h           # Auxiliary library
  lualib.h            # Standard libraries
  *.c                 # Lua implementation (~25K lines total)

tests/
  test_lua_midi.sh    # Test suite (26 tests)

docs/lua-midi/
  README.md           # Overview
  api-reference.md    # Complete API docs
  examples.md         # Code examples
  architecture.md     # This file
```

## Build Process

```text
lua-5.5.0/*.c + midi_module.c + main.c
                   +
              libremidi.a
                   |
                   v
          lua_midi (executable)
```

CMake configuration:

```cmake
set(LUA_DIR ${CMAKE_SOURCE_DIR}/thirdparty/lua-5.5.0/src)

add_executable(lua_midi
    main.c
    midi_module.c
    ${LUA_CORE_SOURCES}
    ${LUA_LIB_SOURCES}
)

target_include_directories(lua_midi PRIVATE ${LUA_DIR})
target_link_libraries(lua_midi PRIVATE libremidi m)

# Optional readline support
find_library(READLINE_LIB readline)
if(READLINE_LIB)
    target_compile_definitions(lua_midi PRIVATE USE_READLINE)
    target_link_libraries(lua_midi PRIVATE ${READLINE_LIB})
endif()
```

## Extending the Module

### Adding a New C Function

1. Write the C function:

```c
static int l_new_func(lua_State *L) {
    int arg = luaL_checkinteger(L, 1);
    // Implementation...
    lua_pushinteger(L, result);
    return 1;
}
```

1. Register it in `midi_funcs`:

```c
static const luaL_Reg midi_funcs[] = {
    // ... existing functions
    {"new_func", l_new_func},
    {NULL, NULL}
};
```

### Adding via Lua Prelude

For simpler additions, extend the prelude:

```c
static const char *lua_prelude =
    // ... existing code ...
    "function midi.new_helper(x)\n"
    "  return x * 2\n"
    "end\n"
    ;
```

This is preferred for:

- Pure Lua logic
- Helper functions
- Constants
- Higher-order functions

## Comparison with Other Implementations

| Aspect | lua-midi | s7-midi | pktpy-midi | forth-midi |
|--------|----------|---------|------------|------------|
| Language | Lua | Scheme | Python | Forth |
| Paradigm | Imperative | Functional | Object-oriented | Stack-based |
| FFI Style | luaL_newlib | s7_define_function | py_bindfunc | Direct C calls |
| Custom Types | Userdata+metatable | s7_make_c_type | py_newtype | None |
| Prelude | Lua code | Scheme code | Python code | Forth words |
| REPL | Built-in+readline | Built-in | Built-in | Built-in |
| Macros | No | Yes | No | No |
| Closures | Yes | Yes | Yes | No |
| Size | 656 KB | 1.8 MB | 1.0 MB | 459 KB |
