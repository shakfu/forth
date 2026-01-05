# Self-Contained mhs-midi Binary

This document describes the design and implementation plan for creating a truly self-contained mhs-midi binary that embeds the MicroHs standard library and MIDI libraries directly in the executable.

## Goals

1. **Single binary distribution** - No external files required (no MHSDIR, no lib/ directories)
2. **Full functionality** - REPL and compiler with access to all standard and MIDI libraries
3. **No MicroHs source modifications** - Achieve embedding via C-level interception
4. **Cross-platform** - Works on macOS, Linux, and Windows

## Background

### Current Architecture

MicroHs compiles Haskell source to C via combinators:

```
Haskell source (.hs)
    |
    v
MicroHs compiler (mhs)
    |
    v
Combinator representation
    |
    v
C code with embedded bytecode (data[] array)
    |
    v
Native binary (linked with runtime)
```

The compiler itself (`mhs`) is distributed as pre-compiled C code in `generated/mhs.c`. This file contains the entire compiler logic as combinator bytecode, making MicroHs self-hosting without requiring GHC.

### Library Loading

When compiling Haskell code, MicroHs reads library source files from disk:

1. `openFilePath` in `Compile.hs` searches through `-i` include paths
2. Each path is tried with `fopen()` via the FFI binding `mhs_fopen`
3. Files are read into memory as strings for parsing

Key FFI binding in `eval.c`:
```c
from_t mhs_fopen(int s) {
    return mhs_from_Ptr(s, 2, fopen(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)));
}
```

### Files to Embed

| Category | Count | Size | Location |
|----------|-------|------|----------|
| MicroHs stdlib | 225 | ~1.5MB | `thirdparty/MicroHs/lib/` |
| MIDI libraries | 6 | ~76KB | `projects/mhs-midi/lib/` |
| **Total** | **231** | **~1.6MB** | |

## Design

### Virtual Filesystem Layer

Create a C module that intercepts file operations and serves embedded content:

```
+------------------+
|  Haskell Code    |
+------------------+
         |
         v
+------------------+
|  FFI: mhs_fopen  |  <-- Override this
+------------------+
         |
         v
+------------------+
|  vfs_fopen()     |  <-- New: check VFS first
+------------------+
         |
    +----+----+
    |         |
    v         v
+-------+  +-------+
|  VFS  |  | fopen |
| lookup|  | (real)|
+-------+  +-------+
```

### Embedded Library Format

Generate C headers containing library sources as string literals:

```c
// Generated: mhs_embedded_libs.h

typedef struct {
    const char* path;      // e.g., "lib/Prelude.hs"
    const char* content;   // File contents as string
    size_t length;
} EmbeddedFile;

static const EmbeddedFile embedded_files[] = {
    { "lib/Prelude.hs",
      "module Prelude(\n"
      "  module Control.Applicative,\n"
      "  ...\n"
      ") where\n"
      "...",
      12345 },
    { "lib/Data/List.hs",
      "module Data.List(...) where\n"
      "...",
      6789 },
    // ... 229 more entries
    { NULL, NULL, 0 }  // Sentinel
};
```

### Path Resolution Strategy

The VFS needs to handle various path formats MicroHs might request:

1. **Absolute paths**: `/path/to/MicroHs/lib/Prelude.hs`
2. **Relative paths**: `lib/Prelude.hs`, `Prelude.hs`
3. **Module paths**: `Data/List.hs`

Resolution algorithm:
```
vfs_lookup(path):
    1. If path contains "lib/" or matches known module pattern:
       - Extract relative portion after last "lib/"
       - Look up in embedded_files table
    2. If found, return embedded content
    3. Otherwise, return NULL (fall through to real filesystem)
```

### fmemopen for Memory-Backed FILE*

Use POSIX `fmemopen()` to create a FILE* from embedded string:

```c
FILE* vfs_fopen(const char* path, const char* mode) {
    const EmbeddedFile* ef = vfs_lookup(path);
    if (ef != NULL) {
        // Return memory-backed FILE handle
        return fmemopen((void*)ef->content, ef->length, "r");
    }
    // Fall back to real filesystem
    return fopen(path, mode);
}
```

**Windows consideration**: `fmemopen` is not available on Windows. Options:
- Use `tmpfile()` + `fwrite()` + `rewind()` as fallback
- Use a custom FILE* implementation
- Use `CreateFileMapping` + custom wrapper

## Implementation Plan

### Phase 1: Library Embedding Script

Create `scripts/embed_libs.py`:

```python
#!/usr/bin/env python3
"""
Convert Haskell library files to C header with embedded strings.

Usage: embed_libs.py <output.h> <libdir1> [libdir2 ...]
"""

def escape_c_string(content):
    """Escape content for C string literal."""
    # Handle backslashes, quotes, newlines, etc.
    ...

def generate_header(output_path, lib_dirs):
    """Generate C header with all embedded files."""
    files = []
    for lib_dir in lib_dirs:
        for hs_file in glob.glob(f"{lib_dir}/**/*.hs", recursive=True):
            rel_path = os.path.relpath(hs_file, os.path.dirname(lib_dir))
            content = open(hs_file).read()
            files.append((rel_path, content))

    with open(output_path, 'w') as f:
        f.write(HEADER_TEMPLATE)
        for path, content in files:
            f.write(f'  {{ "{path}",\n')
            f.write(f'    "{escape_c_string(content)}",\n')
            f.write(f'    {len(content)} }},\n')
        f.write('  { NULL, NULL, 0 }\n};\n')
```

### Phase 2: Virtual Filesystem Module

Create `projects/mhs-midi/vfs.c`:

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mhs_embedded_libs.h"

#ifdef _WIN32
// Windows fallback for fmemopen
static FILE* fmemopen_win(void* buf, size_t size, const char* mode) {
    FILE* tmp = tmpfile();
    if (tmp) {
        fwrite(buf, 1, size, tmp);
        rewind(tmp);
    }
    return tmp;
}
#define fmemopen fmemopen_win
#endif

const EmbeddedFile* vfs_lookup(const char* path) {
    // Normalize path - find "lib/" portion
    const char* lib_start = strstr(path, "lib/");
    const char* search_path = lib_start ? lib_start : path;

    for (const EmbeddedFile* ef = embedded_files; ef->path; ef++) {
        if (strcmp(ef->path, search_path) == 0 ||
            strstr(ef->path, search_path) != NULL) {
            return ef;
        }
    }
    return NULL;
}

FILE* vfs_fopen(const char* path, const char* mode) {
    // Only intercept read operations for .hs files
    if (mode[0] == 'r' && strstr(path, ".hs")) {
        const EmbeddedFile* ef = vfs_lookup(path);
        if (ef) {
            return fmemopen((void*)ef->content, ef->length, "r");
        }
    }
    return fopen(path, mode);
}
```

### Phase 3: FFI Override

Create `projects/mhs-midi/mhs_ffi_override.c`:

```c
#include "eval_types.h"  // MicroHs runtime types

// External reference to vfs_fopen
extern FILE* vfs_fopen(const char* path, const char* mode);

// Override the FFI binding
from_t mhs_fopen(int s) {
    const char* path = mhs_to_Ptr(s, 0);
    const char* mode = mhs_to_Ptr(s, 1);
    return mhs_from_Ptr(s, 2, vfs_fopen(path, mode));
}
```

### Phase 4: Build Integration

Update `projects/mhs-midi/CMakeLists.txt`:

```cmake
# Generate embedded library header
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/mhs_embedded_libs.h
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/scripts/embed_libs.py
        ${CMAKE_CURRENT_BINARY_DIR}/mhs_embedded_libs.h
        ${MHS_DIR}/lib
        ${CMAKE_CURRENT_SOURCE_DIR}/lib
    DEPENDS
        ${CMAKE_SOURCE_DIR}/scripts/embed_libs.py
        # Note: Should also depend on all .hs files
    COMMENT "Generating embedded library header"
)

add_executable(mhs-midi-standalone
    mhs_midi_main.c
    vfs.c
    mhs_ffi_override.c
    ${MHS_DIR}/generated/mhs.c
    ${MHS_DIR}/src/runtime/eval.c
    ${CMAKE_CURRENT_BINARY_DIR}/mhs_embedded_libs.h
)

target_include_directories(mhs-midi-standalone PRIVATE
    ${CMAKE_CURRENT_BINARY_DIR}
    ${MHS_DIR}/src/runtime
)

# Link order matters - our override must come before default
target_link_libraries(mhs-midi-standalone PRIVATE ...)
```

### Phase 5: Testing

1. **Basic compilation test**:
   ```bash
   ./mhs-midi-standalone -r test.hs
   ```
   Where `test.hs` imports standard library modules.

2. **MIDI library test**:
   ```bash
   ./mhs-midi-standalone -r midi_test.hs
   ```
   Where `midi_test.hs` imports Midi, Music modules.

3. **REPL test**:
   ```bash
   echo ':t map' | ./mhs-midi-standalone
   ```

4. **No external dependencies test**:
   ```bash
   unset MHSDIR
   rm -rf ~/.mcabal
   ./mhs-midi-standalone -r test.hs  # Should still work
   ```

## Challenges and Mitigations

### Challenge 1: Path Matching Complexity

MicroHs constructs paths in various ways depending on search path configuration.

**Mitigation**: Log all `fopen` calls during normal operation to understand path patterns:
```c
FILE* vfs_fopen(const char* path, const char* mode) {
    fprintf(stderr, "VFS: fopen(%s, %s)\n", path, mode);
    // ...
}
```

### Challenge 2: Binary Size

Embedding 1.6MB of source increases binary size significantly.

**Mitigations**:
- Compress embedded content (LZ4/zstd), decompress on first access
- Only embed commonly-used modules, fall back to filesystem for rare ones
- Strip comments and whitespace from embedded sources

### Challenge 3: Windows fmemopen

Windows lacks POSIX `fmemopen`.

**Mitigation**: Implement fallback using `tmpfile()`:
```c
static FILE* fmemopen_win(void* buf, size_t size, const char* mode) {
    FILE* tmp = tmpfile();
    fwrite(buf, 1, size, tmp);
    rewind(tmp);
    return tmp;
}
```

### Challenge 4: FFI Override Linking

Must ensure our `mhs_fopen` overrides the default, not conflicts.

**Mitigation**:
- Compile MicroHs runtime with `mhs_fopen` as weak symbol
- Or use `--wrap=mhs_fopen` linker flag
- Or modify eval.c to call through function pointer we can override

## Future Enhancements

1. **Selective embedding** - Analyze imports to embed only required modules
2. **Compressed storage** - Use LZ4 compression for embedded sources
3. **Precompiled cache** - Embed `.mhscache` for even faster startup
4. **Package support** - Embed `.pkg` binary packages instead of source

## References

- MicroHs repository: https://github.com/augustss/MicroHs
- MicroHs compilation model: `thirdparty/MicroHs/README.md`
- FFI implementation: `thirdparty/MicroHs/src/runtime/eval.c`
- File loading: `thirdparty/MicroHs/src/MicroHs/Compile.hs`
