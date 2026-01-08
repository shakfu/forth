# Self-Contained mhs-midi Binaries

Self-contained mhs-midi binaries with all MicroHs libraries embedded, eliminating external file dependencies.

## Build Variants

The mhs-midi project builds multiple executable variants with different trade-offs:

| Executable | Size | Cold Start | Description |
|------------|------|------------|-------------|
| `mhs-midi` | 782KB | ~20s | Requires MHSDIR (auto-detected or set manually) |
| `mhs-midi-src` | 3.3MB | ~20s | Source embedding (default standalone) |
| `mhs-midi-src-zstd` | 1.3MB | ~20s | Compressed source (smallest binary) |
| `mhs-midi-pkg` | 4.8MB | ~1s | Package embedding (fastest startup) |
| `mhs-midi-pkg-zstd` | 3.0MB | ~1s | Compressed packages (best balance) |

All standalone variants provide identical functionality (REPL, compile, run) and are fully self-contained.

## Status: Complete

The standalone binary is fully working:
- 273 files embedded (~2.5MB total)
- All 185 Haskell modules load correctly
- REPL, run, and compile modes all work
- Can compile MIDI programs to standalone executables
- All tests pass

## Goals

1. **Single binary distribution** - No external files required (no MHSDIR, no lib/ directories)
2. **Full functionality** - REPL and compiler with access to all standard and MIDI libraries
3. **No MicroHs source modifications** - Achieve embedding via C-level interception
4. **Cross-platform** - Works on macOS and Linux (Windows has separate MicroHs build issues)

## Usage

### Building

```sh
# Build all variants
cmake --build build --target mhs-midi-all

# Or build individual variants:
cmake --build build --target mhs-midi-src        # Source embedding (default)
cmake --build build --target mhs-midi-src-zstd   # Compressed source
cmake --build build --target mhs-midi-pkg        # Package embedding (fastest)
cmake --build build --target mhs-midi-pkg-zstd   # Compressed packages
```

### Running

```sh
# Standalone variants (no external files needed)
./build/mhs-midi-src -r MyFile.hs
./build/mhs-midi-pkg-zstd              # Recommended for end users

# Non-standalone (requires MHSDIR)
MHSDIR=./thirdparty/MicroHs ./build/mhs-midi -r MyFile.hs
./build/mhs-midi  # auto-detects MHSDIR
```

## Background

### MicroHs Architecture

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

The compiler itself (`mhs`) is distributed as pre-compiled C code in `generated/mhs.c`, making MicroHs self-hosting without requiring GHC.

### Library Loading

When compiling Haskell code, MicroHs reads library source files via the FFI binding `mhs_fopen` in `eval.c`:

```c
from_t mhs_fopen(int s) {
    return mhs_from_Ptr(s, 2, fopen(mhs_to_Ptr(s, 0), mhs_to_Ptr(s, 1)));
}
```

### Files Embedded

| Category | Count | Size | Location |
|----------|-------|------|----------|
| MicroHs stdlib (.hs) | ~230 | ~1MB | `thirdparty/MicroHs/lib/` |
| MIDI Haskell libs (.hs) | ~10 | ~50KB | `projects/mhs-midi/lib/` |
| Runtime (C/H files) | 28 | ~250KB | `thirdparty/MicroHs/src/runtime/` |
| MIDI FFI header | 1 | ~2KB | `projects/mhs-midi/midi_ffi.h` |
| Static libraries (.a) | 4 | ~1.2MB | libremidi, midi_ffi, music_theory, midi_file |
| **Total** | **273** | **~2.5MB** | |

The static libraries enable compiling MIDI programs to standalone executables without requiring libremidi or other dependencies on the target system.

## Implementation

### Architecture

```
+---------------------------------------------------------+
|        Standalone Variants (mhs-midi-src/pkg[-zstd])     |
+---------------------------------------------------------+
|  mhs_midi_standalone_main.c                             |
|    - Initializes VFS                                    |
|    - Detects compilation mode (-o without .c)           |
|    - For -r/REPL: uses VFS (MHSDIR=/mhs-embedded)       |
|    - For -o exe: extracts to temp, injects -optl flags  |
|    - Calls mhs_main()                                   |
+---------------------------------------------------------+
|  mhs_ffi_override.c                                     |
|    - mhs_fopen() checks VFS first, falls back to fopen()|
+---------------------------------------------------------+
|  vfs.c                                                  |
|    - Serves files via VFS_USE_PKG and/or VFS_USE_ZSTD   |
|    - vfs_fopen() looks up path in embedded data         |
|    - Returns fmemopen(content, length, "r") for matches |
|    - Decompresses zstd content on demand (if enabled)   |
|    - vfs_extract_to_temp() for compilation mode         |
+---------------------------------------------------------+
|  Embedded Header (variant-specific, generated)          |
|    mhs_embedded_libs.h      - Source files (~2.5MB)     |
|    mhs_embedded_zstd.h      - Compressed source (~367KB)|
|    mhs_embedded_pkgs.h      - Package files (~2.6MB)    |
|    mhs_embedded_pkgs_zstd.h - Compressed pkgs (~1.3MB)  |
+---------------------------------------------------------+
|  eval_vfs.c (patched from eval.c)                       |
|    - Original mhs_fopen renamed to mhs_fopen_orig       |
|    - Forward declaration for override                   |
+---------------------------------------------------------+

+---------------------------------------------------------+
|                      mhs-midi (782KB)                    |
+---------------------------------------------------------+
|  mhs_midi_main.c                                        |
|    - Auto-detects MHSDIR from executable location       |
|    - Finds MIDI lib directory                           |
|    - Calls mhs_main()                                   |
+---------------------------------------------------------+
|  eval.c (unpatched)                                     |
|    - Uses standard fopen() for file access              |
+---------------------------------------------------------+
```

### Compilation Modes

The standalone binary handles different modes:

| Mode | Command | Mechanism |
|------|---------|-----------|
| REPL | `./mhs-midi-standalone` | VFS serves files from memory |
| Run | `-r File.hs` | VFS serves files from memory |
| C output | `-o File.c` | VFS serves files from memory |
| Executable | `-o File` | Extract to temp, link with embedded libs |

For executable compilation, the standalone:
1. Extracts all embedded files to `/tmp/mhs-XXXXXX/`
2. Sets `MHSDIR` to the temp directory
3. Injects `-optl` flags for MIDI libraries and platform frameworks
4. Cleans up temp directory after compilation

### Key Components

**1. `scripts/embed_libs.py`** - Converts files to a C header with embedded content:

```sh
# Usage with all options:
embed_libs.py output.h lib_dirs... \
    --runtime src/runtime \
    --header midi_ffi.h \
    --lib liblibremidi.a \
    --lib libmidi_ffi.a
```

```c
// Generated: mhs_embedded_libs.h
typedef struct {
    const char* path;
    const char* content;
    size_t length;
} EmbeddedFile;

static const EmbeddedFile embedded_files[] = {
    { "lib/Prelude.hs", "module Prelude...", 12345 },
    { "src/runtime/eval.c", "/* eval.c */...", 198000 },
    { "lib/liblibremidi.a", "<binary>", 1143392 },
    // ... 270 more entries
    { NULL, NULL, 0 }
};
```

**2. `projects/mhs-midi/vfs.c`** - Virtual filesystem using fmemopen:

```c
FILE* vfs_fopen(const char* path, const char* mode) {
    if (strncmp(path, VFS_VIRTUAL_ROOT, root_len) == 0) {
        const EmbeddedFile* ef = find_embedded_file(rel_path);
        if (ef) {
            return fmemopen((void*)ef->content, ef->length, "r");
        }
        return NULL;
    }
    return fopen(path, mode);  // Fallback to real filesystem
}
```

**3. `projects/mhs-midi/mhs_ffi_override.c`** - FFI intercept:

```c
from_t mhs_fopen(int s) {
    const char* path = mhs_to_Ptr(s, 0);
    const char* mode = mhs_to_Ptr(s, 1);
    FILE* result = vfs_fopen(path, mode);
    return mhs_from_Ptr(s, 2, result);
}
```

**4. `scripts/patch_eval_vfs.py`** - Patches eval.c to rename original mhs_fopen

### Build Integration

The CMake build uses a function to define each variant:

```cmake
# Function to create standalone variant with specific VFS mode
function(add_mhs_standalone_variant TARGET_NAME HEADER_FILE)
    cmake_parse_arguments(ARG "USE_PKG;USE_ZSTD" "" "" ${ARGN})

    add_executable(${TARGET_NAME} ...)
    target_include_directories(${TARGET_NAME} PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

    if(ARG_USE_PKG)
        target_compile_definitions(${TARGET_NAME} PRIVATE VFS_USE_PKG)
    endif()
    if(ARG_USE_ZSTD)
        target_compile_definitions(${TARGET_NAME} PRIVATE VFS_USE_ZSTD)
        target_link_libraries(${TARGET_NAME} PRIVATE zstddeclib)
    endif()
endfunction()

# Define all variants
add_mhs_standalone_variant(mhs-midi-src ${EMBEDDED_LIBS_HEADER})
add_mhs_standalone_variant(mhs-midi-src-zstd ${EMBEDDED_ZSTD_HEADER} USE_ZSTD)
add_mhs_standalone_variant(mhs-midi-pkg ${EMBEDDED_PKG_HEADER} USE_PKG)
add_mhs_standalone_variant(mhs-midi-pkg-zstd ${EMBEDDED_PKG_ZSTD_HEADER} USE_PKG USE_ZSTD)

# Convenience target for all variants
add_custom_target(mhs-midi-all DEPENDS
    mhs-midi-src mhs-midi-src-zstd mhs-midi-pkg mhs-midi-pkg-zstd
)

# Non-standalone binary (uses filesystem)
add_executable(mhs-midi
    ${CMAKE_CURRENT_BINARY_DIR}/mhs_repl.c
    mhs_midi_main.c              # MHSDIR auto-detection
    ${MHS_RUNTIME}/eval.c        # Unpatched eval.c
    ...
)
```

## Development Journey

### Approach 1: Temp Directory Extraction (Failed)

**Approach**: Extract embedded files to a temp directory at startup, set `MHSDIR` to point there.

**Problem**: MicroHs loaded only 163 of 185 modules, then produced "ERR: getb_utf8" error. The REPL hung after loading Prelude.

**Investigation**: Files were extracted correctly and byte-identical to originals. Issue was specific to the embedding approach.

### Approach 2: Pure Memory VFS with fmemopen (Initial Failure, Then Success)

**Approach**: Serve embedded files directly from memory using `fmemopen()`.

**Initial Result**: Same "ERR: getb_utf8" error after loading ~163 modules.

### The Root Cause: UTF-8 Encoding Bug

The error originated in MicroHs's `bfile.c` in `getb_utf8()`, which validates UTF-8 byte sequences. Key insight: `getb_utf8` is called via **function pointer dispatch** (`p->getb(p)`), not direct calls.

**Isolating the Problem**:
1. Redirected stdin to /dev/null - error still occurred
2. Closed stdin entirely - error still occurred (not stdin-related)
3. Added VFS debug logging - files opening correctly
4. Added first-byte verification - bytes matched expected
5. Checked `Data/Bifunctor.hs` - found Unicode content (mathematical symbol)
6. Compared byte vs character counts - **found the mismatch**

**The Bug in `embed_libs.py`**:

`Data/Bifunctor.hs` contains the `U+2261` symbol. The script had two bugs:

```python
# BUG 1: Wrong length - counts Unicode characters, not bytes
content = file.read_text()
len(content)  # Returns 4971 (characters), but file has 4995 bytes!

# BUG 2: Invalid escaping for Unicode codepoints
f'\\{ord(char):03o}'  # ord('...') = 8801 -> '\21141' (5 digits!)
# C octal escapes only support 3 digits (0-377)
```

**The Fix**:

```python
# Read as bytes, escape each byte individually
def escape_c_string(content: bytes) -> str:
    for byte in content:
        if byte > 126:
            result.append(f'\\{byte:03o}')  # Always valid 0-377
```

### Challenge: FFI Override Linking

**Problem**: Both `eval.c` and `mhs_ffi_override.c` define `mhs_fopen`, causing duplicate symbol errors.

**Solution**: `patch_eval_vfs.py` renames `mhs_fopen` to `mhs_fopen_orig` in eval.c and adds forward declaration for our override.

### Challenge: Windows Support

**Status**: Not implemented. MicroHs has separate build issues on Windows.

**Note**: `fmemopen` doesn't exist on Windows. A fallback using `tmpfile()` would be required:

```c
#ifdef _WIN32
FILE* fmemopen_win(void* buf, size_t size, const char* mode) {
    FILE* tmp = tmpfile();
    fwrite(buf, 1, size, tmp);
    rewind(tmp);
    return tmp;
}
#define fmemopen fmemopen_win
#endif
```

## File Structure

```
projects/mhs-midi/
    mhs_midi_main.c             # Entry point for mhs-midi (non-standalone)
    mhs_midi_standalone_main.c  # Entry point for mhs-midi-standalone
    vfs.c                       # Virtual filesystem implementation
    vfs.h                       # VFS header
    mhs_ffi_override.c          # FFI intercept for mhs_fopen

scripts/
    embed_libs.py               # Convert .hs files to C header
    patch_eval_vfs.py           # Patch eval.c for VFS override

build/projects/mhs-midi/
    mhs_embedded_libs.h         # Generated embedded content
    eval_vfs.c                  # Patched eval.c
```

## Lessons Learned

1. **Character vs byte counts matter**: Python's `len()` on a string counts Unicode code points, not bytes. For binary embedding, always work with bytes.

2. **C octal escapes are limited**: `\NNN` only handles values 0-255. Unicode codepoints must be UTF-8 encoded first, then each byte escaped individually.

3. **Function pointer dispatch hides call sites**: Searching for `getb_utf8` found no callers because it's called via `p->getb(p)`. Understanding the virtual dispatch mechanism was key.

4. **Isolate variables systematically**: Closing stdin, enabling debug logging, and checking byte values at each step helped narrow down the root cause.

5. **fmemopen works well**: Once the data is correct, `fmemopen()` is an elegant solution for serving embedded content as `FILE*` streams with zero filesystem overhead.

## Future Enhancements

1. ~~**Compression** - Use LZ4/zstd to reduce embedded size~~ **Done** - mhs-midi-src-zstd reduces binary from 3.3MB to 1.3MB
2. ~~**Precompiled packages** - Embed `.pkg` files for faster startup~~ **Done** - mhs-midi-pkg variants start in ~1s vs ~20s
3. **Selective embedding** - Analyze imports to embed only required modules
4. **Windows support** - Implement fmemopen fallback when MicroHs Windows builds work
5. **Incremental extraction** - Only extract files needed for specific compilation

## References

- MicroHs repository: https://github.com/augustss/MicroHs
- fmemopen(3) man page
