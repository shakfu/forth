This document describes how to create a self-contained MicroHs-based application that embeds all MicroHaskell libraries and can compile programs to standalone executables without any external file dependencies.

## Background

[mhs-midi](https://github.com/shakfu/midi-langs) is a MIDI-oriented music programming environment built on MicroHs. It is one of several language implementations that compile into convenient, self-contained executables. In its initial form, `mhs-midi` required extensive manual configuration, including specifying multiple directories such as `MHSDIR`, which points to the original MicroHs installation. To simplify this setup, a Python wrapper script was introduced. This script handled environment configuration and served as a REPL and compiler frontend for running and compiling MIDI-focused Haskell programs, while also integrating with the C/C++ `libremidi` library via FFI.

Despite these improvements, `mhs-midi` remained an outlier: relocating it without friction was still difficult. Ideally, it should be distributable as a single executable that users can download and run immediately, without installing MicroHs or configuring `MHSDIR`. To explore this possibility, an experiment was designed with the following objectives:

1. Single-binary distribution — no external files or dependencies required
2. Full featured — REPL, run, and compile modes all work
3. No MicroHs source changes — achieve embedding via C-level interception
4. Standalone compilation — user programs can be compiled into independent executables

## The Approach: Virtual Filesystem with fmemopen

MicroHs reads library files through the `mhs_fopen` FFI function in `eval.c`. The proposed approach intercepts this function to serve embedded files from memory.

### Step 1: Embed Files as C Arrays

A script converts all `.hs` files to a C header. We provide both Python and C implementations:

```python
# scripts/embed_libs.py (simplified)
def escape_c_string(content: bytes) -> str:
    """Escape raw bytes for C string literal."""
    result = []
    for byte in content:
        if byte == ord('\\'):
            result.append('\\\\')
        elif byte == ord('"'):
            result.append('\\"')
        elif byte == ord('\n'):
            result.append('\\n')
        elif byte < 32 or byte > 126:
            # Non-printable: use octal escape
            result.append(f'\\{byte:03o}')
        else:
            result.append(chr(byte))
    return ''.join(result)

# Generate header
for vfs_path, full_path, content in files:
    escaped = escape_c_string(content)
    f.write(f'    {{ "{vfs_path}", "{escaped}", {len(content)} }},\n')
```

This generates a header like:

```c
// mhs_embedded_libs.h (generated)
typedef struct {
    const char* path;
    const char* content;
    size_t length;
} EmbeddedFile;

static const EmbeddedFile embedded_files[] = {
    { "lib/Prelude.hs", "module Prelude...", 12345 },
    { "lib/Data/List.hs", "module Data.List...", 5678 },
    // ... 240 more entries
    { NULL, NULL, 0 }  // Sentinel
};
```

#### C Implementation (for MicroHs Integration)

Provide a pure C implementation (`scripts/embed_libs.c`) suitable for integration into MicroHs itself:

```c
// Core byte escaping - handles UTF-8 correctly
static int escape_byte(unsigned char byte, char* buf) {
    switch (byte) {
        case '\\': return sprintf(buf, "\\\\");
        case '"':  return sprintf(buf, "\\\"");
        case '\n': return sprintf(buf, "\\n");
        default:
            if (byte < 32 || byte > 126)
                return sprintf(buf, "\\%03o", byte);  // Octal escape
            buf[0] = byte; buf[1] = '\0';
            return 1;
    }
}
```

Usage:

```bash
# Compile the tool
gcc -o embed_libs scripts/embed_libs.c

# Generate header with all dependencies
./embed_libs mhs_embedded.h lib/ \
    --runtime src/runtime/ \
    --header midi_ffi.h \
    --lib liblibremidi.a \
    --lib libmidi_ffi.a
```

Output:
```
Collecting .hs files from lib/...
Collecting runtime files from src/runtime/...
Embedding headers:
  midi_ffi.h (2100 bytes)
Embedding libraries:
  liblibremidi.a (1143392 bytes)
  libmidi_ffi.a (18256 bytes)
Generated: mhs_embedded.h (265 files, 2447946 bytes)
```

The C implementation has no dependencies beyond libc and is portable to any POSIX system.

### Step 2: Virtual Filesystem Using `fmemopen`

The VFS serves embedded files as `FILE*` streams:

```c
// vfs.c
#include "mhs_embedded_libs.h"

#define VFS_VIRTUAL_ROOT "/mhs-embedded"

static const EmbeddedFile* find_embedded_file(const char* rel_path) {
    for (const EmbeddedFile* ef = embedded_files; ef->path; ef++) {
        if (strcmp(ef->path, rel_path) == 0) {
            return ef;
        }
    }
    return NULL;
}

FILE* vfs_fopen(const char* path, const char* mode) {
    // Check if path starts with our virtual root
    if (strncmp(path, VFS_VIRTUAL_ROOT, strlen(VFS_VIRTUAL_ROOT)) == 0) {
        const char* rel_path = path + strlen(VFS_VIRTUAL_ROOT) + 1;
        const EmbeddedFile* ef = find_embedded_file(rel_path);
        if (ef) {
            // Serve from memory - no disk access!
            return fmemopen((void*)ef->content, ef->length, "r");
        }
        return NULL;  // File not found in VFS
    }
    // Fall back to real filesystem
    return fopen(path, mode);
}
```

### Step 3: Intercept MicroHs FFI

As MicroHs calls `mhs_fopen` for all file operations, rename the original and provide an override:

```python
# scripts/patch_eval_vfs.py
# Renames mhs_fopen to mhs_fopen_orig in eval.c
# Adds forward declaration for our override
```

```c
// mhs_ffi_override.c
// Forward declaration of original (renamed by patch script)
from_t mhs_fopen_orig(int s);

// Our override that checks VFS first
from_t mhs_fopen(int s) {
    const char* path = mhs_to_Ptr(s, 0);
    const char* mode = mhs_to_Ptr(s, 1);

    // Try VFS first
    FILE* result = vfs_fopen(path, mode);

    return mhs_from_Ptr(s, 2, result);
}
```

### Step 4: Set MHSDIR to Virtual Root

At startup, point `MHSDIR` to the virtual filesystem:

```c
// main.c
int main(int argc, char** argv) {
    vfs_init();
    setenv("MHSDIR", "/mhs-embedded", 1);
    return mhs_main(argc, argv);
}
```

MicroHs now constructs paths like `/mhs-embedded/lib/Prelude.hs`, which the VFS intercepts and serves from memory.

## Challenge 1: UTF-8 Encoding Bug

**Symptom:** After loading ~163 of 185 modules, we got `ERR: getb_utf8`. 

**Root Cause:** The embedding script had two bugs:

```python
# BUG 1: Wrong length - counts Unicode characters, not bytes
content = file.read_text()
len(content)  # Returns 4971 characters, but file has 4995 bytes!

# BUG 2: Invalid octal escapes for high Unicode codepoints
f'\\{ord(char):03o}'  # ord('...') = 8801 -> '\21141' (invalid!)
```

The file `Data/Bifunctor.hs` contains the Unicode character `...` (U+2261). Python's `len()` on a string counts code points, not bytes. And C octal escapes only support values 0-377 (0-255).

**Solution:** Work with bytes, not strings:

```python
def escape_c_string(content: bytes) -> str:
    for byte in content:  # Iterate bytes, not characters
        if byte > 126:
            result.append(f'\\{byte:03o}')  # Always valid 0-255
```

## Challenge 2: Compilation to Executable

When users compile with `-o program` (not `-o program.c`), MicroHs invokes `cc` which needs real files on disk. The VFS only works within the MicroHs process.

**Solution:** Detect compilation mode and extract to temp directory:

```c
static int needs_extraction(int argc, char** argv) {
    for (int i = 1; i < argc; i++) {
        if (strncmp(argv[i], "-o", 2) == 0) {
            const char* output = (argv[i][2]) ? argv[i] + 2 : argv[i + 1];
            if (output) {
                size_t len = strlen(output);
                // .c output works with VFS, executable needs extraction
                if (len >= 2 && strcmp(output + len - 2, ".c") == 0)
                    return 0;
                return 1;
            }
        }
    }
    return 0;
}

int main(int argc, char** argv) {
    vfs_init();

    if (needs_extraction(argc, argv)) {
        char* temp_dir = vfs_extract_to_temp();
        setenv("MHSDIR", temp_dir, 1);
        // ... run mhs_main ...
        vfs_cleanup_temp(temp_dir);
    } else {
        setenv("MHSDIR", "/mhs-embedded", 1);
        // ... run mhs_main ...
    }
}
```

## Challenge 3: Linking MIDI Libraries

For our MIDI project, compiled executables need to link against libremidi and other libraries. We embed the static libraries (`.a` files) and inject linker flags:

```c
if (linking_midi) {
    // Extract includes .a files to temp_dir/lib/
    new_argv[j++] = "-optl";
    new_argv[j++] = lib_path;  // e.g., /tmp/mhs-xxx/lib/libmidi_ffi.a

    #ifdef __APPLE__
    new_argv[j++] = "-optl"; new_argv[j++] = "-framework";
    new_argv[j++] = "-optl"; new_argv[j++] = "CoreMIDI";
    #endif
}
```

## Generalizing for Other Projects

This approach turned out to be quite useful, and can be adapted for any MicroHs-based application:

### 1. Minimal Standalone (REPL/Run only)

If you only need REPL and `-r` (run) modes, you just need:

- `embed_libs.py` or `embed_lib.c` - embed your `.hs` libraries
- `vfs.c` - the fmemopen-based VFS
- `mhs_ffi_override.c` - intercept `mhs_fopen`
- `patch_eval_vfs.py` - rename original `mhs_fopen`

This gives you a ~1MB overhead for the MicroHs standard library.

### 2. Full Standalone (with Compilation)

To support `-o executable`:

- Also embed `src/runtime/*.c` and `src/runtime/*.h`
- Add `vfs_extract_to_temp()` function
- Detect `-o` without `.c` suffix and extract

### 3. With Additional Libraries

If your application has C dependencies:

- Embed static libraries (`.a` files) as binary content
- Inject `-optl` flags when compiling to executable
- Include any required headers

### Potential MicroHs Enhancement

A potential enhancement to MicroHs itself could be a compile-time option to embed libraries:

```
mhs --embed-libs=./lib --embed-runtime -o my_repl MyRepl.hs
```

This would generate a single C file with embedded libraries, eliminating the need for external VFS machinery. The generated REPL would be truly standalone.

The C implementation of `embed_libs.c` (~500 lines, no dependencies) could be integrated directly into MicroHs to provide this functionality. It handles:

- Recursive directory traversal for `.hs` and `.hs-boot` files
- Runtime C/H file embedding (`--runtime`)
- Static library embedding (`--lib`)
- Header file embedding (`--header`)
- Proper UTF-8 byte escaping
- String literal chunking for C89 compatibility

## Results

| Binary | Size | Capabilities |
|--------|------|--------------|
| mhs-midi | 782KB | Requires MHSDIR (auto-detected) |
| mhs-midi-standalone | 3.2MB | Fully self-contained |
| mhs-midi-standalone (zstd) | 1.3MB | Fully self-contained, compressed |

The standalone binary:
- Embeds 273 files (~2.5MB of content)
- Starts REPL in ~0.5s (with `-C` caching)
- Can compile HelloMidi.hs to 859KB standalone executable
- Works on macOS and Linux (Windows has separate MicroHs build issues)

## Optional: Zstd Compression

The embedded files can be compressed using zstd with dictionary mode to significantly reduce binary size. This is an optional build-time feature controlled by the `MHS_USE_ZSTD` CMake option.

### Compression Results

| Metric | Uncompressed | Zstd Compressed |
|--------|--------------|-----------------|
| Binary size | 3.2 MB | 1.3 MB |
| Embedded data | 2.5 MB | 367 KB |
| Compression ratio | 1x | 5.2x |
| Startup overhead | None | ~50ms |

Dictionary-based compression is particularly effective for Haskell source files because they share common patterns (`module`, `import`, type signatures, etc.).

### Build Options

```bash
# Default: uncompressed (faster startup)
cmake -B build
cmake --build build --target mhs-midi-standalone

# With zstd compression (smaller binary)
cmake -B build -DMHS_USE_ZSTD=ON
cmake --build build --target mhs-midi-standalone
```

### How It Works

The compression system consists of:

1. **`embed_libs_zstd.c`** - A C tool that:
   - Collects all files to embed
   - Trains a zstd dictionary (~112KB) on text files (.hs, .c, .h)
   - Compresses each file using the dictionary
   - Generates `mhs_embedded_zstd.h` with compressed byte arrays

2. **`vfs_unified.c`** - A unified VFS that:
   - When `VFS_USE_ZSTD` is defined: decompresses files on demand using the embedded dictionary
   - When undefined: serves uncompressed files directly
   - Caches decompressed content for repeated access

3. **`zstddeclib.c`** - The decompress-only zstd library (~900KB source) linked into the binary

### Implementation Details

```c
// vfs_unified.c - compile-time switch
#ifdef VFS_USE_ZSTD
#include "zstd.h"
#include "mhs_embedded_zstd.h"  // Compressed data + dictionary
#else
#include "mhs_embedded_libs.h"  // Uncompressed data
#endif

FILE* vfs_fopen(const char* path, const char* mode) {
    // ... lookup file ...
#ifdef VFS_USE_ZSTD
    // Decompress on first access, cache result
    CachedFile* cached = get_cache_entry(ef->path);
    if (!cached) {
        cached = decompress_and_cache(ef);
    }
    return fmemopen(cached->content, cached->length, "r");
#else
    return fmemopen((void*)ef->content, ef->length, "r");
#endif
}
```

The dictionary training analyzes patterns across all text files:

```bash
# Build the compression tool
cc -O2 -o embed_libs_zstd scripts/embed_libs_zstd.c \
   thirdparty/zstd-1.5.7/zstd.c -Ithirdparty/zstd-1.5.7 -lpthread

# Generate compressed header
./embed_libs_zstd mhs_embedded_zstd.h lib/ \
   --runtime src/runtime/ \
   --lib liblibremidi.a

# Output:
# Training dictionary from 268 samples (1361249 bytes)...
# Dictionary trained: 114688 bytes
# Compression: 1361249 -> 261100 bytes (5.21x)
```

### Trade-offs

| Aspect | Uncompressed | Zstd Compressed |
|--------|--------------|-----------------|
| Binary size | Larger (+1.9 MB) | Smaller |
| Startup time | Instant | ~50ms dictionary load |
| Runtime memory | Lower | Cache grows on demand |
| Build complexity | Simple | Requires zstd |
| Code size | ~200 lines | ~400 lines + zstddeclib |

Choose zstd compression when binary size matters (distribution, embedded systems). Choose uncompressed when startup latency is critical or build simplicity is preferred.

## Files

```
scripts/
    embed_libs.py         # Convert files to C header (Python)
    embed_libs.c          # Convert files to C header (C)
    embed_libs_zstd.c     # Convert files to C header with zstd compression
    patch_eval_vfs.py     # Patch eval.c for override

projects/mhs-midi/
    vfs.c, vfs.h          # Virtual filesystem (uncompressed)
    vfs_unified.c         # Unified VFS (both compressed and uncompressed)
    vfs_unified.h         # Header for unified VFS
    vfs_zstd.c, vfs_zstd.h  # Zstd-only VFS (alternative)
    mhs_ffi_override.c    # FFI intercept
    mhs_midi_standalone_main.c  # Entry point

thirdparty/zstd-1.5.7/
    zstd.c, zstd.h        # Full zstd library (for compression tool)
    zstddeclib.c          # Decompress-only library (for runtime)
    zdict.h               # Dictionary training API
```

## Acknowledgments

Thanks to Lennart Augustsson for creating `MicroHs`, which makes this kind of embedding possible through its use of combinators, clean FFI design and single-file C output.

The exploratory work to develop the standalone `mhs-midi` implementation was greatly accelerated by the use of claude-code.

## References

- [MicroHs](https://github.com/augustss/MicroHs) - A small Haskell compiler
- [fmemopen(3)](https://man7.org/linux/man-pages/man3/fmemopen.3.html) - Memory stream interface
- [midi-langs](https://github.com/shakfu/midi-langs) - MIDI programming languages including mhs-midi
- [Zstandard](https://github.com/facebook/zstd) - Fast lossless compression algorithm with dictionary support
