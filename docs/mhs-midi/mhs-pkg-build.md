# Embedding Precompiled .pkg Files in a Standalone MicroHs Binary

This document provides some details about the development of `MHS_USE_PKG` mode for the standalone mhs-midi variants (`mhs-midi-pkg`, `mhs-midi-pkg-zstd`), self-contained MicroHs REPLs with embedded MIDI support.

The goal was to improve on the earlier demonstration of embedding the base haskell source files, app source files, MicroHS runtime, and statically compiled FFI dependencies.

## Summary

The standalone mhs-midi binaries embed Haskell source files using a Virtual Filesystem (VFS), but this requires parsing and compiling ~274 modules on first run, resulting in a ~20 second cold-start delay. This document provides details about the development of `MHS_USE_PKG` mode, which embeds precompiled `.pkg` files instead of source files.

**Key Results:**

- **20x faster cold start**: Reduced from ~20s to ~1s
- **Full functionality preserved**: REPL, compilation to executables, and all MIDI features work identically
- **Self-contained**: No external dependencies required
- **The use of .pkg improves startup time if a `.mshcache` is not available**. If it subsequently avialable then using then startup times for the other variants are almost equal.

**Implementation Challenges Solved:**

1. Virtual directory operations for package discovery (MicroHs scans directories, not just files)
2. Module-to-package mapping via `.txt` files (185+ mapping files needed)
3. Hybrid embedding for compilation support (runtime source files still required for `cc`)

**When to Use:**

| Build Mode | Cold Start | Warm Cache | Binary Size |
| ---------- | ---------- | ---------- | ----------- |
| Default (.hs embedding) | ~20s | ~0.5s | ~3.3 MB |
| MHS_USE_PKG | ~1s | ~0.95s | ~4.8 MB |
| MHS_USE_ZSTD | ~20s | ~0.5s | ~1.3 MB |
| MHS_USE_PKG + MHS_USE_ZSTD | ~1s | ~0.95s | ~3.1M MB |

- **`MHS_USE_PKG + MHS_USE_ZSTD`**: Best for distribution: with best balance of fast startup and small size (~1s startup, ~3.1M MB binary)
- **Default mode**: Suitable for development with persistent `.mhscache` (faster warm starts, ~3.3 MB binary)

## Background: The Cold Start Problem

The standalone mhs-midi binaries embed all necessary Haskell source files (~274 .hs files from `MicroHs/lib` and custom MIDI libraries) using a Virtual Filesystem (VFS). On first run, MicroHs must parse and compile every module before presenting the REPL prompt. This takes approximately 20 seconds.

Subsequent runs load from `.mhscache` in ~0.5 seconds, but the cold-start penalty is problematic for:

- First-time users evaluating the tool
- CI/CD pipelines without persistent caches
- Distribution to end users

## MicroHs Package System

MicroHs supports precompiled packages via the `-P` flag during compilation:

```sh
PKG_VER=0.1.0
PKG_NAME=music
THIRDPARTY=`pwd`/thirdparty
MHS_MIDI=`pwd`/projects/mhs-midi

MHSDIR=${THIRDPARTY}/MicroHs ${THIRDPARTY}/MicroHs/bin/mhs \
    -P${PKG_NAME}-${PKG_VER} \
    -i${MHS_MIDI}/lib \
    -o ${PKG_NAME}-${PKG_VER}.pkg \
    Async Midi MidiPerform Music MusicPerform
```

This creates a `.pkg` file containing serialized, already-compiled modules.

To pre-compile the `base` package or MicroHs standard library

```sh
cd path/to/MicroHs
make install
```

Then it becomes possible to load packages at runtime with `-p`:

```sh
~/.mcabal/bin/mhs -pbase-0.15.2.0

# OR

~/.mcabal/bin/mhs -pbase
```

The key point is that in the default installation of the `mhs` binary, `MHSDIR` is set automatically. If `MHSDIR` is overridden by passing it as an environment variable, it changes not just the package search path but also where MicroHs looks for source files, causing the above commands to fail.

When using `mhs` as a local dev binary with `MHSDIR` set, you can specify the package path explicitly:

```sh
MHSDIR=./thirdparty/MicroHs ./thirdparty/MicroHs/bin/mhs \
  -a"$HOME/.mcabal/mhs-0.15.2.0" -pbase
```

The `-a` flag sets the package search path (looks in `<path>/packages/`), and `-p` loads named packages.

### The Local Dev Binary Problem

However, there's a critical limitation: **the local dev binary still recompiles all modules from source**, even when packages are preloaded:

**Installed binary (works correctly):**

```sh
~/.mcabal/bin/mhs -pbase
Loading package /Users/sa/.mcabal/mhs-0.15.2.0/packages/base-0.15.2.0.pkg
Type ':quit' to quit, ':help' for help
>
```

**Local dev binary (recompiles everything despite -p flag):**

```sh
MHSDIR=./thirdparty/MicroHs ./thirdparty/MicroHs/bin/mhs -a"$HOME/.mcabal/mhs-0.15.2.0" -pbase
Welcome to interactive MicroHs, version 0.15.2.0
    importing done Data.Bool_Type, 3ms
    importing done Data.Ordering_Type, 2ms
    importing done Primitives, 148ms
    ... (continues loading all modules from source)
```

When `MHSDIR` is overridden, it changes not just the package search path but also where MicroHs looks for source files. The compiler prefers source files from `MHSDIR/lib/` over preloaded package contents.

This limitation is why the VFS embedding approach was necessary - it's the only way to achieve fast startup with a fully self-contained binary that doesn't depend on external paths.

## Implementation Strategy

The existing VFS intercepts `mhs_fopen` FFI calls to serve embedded files via `fmemopen()`. Simply embedding `.pkg` files and using `-a`/`-p` flags wouldn't work because of the local dev binary limitation described above.

The solution was to embed packages AND intercept all file operations so MicroHs has no access to external source files - forcing it to use the preloaded packages.

## Challenge 1: Package Discovery via Directory Operations

Initial testing with embedded `.pkg` files failed immediately:

```sh
Package not found: base
```

Debugging revealed that MicroHs discovers packages by scanning the `packages/` directory:

```haskell
-- From MicroHs/src/MicroHs/Packages.hs
findPkgs :: FilePath -> IO [FilePath]
findPkgs path = do
    let pkgdir = path </> "packages"
    fs <- getDirectoryContents pkgdir
    return [pkgdir </> f | f <- fs, ".pkg" `isSuffixOf` f]
```

This calls `opendir`/`readdir`/`closedir` - not `fopen`. The VFS needed to intercept directory operations.

### Solution: Virtual Directory Listing

Extended `mhs_ffi_override.c` to intercept directory operations:

```c
typedef struct {
    int index;
    int is_virtual;
} VirtualDir;

void* mhs_opendir(const char* path) {
    if (is_virtual_packages_dir(path)) {
        VirtualDir* vd = malloc(sizeof(VirtualDir));
        vd->index = 0;
        vd->is_virtual = 1;
        return vd;
    }
    return opendir_orig(path);
}

struct dirent* mhs_readdir(void* dirp) {
    VirtualDir* vd = dirp;
    if (vd->is_virtual) {
        // Return embedded package names from table
        if (embedded_packages[vd->index].path) {
            static struct dirent de;
            strcpy(de.d_name, embedded_packages[vd->index++].filename);
            return &de;
        }
        return NULL;
    }
    return readdir_orig(dirp);
}
```

Packages were now discovered. But loading still failed.

## Challenge 2: Module-to-Package Mapping

```sh
Module not found: Prelude
```

MicroHs doesn't scan package contents to find modules. Instead, it uses `.txt` mapping files:

```sh
~/.mcabal/mhs-0.15.2.0/
    Prelude.txt          # Contains: "base-0.15.2.0.pkg"
    Data/List.txt        # Contains: "base-0.15.2.0.pkg"
    Control/Monad.txt    # Contains: "base-0.15.2.0.pkg"
    ...
```

When searching for module `Data.List`, MicroHs looks for `Data/List.txt` and reads the package name from it.

### Solution: Embed .txt Mapping Files

Updated `mhs-embed.c` (with `--pkg-mode`) to collect and embed all `.txt` files from the MicroHs installation (approximately 190 files):

```python
def collect_txt_files(base_dir: Path) -> list[tuple[str, Path]]:
    """Collect all .txt module mapping files."""
    txt_files = []
    for txt_path in base_dir.rglob("*.txt"):
        if "packages" in txt_path.parts:
            continue  # Skip .txt inside package directories
        rel_path = txt_path.relative_to(base_dir)
        txt_files.append((str(rel_path), txt_path))
    return sorted(txt_files)
```

For custom modules (Midi, Music, etc.), synthetic `.txt` entries are generated:

```python
def generate_music_txt_files(pkg_name: str, modules: list[str]):
    return [(f"{module}.txt", pkg_name.encode()) for module in modules]
```

The VFS serves these as small strings:

```c
static const char txt_Prelude_txt[] = "base-0.15.2.0.pkg";
static const char txt_Data_List_txt[] = "base-0.15.2.0.pkg";
// ...
```

## Challenge 3: Compilation Support

With packages loading correctly, the REPL started in ~1 second. But the compilation test failed:

```sh
mhs-midi-pkg -o /tmp/test /tmp/Test.hs
# Error: Cannot find runtime files for cc
```

MicroHs compiles Haskell to C, then invokes `cc` with runtime source files (`eval.c`, `main.c`, headers). In package mode, these weren't embedded.

### Solution: Hybrid Embedding

The solution embeds both precompiled packages AND runtime source files:

```python
def collect_runtime_files(base_dir: Path) -> list[tuple[str, Path]]:
    """Collect runtime source files from packages/mhs-X.Y.Z/data/src/runtime/"""
    runtime_files = []
    packages_dir = base_dir / "packages"
    for mhs_dir in packages_dir.iterdir():
        if mhs_dir.name.startswith("mhs-"):
            runtime_dir = mhs_dir / "data" / "src" / "runtime"
            if runtime_dir.exists():
                for file_path in runtime_dir.rglob("*"):
                    if file_path.is_file():
                        rel_path = file_path.relative_to(runtime_dir)
                        vfs_path = f"src/runtime/{rel_path}"
                        runtime_files.append((vfs_path, file_path))
    return sorted(runtime_files)
```

Static libraries are also embedded for linking:

```sh
# CMake handles this automatically, but the equivalent command is:
./mhs-embed output.h --pkg-mode \
    --pkg packages/base-0.15.2.0.pkg=build/mcabal/mhs-0.15.2.0/packages/base-0.15.2.0.pkg \
    --pkg packages/music-0.1.0.pkg=build/music-0.1.0.pkg \
    --txt-dir build/mcabal/mhs-0.15.2.0 \
    --music-modules music-0.1.0.pkg:Async,Midi,MidiPerform,Music,MusicPerform \
    --lib build/libmidi_ffi.a \
    --lib build/liblibremidi.a \
    --header src/midi_ffi.h
```

During compilation, the VFS extracts these to a temporary directory for `cc` to consume.

## Final Embedded Content (MHS_USE_PKG)

| Category | Count | Size |
| -------- | ----- | ---- |
| .pkg packages | 2 | 2.7 MB |
| .txt module mappings | 190 | 3.2 KB |
| Runtime source files | 33 | 1.5 MB |
| Static libraries | 4 | 1.2 MB |
| Header files | 1 | 2.1 KB |

## Performance Comparison

### Startup Time

| Build Mode | Cold Start | Warm Cache | Binary Size |
| ---------- | ---------- | ---------- | ----------- |
| Default (.hs embedding) | ~20s | ~0.5s | ~3.3 MB |
| MHS_USE_PKG | ~1s | ~0.95s | ~4.8 MB |
| MHS_USE_ZSTD | ~20s | ~0.5s | ~1.3 MB |
| MHS_USE_PKG + MHS_USE_ZSTD | ~1s | ~0.95s | ~3.1M MB |

Key insight: `.mhscache` is more efficient than `.pkg` for warm starts because it's a single pre-processed blob. The PKG mode's value is eliminating the 20-second cold-start penalty. Note that `MHS_USE_ZSTD` compresses source files but still requires parsing and compilation, so cold-start time remains ~20s.

### Binary Size

| Build Mode | Binary Size |
| ---------- | ----------- |
| Default (.hs embedding) | ~3.3 MB |
| MHS_USE_PKG | ~4.8 MB |
| MHS_USE_ZSTD | ~1.3 MB |
| MHS_USE_PKG + MHS_USE_ZSTD | ~3.1 MB |

### Feature Matrix

| Feature | Default | MHS_USE_PKG | MHS_USE_ZSTD | Both |
| ------- | ------- | ----------- | ------------ | ---- |
| Fast cold start | No | Yes | No | Yes |
| Smallest binary | No | No | Yes | No |
| Compile to executable | Yes | Yes | Yes | Yes |
| No external dependencies | Yes | Yes | Yes | Yes |

### Error Messages and Debugging

Error messages and debugging information are functionally identical across all build modes:

- **Runtime errors in user code**: File, line, and column are shown identically
- **Type errors**: Full constraint information preserved
- **Stack traces**: MicroHs shows the error location only (no full call stack in any mode)

The only difference is in errors originating from Prelude/library code:

```sh
# Source embedding mode (VFS path):
"/mhs-embedded/lib/Data/List.hs",389:11: head: empty list

# PKG mode (original compile-time path):
"/Users/.../thirdparty/MicroHs/lib/Data/List.hs",389:11: head: empty list
```

Line and column numbers are preserved in both cases. There is no degradation in debugging capability when using precompiled packages.

## Build Instructions

```sh
# Build all variants
make mhs-midi-all

# Or build specific variants:
make mhs-midi-src        # Source embedding (uncompressed)
make mhs-midi-src-zstd   # Source embedding (compressed, smallest)
make mhs-midi-pkg        # Package embedding (fast startup)
make mhs-midi-pkg-zstd   # Package + compression (recommended)

# Or using cmake directly:
cmake --build build --target mhs-midi-all
cmake --build build --target mhs-midi-pkg-zstd
```

**Variant Summary:**

- **`mhs-midi-src`**: Embeds .hs source files, ~20s cold start, ~3.3MB
- **`mhs-midi-src-zstd`**: Compressed source, ~20s cold start, ~1.3MB (smallest)
- **`mhs-midi-pkg`**: Precompiled packages, ~1s cold start, ~4.8MB
- **`mhs-midi-pkg-zstd`**: Compressed packages, ~1s cold start, ~3.1MB (recommended)

## Prerequisites for MHS_USE_PKG

The package mode requires MicroHs to be built (but not installed):

```sh
cd thirdparty/MicroHs
make
```

The CMake build handles everything else automatically:

- Builds `base-0.15.2.0.pkg` locally in `build/mhs-base-src/dist-mcabal/`
- Installs packages to `build/mcabal/` (not `~/.mcabal`)
- Generates module mapping `.txt` files that `mhs-embed.c --pkg-mode` reads

This keeps the source tree clean and avoids modifying user's home directory.

## Conclusion

Embedding precompiled `.pkg` files required solving three unexpected challenges:

1. Virtual directory operations for package discovery
2. Module-to-package `.txt` mapping files
3. Hybrid embedding for compilation support

The result is a ~20x improvement in cold-start time (20s to 1s) while maintaining full functionality including the ability to compile Haskell programs to standalone executables.

For distribution to end users, `mhs-midi-pkg-zstd` is recommended.
