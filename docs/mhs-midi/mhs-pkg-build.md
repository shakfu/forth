# Embedding Precompiled .pkg Files in a Standalone MicroHs Binary

This article documents the development of `MHS_USE_PKG` mode for `mhs-midi-standalone`, a self-contained MicroHs REPL with embedded MIDI support. The goal was to reduce cold-start time from ~20 seconds to under 1 second while retaining the ability to compile Haskell to standalone executables.

## Background: The Cold Start Problem

The `mhs-midi-standalone` binary embeds all necessary Haskell source files (~274 .hs files from MicroHs/lib and custom MIDI libraries) using a Virtual Filesystem (VFS). On first run, MicroHs must parse and compile every module before presenting the REPL prompt. This takes approximately 20 seconds.

Subsequent runs load from `.mhscache` in ~0.5 seconds, but the cold-start penalty is problematic for:
- First-time users evaluating the tool
- CI/CD pipelines without persistent caches
- Distribution to end users

## MicroHs Package System

MicroHs supports precompiled packages via the `-P` flag during compilation:

```bash
mhs -Pmusic-0.1.0 -i./lib Async Midi Music -o music-0.1.0.pkg
```

This creates a `.pkg` file containing serialized, already-compiled modules. At runtime, packages are loaded with `-p`:

```bash
mhs -a/path/to/packages -pbase -pmusic
```

The `-a` flag sets the package search path (looks in `<path>/packages/`), and `-p` loads named packages.

## Implementation Strategy

The existing VFS intercepts `mhs_fopen` FFI calls to serve embedded files via `fmemopen()`. The plan was straightforward: embed `.pkg` files instead of `.hs` files and adjust the command-line arguments.

Reality proved more complex.

## Challenge 1: Package Discovery via Directory Operations

Initial testing with embedded `.pkg` files failed immediately:

```
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

```
Module not found: Prelude
```

MicroHs doesn't scan package contents to find modules. Instead, it uses `.txt` mapping files:

```
~/.mcabal/mhs-0.15.2.0/
    Prelude.txt          # Contains: "base-0.15.2.0.pkg"
    Data/List.txt        # Contains: "base-0.15.2.0.pkg"
    Control/Monad.txt    # Contains: "base-0.15.2.0.pkg"
    ...
```

When searching for module `Data.List`, MicroHs looks for `Data/List.txt` and reads the package name from it.

### Solution: Embed .txt Mapping Files

Updated `embed_pkgs.py` to collect and embed all 185 `.txt` files from the MicroHs installation:

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

```
mhs-midi-standalone -o /tmp/test /tmp/Test.hs
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

```bash
python embed_pkgs.py output.h \
    --base-pkg ~/.mcabal/mhs-0.15.2.0/packages/base-0.15.2.0.pkg \
    --music-pkg build/music-0.1.0.pkg \
    --base-dir ~/.mcabal/mhs-0.15.2.0 \
    --lib lib/libmidi_ffi.a=build/libmidi_ffi.a \
    --lib lib/liblibremidi.a=build/liblibremidi.a \
    --header include/midi_ffi.h=src/midi_ffi.h
```

During compilation, the VFS extracts these to a temporary directory for `cc` to consume.

## Final Embedded Content (MHS_USE_PKG)

| Category | Count | Size |
|----------|-------|------|
| .pkg packages | 2 | 2.7 MB |
| .txt module mappings | 190 | 3.2 KB |
| Runtime source files | 33 | 1.5 MB |
| Static libraries | 4 | 1.2 MB |
| Header files | 1 | 2.1 KB |

## Performance Comparison

### Startup Time

| Build Mode | Cold Start | Warm Cache (.mhscache) |
|------------|------------|------------------------|
| Default (.hs embedding) | ~20s | ~0.5s |
| MHS_USE_PKG | ~1s | ~0.95s |
| MHS_USE_ZSTD | ~20s | ~0.5s |

Key insight: `.mhscache` is more efficient than `.pkg` for warm starts because it's a single pre-processed blob. The PKG mode's value is eliminating the 20-second cold-start penalty.

### Binary Size

| Build Mode | Binary Size |
|------------|-------------|
| Default (.hs embedding) | ~3.2 MB |
| MHS_USE_PKG | ~5.5 MB |
| MHS_USE_ZSTD | ~1.3 MB |
| MHS_USE_PKG + MHS_USE_ZSTD | ~2.5 MB |

### Feature Matrix

The three modes are:

| Mode         | What's embedded           | Compression |
|--------------|---------------------------|-------------|
| Default      | .hs source files          | None        |
| MHS_USE_ZSTD | .hs source files          | zstd        |
| MHS_USE_PKG  | .pkg precompiled packages | None        |
| Both         | .pkg precompiled packages | zstd        |


They lead to this feature matrix:


| Feature | Default | MHS_USE_PKG | MHS_USE_ZSTD |
|---------|---------|-------------|--------------|
| Fast cold start | No | Yes | No |
| Smallest binary | No | No | Yes |
| Compile to executable | Yes | Yes | Yes |
| No external dependencies | Yes | Yes | Yes |

## Build Instructions

```bash
# Default: .hs source embedding
cmake -B build
cmake --build build --target mhs-midi-standalone

# Default mk2: .hs source embedding + compression
cmake -B build -DMHS_USE_ZSTD=ON
cmake --build build --target mhs-midi-standalone

# Package mode: fast cold start
cmake -B build -DMHS_USE_PKG=ON
cmake --build build --target mhs-midi-standalone

# Compressed mode: smallest binary
cmake -B build -DMHS_USE_ZSTD=ON
cmake --build build --target mhs-midi-standalone

# Combined: fast start + compression
cmake -B build -DMHS_USE_PKG=ON -DMHS_USE_ZSTD=ON
cmake --build build --target mhs-midi-standalone
```

## Prerequisites for MHS_USE_PKG

The package mode requires MicroHs to be installed:

```bash
cd thirdparty/MicroHs
make
make install  # Creates ~/.mcabal/mhs-0.15.2.0/
```

This installs `base-0.15.2.0.pkg` and the module mapping `.txt` files that `embed_pkgs.py` reads during the build.

## Conclusion

Embedding precompiled `.pkg` files required solving three unexpected challenges:
1. Virtual directory operations for package discovery
2. Module-to-package `.txt` mapping files
3. Hybrid embedding for compilation support

The result is a ~20x improvement in cold-start time (20s to 1s) while maintaining full functionality including the ability to compile Haskell programs to standalone executables.

For distribution to end users, `MHS_USE_PKG` is recommended. For development with persistent `.mhscache`, the default mode offers slightly faster warm starts and smaller binaries.
