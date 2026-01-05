#!/usr/bin/env python3
"""
Convert files to C header with zstd compression and dictionary support.

Generates compressed embedded files that achieve ~3-4x compression ratio
for Haskell source code through dictionary-based compression.

Usage:
    embed_libs_zstd.py <output.h> <libdir1> [libdir2 ...] [options]

Example:
    embed_libs_zstd.py build/mhs_embedded_zstd.h thirdparty/MicroHs/lib \
        projects/mhs-midi/lib --runtime thirdparty/MicroHs/src/runtime

Requirements:
    pip install zstandard
"""

import argparse
import os
import sys
from pathlib import Path

try:
    import zstandard as zstd
except ImportError:
    print("Error: zstandard package required. Install with: pip install zstandard",
          file=sys.stderr)
    sys.exit(1)


# Compression settings
COMPRESSION_LEVEL = 19  # Max compression (1-22, higher = better ratio, slower)
DICT_SIZE = 112 * 1024  # 112KB dictionary - good balance for source code
MIN_DICT_SAMPLES = 100  # Minimum samples to train dictionary


def bytes_to_c_array(data: bytes, var_name: str, per_line: int = 16) -> str:
    """Convert bytes to C array initialization."""
    lines = []
    lines.append(f"static const unsigned char {var_name}[] = {{")

    for i in range(0, len(data), per_line):
        chunk = data[i:i + per_line]
        hex_vals = ", ".join(f"0x{b:02x}" for b in chunk)
        lines.append(f"    {hex_vals},")

    lines.append("};")
    return "\n".join(lines)


def train_dictionary(samples: list[bytes], dict_size: int = DICT_SIZE) -> bytes:
    """
    Train a zstd dictionary from sample data.

    Dictionary training analyzes common patterns across samples to create
    a shared context that improves compression of similar content.
    """
    if len(samples) < MIN_DICT_SAMPLES:
        print(f"Warning: Only {len(samples)} samples, dictionary may be suboptimal",
              file=sys.stderr)

    # Calculate total sample size for training parameters
    total_size = sum(len(s) for s in samples)
    print(f"Training dictionary from {len(samples)} samples ({total_size:,} bytes)...")

    # Train the dictionary
    dict_data = zstd.train_dictionary(dict_size, samples)

    print(f"Dictionary trained: {len(dict_data.as_bytes()):,} bytes")
    return dict_data.as_bytes()


def compress_with_dict(data: bytes, dict_data: bytes) -> bytes:
    """Compress data using a pre-trained dictionary."""
    cdict = zstd.ZstdCompressionDict(dict_data)
    compressor = zstd.ZstdCompressor(level=COMPRESSION_LEVEL, dict_data=cdict)
    return compressor.compress(data)


def compress_without_dict(data: bytes) -> bytes:
    """Compress data without dictionary (for binary files)."""
    compressor = zstd.ZstdCompressor(level=COMPRESSION_LEVEL)
    return compressor.compress(data)


def collect_hs_files(lib_dirs: list[str]) -> list[tuple[str, str, bytes]]:
    """Collect all .hs files from library directories."""
    files = []
    seen_paths = set()

    for lib_dir in lib_dirs:
        lib_path = Path(lib_dir).resolve()
        if not lib_path.exists():
            print(f"Warning: Library directory not found: {lib_dir}", file=sys.stderr)
            continue

        base_name = lib_path.name

        for pattern in ["*.hs", "*.hs-boot"]:
            for hs_file in lib_path.rglob(pattern):
                rel_path = hs_file.relative_to(lib_path)
                vfs_path = f"{base_name}/{rel_path}"

                if vfs_path in seen_paths:
                    print(f"Note: Skipping duplicate {vfs_path}", file=sys.stderr)
                    continue
                seen_paths.add(vfs_path)

                try:
                    content = hs_file.read_bytes()
                    files.append((vfs_path, str(hs_file), content))
                except Exception as e:
                    print(f"Warning: Could not read {hs_file}: {e}", file=sys.stderr)

    return files


def collect_runtime_files(runtime_dir: str) -> list[tuple[str, str, bytes]]:
    """Collect runtime C/H files."""
    files = []
    runtime_path = Path(runtime_dir).resolve()
    if not runtime_path.exists():
        return files

    src_parent = runtime_path.parent

    for pattern in ["*.c", "*.h"]:
        for c_file in runtime_path.rglob(pattern):
            rel_path = c_file.relative_to(src_parent.parent)
            vfs_path = str(rel_path)

            try:
                content = c_file.read_bytes()
                files.append((vfs_path, str(c_file), content))
            except Exception as e:
                print(f"Warning: Could not read {c_file}: {e}", file=sys.stderr)

    return files


def collect_extra_files(file_paths: list[str], vfs_prefix: str) -> list[tuple[str, str, bytes]]:
    """Collect extra files for embedding."""
    files = []

    for file_path in file_paths:
        path = Path(file_path).resolve()
        if not path.exists():
            print(f"Warning: File not found: {file_path}", file=sys.stderr)
            continue

        vfs_path = f"{vfs_prefix}/{path.name}"

        try:
            content = path.read_bytes()
            files.append((vfs_path, str(path), content))
            print(f"  Embedding: {path.name} ({len(content):,} bytes)")
        except Exception as e:
            print(f"Warning: Could not read {path}: {e}", file=sys.stderr)

    return files


def is_text_file(vfs_path: str) -> bool:
    """Check if file is a text file that benefits from dictionary compression."""
    text_extensions = {'.hs', '.hs-boot', '.c', '.h'}
    return any(vfs_path.endswith(ext) for ext in text_extensions)


def generate_header(output_path: str, lib_dirs: list[str], runtime_dir: str = None,
                    extra_headers: list[str] = None, extra_libs: list[str] = None,
                    extra_sources: list[str] = None) -> None:
    """Generate C header with zstd-compressed embedded files."""

    # Collect all files
    files = collect_hs_files(lib_dirs)
    hs_count = len(files)

    runtime_count = 0
    if runtime_dir:
        runtime_files = collect_runtime_files(runtime_dir)
        runtime_count = len(runtime_files)
        files.extend(runtime_files)

    header_count = 0
    if extra_headers:
        header_files = collect_extra_files(extra_headers, "src/runtime")
        header_count = len(header_files)
        files.extend(header_files)

    lib_count = 0
    if extra_libs:
        print("Embedding libraries:")
        lib_files = collect_extra_files(extra_libs, "lib")
        lib_count = len(lib_files)
        files.extend(lib_files)

    source_count = 0
    if extra_sources:
        print("Embedding sources:")
        source_files = collect_extra_files(extra_sources, "src/runtime")
        source_count = len(source_files)
        files.extend(source_files)

    if not files:
        print("Error: No files found", file=sys.stderr)
        sys.exit(1)

    # Separate text files (for dictionary) from binary files
    text_files = [(p, fp, c) for p, fp, c in files if is_text_file(p)]
    binary_files = [(p, fp, c) for p, fp, c in files if not is_text_file(p)]

    total_uncompressed = sum(len(c) for _, _, c in files)
    print(f"\nCollected {len(files)} files ({total_uncompressed:,} bytes uncompressed)")
    print(f"  Text files: {len(text_files)} (dictionary compression)")
    print(f"  Binary files: {len(binary_files)} (standard compression)")

    # Train dictionary on text file contents
    dict_data = b""
    if text_files:
        text_samples = [content for _, _, content in text_files]
        dict_data = train_dictionary(text_samples)

    # Compress all files
    compressed_files = []
    total_compressed = 0

    print("\nCompressing files...")
    for vfs_path, full_path, content in sorted(files):
        if is_text_file(vfs_path) and dict_data:
            compressed = compress_with_dict(content, dict_data)
            use_dict = True
        else:
            compressed = compress_without_dict(content)
            use_dict = False

        ratio = len(content) / len(compressed) if compressed else 0
        compressed_files.append((vfs_path, full_path, content, compressed, use_dict))
        total_compressed += len(compressed)

    # Calculate stats
    dict_size = len(dict_data)
    total_embedded = total_compressed + dict_size
    overall_ratio = total_uncompressed / total_embedded if total_embedded else 0

    print(f"\nCompression results:")
    print(f"  Original:     {total_uncompressed:,} bytes")
    print(f"  Compressed:   {total_compressed:,} bytes")
    print(f"  Dictionary:   {dict_size:,} bytes")
    print(f"  Total embed:  {total_embedded:,} bytes")
    print(f"  Ratio:        {overall_ratio:.2f}x")
    print(f"  Savings:      {total_uncompressed - total_embedded:,} bytes ({100 * (1 - total_embedded/total_uncompressed):.1f}%)")

    # Generate C header
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("/* Auto-generated by embed_libs_zstd.py - DO NOT EDIT */\n")
        f.write(f"/* {len(files)} files, zstd compressed with dictionary */\n")
        f.write(f"/* Original: {total_uncompressed:,} bytes -> Embedded: {total_embedded:,} bytes ({overall_ratio:.2f}x) */\n\n")

        f.write("#ifndef MHS_EMBEDDED_ZSTD_H\n")
        f.write("#define MHS_EMBEDDED_ZSTD_H\n\n")
        f.write("#include <stddef.h>\n")
        f.write("#include <stdint.h>\n\n")

        # Embed dictionary
        if dict_data:
            f.write("/* Zstd dictionary for text file decompression */\n")
            f.write(bytes_to_c_array(dict_data, "embedded_zstd_dict"))
            f.write(f"\n#define EMBEDDED_DICT_SIZE {len(dict_data)}\n\n")
        else:
            f.write("static const unsigned char embedded_zstd_dict[] = {};\n")
            f.write("#define EMBEDDED_DICT_SIZE 0\n\n")

        # File entry structure
        f.write("typedef struct {\n")
        f.write("    const char* path;              /* VFS path */\n")
        f.write("    const unsigned char* data;     /* Compressed data */\n")
        f.write("    uint32_t compressed_size;      /* Compressed size */\n")
        f.write("    uint32_t original_size;        /* Original size */\n")
        f.write("    uint8_t use_dict;              /* 1 if compressed with dictionary */\n")
        f.write("} EmbeddedFileZstd;\n\n")

        # Generate compressed data arrays
        f.write("/* Compressed file data */\n")
        for i, (vfs_path, full_path, original, compressed, use_dict) in enumerate(compressed_files):
            var_name = f"file_data_{i}"
            f.write(f"/* {vfs_path} ({len(original)} -> {len(compressed)}) */\n")
            f.write(bytes_to_c_array(compressed, var_name))
            f.write("\n\n")

        # Generate file table
        f.write("static const EmbeddedFileZstd embedded_files_zstd[] = {\n")
        for i, (vfs_path, full_path, original, compressed, use_dict) in enumerate(compressed_files):
            f.write(f'    {{ "{vfs_path}", file_data_{i}, {len(compressed)}, {len(original)}, {1 if use_dict else 0} }},\n')
        f.write("    { NULL, NULL, 0, 0, 0 }  /* Sentinel */\n")
        f.write("};\n\n")

        f.write(f"#define EMBEDDED_FILE_COUNT {len(files)}\n")
        f.write(f"#define EMBEDDED_TOTAL_COMPRESSED {total_compressed}\n")
        f.write(f"#define EMBEDDED_TOTAL_ORIGINAL {total_uncompressed}\n\n")

        f.write("#endif /* MHS_EMBEDDED_ZSTD_H */\n")

    print(f"\nGenerated: {output_path}")


def main():
    parser = argparse.ArgumentParser(
        description="Convert files to C header with zstd compression"
    )
    parser.add_argument("output", help="Output header file path")
    parser.add_argument("libdirs", nargs="+", help="Library directories to embed")
    parser.add_argument("--runtime", help="Runtime directory to embed")
    parser.add_argument("--header", action="append", dest="headers",
                        help="Extra header file to embed (can be repeated)")
    parser.add_argument("--lib", action="append", dest="libs",
                        help="Library file (.a) to embed (can be repeated)")
    parser.add_argument("--source", action="append", dest="sources",
                        help="Source file (.c) to embed (can be repeated)")
    parser.add_argument("--dict-size", type=int, default=112 * 1024,
                        help="Dictionary size in bytes (default: 112KB)")
    parser.add_argument("--level", type=int, default=19,
                        help="Compression level 1-22 (default: 19)")

    args = parser.parse_args()

    # Update module-level settings
    global DICT_SIZE, COMPRESSION_LEVEL
    DICT_SIZE = args.dict_size
    COMPRESSION_LEVEL = args.level

    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    generate_header(args.output, args.libdirs, args.runtime,
                    args.headers, args.libs, args.sources)


if __name__ == "__main__":
    main()
