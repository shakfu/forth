#!/usr/bin/env python3
"""
Convert MicroHs .pkg files and module mapping .txt files to C header.

Usage: embed_pkgs.py <output.h> --base-pkg <base.pkg> --music-pkg <music.pkg> --base-dir <dir>

Example:
    embed_pkgs.py build/mhs_embedded_pkgs.h \
        --base-pkg ~/.mcabal/mhs-0.15.2.0/packages/base-0.15.2.0.pkg \
        --music-pkg build/music-0.1.0.pkg \
        --base-dir ~/.mcabal/mhs-0.15.2.0

    # With zstd compression:
    embed_pkgs.py build/mhs_embedded_pkgs_zstd.h \
        --base-pkg ~/.mcabal/mhs-0.15.2.0/packages/base-0.15.2.0.pkg \
        --music-pkg build/music-0.1.0.pkg \
        --base-dir ~/.mcabal/mhs-0.15.2.0 \
        --zstd
"""

import argparse
import os
import sys
from pathlib import Path

# Optional zstd support
try:
    import zstandard as zstd
    HAS_ZSTD = True
except ImportError:
    HAS_ZSTD = False

# Compression settings
COMPRESSION_LEVEL = 19  # Max compression (1-22, higher = better ratio, slower)
DICT_SIZE = 112 * 1024  # 112KB dictionary


def format_byte_array(data: bytes, bytes_per_line: int = 16) -> str:
    """
    Format binary data as C byte array initializer.
    Returns string like "0x7a, 0x00, 0x01, ..."
    """
    lines = []
    for i in range(0, len(data), bytes_per_line):
        chunk = data[i:i + bytes_per_line]
        hex_bytes = ', '.join(f'0x{b:02x}' for b in chunk)
        lines.append(f'    {hex_bytes}')
    return ',\n'.join(lines)


def sanitize_name(name: str) -> str:
    """Convert path/filename to valid C identifier."""
    result = []
    for c in name:
        if c.isalnum():
            result.append(c)
        else:
            result.append('_')
    return ''.join(result)


def collect_txt_files(base_dir: Path) -> list[tuple[str, Path]]:
    """
    Collect all .txt module mapping files from the base directory.
    Returns list of (vfs_path, file_path) tuples.
    The .txt files map module names to package names.
    """
    txt_files = []
    for txt_path in base_dir.rglob("*.txt"):
        # Skip .txt files inside packages/ directory
        if "packages" in txt_path.parts:
            continue
        # Get path relative to base_dir
        rel_path = txt_path.relative_to(base_dir)
        # VFS path is just the relative path (e.g., "Prelude.txt", "Data/List.txt")
        vfs_path = str(rel_path)
        txt_files.append((vfs_path, txt_path))
    return sorted(txt_files, key=lambda x: x[0])


def collect_runtime_files(base_dir: Path) -> list[tuple[str, Path]]:
    """
    Collect runtime source files needed for compilation.
    These are in packages/mhs-X.Y.Z/data/src/runtime/
    Returns list of (vfs_path, file_path) tuples.
    """
    runtime_files = []
    packages_dir = base_dir / "packages"

    if not packages_dir.exists():
        return []

    # Find the mhs-* directory containing runtime
    for mhs_dir in packages_dir.iterdir():
        if mhs_dir.is_dir() and mhs_dir.name.startswith("mhs-"):
            runtime_dir = mhs_dir / "data" / "src" / "runtime"
            if runtime_dir.exists():
                for file_path in runtime_dir.rglob("*"):
                    if file_path.is_file():
                        # VFS path: src/runtime/<relative>
                        rel_path = file_path.relative_to(runtime_dir)
                        vfs_path = f"src/runtime/{rel_path}"
                        runtime_files.append((vfs_path, file_path))
                break

    return sorted(runtime_files, key=lambda x: x[0])


def generate_header(output_path: str, packages: list[tuple[str, str]],
                    txt_files: list[tuple[str, Path]],
                    runtime_files: list[tuple[str, Path]] = None) -> None:
    """
    Generate C header with embedded package data, module mappings, and runtime files.

    packages: list of (vfs_path, file_path) tuples for .pkg files
    txt_files: list of (vfs_path, file_path) tuples for .txt module mappings
    runtime_files: list of (vfs_path, file_path) tuples for runtime source files
    """
    if runtime_files is None:
        runtime_files = []

    pkg_data = []
    total_pkg_size = 0

    # Process .pkg files
    for vfs_path, file_path in packages:
        path = Path(file_path).expanduser().resolve()
        if not path.exists():
            print(f"Error: Package file not found: {file_path}", file=sys.stderr)
            sys.exit(1)

        content = path.read_bytes()
        total_pkg_size += len(content)

        var_name = sanitize_name(path.name) + '_data'

        pkg_data.append({
            'vfs_path': vfs_path,
            'file_path': str(path),
            'var_name': var_name,
            'content': content,
            'size': len(content)
        })

        print(f"  {path.name}: {len(content):,} bytes")

    print(f"Packages: {len(packages)} files, {total_pkg_size:,} bytes")

    # Process .txt files
    txt_data = []
    total_txt_size = 0

    for vfs_path, source in txt_files:
        # source can be a Path object or bytes directly
        if isinstance(source, Path):
            content = source.read_bytes()
        else:
            content = source

        total_txt_size += len(content)

        # Create unique variable name from path
        var_name = 'txt_' + sanitize_name(vfs_path)

        txt_data.append({
            'vfs_path': vfs_path,
            'var_name': var_name,
            'content': content,
            'size': len(content)
        })

    print(f"Module mappings: {len(txt_files)} .txt files, {total_txt_size:,} bytes")

    # Process runtime files
    rt_data = []
    total_rt_size = 0

    for vfs_path, file_path in runtime_files:
        content = file_path.read_bytes()
        total_rt_size += len(content)

        var_name = 'rt_' + sanitize_name(vfs_path)

        rt_data.append({
            'vfs_path': vfs_path,
            'var_name': var_name,
            'content': content,
            'size': len(content)
        })

    print(f"Runtime files: {len(runtime_files)} files, {total_rt_size:,} bytes")

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("/* Auto-generated by embed_pkgs.py - DO NOT EDIT */\n")
        f.write(f"/* {len(packages)} packages ({total_pkg_size:,} bytes), ")
        f.write(f"{len(txt_files)} module mappings ({total_txt_size:,} bytes), ")
        f.write(f"{len(runtime_files)} runtime files ({total_rt_size:,} bytes) */\n\n")
        f.write("#ifndef MHS_EMBEDDED_PKGS_H\n")
        f.write("#define MHS_EMBEDDED_PKGS_H\n\n")
        f.write("#include <stddef.h>\n\n")

        # Write each package as a byte array
        f.write("/* Package binary data */\n")
        for pkg in pkg_data:
            f.write(f"/* {pkg['file_path']} */\n")
            f.write(f"static const unsigned char {pkg['var_name']}[] = {{\n")
            f.write(format_byte_array(pkg['content']))
            f.write('\n};\n\n')

        # Write module mapping .txt data (small strings)
        f.write("/* Module mapping .txt files */\n")
        for txt in txt_data:
            # .txt files are small, just write as single-line string
            content_str = txt['content'].decode('utf-8', errors='replace').strip()
            f.write(f'static const char {txt["var_name"]}[] = "{content_str}";\n')
        f.write("\n")

        # Write runtime files as byte arrays
        f.write("/* Runtime source files for compilation */\n")
        for rt in rt_data:
            f.write(f"/* {rt['vfs_path']} */\n")
            f.write(f"static const unsigned char {rt['var_name']}[] = {{\n")
            f.write(format_byte_array(rt['content']))
            f.write('\n};\n\n')

        # Write struct types
        f.write("typedef struct {\n")
        f.write("    const char* path;              /* VFS path */\n")
        f.write("    const unsigned char* content;  /* Binary data */\n")
        f.write("    size_t length;                 /* Content length */\n")
        f.write("} EmbeddedPackage;\n\n")

        f.write("typedef struct {\n")
        f.write("    const char* path;              /* VFS path, e.g., \"Prelude.txt\" */\n")
        f.write("    const char* content;           /* Package name string */\n")
        f.write("    size_t length;                 /* Content length */\n")
        f.write("} EmbeddedTxtFile;\n\n")

        f.write("typedef struct {\n")
        f.write("    const char* path;              /* VFS path, e.g., \"src/runtime/eval.c\" */\n")
        f.write("    const unsigned char* content;  /* File content */\n")
        f.write("    size_t length;                 /* Content length */\n")
        f.write("} EmbeddedRuntimeFile;\n\n")

        # Write tables
        f.write("static const EmbeddedPackage embedded_packages[] = {\n")
        for pkg in pkg_data:
            f.write(f'    {{ "{pkg["vfs_path"]}", {pkg["var_name"]}, {pkg["size"]} }},\n')
        f.write("    { NULL, NULL, 0 }\n")
        f.write("};\n\n")

        f.write("static const EmbeddedTxtFile embedded_txt_files[] = {\n")
        for txt in txt_data:
            f.write(f'    {{ "{txt["vfs_path"]}", {txt["var_name"]}, {txt["size"]} }},\n')
        f.write("    { NULL, NULL, 0 }\n")
        f.write("};\n\n")

        f.write("static const EmbeddedRuntimeFile embedded_runtime_files[] = {\n")
        for rt in rt_data:
            f.write(f'    {{ "{rt["vfs_path"]}", {rt["var_name"]}, {rt["size"]} }},\n')
        f.write("    { NULL, NULL, 0 }\n")
        f.write("};\n\n")

        f.write(f"#define EMBEDDED_PACKAGE_COUNT {len(packages)}\n")
        f.write(f"#define EMBEDDED_TXT_COUNT {len(txt_files)}\n")
        f.write(f"#define EMBEDDED_RUNTIME_COUNT {len(runtime_files)}\n\n")
        f.write("#endif /* MHS_EMBEDDED_PKGS_H */\n")

    print(f"Generated: {output_path}")


def generate_music_txt_files(music_pkg_name: str, modules: list[str]) -> list[tuple[str, bytes]]:
    """
    Generate synthetic .txt mapping entries for music package modules.
    Returns list of (vfs_path, content_bytes) tuples.
    """
    return [(f"{module}.txt", music_pkg_name.encode()) for module in modules]


def train_dictionary(samples: list[bytes], dict_size: int = DICT_SIZE) -> bytes:
    """
    Train a zstd dictionary from sample data.
    """
    if not samples:
        return b""

    total_size = sum(len(s) for s in samples)
    print(f"Training dictionary from {len(samples)} samples ({total_size:,} bytes)...")

    dict_data = zstd.train_dictionary(dict_size, samples)
    print(f"Dictionary trained: {len(dict_data.as_bytes()):,} bytes")
    return dict_data.as_bytes()


def compress_with_dict(data: bytes, dict_data: bytes) -> bytes:
    """Compress data using a pre-trained dictionary."""
    if dict_data:
        cdict = zstd.ZstdCompressionDict(dict_data)
        compressor = zstd.ZstdCompressor(level=COMPRESSION_LEVEL, dict_data=cdict)
    else:
        compressor = zstd.ZstdCompressor(level=COMPRESSION_LEVEL)
    return compressor.compress(data)


def generate_header_zstd(output_path: str, packages: list[tuple[str, str]],
                         txt_files: list[tuple[str, Path]],
                         runtime_files: list[tuple[str, Path]] = None) -> None:
    """
    Generate C header with zstd-compressed embedded package data, module mappings, and runtime files.

    packages: list of (vfs_path, file_path) tuples for .pkg files
    txt_files: list of (vfs_path, file_path or content_bytes) tuples for .txt module mappings
    runtime_files: list of (vfs_path, file_path) tuples for runtime source files
    """
    if not HAS_ZSTD:
        print("Error: zstandard package required for --zstd. Install with: pip install zstandard",
              file=sys.stderr)
        sys.exit(1)

    if runtime_files is None:
        runtime_files = []

    # Process packages
    pkg_entries = []
    total_pkg_size = 0
    for vfs_path, file_path in packages:
        path = Path(file_path).expanduser().resolve()
        if not path.exists():
            print(f"Error: Package file not found: {file_path}", file=sys.stderr)
            sys.exit(1)
        content = path.read_bytes()
        total_pkg_size += len(content)
        pkg_entries.append({
            'vfs_path': vfs_path,
            'content': content,
            'type': 'pkg'
        })
        print(f"  {path.name}: {len(content):,} bytes")

    print(f"Packages: {len(packages)} files, {total_pkg_size:,} bytes")

    # Process .txt files
    txt_entries = []
    total_txt_size = 0
    for vfs_path, source in txt_files:
        if isinstance(source, Path):
            content = source.read_bytes()
        else:
            content = source
        total_txt_size += len(content)
        txt_entries.append({
            'vfs_path': vfs_path,
            'content': content,
            'type': 'txt'
        })

    print(f"Module mappings: {len(txt_files)} .txt files, {total_txt_size:,} bytes")

    # Process runtime files
    rt_entries = []
    total_rt_size = 0
    for vfs_path, file_path in runtime_files:
        content = file_path.read_bytes()
        total_rt_size += len(content)
        rt_entries.append({
            'vfs_path': vfs_path,
            'content': content,
            'type': 'runtime'
        })

    print(f"Runtime files: {len(runtime_files)} files, {total_rt_size:,} bytes")

    total_uncompressed = total_pkg_size + total_txt_size + total_rt_size

    # Train dictionary on all content for best compression
    all_content = [e['content'] for e in pkg_entries + txt_entries + rt_entries]
    dict_data = train_dictionary(all_content) if all_content else b""

    # Compress all entries
    compressed_entries = []
    total_compressed = 0

    print("\nCompressing files...")
    for entry in pkg_entries + txt_entries + rt_entries:
        compressed = compress_with_dict(entry['content'], dict_data)
        total_compressed += len(compressed)
        compressed_entries.append({
            'vfs_path': entry['vfs_path'],
            'original': entry['content'],
            'compressed': compressed,
            'type': entry['type']
        })

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
        f.write("/* Auto-generated by embed_pkgs.py --zstd - DO NOT EDIT */\n")
        f.write(f"/* {len(pkg_entries)} packages, {len(txt_entries)} txt files, {len(rt_entries)} runtime files */\n")
        f.write(f"/* Original: {total_uncompressed:,} bytes -> Embedded: {total_embedded:,} bytes ({overall_ratio:.2f}x) */\n\n")

        f.write("#ifndef MHS_EMBEDDED_PKGS_ZSTD_H\n")
        f.write("#define MHS_EMBEDDED_PKGS_ZSTD_H\n\n")
        f.write("#include <stddef.h>\n")
        f.write("#include <stdint.h>\n\n")

        # Embed dictionary
        if dict_data:
            f.write("/* Zstd dictionary for decompression */\n")
            f.write("static const unsigned char embedded_pkg_zstd_dict[] = {\n")
            f.write(format_byte_array(dict_data))
            f.write('\n};\n')
            f.write(f"#define EMBEDDED_PKG_DICT_SIZE {len(dict_data)}\n\n")
        else:
            f.write("static const unsigned char embedded_pkg_zstd_dict[] = {};\n")
            f.write("#define EMBEDDED_PKG_DICT_SIZE 0\n\n")

        # Write compressed data arrays
        f.write("/* Compressed file data */\n")
        for i, entry in enumerate(compressed_entries):
            var_name = f"pkgzstd_data_{i}"
            f.write(f"/* {entry['vfs_path']} ({len(entry['original'])} -> {len(entry['compressed'])}) */\n")
            f.write(f"static const unsigned char {var_name}[] = {{\n")
            f.write(format_byte_array(entry['compressed']))
            f.write('\n};\n\n')

        # File entry structure (unified for all types)
        f.write("typedef struct {\n")
        f.write("    const char* path;              /* VFS path */\n")
        f.write("    const unsigned char* data;     /* Compressed data */\n")
        f.write("    uint32_t compressed_size;      /* Compressed size */\n")
        f.write("    uint32_t original_size;        /* Original size */\n")
        f.write("    uint8_t file_type;             /* 0=pkg, 1=txt, 2=runtime */\n")
        f.write("} EmbeddedFilePkgZstd;\n\n")

        f.write("#define PKG_FILE_TYPE_PKG     0\n")
        f.write("#define PKG_FILE_TYPE_TXT     1\n")
        f.write("#define PKG_FILE_TYPE_RUNTIME 2\n\n")

        # Generate file table
        f.write("static const EmbeddedFilePkgZstd embedded_files_pkg_zstd[] = {\n")
        type_map = {'pkg': 0, 'txt': 1, 'runtime': 2}
        for i, entry in enumerate(compressed_entries):
            var_name = f"pkgzstd_data_{i}"
            file_type = type_map[entry['type']]
            f.write(f'    {{ "{entry["vfs_path"]}", {var_name}, {len(entry["compressed"])}, {len(entry["original"])}, {file_type} }},\n')
        f.write("    { NULL, NULL, 0, 0, 0 }  /* Sentinel */\n")
        f.write("};\n\n")

        # Count macros
        pkg_count = sum(1 for e in compressed_entries if e['type'] == 'pkg')
        txt_count = sum(1 for e in compressed_entries if e['type'] == 'txt')
        rt_count = sum(1 for e in compressed_entries if e['type'] == 'runtime')

        f.write(f"#define EMBEDDED_PKG_ZSTD_PACKAGE_COUNT {pkg_count}\n")
        f.write(f"#define EMBEDDED_PKG_ZSTD_TXT_COUNT {txt_count}\n")
        f.write(f"#define EMBEDDED_PKG_ZSTD_RUNTIME_COUNT {rt_count}\n")
        f.write(f"#define EMBEDDED_PKG_ZSTD_TOTAL_COUNT {len(compressed_entries)}\n")
        f.write(f"#define EMBEDDED_PKG_ZSTD_TOTAL_COMPRESSED {total_compressed}\n")
        f.write(f"#define EMBEDDED_PKG_ZSTD_TOTAL_ORIGINAL {total_uncompressed}\n\n")

        f.write("#endif /* MHS_EMBEDDED_PKGS_ZSTD_H */\n")

    print(f"Generated: {output_path}")


def main():
    parser = argparse.ArgumentParser(
        description="Convert MicroHs .pkg files and module mappings to C header"
    )
    parser.add_argument(
        "output",
        help="Output header file path"
    )
    parser.add_argument(
        "--base-pkg",
        required=True,
        help="Path to base.pkg (e.g., ~/.mcabal/mhs-0.15.2.0/packages/base-0.15.2.0.pkg)"
    )
    parser.add_argument(
        "--music-pkg",
        required=True,
        help="Path to music.pkg (e.g., build/music-0.1.0.pkg)"
    )
    parser.add_argument(
        "--base-dir",
        required=True,
        help="Path to base package directory containing .txt files (e.g., ~/.mcabal/mhs-0.15.2.0)"
    )
    parser.add_argument(
        "--music-modules",
        default="Async,Midi,MidiPerform,Music,MusicPerform",
        help="Comma-separated list of modules in music package (default: Async,Midi,MidiPerform,Music,MusicPerform)"
    )
    parser.add_argument(
        "--lib",
        action="append",
        dest="libs",
        default=[],
        help="Static library to embed (can be repeated). Format: vfs_path=file_path"
    )
    parser.add_argument(
        "--header",
        dest="headers",
        action="append",
        default=[],
        help="Header file to embed (can be repeated). Format: vfs_path=file_path"
    )
    parser.add_argument(
        "--zstd",
        action="store_true",
        help="Enable zstd compression (requires zstandard package)"
    )

    args = parser.parse_args()

    # Ensure output directory exists
    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    # Resolve paths
    base_dir = Path(args.base_dir).expanduser().resolve()
    if not base_dir.exists():
        print(f"Error: Base directory not found: {args.base_dir}", file=sys.stderr)
        sys.exit(1)

    # Collect .txt files from base directory
    txt_files = collect_txt_files(base_dir)

    # Collect runtime files for compilation support
    runtime_files = collect_runtime_files(base_dir)

    # Add libraries and headers to runtime files (for extraction during compilation)
    for lib_spec in args.libs:
        if '=' in lib_spec:
            vfs_path, file_path = lib_spec.split('=', 1)
        else:
            # Default: lib/<filename>
            file_path = lib_spec
            vfs_path = f"lib/{Path(file_path).name}"
        path = Path(file_path).expanduser().resolve()
        if path.exists():
            runtime_files.append((vfs_path, path))
            print(f"  Library: {vfs_path} ({path.stat().st_size:,} bytes)")
        else:
            print(f"Warning: Library not found: {file_path}", file=sys.stderr)

    for hdr_spec in args.headers:
        if '=' in hdr_spec:
            vfs_path, file_path = hdr_spec.split('=', 1)
        else:
            # Default: include/<filename>
            file_path = hdr_spec
            vfs_path = f"include/{Path(file_path).name}"
        path = Path(file_path).expanduser().resolve()
        if path.exists():
            runtime_files.append((vfs_path, path))
            print(f"  Header: {vfs_path} ({path.stat().st_size:,} bytes)")
        else:
            print(f"Warning: Header not found: {file_path}", file=sys.stderr)

    # Build package list with VFS paths
    base_name = Path(args.base_pkg).name
    music_name = Path(args.music_pkg).name

    packages = [
        (f"packages/{base_name}", args.base_pkg),
        (f"packages/{music_name}", args.music_pkg),
    ]

    # Add synthetic .txt entries for music modules
    music_modules = [m.strip() for m in args.music_modules.split(',') if m.strip()]
    music_txt = generate_music_txt_files(music_name, music_modules)

    # Add to txt_files list (as tuples with content bytes, not paths)
    for vfs_path, content in music_txt:
        txt_files.append((vfs_path, content))

    print("Embedding packages, module mappings, and runtime files:")
    if args.zstd:
        if not HAS_ZSTD:
            print("Error: zstandard package required for --zstd. Install with: pip install zstandard",
                  file=sys.stderr)
            sys.exit(1)
        generate_header_zstd(args.output, packages, txt_files, runtime_files)
    else:
        generate_header(args.output, packages, txt_files, runtime_files)


if __name__ == "__main__":
    main()
