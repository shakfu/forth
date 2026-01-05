#!/usr/bin/env python3
"""
Convert Haskell library files and runtime C files to C header with embedded strings.

Usage: embed_libs.py <output.h> <libdir1> [libdir2 ...] [--runtime <runtime_dir>]

Example:
    embed_libs.py build/mhs_embedded_libs.h thirdparty/MicroHs/lib projects/mhs-midi/lib \
        --runtime thirdparty/MicroHs/src/runtime
"""

import argparse
import os
import sys
from pathlib import Path


def escape_c_string(content: bytes) -> str:
    """Escape raw bytes for C string literal.

    Takes bytes, not str, to properly handle UTF-8 byte sequences.
    Each byte is escaped individually to produce valid C octal escapes.
    """
    result = []
    for byte in content:
        if byte == ord('\\'):
            result.append('\\\\')
        elif byte == ord('"'):
            result.append('\\"')
        elif byte == ord('\n'):
            result.append('\\n')
        elif byte == ord('\r'):
            result.append('\\r')
        elif byte == ord('\t'):
            result.append('\\t')
        elif byte < 32 or byte > 126:
            # Non-printable or non-ASCII: use octal escape (max 3 digits, 0-255)
            result.append(f'\\{byte:03o}')
        else:
            result.append(chr(byte))
    return ''.join(result)


def split_string_literal(escaped: str, max_len: int = 4000) -> list[str]:
    """
    Split a long string into multiple literals to avoid compiler limits.
    C89 requires at least 509 chars, C99 requires 4095.
    We split conservatively to handle escape sequences.
    """
    if len(escaped) <= max_len:
        return [escaped]

    chunks = []
    current = []
    current_len = 0

    i = 0
    while i < len(escaped):
        # Handle escape sequences as atomic units
        if escaped[i] == '\\' and i + 1 < len(escaped):
            if escaped[i + 1] in '\\"\'\n\r\t':
                seq = escaped[i:i + 2]
                i += 2
            elif escaped[i + 1].isdigit():
                # Octal escape: up to 3 digits
                end = i + 2
                while end < len(escaped) and end < i + 4 and escaped[end].isdigit():
                    end += 1
                seq = escaped[i:end]
                i = end
            else:
                seq = escaped[i:i + 2]
                i += 2
        else:
            seq = escaped[i]
            i += 1

        if current_len + len(seq) > max_len:
            chunks.append(''.join(current))
            current = [seq]
            current_len = len(seq)
        else:
            current.append(seq)
            current_len += len(seq)

    if current:
        chunks.append(''.join(current))

    return chunks


def collect_hs_files(lib_dirs: list[str]) -> list[tuple[str, str, bytes]]:
    """
    Collect all .hs files from library directories.
    Returns list of (vfs_path, full_path, content_bytes).
    """
    files = []
    seen_paths = set()

    for lib_dir in lib_dirs:
        lib_path = Path(lib_dir).resolve()
        if not lib_path.exists():
            print(f"Warning: Library directory not found: {lib_dir}", file=sys.stderr)
            continue

        # Determine the base name for VFS paths
        # e.g., thirdparty/MicroHs/lib -> lib/
        # e.g., projects/mhs-midi/lib -> lib/
        base_name = lib_path.name  # "lib"

        # Include both .hs and .hs-boot files
        for pattern in ["*.hs", "*.hs-boot"]:
          for hs_file in lib_path.rglob(pattern):
            # Create VFS path: lib/Module/Name.hs
            rel_path = hs_file.relative_to(lib_path)
            vfs_path = f"{base_name}/{rel_path}"

            # Skip duplicates (MIDI lib might override stdlib)
            if vfs_path in seen_paths:
                print(f"Note: Skipping duplicate {vfs_path} from {hs_file}", file=sys.stderr)
                continue
            seen_paths.add(vfs_path)

            try:
                # Read as bytes to preserve exact file content
                content = hs_file.read_bytes()
                files.append((vfs_path, str(hs_file), content))
            except Exception as e:
                print(f"Warning: Could not read {hs_file}: {e}", file=sys.stderr)

    return files


def collect_runtime_files(runtime_dir: str) -> list[tuple[str, str, bytes]]:
    """
    Collect runtime C/H files for embedding.
    Returns list of (vfs_path, full_path, content_bytes).
    VFS paths are like: src/runtime/eval.c, src/runtime/unix/config.h
    """
    files = []

    runtime_path = Path(runtime_dir).resolve()
    if not runtime_path.exists():
        print(f"Warning: Runtime directory not found: {runtime_dir}", file=sys.stderr)
        return files

    # We need to preserve the src/runtime structure for cc -I paths
    # runtime_dir is typically thirdparty/MicroHs/src/runtime
    # We want VFS paths like: src/runtime/eval.c

    # Find the 'src' parent to get correct relative paths
    src_parent = runtime_path.parent  # should be 'src'

    for pattern in ["*.c", "*.h"]:
        for c_file in runtime_path.rglob(pattern):
            # Create VFS path: src/runtime/file.c or src/runtime/unix/file.h
            rel_path = c_file.relative_to(src_parent.parent)  # relative to MicroHs dir
            vfs_path = str(rel_path)

            try:
                content = c_file.read_bytes()
                files.append((vfs_path, str(c_file), content))
            except Exception as e:
                print(f"Warning: Could not read {c_file}: {e}", file=sys.stderr)

    return files


def collect_extra_headers(header_paths: list[str]) -> list[tuple[str, str, bytes]]:
    """
    Collect extra header files for embedding.
    These are placed in src/runtime/ so they're found by cc -I.
    """
    files = []

    for header_path in header_paths:
        path = Path(header_path).resolve()
        if not path.exists():
            print(f"Warning: Header file not found: {header_path}", file=sys.stderr)
            continue

        # Place in src/runtime/ so cc -I finds them
        vfs_path = f"src/runtime/{path.name}"

        try:
            content = path.read_bytes()
            files.append((vfs_path, str(path), content))
        except Exception as e:
            print(f"Warning: Could not read {path}: {e}", file=sys.stderr)

    return files


def generate_header(output_path: str, lib_dirs: list[str], runtime_dir: str = None, extra_headers: list[str] = None) -> None:
    """Generate C header with all embedded files."""
    files = collect_hs_files(lib_dirs)
    hs_count = len(files)

    runtime_count = 0
    if runtime_dir:
        runtime_files = collect_runtime_files(runtime_dir)
        runtime_count = len(runtime_files)
        files.extend(runtime_files)

    header_count = 0
    if extra_headers:
        header_files = collect_extra_headers(extra_headers)
        header_count = len(header_files)
        files.extend(header_files)

    if not files:
        print("Error: No files found in specified directories", file=sys.stderr)
        sys.exit(1)

    total_size = sum(len(content) for _, _, content in files)
    print(f"Embedding {len(files)} files ({hs_count} .hs, {runtime_count} runtime, {header_count} extra headers, {total_size:,} bytes total)")

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("/* Auto-generated by embed_libs.py - DO NOT EDIT */\n")
        f.write(f"/* {len(files)} embedded files ({hs_count} .hs, {runtime_count} runtime) */\n\n")
        f.write("#ifndef MHS_EMBEDDED_LIBS_H\n")
        f.write("#define MHS_EMBEDDED_LIBS_H\n\n")
        f.write("#include <stddef.h>\n\n")

        f.write("typedef struct {\n")
        f.write("    const char* path;      /* VFS path, e.g., \"lib/Prelude.hs\" */\n")
        f.write("    const char* content;   /* File contents */\n")
        f.write("    size_t length;         /* Content length in bytes */\n")
        f.write("} EmbeddedFile;\n\n")

        f.write("static const EmbeddedFile embedded_files[] = {\n")

        for vfs_path, full_path, content in sorted(files):
            escaped = escape_c_string(content)
            chunks = split_string_literal(escaped)

            f.write(f"    /* {full_path} */\n")
            f.write(f'    {{ "{vfs_path}",\n')

            if len(chunks) == 1:
                f.write(f'      "{chunks[0]}",\n')
            else:
                # Multiple chunks need concatenation
                f.write('      ')
                for i, chunk in enumerate(chunks):
                    if i > 0:
                        f.write('\n      ')
                    f.write(f'"{chunk}"')
                f.write(',\n')

            f.write(f'      {len(content)} }},\n\n')

        f.write("    /* Sentinel */\n")
        f.write("    { NULL, NULL, 0 }\n")
        f.write("};\n\n")

        f.write(f"#define EMBEDDED_FILE_COUNT {len(files)}\n\n")
        f.write("#endif /* MHS_EMBEDDED_LIBS_H */\n")

    print(f"Generated: {output_path}")


def main():
    parser = argparse.ArgumentParser(
        description="Convert Haskell library files and runtime to C header with embedded strings"
    )
    parser.add_argument(
        "output",
        help="Output header file path"
    )
    parser.add_argument(
        "libdirs",
        nargs="+",
        help="Library directories to embed (e.g., thirdparty/MicroHs/lib)"
    )
    parser.add_argument(
        "--runtime",
        help="Runtime directory to embed (e.g., thirdparty/MicroHs/src/runtime)"
    )
    parser.add_argument(
        "--header",
        action="append",
        dest="headers",
        help="Extra header file to embed (can be repeated)"
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Print each file being processed"
    )

    args = parser.parse_args()

    # Ensure output directory exists
    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    generate_header(args.output, args.libdirs, args.runtime, args.headers)


if __name__ == "__main__":
    main()
