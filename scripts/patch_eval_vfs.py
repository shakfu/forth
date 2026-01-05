#!/usr/bin/env python3
"""
Patch eval.c to support VFS override of mhs_fopen.

This script:
1. Renames mhs_fopen to mhs_fopen_orig (the original implementation)
2. Adds extern declaration for mhs_fopen (our VFS override)

Usage: patch_eval_vfs.py <input_eval.c> <output_eval_vfs.c>
"""

import sys
import re


def patch_eval(input_path: str, output_path: str) -> None:
    with open(input_path, 'r') as f:
        content = f.read()

    # Rename the function definition
    content = re.sub(
        r'^(from_t) mhs_fopen\(',
        r'\1 mhs_fopen_orig(',
        content,
        flags=re.MULTILINE
    )

    # Add extern declaration before ffi_table
    extern_decl = (
        '/* Forward declaration for VFS override - mhs_fopen provided by mhs_ffi_override.c */\n'
        'extern from_t mhs_fopen(int s);\n\n'
    )

    # Insert before "const struct ffi_entry ffi_table[] = {"
    content = re.sub(
        r'^(const struct ffi_entry ffi_table\[\] = \{)',
        extern_decl + r'\1',
        content,
        flags=re.MULTILINE
    )

    with open(output_path, 'w') as f:
        f.write(content)

    print(f"Patched: {input_path} -> {output_path}")


if __name__ == '__main__':
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <input_eval.c> <output_eval_vfs.c>")
        sys.exit(1)

    patch_eval(sys.argv[1], sys.argv[2])
