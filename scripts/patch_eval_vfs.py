#!/usr/bin/env python3
"""
Patch eval.c to support VFS override of file and directory operations.

This script:
1. Renames mhs_fopen to mhs_fopen_orig (the original implementation)
2. Renames mhs_opendir to mhs_opendir_orig (for VFS directory support)
3. Renames mhs_readdir to mhs_readdir_orig (for VFS directory support)
4. Renames mhs_closedir to mhs_closedir_orig (for VFS directory support)
5. Adds extern declarations for VFS overrides

Usage: patch_eval_vfs.py <input_eval.c> <output_eval_vfs.c>
"""

import sys
import re


def patch_eval(input_path: str, output_path: str) -> None:
    with open(input_path, 'r') as f:
        content = f.read()

    # Rename function definitions
    functions_to_rename = ['mhs_fopen', 'mhs_opendir', 'mhs_readdir', 'mhs_closedir']
    for func in functions_to_rename:
        content = re.sub(
            rf'^(from_t) {func}\(',
            rf'\1 {func}_orig(',
            content,
            flags=re.MULTILINE
        )

    # Add extern declarations before ffi_table
    extern_decl = (
        '/* Forward declarations for VFS overrides - provided by mhs_ffi_override.c */\n'
        'extern from_t mhs_fopen(int s);\n'
        'extern from_t mhs_opendir(int s);\n'
        'extern from_t mhs_readdir(int s);\n'
        'extern from_t mhs_closedir(int s);\n\n'
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
