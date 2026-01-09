#!/usr/bin/env python3
"""
Patch mhs.c to remove xffi_table definition.

This allows midi_ffi_wrappers.c to provide its own xffi_table with MIDI FFI functions.
"""

import sys

if len(sys.argv) != 3:
    print(f"Usage: {sys.argv[0]} <input.c> <output.c>", file=sys.stderr)
    sys.exit(1)

input_file = sys.argv[1]
output_file = sys.argv[2]

with open(input_file, 'r') as f:
    content = f.read()

# Replace the xffi_table definition with a comment
content = content.replace(
    'const struct ffi_entry *xffi_table = imp_table;',
    '// xffi_table defined in midi_ffi_wrappers.c'
)

with open(output_file, 'w') as f:
    f.write(content)
