/* mhs_ffi_override.c - Override MicroHs FFI fopen for VFS support
 *
 * This file provides a replacement for MicroHs's mhs_fopen that routes
 * file opens through the VFS layer, allowing embedded files to be
 * served from memory.
 *
 * The original mhs_fopen in eval.c is renamed to mhs_fopen_orig via sed
 * during the build process.
 */

#include <stdio.h>
#include "mhsffi.h"
#include "vfs.h"

/* Original fopen FFI function (renamed from mhs_fopen in eval.c) */
extern from_t mhs_fopen_orig(int s);

/*
 * Override mhs_fopen to use VFS-aware file opening.
 *
 * This intercepts all fopen calls from the MicroHs runtime and routes
 * them through vfs_fopen, which checks for embedded library files before
 * falling back to the real filesystem.
 *
 * The FFI calling convention (from eval.c):
 *   - mhs_to_Ptr(s, 0) = path (const char*)
 *   - mhs_to_Ptr(s, 1) = mode (const char*)
 *   - mhs_from_Ptr(s, 2, result) = return FILE*
 */
from_t mhs_fopen(int s) {
    const char* path = mhs_to_Ptr(s, 0);
    const char* mode = mhs_to_Ptr(s, 1);

    /* Use VFS fopen which checks embedded files first */
    FILE* result = vfs_fopen(path, mode);

    return mhs_from_Ptr(s, 2, result);
}
