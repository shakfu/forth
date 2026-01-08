/* mhs_ffi_override.c - Override MicroHs FFI functions for VFS support
 *
 * This file provides replacements for MicroHs's file and directory
 * operations that route through the VFS layer, allowing embedded files
 * to be served from memory.
 *
 * The original functions in eval.c are renamed via patch_eval_vfs.py:
 *   mhs_fopen -> mhs_fopen_orig
 *   mhs_opendir -> mhs_opendir_orig
 *   mhs_readdir -> mhs_readdir_orig
 *   mhs_closedir -> mhs_closedir_orig
 */

#include <stdio.h>
#include <dirent.h>
#include "mhsffi.h"
#include "vfs.h"

/* Original FFI functions (renamed in eval.c) */
extern from_t mhs_fopen_orig(int s);
extern from_t mhs_opendir_orig(int s);
extern from_t mhs_readdir_orig(int s);
extern from_t mhs_closedir_orig(int s);

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

/*
 * Override mhs_opendir to use VFS-aware directory opening.
 *
 * The FFI calling convention:
 *   - mhs_to_Ptr(s, 0) = path (const char*)
 *   - mhs_from_Ptr(s, 1, result) = return DIR*
 */
from_t mhs_opendir(int s) {
    const char* path = mhs_to_Ptr(s, 0);

    /* Use VFS opendir which checks virtual directories first */
    DIR* result = vfs_opendir(path);

    return mhs_from_Ptr(s, 1, result);
}

/*
 * Override mhs_readdir to use VFS-aware directory reading.
 *
 * The FFI calling convention:
 *   - mhs_to_Ptr(s, 0) = dirp (DIR*)
 *   - mhs_from_Ptr(s, 1, result) = return struct dirent*
 */
from_t mhs_readdir(int s) {
    DIR* dirp = mhs_to_Ptr(s, 0);

    /* Use VFS readdir which handles virtual directories */
    struct dirent* result = vfs_readdir(dirp);

    return mhs_from_Ptr(s, 1, result);
}

/*
 * Override mhs_closedir to use VFS-aware directory closing.
 *
 * The FFI calling convention:
 *   - mhs_to_Ptr(s, 0) = dirp (DIR*)
 *   - mhs_from_Int(s, 1, result) = return int
 */
from_t mhs_closedir(int s) {
    DIR* dirp = mhs_to_Ptr(s, 0);

    /* Use VFS closedir which handles virtual directories */
    int result = vfs_closedir(dirp);

    return mhs_from_Int(s, 1, result);
}
