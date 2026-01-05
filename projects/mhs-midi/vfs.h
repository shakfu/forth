/* vfs.h - Virtual Filesystem for embedded Haskell libraries */

#ifndef VFS_H
#define VFS_H

#include <stdio.h>
#include <stddef.h>

/* Initialize VFS.
 * Returns 0 on success, -1 on failure.
 */
int vfs_init(void);

/* Get the virtual root path (for setting MHSDIR).
 * Returns "/mhs-embedded" - a virtual path prefix.
 */
const char* vfs_get_temp_dir(void);

/* Open a file - checks embedded files first, then falls back to filesystem.
 * For paths starting with the virtual root, looks up in embedded files
 * and uses fmemopen() to return a FILE* from memory.
 */
FILE* vfs_fopen(const char* path, const char* mode);

/* Get total number of embedded files */
int vfs_file_count(void);

/* Get total size of embedded content */
size_t vfs_total_size(void);

/* Print VFS statistics */
void vfs_print_stats(void);

/* List all embedded files (for debugging) */
void vfs_list_files(void);

#endif /* VFS_H */
