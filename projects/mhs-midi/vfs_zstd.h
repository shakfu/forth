/* vfs_zstd.h - Virtual Filesystem with zstd decompression */

#ifndef VFS_ZSTD_H
#define VFS_ZSTD_H

#include <stdio.h>
#include <stddef.h>

/*
 * Initialize VFS with zstd decompression support.
 * Prepares the decompression dictionary for use.
 * Returns 0 on success, -1 on failure.
 */
int vfs_zstd_init(void);

/*
 * Shutdown VFS and free resources.
 */
void vfs_zstd_shutdown(void);

/*
 * Get the virtual root path (for setting MHSDIR).
 * Returns "/mhs-embedded" - a virtual path prefix.
 */
const char* vfs_zstd_get_root(void);

/*
 * Open a file - decompresses from embedded data on demand.
 * For paths starting with the virtual root, looks up in embedded files,
 * decompresses using zstd, and returns a FILE* via fmemopen().
 *
 * Decompressed content is cached for subsequent opens of the same file.
 */
FILE* vfs_zstd_fopen(const char* path, const char* mode);

/*
 * Get total number of embedded files.
 */
int vfs_zstd_file_count(void);

/*
 * Get total size of embedded compressed content (excluding dictionary).
 */
size_t vfs_zstd_compressed_size(void);

/*
 * Get total size of original uncompressed content.
 */
size_t vfs_zstd_original_size(void);

/*
 * Print VFS statistics (compression ratio, cache stats, etc).
 */
void vfs_zstd_print_stats(void);

/*
 * Extract all embedded files to a temp directory.
 * Decompresses all files and writes to disk for cc to access.
 * Returns the path to the temp directory, or NULL on failure.
 * Caller is responsible for cleanup via vfs_zstd_cleanup_temp().
 */
char* vfs_zstd_extract_to_temp(void);

/*
 * Clean up extracted temp directory.
 */
void vfs_zstd_cleanup_temp(char* temp_dir);

/*
 * Clear the decompression cache to free memory.
 * Useful if memory is constrained after initial loading.
 */
void vfs_zstd_clear_cache(void);

#endif /* VFS_ZSTD_H */
