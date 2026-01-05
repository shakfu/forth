/* vfs_unified.h - Virtual Filesystem with optional zstd compression
 *
 * Compile options:
 *   -DVFS_USE_ZSTD    Enable zstd compression (smaller binary, ~50ms init)
 *   (default)         Uncompressed (faster startup, larger binary)
 *
 * The API is identical regardless of compression mode.
 */

#ifndef VFS_UNIFIED_H
#define VFS_UNIFIED_H

#include <stdio.h>
#include <stddef.h>

/*
 * Initialize VFS.
 * With zstd: creates decompression context and loads dictionary.
 * Returns 0 on success, -1 on failure.
 */
int vfs_init(void);

/*
 * Shutdown VFS and free resources.
 * With zstd: frees decompression cache and context.
 */
void vfs_shutdown(void);

/*
 * Get the virtual root path (for setting MHSDIR).
 * Returns "/mhs-embedded".
 */
const char* vfs_get_root(void);

/*
 * Open a file from VFS or filesystem.
 * Paths starting with vfs_get_root() are served from embedded data.
 * Other paths fall through to fopen().
 *
 * With zstd: decompresses on first access, caches result.
 */
FILE* vfs_fopen(const char* path, const char* mode);

/*
 * Get number of embedded files.
 */
int vfs_file_count(void);

/*
 * Get total size of original (uncompressed) content.
 */
size_t vfs_total_size(void);

/*
 * Get size of embedded data in binary.
 * With zstd: compressed size + dictionary.
 * Without: same as vfs_total_size().
 */
size_t vfs_embedded_size(void);

/*
 * Print VFS statistics.
 * With zstd: includes compression ratio and cache stats.
 */
void vfs_print_stats(void);

/*
 * Extract all files to a temp directory.
 * Use when external tools need filesystem access (e.g., cc).
 * Returns temp directory path (caller must free with vfs_cleanup_temp).
 */
char* vfs_extract_to_temp(void);

/*
 * Clean up extracted temp directory.
 */
void vfs_cleanup_temp(char* temp_dir);

#ifdef VFS_USE_ZSTD
/*
 * Clear decompression cache to free memory.
 * Only available with zstd compression.
 */
void vfs_clear_cache(void);
#endif

#endif /* VFS_UNIFIED_H */
