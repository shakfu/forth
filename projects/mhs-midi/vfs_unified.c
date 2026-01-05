/* vfs_unified.c - Virtual Filesystem with optional zstd compression
 *
 * Compile options:
 *   -DVFS_USE_ZSTD    Enable zstd decompression (requires zstd library)
 *   (default)         Use uncompressed embedded files
 *
 * Usage:
 *   # Uncompressed (simpler, faster startup)
 *   cc -c vfs_unified.c -o vfs.o
 *
 *   # Compressed (smaller binary)
 *   cc -DVFS_USE_ZSTD -c vfs_unified.c -o vfs.o
 *   cc vfs.o zstddeclib.o ...
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include <libgen.h>
#include <unistd.h>

#ifdef VFS_USE_ZSTD
#define ZSTD_STATIC_LINKING_ONLY
#include "zstd.h"
#include "mhs_embedded_zstd.h"
#else
#include "mhs_embedded_libs.h"
#endif

/* Use original vfs.h API for drop-in compatibility */
#include "vfs.h"

/* Debug logging */
#ifndef VFS_DEBUG
#define VFS_DEBUG 0
#endif

#if VFS_DEBUG
#define VFS_LOG(...) fprintf(stderr, "VFS: " __VA_ARGS__)
#else
#define VFS_LOG(...) ((void)0)
#endif

/*-----------------------------------------------------------
 * Constants and state
 *-----------------------------------------------------------*/

#define VFS_VIRTUAL_ROOT "/mhs-embedded"

static int g_initialized = 0;

#ifdef VFS_USE_ZSTD
/* Zstd decompression state */
static ZSTD_DCtx* g_dctx = NULL;
static ZSTD_DDict* g_ddict = NULL;

/* Decompression cache */
typedef struct {
    const char* path;
    char* content;
    size_t length;
} CachedFile;

static CachedFile* g_cache = NULL;
static int g_cache_size = 0;
static int g_cache_hits = 0;
static int g_cache_misses = 0;
#endif

/*-----------------------------------------------------------
 * File lookup
 *-----------------------------------------------------------*/

#ifdef VFS_USE_ZSTD

static const EmbeddedFileZstd* find_file(const char* rel_path) {
    for (const EmbeddedFileZstd* ef = embedded_files_zstd; ef->path; ef++) {
        if (strcmp(ef->path, rel_path) == 0) {
            return ef;
        }
    }
    return NULL;
}

static CachedFile* get_cache_entry(const char* path) {
    for (int i = 0; i < g_cache_size; i++) {
        if (g_cache[i].path && strcmp(g_cache[i].path, path) == 0) {
            return &g_cache[i];
        }
    }
    return NULL;
}

static CachedFile* decompress_and_cache(const EmbeddedFileZstd* ef) {
    char* buffer = malloc(ef->original_size);
    if (!buffer) {
        VFS_LOG("Failed to allocate %u bytes for %s\n", ef->original_size, ef->path);
        return NULL;
    }

    size_t result;
    if (ef->use_dict && g_ddict) {
        result = ZSTD_decompress_usingDDict(
            g_dctx, buffer, ef->original_size,
            ef->data, ef->compressed_size, g_ddict
        );
    } else {
        result = ZSTD_decompressDCtx(
            g_dctx, buffer, ef->original_size,
            ef->data, ef->compressed_size
        );
    }

    if (ZSTD_isError(result)) {
        VFS_LOG("Decompression failed for %s: %s\n", ef->path, ZSTD_getErrorName(result));
        free(buffer);
        return NULL;
    }

    if (result != ef->original_size) {
        VFS_LOG("Size mismatch for %s: expected %u, got %zu\n",
                ef->path, ef->original_size, result);
        free(buffer);
        return NULL;
    }

    VFS_LOG("Decompressed %s: %u -> %u bytes\n",
            ef->path, ef->compressed_size, ef->original_size);

    g_cache = realloc(g_cache, (g_cache_size + 1) * sizeof(CachedFile));
    if (!g_cache) {
        free(buffer);
        return NULL;
    }

    CachedFile* entry = &g_cache[g_cache_size++];
    entry->path = ef->path;
    entry->content = buffer;
    entry->length = ef->original_size;

    return entry;
}

#else /* !VFS_USE_ZSTD */

static const EmbeddedFile* find_file(const char* rel_path) {
    for (const EmbeddedFile* ef = embedded_files; ef->path; ef++) {
        if (strcmp(ef->path, rel_path) == 0) {
            return ef;
        }
    }
    return NULL;
}

#endif /* VFS_USE_ZSTD */

/*-----------------------------------------------------------
 * Directory creation helper
 *-----------------------------------------------------------*/

static int mkdirs(const char* path) {
    char tmp[4096];
    char* p = NULL;
    size_t len;

    snprintf(tmp, sizeof(tmp), "%s", path);
    len = strlen(tmp);
    if (tmp[len - 1] == '/') {
        tmp[len - 1] = 0;
    }
    for (p = tmp + 1; *p; p++) {
        if (*p == '/') {
            *p = 0;
            if (mkdir(tmp, 0755) != 0 && errno != EEXIST) {
                return -1;
            }
            *p = '/';
        }
    }
    if (mkdir(tmp, 0755) != 0 && errno != EEXIST) {
        return -1;
    }
    return 0;
}

/*-----------------------------------------------------------
 * Public API
 *-----------------------------------------------------------*/

int vfs_init(void) {
    if (g_initialized) {
        return 0;
    }

#ifdef VFS_USE_ZSTD
    g_dctx = ZSTD_createDCtx();
    if (!g_dctx) {
        fprintf(stderr, "VFS: Failed to create zstd decompression context\n");
        return -1;
    }

    if (EMBEDDED_DICT_SIZE > 0) {
        g_ddict = ZSTD_createDDict(embedded_zstd_dict, EMBEDDED_DICT_SIZE);
        if (!g_ddict) {
            fprintf(stderr, "VFS: Failed to create zstd dictionary\n");
            ZSTD_freeDCtx(g_dctx);
            g_dctx = NULL;
            return -1;
        }
        VFS_LOG("Loaded dictionary: %d bytes\n", EMBEDDED_DICT_SIZE);
    }

    VFS_LOG("Initialized (zstd): %d files, %zu -> %zu bytes\n",
            EMBEDDED_FILE_COUNT,
            (size_t)EMBEDDED_TOTAL_COMPRESSED,
            (size_t)EMBEDDED_TOTAL_ORIGINAL);
#else
    VFS_LOG("Initialized: %d embedded files\n", EMBEDDED_FILE_COUNT);
#endif

    g_initialized = 1;
    return 0;
}

void vfs_shutdown(void) {
    if (!g_initialized) {
        return;
    }

#ifdef VFS_USE_ZSTD
    for (int i = 0; i < g_cache_size; i++) {
        free(g_cache[i].content);
    }
    free(g_cache);
    g_cache = NULL;
    g_cache_size = 0;

    if (g_ddict) {
        ZSTD_freeDDict(g_ddict);
        g_ddict = NULL;
    }
    if (g_dctx) {
        ZSTD_freeDCtx(g_dctx);
        g_dctx = NULL;
    }
#endif

    g_initialized = 0;
}

const char* vfs_get_temp_dir(void) {
    return VFS_VIRTUAL_ROOT;
}

FILE* vfs_fopen(const char* path, const char* mode) {
    if (!path || !mode) {
        return NULL;
    }

    VFS_LOG("fopen: %s (mode=%s)\n", path, mode);

    const size_t root_len = strlen(VFS_VIRTUAL_ROOT);
    if (strncmp(path, VFS_VIRTUAL_ROOT, root_len) == 0) {
        const char* rel_path = path + root_len;
        if (*rel_path == '/') {
            rel_path++;
        }

        VFS_LOG("Looking up: %s\n", rel_path);

#ifdef VFS_USE_ZSTD
        const EmbeddedFileZstd* ef = find_file(rel_path);
        if (!ef) {
            VFS_LOG("Not found: %s\n", rel_path);
            return NULL;
        }

        CachedFile* cached = get_cache_entry(ef->path);
        if (cached) {
            g_cache_hits++;
            VFS_LOG("Cache hit: %s\n", ef->path);
            return fmemopen(cached->content, cached->length, "r");
        }

        g_cache_misses++;
        cached = decompress_and_cache(ef);
        if (!cached) {
            return NULL;
        }

        return fmemopen(cached->content, cached->length, "r");
#else
        const EmbeddedFile* ef = find_file(rel_path);
        if (ef) {
            VFS_LOG("Found: %s (%zu bytes)\n", ef->path, ef->length);
            return fmemopen((void*)ef->content, ef->length, "r");
        }
        VFS_LOG("Not found: %s\n", rel_path);
        return NULL;
#endif
    }

    return fopen(path, mode);
}

int vfs_file_count(void) {
    return EMBEDDED_FILE_COUNT;
}

size_t vfs_total_size(void) {
#ifdef VFS_USE_ZSTD
    return EMBEDDED_TOTAL_ORIGINAL;
#else
    size_t total = 0;
    for (const EmbeddedFile* ef = embedded_files; ef->path; ef++) {
        total += ef->length;
    }
    return total;
#endif
}

size_t vfs_embedded_size(void) {
#ifdef VFS_USE_ZSTD
    return EMBEDDED_TOTAL_COMPRESSED + EMBEDDED_DICT_SIZE;
#else
    return vfs_total_size();
#endif
}

void vfs_print_stats(void) {
#ifdef VFS_USE_ZSTD
    double ratio = (double)EMBEDDED_TOTAL_ORIGINAL /
                   (EMBEDDED_TOTAL_COMPRESSED + EMBEDDED_DICT_SIZE);
    printf("VFS Statistics (zstd compressed):\n");
    printf("  Files:        %d\n", EMBEDDED_FILE_COUNT);
    printf("  Original:     %zu bytes\n", (size_t)EMBEDDED_TOTAL_ORIGINAL);
    printf("  Compressed:   %zu bytes\n", (size_t)EMBEDDED_TOTAL_COMPRESSED);
    printf("  Dictionary:   %d bytes\n", EMBEDDED_DICT_SIZE);
    printf("  Ratio:        %.2fx\n", ratio);
    printf("  Cache:        %d entries (%d hits, %d misses)\n",
           g_cache_size, g_cache_hits, g_cache_misses);
#else
    printf("VFS Statistics (uncompressed):\n");
    printf("  Files:        %d\n", EMBEDDED_FILE_COUNT);
    printf("  Total size:   %zu bytes\n", vfs_total_size());
#endif
}

void vfs_list_files(void) {
    printf("Embedded files:\n");
#ifdef VFS_USE_ZSTD
    for (const EmbeddedFileZstd* ef = embedded_files_zstd; ef->path; ef++) {
        printf("  %s (%u bytes, compressed: %u)\n",
               ef->path, ef->original_size, ef->compressed_size);
    }
#else
    for (const EmbeddedFile* ef = embedded_files; ef->path; ef++) {
        printf("  %s (%zu bytes)\n", ef->path, ef->length);
    }
#endif
}

char* vfs_extract_to_temp(void) {
    char template[] = "/tmp/mhs-XXXXXX";
    char* temp_dir = mkdtemp(template);
    if (!temp_dir) {
        fprintf(stderr, "Error: Could not create temp directory: %s\n", strerror(errno));
        return NULL;
    }

    char* result = strdup(temp_dir);
    if (!result) {
        return NULL;
    }

    VFS_LOG("Extracting to: %s\n", result);

#ifdef VFS_USE_ZSTD
    for (const EmbeddedFileZstd* ef = embedded_files_zstd; ef->path; ef++) {
        char full_path[4096];
        snprintf(full_path, sizeof(full_path), "%s/%s", result, ef->path);

        char* dir_path = strdup(full_path);
        if (dir_path) {
            char* dir = dirname(dir_path);
            if (mkdirs(dir) != 0) {
                fprintf(stderr, "Error: Could not create directory: %s\n", dir);
                free(dir_path);
                free(result);
                return NULL;
            }
            free(dir_path);
        }

        CachedFile* cached = get_cache_entry(ef->path);
        if (!cached) {
            cached = decompress_and_cache(ef);
            if (!cached) {
                fprintf(stderr, "Error: Could not decompress: %s\n", ef->path);
                free(result);
                return NULL;
            }
        }

        FILE* f = fopen(full_path, "wb");
        if (!f) {
            fprintf(stderr, "Error: Could not create file: %s\n", full_path);
            free(result);
            return NULL;
        }

        size_t written = fwrite(cached->content, 1, cached->length, f);
        fclose(f);

        if (written != cached->length) {
            fprintf(stderr, "Error: Could not write file: %s\n", full_path);
            free(result);
            return NULL;
        }

        VFS_LOG("Extracted: %s (%zu bytes)\n", ef->path, cached->length);
    }
#else
    for (const EmbeddedFile* ef = embedded_files; ef->path; ef++) {
        char full_path[4096];
        snprintf(full_path, sizeof(full_path), "%s/%s", result, ef->path);

        char* dir_path = strdup(full_path);
        if (dir_path) {
            char* dir = dirname(dir_path);
            if (mkdirs(dir) != 0) {
                fprintf(stderr, "Error: Could not create directory: %s\n", dir);
                free(dir_path);
                free(result);
                return NULL;
            }
            free(dir_path);
        }

        FILE* f = fopen(full_path, "wb");
        if (!f) {
            fprintf(stderr, "Error: Could not create file: %s\n", full_path);
            free(result);
            return NULL;
        }

        size_t written = fwrite(ef->content, 1, ef->length, f);
        fclose(f);

        if (written != ef->length) {
            fprintf(stderr, "Error: Could not write file: %s\n", full_path);
            free(result);
            return NULL;
        }

        VFS_LOG("Extracted: %s (%zu bytes)\n", ef->path, ef->length);
    }
#endif

    VFS_LOG("Extracted %d files to %s\n", EMBEDDED_FILE_COUNT, result);
    return result;
}

void vfs_cleanup_temp(char* temp_dir) {
    if (temp_dir) {
        VFS_LOG("Cleaning up: %s\n", temp_dir);
        char cmd[4096];
        snprintf(cmd, sizeof(cmd), "rm -rf '%s'", temp_dir);
        system(cmd);
        free(temp_dir);
    }
}

#ifdef VFS_USE_ZSTD
void vfs_clear_cache(void) {
    for (int i = 0; i < g_cache_size; i++) {
        free(g_cache[i].content);
    }
    free(g_cache);
    g_cache = NULL;
    g_cache_size = 0;
    g_cache_hits = 0;
    g_cache_misses = 0;
    VFS_LOG("Cache cleared\n");
}
#endif
