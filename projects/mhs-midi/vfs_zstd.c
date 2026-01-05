/* vfs_zstd.c - Virtual Filesystem with zstd decompression
 *
 * Serves embedded files compressed with zstd dictionary mode.
 * Decompresses on demand and caches results for repeated access.
 *
 * Build with zstddeclib.c (decompress-only, ~900KB) for minimal footprint:
 *   gcc -c thirdparty/zstd-1.5.7/zstddeclib.c -o zstddeclib.o
 *   gcc vfs_zstd.c zstddeclib.o -o program
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include <libgen.h>
#include <unistd.h>

/* Zstd headers - we only use decompression */
#define ZSTD_STATIC_LINKING_ONLY
#include "zstd.h"

/* Include generated compressed data */
#include "mhs_embedded_zstd.h"

#include "vfs_zstd.h"

/* Debug logging */
#ifndef VFS_ZSTD_DEBUG
#define VFS_ZSTD_DEBUG 0
#endif

#if VFS_ZSTD_DEBUG
#define VFS_LOG(...) fprintf(stderr, "VFS_ZSTD: " __VA_ARGS__)
#else
#define VFS_LOG(...) ((void)0)
#endif

/*-----------------------------------------------------------
 * Constants and state
 *-----------------------------------------------------------*/

#define VFS_VIRTUAL_ROOT "/mhs-embedded"

/* Decompression context and dictionary */
static ZSTD_DCtx* g_dctx = NULL;
static ZSTD_DDict* g_ddict = NULL;

/* Cache for decompressed files */
typedef struct {
    const char* path;       /* Points to embedded_files_zstd[].path */
    char* content;          /* Decompressed content (malloc'd) */
    size_t length;          /* Decompressed length */
} CachedFile;

static CachedFile* g_cache = NULL;
static int g_cache_size = 0;
static int g_cache_hits = 0;
static int g_cache_misses = 0;

static int g_initialized = 0;

/*-----------------------------------------------------------
 * Internal helpers
 *-----------------------------------------------------------*/

/* Find compressed file entry by relative path */
static const EmbeddedFileZstd* find_compressed_file(const char* rel_path) {
    for (const EmbeddedFileZstd* ef = embedded_files_zstd; ef->path; ef++) {
        if (strcmp(ef->path, rel_path) == 0) {
            return ef;
        }
    }
    return NULL;
}

/* Find or create cache entry for a file */
static CachedFile* get_cache_entry(const char* path) {
    /* Check existing entries */
    for (int i = 0; i < g_cache_size; i++) {
        if (g_cache[i].path && strcmp(g_cache[i].path, path) == 0) {
            return &g_cache[i];
        }
    }
    return NULL;
}

/* Decompress a file and cache the result */
static CachedFile* decompress_and_cache(const EmbeddedFileZstd* ef) {
    /* Allocate decompression buffer */
    char* buffer = malloc(ef->original_size);
    if (!buffer) {
        VFS_LOG("Failed to allocate %u bytes for %s\n", ef->original_size, ef->path);
        return NULL;
    }

    /* Decompress */
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
        VFS_LOG("Decompression failed for %s: %s\n",
                ef->path, ZSTD_getErrorName(result));
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

    /* Add to cache */
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

/* Recursively create directories */
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

int vfs_zstd_init(void) {
    if (g_initialized) {
        return 0;
    }

    /* Create decompression context */
    g_dctx = ZSTD_createDCtx();
    if (!g_dctx) {
        fprintf(stderr, "VFS: Failed to create zstd decompression context\n");
        return -1;
    }

    /* Create dictionary if we have one */
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

    VFS_LOG("Initialized with %d files (%zu compressed, %zu original)\n",
            EMBEDDED_FILE_COUNT, (size_t)EMBEDDED_TOTAL_COMPRESSED,
            (size_t)EMBEDDED_TOTAL_ORIGINAL);

    g_initialized = 1;
    return 0;
}

void vfs_zstd_shutdown(void) {
    if (!g_initialized) {
        return;
    }

    /* Free cache */
    for (int i = 0; i < g_cache_size; i++) {
        free(g_cache[i].content);
    }
    free(g_cache);
    g_cache = NULL;
    g_cache_size = 0;

    /* Free zstd resources */
    if (g_ddict) {
        ZSTD_freeDDict(g_ddict);
        g_ddict = NULL;
    }
    if (g_dctx) {
        ZSTD_freeDCtx(g_dctx);
        g_dctx = NULL;
    }

    g_initialized = 0;
}

const char* vfs_zstd_get_root(void) {
    return VFS_VIRTUAL_ROOT;
}

FILE* vfs_zstd_fopen(const char* path, const char* mode) {
    if (!path || !mode) {
        return NULL;
    }

    VFS_LOG("fopen: %s (mode=%s)\n", path, mode);

    /* Check if path starts with virtual root */
    const size_t root_len = strlen(VFS_VIRTUAL_ROOT);
    if (strncmp(path, VFS_VIRTUAL_ROOT, root_len) == 0) {
        /* Extract relative path */
        const char* rel_path = path + root_len;
        if (*rel_path == '/') {
            rel_path++;
        }

        VFS_LOG("Looking up: %s\n", rel_path);

        /* Find compressed file entry */
        const EmbeddedFileZstd* ef = find_compressed_file(rel_path);
        if (!ef) {
            VFS_LOG("Not found: %s\n", rel_path);
            return NULL;
        }

        /* Check cache first */
        CachedFile* cached = get_cache_entry(ef->path);
        if (cached) {
            g_cache_hits++;
            VFS_LOG("Cache hit: %s\n", ef->path);
            return fmemopen(cached->content, cached->length, "r");
        }

        /* Decompress and cache */
        g_cache_misses++;
        cached = decompress_and_cache(ef);
        if (!cached) {
            return NULL;
        }

        return fmemopen(cached->content, cached->length, "r");
    }

    /* Not a virtual path - use real filesystem */
    return fopen(path, mode);
}

int vfs_zstd_file_count(void) {
    return EMBEDDED_FILE_COUNT;
}

size_t vfs_zstd_compressed_size(void) {
    return EMBEDDED_TOTAL_COMPRESSED;
}

size_t vfs_zstd_original_size(void) {
    return EMBEDDED_TOTAL_ORIGINAL;
}

void vfs_zstd_print_stats(void) {
    double ratio = (double)EMBEDDED_TOTAL_ORIGINAL /
                   (EMBEDDED_TOTAL_COMPRESSED + EMBEDDED_DICT_SIZE);

    printf("VFS Zstd Statistics:\n");
    printf("  Files:        %d\n", EMBEDDED_FILE_COUNT);
    printf("  Original:     %zu bytes\n", (size_t)EMBEDDED_TOTAL_ORIGINAL);
    printf("  Compressed:   %zu bytes\n", (size_t)EMBEDDED_TOTAL_COMPRESSED);
    printf("  Dictionary:   %d bytes\n", EMBEDDED_DICT_SIZE);
    printf("  Ratio:        %.2fx\n", ratio);
    printf("  Cache:        %d entries\n", g_cache_size);
    printf("  Cache hits:   %d\n", g_cache_hits);
    printf("  Cache misses: %d\n", g_cache_misses);
}

char* vfs_zstd_extract_to_temp(void) {
    /* Create temp directory */
    char template[] = "/tmp/mhs-zstd-XXXXXX";
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

    /* Extract each file */
    for (const EmbeddedFileZstd* ef = embedded_files_zstd; ef->path; ef++) {
        char full_path[4096];
        snprintf(full_path, sizeof(full_path), "%s/%s", result, ef->path);

        /* Create parent directories */
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

        /* Get decompressed content (from cache or decompress) */
        CachedFile* cached = get_cache_entry(ef->path);
        if (!cached) {
            cached = decompress_and_cache(ef);
            if (!cached) {
                fprintf(stderr, "Error: Could not decompress: %s\n", ef->path);
                free(result);
                return NULL;
            }
        }

        /* Write file */
        FILE* f = fopen(full_path, "wb");
        if (!f) {
            fprintf(stderr, "Error: Could not create file: %s: %s\n",
                    full_path, strerror(errno));
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

    VFS_LOG("Extracted %d files to %s\n", EMBEDDED_FILE_COUNT, result);
    return result;
}

void vfs_zstd_cleanup_temp(char* temp_dir) {
    if (temp_dir) {
        VFS_LOG("Cleaning up: %s\n", temp_dir);
        char cmd[4096];
        snprintf(cmd, sizeof(cmd), "rm -rf '%s'", temp_dir);
        system(cmd);
        free(temp_dir);
    }
}

void vfs_zstd_clear_cache(void) {
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
