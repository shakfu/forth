/* vfs.c - Virtual Filesystem with optional zstd compression or .pkg support
 *
 * Compile options:
 *   -DVFS_USE_ZSTD                Enable zstd decompression (requires zstd library)
 *   -DVFS_USE_PKG                 Use precompiled .pkg files instead of .hs source files
 *   -DVFS_USE_PKG -DVFS_USE_ZSTD  Use compressed .pkg files (smallest+fastest)
 *   (default)                     Use uncompressed embedded .hs files
 *
 * Usage:
 *   # Uncompressed .hs files (simpler, slower startup)
 *   cc -c vfs.c -o vfs.o
 *
 *   # Compressed .hs files (smaller binary, slower startup)
 *   cc -DVFS_USE_ZSTD -c vfs.c -o vfs.o
 *
 *   # Precompiled .pkg files (fast startup, larger binary)
 *   cc -DVFS_USE_PKG -c vfs.c -o vfs.o
 *
 *   # Compressed .pkg files (smallest binary, fast startup)
 *   cc -DVFS_USE_PKG -DVFS_USE_ZSTD -c vfs.c -o vfs.o
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include <libgen.h>
#include <unistd.h>

#if defined(VFS_USE_PKG) && defined(VFS_USE_ZSTD)
/* Compressed packages mode - smallest binary + fast startup */
#define ZSTD_STATIC_LINKING_ONLY
#include "zstd.h"
#include "mhs_embedded_pkgs_zstd.h"
#elif defined(VFS_USE_ZSTD)
/* Compressed source mode */
#define ZSTD_STATIC_LINKING_ONLY
#include "zstd.h"
#include "mhs_embedded_zstd.h"
#elif defined(VFS_USE_PKG)
/* Uncompressed packages mode */
#include "mhs_embedded_pkgs.h"
#else
/* Uncompressed source mode */
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

#if defined(VFS_USE_ZSTD)
/* Zstd decompression state (used by both source-zstd and pkg-zstd modes) */
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

#if defined(VFS_USE_PKG) && defined(VFS_USE_ZSTD)

/* Pkg+Zstd mode: compressed packages */
static const EmbeddedFilePkgZstd* find_pkg_zstd_file(const char* rel_path) {
    for (const EmbeddedFilePkgZstd* ef = embedded_files_pkg_zstd; ef->path; ef++) {
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

static CachedFile* decompress_and_cache_pkg(const EmbeddedFilePkgZstd* ef) {
    char* buffer = malloc(ef->original_size);
    if (!buffer) {
        VFS_LOG("Failed to allocate %u bytes for %s\n", ef->original_size, ef->path);
        return NULL;
    }

    size_t result;
    if (g_ddict) {
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

#elif defined(VFS_USE_ZSTD)

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

#elif defined(VFS_USE_PKG) /* !VFS_USE_ZSTD && VFS_USE_PKG */

static const EmbeddedPackage* find_package(const char* rel_path) {
    for (const EmbeddedPackage* ep = embedded_packages; ep->path; ep++) {
        if (strcmp(ep->path, rel_path) == 0) {
            return ep;
        }
    }
    return NULL;
}

static const EmbeddedTxtFile* find_txt_file(const char* rel_path) {
    for (const EmbeddedTxtFile* tf = embedded_txt_files; tf->path; tf++) {
        if (strcmp(tf->path, rel_path) == 0) {
            return tf;
        }
    }
    return NULL;
}

static const EmbeddedRuntimeFile* find_runtime_file(const char* rel_path) {
    for (const EmbeddedRuntimeFile* rf = embedded_runtime_files; rf->path; rf++) {
        if (strcmp(rf->path, rel_path) == 0) {
            return rf;
        }
    }
    return NULL;
}

#else /* !VFS_USE_ZSTD && !VFS_USE_PKG */

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

#if defined(VFS_USE_PKG) && defined(VFS_USE_ZSTD)
    /* Compressed packages mode */
    g_dctx = ZSTD_createDCtx();
    if (!g_dctx) {
        fprintf(stderr, "VFS: Failed to create zstd decompression context\n");
        return -1;
    }

    if (EMBEDDED_PKG_DICT_SIZE > 0) {
        g_ddict = ZSTD_createDDict(embedded_pkg_zstd_dict, EMBEDDED_PKG_DICT_SIZE);
        if (!g_ddict) {
            fprintf(stderr, "VFS: Failed to create zstd dictionary\n");
            ZSTD_freeDCtx(g_dctx);
            g_dctx = NULL;
            return -1;
        }
        VFS_LOG("Loaded dictionary: %d bytes\n", EMBEDDED_PKG_DICT_SIZE);
    }

    VFS_LOG("Initialized (pkg-zstd): %d files, %zu -> %zu bytes\n",
            EMBEDDED_PKG_ZSTD_TOTAL_COUNT,
            (size_t)EMBEDDED_PKG_ZSTD_TOTAL_COMPRESSED,
            (size_t)EMBEDDED_PKG_ZSTD_TOTAL_ORIGINAL);
#elif defined(VFS_USE_ZSTD)
    /* Compressed source mode */
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
#elif defined(VFS_USE_PKG)
    VFS_LOG("Initialized (pkg): %d packages, %d module mappings, %d runtime files\n",
            EMBEDDED_PACKAGE_COUNT, EMBEDDED_TXT_COUNT, EMBEDDED_RUNTIME_COUNT);
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

#if defined(VFS_USE_PKG) && defined(VFS_USE_ZSTD)
        /* Compressed packages mode */
        const EmbeddedFilePkgZstd* ef = find_pkg_zstd_file(rel_path);
        if (!ef) {
            VFS_LOG("Not found: %s\n", rel_path);
            return NULL;
        }

        CachedFile* cached = get_cache_entry(ef->path);
        if (cached) {
            g_cache_hits++;
            VFS_LOG("Cache hit: %s\n", ef->path);
            return fmemopen(cached->content, cached->length, mode);
        }

        g_cache_misses++;
        cached = decompress_and_cache_pkg(ef);
        if (!cached) {
            return NULL;
        }

        return fmemopen(cached->content, cached->length, mode);
#elif defined(VFS_USE_ZSTD)
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
#elif defined(VFS_USE_PKG)
        /* Check for package files first */
        const EmbeddedPackage* ep = find_package(rel_path);
        if (ep) {
            VFS_LOG("Found package: %s (%zu bytes)\n", ep->path, ep->length);
            return fmemopen((void*)ep->content, ep->length, mode);
        }
        /* Check for .txt module mapping files */
        const EmbeddedTxtFile* tf = find_txt_file(rel_path);
        if (tf) {
            VFS_LOG("Found txt mapping: %s -> %s\n", tf->path, tf->content);
            return fmemopen((void*)tf->content, tf->length, mode);
        }
        /* Check for runtime source files */
        const EmbeddedRuntimeFile* rf = find_runtime_file(rel_path);
        if (rf) {
            VFS_LOG("Found runtime file: %s (%zu bytes)\n", rf->path, rf->length);
            return fmemopen((void*)rf->content, rf->length, mode);
        }
        VFS_LOG("Not found: %s\n", rel_path);
        return NULL;
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
#if defined(VFS_USE_PKG) && defined(VFS_USE_ZSTD)
    return EMBEDDED_PKG_ZSTD_TOTAL_COUNT;
#elif defined(VFS_USE_PKG)
    return EMBEDDED_PACKAGE_COUNT;
#else
    return EMBEDDED_FILE_COUNT;
#endif
}

size_t vfs_total_size(void) {
#if defined(VFS_USE_PKG) && defined(VFS_USE_ZSTD)
    return EMBEDDED_PKG_ZSTD_TOTAL_ORIGINAL;
#elif defined(VFS_USE_ZSTD)
    return EMBEDDED_TOTAL_ORIGINAL;
#elif defined(VFS_USE_PKG)
    size_t total = 0;
    for (const EmbeddedPackage* ep = embedded_packages; ep->path; ep++) {
        total += ep->length;
    }
    return total;
#else
    size_t total = 0;
    for (const EmbeddedFile* ef = embedded_files; ef->path; ef++) {
        total += ef->length;
    }
    return total;
#endif
}

size_t vfs_embedded_size(void) {
#if defined(VFS_USE_PKG) && defined(VFS_USE_ZSTD)
    return EMBEDDED_PKG_ZSTD_TOTAL_COMPRESSED + EMBEDDED_PKG_DICT_SIZE;
#elif defined(VFS_USE_ZSTD)
    return EMBEDDED_TOTAL_COMPRESSED + EMBEDDED_DICT_SIZE;
#else
    return vfs_total_size();
#endif
}

void vfs_print_stats(void) {
#if defined(VFS_USE_PKG) && defined(VFS_USE_ZSTD)
    double ratio = (double)EMBEDDED_PKG_ZSTD_TOTAL_ORIGINAL /
                   (EMBEDDED_PKG_ZSTD_TOTAL_COMPRESSED + EMBEDDED_PKG_DICT_SIZE);
    printf("VFS Statistics (pkg-zstd compressed):\n");
    printf("  Packages:     %d\n", EMBEDDED_PKG_ZSTD_PACKAGE_COUNT);
    printf("  Txt files:    %d\n", EMBEDDED_PKG_ZSTD_TXT_COUNT);
    printf("  Runtime:      %d\n", EMBEDDED_PKG_ZSTD_RUNTIME_COUNT);
    printf("  Original:     %zu bytes\n", (size_t)EMBEDDED_PKG_ZSTD_TOTAL_ORIGINAL);
    printf("  Compressed:   %zu bytes\n", (size_t)EMBEDDED_PKG_ZSTD_TOTAL_COMPRESSED);
    printf("  Dictionary:   %d bytes\n", EMBEDDED_PKG_DICT_SIZE);
    printf("  Ratio:        %.2fx\n", ratio);
    printf("  Cache:        %d entries (%d hits, %d misses)\n",
           g_cache_size, g_cache_hits, g_cache_misses);
#elif defined(VFS_USE_ZSTD)
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
#elif defined(VFS_USE_PKG)
    printf("VFS Statistics (packages):\n");
    printf("  Packages:     %d\n", EMBEDDED_PACKAGE_COUNT);
    printf("  Txt mappings: %d\n", EMBEDDED_TXT_COUNT);
    printf("  Total size:   %zu bytes\n", vfs_total_size());
#else
    printf("VFS Statistics (uncompressed):\n");
    printf("  Files:        %d\n", EMBEDDED_FILE_COUNT);
    printf("  Total size:   %zu bytes\n", vfs_total_size());
#endif
}

void vfs_list_files(void) {
#if defined(VFS_USE_PKG) && defined(VFS_USE_ZSTD)
    printf("Embedded files (pkg-zstd):\n");
    for (const EmbeddedFilePkgZstd* ef = embedded_files_pkg_zstd; ef->path; ef++) {
        const char* type_str = ef->file_type == 0 ? "pkg" :
                               ef->file_type == 1 ? "txt" : "runtime";
        printf("  %s (%u -> %u bytes, %s)\n",
               ef->path, ef->original_size, ef->compressed_size, type_str);
    }
#elif defined(VFS_USE_ZSTD)
    printf("Embedded files:\n");
    for (const EmbeddedFileZstd* ef = embedded_files_zstd; ef->path; ef++) {
        printf("  %s (%u bytes, compressed: %u)\n",
               ef->path, ef->original_size, ef->compressed_size);
    }
#elif defined(VFS_USE_PKG)
    printf("Embedded packages:\n");
    for (const EmbeddedPackage* ep = embedded_packages; ep->path; ep++) {
        printf("  %s (%zu bytes)\n", ep->path, ep->length);
    }
#else
    printf("Embedded files:\n");
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

#if defined(VFS_USE_PKG) && defined(VFS_USE_ZSTD)
    /* Extract compressed packages/txt/runtime files */
    for (const EmbeddedFilePkgZstd* ef = embedded_files_pkg_zstd; ef->path; ef++) {
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
            cached = decompress_and_cache_pkg(ef);
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
#elif defined(VFS_USE_ZSTD)
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
#elif defined(VFS_USE_PKG)
    /* Extract package files */
    for (const EmbeddedPackage* ep = embedded_packages; ep->path; ep++) {
        char full_path[4096];
        snprintf(full_path, sizeof(full_path), "%s/%s", result, ep->path);

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

        size_t written = fwrite(ep->content, 1, ep->length, f);
        fclose(f);

        if (written != ep->length) {
            fprintf(stderr, "Error: Could not write file: %s\n", full_path);
            free(result);
            return NULL;
        }

        VFS_LOG("Extracted: %s (%zu bytes)\n", ep->path, ep->length);
    }

    /* Extract runtime source files for compilation */
    for (const EmbeddedRuntimeFile* rf = embedded_runtime_files; rf->path; rf++) {
        char full_path[4096];
        snprintf(full_path, sizeof(full_path), "%s/%s", result, rf->path);

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

        size_t written = fwrite(rf->content, 1, rf->length, f);
        fclose(f);

        if (written != rf->length) {
            fprintf(stderr, "Error: Could not write file: %s\n", full_path);
            free(result);
            return NULL;
        }

        VFS_LOG("Extracted: %s (%zu bytes)\n", rf->path, rf->length);
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

    VFS_LOG("Extracted %d files to %s\n", vfs_file_count(), result);
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

/*-----------------------------------------------------------
 * VFS Directory Operations
 *-----------------------------------------------------------*/

/* Magic value to identify VFS directory handles */
#define VFS_DIR_MAGIC 0x5F5D1234

/* VFS directory state for iterating over embedded files */
typedef struct {
    unsigned int magic;
    int index;           /* Current index in embedded files/packages */
    char dir_prefix[256]; /* Directory being listed (e.g., "packages/") */
    size_t prefix_len;
    struct dirent entry; /* Reusable dirent structure */
} VfsDirState;

/* Track active VFS directory handles */
static VfsDirState* g_vfs_dirs[16] = {0};
static int g_vfs_dir_count = 0;

static VfsDirState* vfs_dir_create(const char* prefix) {
    if (g_vfs_dir_count >= 16) {
        return NULL;  /* Too many open directories */
    }
    VfsDirState* state = malloc(sizeof(VfsDirState));
    if (!state) return NULL;

    state->magic = VFS_DIR_MAGIC;
    state->index = 0;
    strncpy(state->dir_prefix, prefix, sizeof(state->dir_prefix) - 1);
    state->dir_prefix[sizeof(state->dir_prefix) - 1] = '\0';
    state->prefix_len = strlen(state->dir_prefix);
    memset(&state->entry, 0, sizeof(state->entry));

    /* Track it */
    for (int i = 0; i < 16; i++) {
        if (!g_vfs_dirs[i]) {
            g_vfs_dirs[i] = state;
            g_vfs_dir_count++;
            break;
        }
    }

    return state;
}

static int vfs_dir_is_vfs(DIR* dirp) {
    VfsDirState* state = (VfsDirState*)dirp;
    if (!state) return 0;
    /* Check if it's in our tracking array */
    for (int i = 0; i < 16; i++) {
        if (g_vfs_dirs[i] == state && state->magic == VFS_DIR_MAGIC) {
            return 1;
        }
    }
    return 0;
}

static void vfs_dir_free(VfsDirState* state) {
    for (int i = 0; i < 16; i++) {
        if (g_vfs_dirs[i] == state) {
            g_vfs_dirs[i] = NULL;
            g_vfs_dir_count--;
            break;
        }
    }
    free(state);
}

DIR* vfs_opendir(const char* path) {
    if (!path) return NULL;

    VFS_LOG("opendir: %s\n", path);

    const size_t root_len = strlen(VFS_VIRTUAL_ROOT);
    if (strncmp(path, VFS_VIRTUAL_ROOT, root_len) == 0) {
        const char* rel_path = path + root_len;
        if (*rel_path == '/') rel_path++;

        /* Add trailing slash if not present */
        char prefix[256];
        size_t len = strlen(rel_path);
        if (len > 0 && rel_path[len-1] != '/') {
            snprintf(prefix, sizeof(prefix), "%s/", rel_path);
        } else {
            strncpy(prefix, rel_path, sizeof(prefix) - 1);
            prefix[sizeof(prefix) - 1] = '\0';
        }

        VFS_LOG("VFS opendir: prefix='%s'\n", prefix);

#if defined(VFS_USE_PKG) && defined(VFS_USE_ZSTD)
        /* For pkg-zstd mode, check if any embedded files match this prefix */
        int has_files = 0;
        for (const EmbeddedFilePkgZstd* ef = embedded_files_pkg_zstd; ef->path; ef++) {
            if (strncmp(ef->path, prefix, strlen(prefix)) == 0) {
                has_files = 1;
                break;
            }
        }
        if (has_files || strlen(prefix) == 0) {
            VfsDirState* state = vfs_dir_create(prefix);
            if (state) {
                VFS_LOG("Created VFS dir state for %s\n", prefix);
                return (DIR*)state;
            }
        }
#elif defined(VFS_USE_PKG)
        /* For package mode, check if this is the packages directory */
        if (strcmp(prefix, "packages/") == 0) {
            VfsDirState* state = vfs_dir_create(prefix);
            if (state) {
                VFS_LOG("Created VFS dir state for packages/\n");
                return (DIR*)state;
            }
        }
#elif defined(VFS_USE_ZSTD)
        /* For ZSTD source mode, check if any embedded files match this prefix */
        int has_files = 0;
        for (const EmbeddedFileZstd* ef = embedded_files_zstd; ef->path; ef++) {
            if (strncmp(ef->path, prefix, strlen(prefix)) == 0) {
                has_files = 1;
                break;
            }
        }
        if (has_files) {
            VfsDirState* state = vfs_dir_create(prefix);
            if (state) {
                VFS_LOG("Created VFS dir state for %s\n", prefix);
                return (DIR*)state;
            }
        }
#else
        /* For default source mode, check if any embedded files match this prefix */
        int has_files = 0;
        for (const EmbeddedFile* ef = embedded_files; ef->path; ef++) {
            if (strncmp(ef->path, prefix, strlen(prefix)) == 0) {
                has_files = 1;
                break;
            }
        }
        if (has_files) {
            VfsDirState* state = vfs_dir_create(prefix);
            if (state) {
                VFS_LOG("Created VFS dir state for %s\n", prefix);
                return (DIR*)state;
            }
        }
#endif
        /* Not a VFS directory */
        VFS_LOG("Not a VFS directory: %s\n", path);
        return NULL;
    }

    /* Fall through to real opendir */
    return opendir(path);
}

struct dirent* vfs_readdir(DIR* dirp) {
    if (!dirp) return NULL;

    if (vfs_dir_is_vfs(dirp)) {
        VfsDirState* state = (VfsDirState*)dirp;

#if defined(VFS_USE_PKG) && defined(VFS_USE_ZSTD)
        /* Iterate over pkg-zstd embedded files */
        while (embedded_files_pkg_zstd[state->index].path) {
            const EmbeddedFilePkgZstd* ef = &embedded_files_pkg_zstd[state->index];
            state->index++;

            if (strncmp(ef->path, state->dir_prefix, state->prefix_len) == 0) {
                const char* filename = ef->path + state->prefix_len;
                if (strchr(filename, '/') == NULL) {
                    strncpy(state->entry.d_name, filename, sizeof(state->entry.d_name) - 1);
                    state->entry.d_name[sizeof(state->entry.d_name) - 1] = '\0';
                    state->entry.d_type = DT_REG;
                    VFS_LOG("VFS readdir: %s\n", state->entry.d_name);
                    return &state->entry;
                }
            }
        }
#elif defined(VFS_USE_PKG)
        /* Iterate over embedded packages */
        while (embedded_packages[state->index].path) {
            const EmbeddedPackage* ep = &embedded_packages[state->index];
            state->index++;

            /* Check if this package is in our directory */
            if (strncmp(ep->path, state->dir_prefix, state->prefix_len) == 0) {
                /* Extract filename from path */
                const char* filename = ep->path + state->prefix_len;
                /* Skip if there's another / (subdirectory) */
                if (strchr(filename, '/') == NULL) {
                    strncpy(state->entry.d_name, filename, sizeof(state->entry.d_name) - 1);
                    state->entry.d_name[sizeof(state->entry.d_name) - 1] = '\0';
                    state->entry.d_type = DT_REG;
                    VFS_LOG("VFS readdir: %s\n", state->entry.d_name);
                    return &state->entry;
                }
            }
        }
#elif defined(VFS_USE_ZSTD)
        /* Iterate over ZSTD-compressed embedded files */
        while (embedded_files_zstd[state->index].path) {
            const EmbeddedFileZstd* ef = &embedded_files_zstd[state->index];
            state->index++;

            if (strncmp(ef->path, state->dir_prefix, state->prefix_len) == 0) {
                const char* filename = ef->path + state->prefix_len;
                if (strchr(filename, '/') == NULL) {
                    strncpy(state->entry.d_name, filename, sizeof(state->entry.d_name) - 1);
                    state->entry.d_name[sizeof(state->entry.d_name) - 1] = '\0';
                    state->entry.d_type = DT_REG;
                    VFS_LOG("VFS readdir: %s\n", state->entry.d_name);
                    return &state->entry;
                }
            }
        }
#else
        /* Iterate over embedded files */
        while (embedded_files[state->index].path) {
            const EmbeddedFile* ef = &embedded_files[state->index];
            state->index++;

            if (strncmp(ef->path, state->dir_prefix, state->prefix_len) == 0) {
                const char* filename = ef->path + state->prefix_len;
                if (strchr(filename, '/') == NULL) {
                    strncpy(state->entry.d_name, filename, sizeof(state->entry.d_name) - 1);
                    state->entry.d_name[sizeof(state->entry.d_name) - 1] = '\0';
                    state->entry.d_type = DT_REG;
                    VFS_LOG("VFS readdir: %s\n", state->entry.d_name);
                    return &state->entry;
                }
            }
        }
#endif
        return NULL;  /* End of directory */
    }

    /* Real directory */
    return readdir(dirp);
}

int vfs_closedir(DIR* dirp) {
    if (!dirp) return -1;

    if (vfs_dir_is_vfs(dirp)) {
        VfsDirState* state = (VfsDirState*)dirp;
        VFS_LOG("VFS closedir\n");
        vfs_dir_free(state);
        return 0;
    }

    return closedir(dirp);
}
