/* vfs.c - Virtual Filesystem for embedded Haskell libraries
 *
 * Pure memory-based approach using fmemopen() to serve embedded files
 * directly from memory without any filesystem operations.
 *
 * For compilation to executable (-o without .c), files can be extracted
 * to a temp directory so that cc can access them.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include <libgen.h>
#include <unistd.h>

/* Include the generated embedded libraries header */
#include "mhs_embedded_libs.h"

/* Debug logging (set to 1 to enable) */
#ifndef VFS_DEBUG
#define VFS_DEBUG 0
#endif

#if VFS_DEBUG
#define VFS_LOG(...) fprintf(stderr, "VFS: " __VA_ARGS__)
#else
#define VFS_LOG(...) ((void)0)
#endif

/*-----------------------------------------------------------
 * Virtual root path
 *-----------------------------------------------------------*/

/* Virtual root that MHSDIR will be set to */
#define VFS_VIRTUAL_ROOT "/mhs-embedded"

static int vfs_initialized = 0;

/*-----------------------------------------------------------
 * File lookup
 *-----------------------------------------------------------*/

/* Find an embedded file by its relative path (e.g., "lib/Prelude.hs") */
static const EmbeddedFile* find_embedded_file(const char* rel_path) {
    for (const EmbeddedFile* ef = embedded_files; ef->path; ef++) {
        if (strcmp(ef->path, rel_path) == 0) {
            return ef;
        }
    }
    return NULL;
}

/*-----------------------------------------------------------
 * VFS API
 *-----------------------------------------------------------*/

/* Initialize VFS - just marks as ready */
int vfs_init(void) {
    if (vfs_initialized) {
        return 0;
    }
    VFS_LOG("Initialized with %d embedded files\n", EMBEDDED_FILE_COUNT);
    vfs_initialized = 1;
    return 0;
}

/* Get the virtual root path (for setting MHSDIR) */
const char* vfs_get_temp_dir(void) {
    return VFS_VIRTUAL_ROOT;
}

/* Open a file - check embedded files first, then fall back to real filesystem */
FILE* vfs_fopen(const char* path, const char* mode) {
    if (!path || !mode) {
        return NULL;
    }

    VFS_LOG("fopen: %s (mode=%s)\n", path, mode);

    /* Check if path starts with our virtual root */
    const size_t root_len = strlen(VFS_VIRTUAL_ROOT);
    if (strncmp(path, VFS_VIRTUAL_ROOT, root_len) == 0) {
        /* Extract relative path (skip root and leading slash) */
        const char* rel_path = path + root_len;
        if (*rel_path == '/') {
            rel_path++;
        }

        VFS_LOG("Looking up embedded: %s\n", rel_path);

        /* Find in embedded files */
        const EmbeddedFile* ef = find_embedded_file(rel_path);
        if (ef) {
            VFS_LOG("Found embedded file: %s (%zu bytes)\n", ef->path, ef->length);
            /* Use fmemopen to create FILE* from memory buffer */
            FILE* f = fmemopen((void*)ef->content, ef->length, "r");
            if (!f) {
                VFS_LOG("fmemopen failed for %s\n", ef->path);
            }
            return f;
        }

        VFS_LOG("Not found in embedded files: %s\n", rel_path);
        /* Not found in embedded files - return NULL (file not found) */
        return NULL;
    }

    /* Not a virtual path - use real filesystem */
    return fopen(path, mode);
}

/*-----------------------------------------------------------
 * VFS statistics (for debugging)
 *-----------------------------------------------------------*/

/* Get total number of embedded files */
int vfs_file_count(void) {
    return EMBEDDED_FILE_COUNT;
}

/* Get total size of embedded content */
size_t vfs_total_size(void) {
    size_t total = 0;
    for (const EmbeddedFile* ef = embedded_files; ef->path; ef++) {
        total += ef->length;
    }
    return total;
}

/* Print VFS statistics */
void vfs_print_stats(void) {
    printf("VFS: %d embedded files, %zu bytes total\n",
           vfs_file_count(), vfs_total_size());
}

/* List all embedded files (for debugging) */
void vfs_list_files(void) {
    printf("Embedded files:\n");
    for (const EmbeddedFile* ef = embedded_files; ef->path; ef++) {
        printf("  %s (%zu bytes)\n", ef->path, ef->length);
    }
}

/*-----------------------------------------------------------
 * File extraction (for compilation to executable)
 *-----------------------------------------------------------*/

/* Recursively create directories for a path */
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

/* Extract all embedded files to a temp directory.
 * Returns the path to the temp directory, or NULL on failure.
 * The caller is responsible for cleaning up the directory.
 */
char* vfs_extract_to_temp(void) {
    /* Create temp directory */
    char template[] = "/tmp/mhs-XXXXXX";
    char* temp_dir = mkdtemp(template);
    if (!temp_dir) {
        fprintf(stderr, "Error: Could not create temp directory: %s\n", strerror(errno));
        return NULL;
    }

    /* Need to return a copy since template is on stack */
    char* result = strdup(temp_dir);
    if (!result) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        return NULL;
    }

    VFS_LOG("Extracting to: %s\n", result);

    /* Extract each embedded file */
    for (const EmbeddedFile* ef = embedded_files; ef->path; ef++) {
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

        /* Write file */
        FILE* f = fopen(full_path, "wb");
        if (!f) {
            fprintf(stderr, "Error: Could not create file: %s: %s\n", full_path, strerror(errno));
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

    VFS_LOG("Extracted %d files to %s\n", EMBEDDED_FILE_COUNT, result);
    return result;
}

/* Recursively remove a directory and its contents */
static int rmdir_recursive(const char* path) {
    char cmd[4096];
    snprintf(cmd, sizeof(cmd), "rm -rf '%s'", path);
    return system(cmd);
}

/* Clean up extracted temp directory */
void vfs_cleanup_temp(char* temp_dir) {
    if (temp_dir) {
        VFS_LOG("Cleaning up: %s\n", temp_dir);
        rmdir_recursive(temp_dir);
        free(temp_dir);
    }
}
