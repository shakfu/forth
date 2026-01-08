/*
 * mhs-embed.c - Embed files into C headers for MicroHs standalone binaries
 *
 * Unified tool that supports:
 * - Source mode: Embed .hs/.c/.h files (default: zstd compressed)
 * - Package mode: Embed .pkg files with module mappings (--pkg-mode)
 * - Uncompressed mode: Plain C string embedding (--no-compress)
 *
 * Usage: mhs-embed <output.h> [libdir ...] [options]
 *
 * Options:
 *   --no-compress       Disable compression (plain C strings)
 *   --runtime <dir>     Embed runtime C/H files from <dir>
 *   --lib <file>        Embed a library file (.a) in lib/
 *   --header <file>     Embed a header file in src/runtime/
 *   --dict-size <bytes> Dictionary size (default: 112KB)
 *   --level <1-22>      Compression level (default: 19)
 *
 * Package mode options (--pkg-mode):
 *   --pkg-mode              Output in pkg format with file types
 *   --pkg <vfs>=<file>      Embed a .pkg file
 *   --txt-dir <dir>         Collect .txt module mapping files from <dir>
 *   --music-modules <p:m1,m2> Generate synthetic .txt for music modules
 *
 * Compile:
 *   cc -O2 -o mhs-embed mhs-embed.c \
 *      ../thirdparty/zstd-1.5.7/zstd.c -I../thirdparty/zstd-1.5.7 -lpthread
 *
 * For --no-compress mode, zstd is not required:
 *   cc -O2 -DMHS_EMBED_NO_ZSTD -o mhs-embed-nocompress mhs-embed.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>
#include <errno.h>

#ifndef MHS_EMBED_NO_ZSTD
#define ZSTD_STATIC_LINKING_ONLY
#include "zstd.h"
#include "zdict.h"
#endif

#define MAX_FILES 1024
#define MAX_PATH 4096
#define CHUNK_SIZE 4000  /* Max string literal chunk size for C89 compatibility */
#define DEFAULT_DICT_SIZE (112 * 1024)
#define DEFAULT_COMP_LEVEL 19

/* File types for pkg-mode */
#define FILE_TYPE_SOURCE  0   /* .hs, .c, .h files (default) */
#define FILE_TYPE_PKG     1   /* .pkg package files */
#define FILE_TYPE_TXT     2   /* .txt module mapping files */
#define FILE_TYPE_RUNTIME 3   /* runtime files (lib/, include/) */

typedef struct {
    char* vfs_path;           /* Path in VFS */
    char* full_path;          /* Full filesystem path */
    unsigned char* content;   /* Original content */
    size_t length;            /* Original length */
    unsigned char* compressed;/* Compressed content (NULL if --no-compress) */
    size_t compressed_len;    /* Compressed length */
    int use_dict;             /* 1 if compressed with dictionary */
    int file_type;            /* FILE_TYPE_* for pkg-mode */
} FileEntry;

static FileEntry g_files[MAX_FILES];
static int g_file_count = 0;

static size_t g_dict_size = DEFAULT_DICT_SIZE;
static int g_comp_level = DEFAULT_COMP_LEVEL;
static int g_pkg_mode = 0;      /* Output in pkg format */
static int g_no_compress = 0;   /* Disable compression */

/* Forward declarations */
static int collect_files_recursive(const char* dir_path, const char* base_name,
                                   const char* pattern, int include_subdirs);
static void free_files(void);

/*-----------------------------------------------------------
 * File utilities
 *-----------------------------------------------------------*/

static int matches_pattern(const char* filename, const char* pattern) {
    if (pattern[0] != '*') return 0;
    const char* suffix = pattern + 1;
    size_t suffix_len = strlen(suffix);
    size_t name_len = strlen(filename);
    if (name_len < suffix_len) return 0;
    return strcmp(filename + name_len - suffix_len, suffix) == 0;
}

static int is_directory(const char* path) {
    struct stat st;
    if (stat(path, &st) != 0) return 0;
    return S_ISDIR(st.st_mode);
}

static int is_text_file(const char* path) {
    return matches_pattern(path, "*.hs") ||
           matches_pattern(path, "*.hs-boot") ||
           matches_pattern(path, "*.c") ||
           matches_pattern(path, "*.h") ||
           matches_pattern(path, "*.txt");
}

static unsigned char* read_file(const char* path, size_t* out_length) {
    FILE* f = fopen(path, "rb");
    if (!f) return NULL;

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    if (size < 0) {
        fclose(f);
        return NULL;
    }
    fseek(f, 0, SEEK_SET);

    unsigned char* content = malloc(size + 1);
    if (!content) {
        fclose(f);
        return NULL;
    }

    size_t nread = fread(content, 1, size, f);
    fclose(f);

    if ((long)nread != size) {
        free(content);
        return NULL;
    }

    content[size] = '\0';
    *out_length = size;
    return content;
}

static int add_file_with_type(const char* vfs_path, const char* full_path, int file_type) {
    if (g_file_count >= MAX_FILES) {
        fprintf(stderr, "Error: Too many files (max %d)\n", MAX_FILES);
        return -1;
    }

    /* Check for duplicates */
    for (int i = 0; i < g_file_count; i++) {
        if (strcmp(g_files[i].vfs_path, vfs_path) == 0) {
            fprintf(stderr, "Note: Skipping duplicate %s\n", vfs_path);
            return 0;
        }
    }

    size_t length;
    unsigned char* content = read_file(full_path, &length);
    if (!content) {
        fprintf(stderr, "Warning: Could not read %s: %s\n", full_path, strerror(errno));
        return 0;
    }

    FileEntry* entry = &g_files[g_file_count++];
    entry->vfs_path = strdup(vfs_path);
    entry->full_path = strdup(full_path);
    entry->content = content;
    entry->length = length;
    entry->compressed = NULL;
    entry->compressed_len = 0;
    entry->use_dict = is_text_file(vfs_path);
    entry->file_type = file_type;

    return 0;
}

static int add_file(const char* vfs_path, const char* full_path) {
    return add_file_with_type(vfs_path, full_path, FILE_TYPE_SOURCE);
}

/* Add synthetic content (for generated .txt files) */
static int add_synthetic_file(const char* vfs_path, const char* content_str, int file_type) {
    if (g_file_count >= MAX_FILES) {
        fprintf(stderr, "Error: Too many files (max %d)\n", MAX_FILES);
        return -1;
    }

    size_t length = strlen(content_str);
    unsigned char* content = malloc(length + 1);
    if (!content) return -1;
    memcpy(content, content_str, length + 1);

    FileEntry* entry = &g_files[g_file_count++];
    entry->vfs_path = strdup(vfs_path);
    entry->full_path = strdup("(synthetic)");
    entry->content = content;
    entry->length = length;
    entry->compressed = NULL;
    entry->compressed_len = 0;
    entry->use_dict = 1;
    entry->file_type = file_type;

    return 0;
}

/*-----------------------------------------------------------
 * Directory traversal
 *-----------------------------------------------------------*/

static int collect_files_recursive_typed(const char* dir_path, const char* base_name,
                                         const char* pattern, int include_subdirs,
                                         int file_type) {
    DIR* dir = opendir(dir_path);
    if (!dir) {
        fprintf(stderr, "Warning: Could not open directory %s: %s\n",
                dir_path, strerror(errno));
        return 0;
    }

    struct dirent* entry;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;

        char full_path[MAX_PATH];
        snprintf(full_path, sizeof(full_path), "%s/%s", dir_path, entry->d_name);

        if (is_directory(full_path)) {
            if (include_subdirs) {
                char new_base[MAX_PATH];
                if (base_name[0])
                    snprintf(new_base, sizeof(new_base), "%s/%s", base_name, entry->d_name);
                else
                    snprintf(new_base, sizeof(new_base), "%s", entry->d_name);
                collect_files_recursive_typed(full_path, new_base, pattern, 1, file_type);
            }
        } else if (matches_pattern(entry->d_name, pattern)) {
            char vfs_path[MAX_PATH];
            if (base_name[0])
                snprintf(vfs_path, sizeof(vfs_path), "%s/%s", base_name, entry->d_name);
            else
                snprintf(vfs_path, sizeof(vfs_path), "%s", entry->d_name);
            add_file_with_type(vfs_path, full_path, file_type);
        }
    }

    closedir(dir);
    return 0;
}

static int collect_files_recursive(const char* dir_path, const char* base_name,
                                   const char* pattern, int include_subdirs) {
    return collect_files_recursive_typed(dir_path, base_name, pattern, include_subdirs, FILE_TYPE_SOURCE);
}

static int collect_hs_files(const char* lib_dir) {
    const char* base = strrchr(lib_dir, '/');
    base = base ? base + 1 : lib_dir;

    char dir_copy[MAX_PATH];
    strncpy(dir_copy, lib_dir, sizeof(dir_copy) - 1);
    dir_copy[sizeof(dir_copy) - 1] = '\0';

    size_t len = strlen(dir_copy);
    if (len > 0 && dir_copy[len - 1] == '/')
        dir_copy[len - 1] = '\0';

    collect_files_recursive(dir_copy, base, "*.hs", 1);
    collect_files_recursive(dir_copy, base, "*.hs-boot", 1);

    return 0;
}

static int collect_runtime_files(const char* runtime_dir) {
    char dir_copy[MAX_PATH];
    strncpy(dir_copy, runtime_dir, sizeof(dir_copy) - 1);
    dir_copy[sizeof(dir_copy) - 1] = '\0';

    size_t len = strlen(dir_copy);
    if (len > 0 && dir_copy[len - 1] == '/')
        dir_copy[len - 1] = '\0';

    const char* src_pos = strstr(dir_copy, "src/runtime");
    const char* base = src_pos ? src_pos : "src/runtime";

    collect_files_recursive_typed(dir_copy, base, "*.c", 1, FILE_TYPE_RUNTIME);
    collect_files_recursive_typed(dir_copy, base, "*.h", 1, FILE_TYPE_RUNTIME);

    return 0;
}

static int embed_single_file(const char* file_path, const char* vfs_prefix) {
    const char* basename = strrchr(file_path, '/');
    basename = basename ? basename + 1 : file_path;

    char vfs_path[MAX_PATH];
    snprintf(vfs_path, sizeof(vfs_path), "%s/%s", vfs_prefix, basename);

    struct stat st;
    if (stat(file_path, &st) == 0) {
        printf("  %s (%ld bytes)\n", basename, (long)st.st_size);
    }

    return add_file_with_type(vfs_path, file_path, FILE_TYPE_RUNTIME);
}

/* Collect .txt module mapping files from a directory (for pkg-mode) */
static int collect_txt_files(const char* base_dir) {
    DIR* dir = opendir(base_dir);
    if (!dir) {
        fprintf(stderr, "Warning: Could not open txt directory %s: %s\n",
                base_dir, strerror(errno));
        return 0;
    }

    struct dirent* entry;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;

        char full_path[MAX_PATH];
        snprintf(full_path, sizeof(full_path), "%s/%s", base_dir, entry->d_name);

        if (is_directory(full_path)) {
            if (strcmp(entry->d_name, "packages") == 0)
                continue;
            collect_txt_files(full_path);
        } else if (matches_pattern(entry->d_name, "*.txt")) {
            const char* rel_start = full_path + strlen(base_dir);
            if (*rel_start == '/') rel_start++;
            add_file_with_type(rel_start, full_path, FILE_TYPE_TXT);
        }
    }

    closedir(dir);
    return 0;
}

/* Add a .pkg file with explicit vfs_path (for pkg-mode) */
static int add_pkg_file(const char* spec) {
    char* eq = strchr(spec, '=');
    if (!eq) {
        fprintf(stderr, "Error: --pkg requires format vfs_path=file_path\n");
        return -1;
    }

    char vfs_path[MAX_PATH];
    size_t vfs_len = eq - spec;
    if (vfs_len >= MAX_PATH) vfs_len = MAX_PATH - 1;
    strncpy(vfs_path, spec, vfs_len);
    vfs_path[vfs_len] = '\0';

    const char* file_path = eq + 1;

    struct stat st;
    if (stat(file_path, &st) == 0) {
        printf("  %s (%ld bytes)\n", vfs_path, (long)st.st_size);
    }

    return add_file_with_type(vfs_path, file_path, FILE_TYPE_PKG);
}

/* Parse music-modules spec and add synthetic .txt files */
static int add_music_modules(const char* spec) {
    char* colon = strchr(spec, ':');
    if (!colon) {
        fprintf(stderr, "Error: --music-modules requires format pkg_name:mod1,mod2,...\n");
        return -1;
    }

    char pkg_name[256];
    size_t pkg_len = colon - spec;
    if (pkg_len >= sizeof(pkg_name)) pkg_len = sizeof(pkg_name) - 1;
    strncpy(pkg_name, spec, pkg_len);
    pkg_name[pkg_len] = '\0';

    const char* modules = colon + 1;
    char mod_list[1024];
    strncpy(mod_list, modules, sizeof(mod_list) - 1);
    mod_list[sizeof(mod_list) - 1] = '\0';

    char* mod = strtok(mod_list, ",");
    while (mod) {
        while (*mod == ' ') mod++;
        char* end = mod + strlen(mod) - 1;
        while (end > mod && *end == ' ') *end-- = '\0';

        if (*mod) {
            char vfs_path[MAX_PATH];
            snprintf(vfs_path, sizeof(vfs_path), "%s.txt", mod);
            add_synthetic_file(vfs_path, pkg_name, FILE_TYPE_TXT);
            printf("  %s -> %s\n", vfs_path, pkg_name);
        }

        mod = strtok(NULL, ",");
    }

    return 0;
}

/*-----------------------------------------------------------
 * Zstd compression (only when not --no-compress)
 *-----------------------------------------------------------*/

#ifndef MHS_EMBED_NO_ZSTD
static unsigned char* train_dictionary(size_t* out_dict_len) {
    int text_count = 0;
    size_t total_size = 0;

    for (int i = 0; i < g_file_count; i++) {
        if (g_files[i].use_dict) {
            text_count++;
            total_size += g_files[i].length;
        }
    }

    if (text_count == 0) {
        printf("No text files for dictionary training\n");
        *out_dict_len = 0;
        return NULL;
    }

    printf("Training dictionary from %d samples (%zu bytes)...\n", text_count, total_size);

    unsigned char* samples = malloc(total_size);
    size_t* sample_sizes = malloc(text_count * sizeof(size_t));
    if (!samples || !sample_sizes) {
        free(samples);
        free(sample_sizes);
        fprintf(stderr, "Error: Memory allocation failed\n");
        return NULL;
    }

    size_t offset = 0;
    int idx = 0;
    for (int i = 0; i < g_file_count; i++) {
        if (g_files[i].use_dict) {
            memcpy(samples + offset, g_files[i].content, g_files[i].length);
            sample_sizes[idx++] = g_files[i].length;
            offset += g_files[i].length;
        }
    }

    unsigned char* dict = malloc(g_dict_size);
    if (!dict) {
        free(samples);
        free(sample_sizes);
        fprintf(stderr, "Error: Memory allocation failed\n");
        return NULL;
    }

    size_t dict_len = ZDICT_trainFromBuffer(dict, g_dict_size,
                                            samples, sample_sizes, text_count);

    free(samples);
    free(sample_sizes);

    if (ZDICT_isError(dict_len)) {
        fprintf(stderr, "Warning: Dictionary training failed: %s\n",
                ZDICT_getErrorName(dict_len));
        free(dict);
        *out_dict_len = 0;
        return NULL;
    }

    printf("Dictionary trained: %zu bytes\n", dict_len);
    *out_dict_len = dict_len;

    unsigned char* shrunk = realloc(dict, dict_len);
    return shrunk ? shrunk : dict;
}

static int compress_files(const unsigned char* dict, size_t dict_len) {
    ZSTD_CCtx* cctx = ZSTD_createCCtx();
    if (!cctx) {
        fprintf(stderr, "Error: Failed to create compression context\n");
        return -1;
    }

    ZSTD_CDict* cdict = NULL;
    if (dict && dict_len > 0) {
        cdict = ZSTD_createCDict(dict, dict_len, g_comp_level);
        if (!cdict) {
            fprintf(stderr, "Error: Failed to create compression dictionary\n");
            ZSTD_freeCCtx(cctx);
            return -1;
        }
    }

    ZSTD_CCtx_setParameter(cctx, ZSTD_c_compressionLevel, g_comp_level);

    size_t total_original = 0;
    size_t total_compressed = 0;

    for (int i = 0; i < g_file_count; i++) {
        FileEntry* entry = &g_files[i];

        size_t bound = ZSTD_compressBound(entry->length);
        entry->compressed = malloc(bound);
        if (!entry->compressed) {
            fprintf(stderr, "Error: Memory allocation failed for %s\n", entry->vfs_path);
            continue;
        }

        size_t result;
        if (entry->use_dict && cdict) {
            result = ZSTD_compress_usingCDict(cctx,
                entry->compressed, bound,
                entry->content, entry->length,
                cdict);
        } else {
            result = ZSTD_compressCCtx(cctx,
                entry->compressed, bound,
                entry->content, entry->length,
                g_comp_level);
        }

        if (ZSTD_isError(result)) {
            fprintf(stderr, "Error: Compression failed for %s: %s\n",
                    entry->vfs_path, ZSTD_getErrorName(result));
            free(entry->compressed);
            entry->compressed = NULL;
            entry->compressed_len = 0;
            continue;
        }

        entry->compressed_len = result;
        total_original += entry->length;
        total_compressed += result;

        unsigned char* shrunk = realloc(entry->compressed, result);
        if (shrunk) entry->compressed = shrunk;
    }

    if (cdict) ZSTD_freeCDict(cdict);
    ZSTD_freeCCtx(cctx);

    double ratio = total_compressed > 0 ?
        (double)total_original / total_compressed : 0;
    printf("\nCompression: %zu -> %zu bytes (%.2fx)\n",
           total_original, total_compressed, ratio);

    return 0;
}
#endif /* MHS_EMBED_NO_ZSTD */

/*-----------------------------------------------------------
 * Output generation - Uncompressed mode
 *-----------------------------------------------------------*/

/* Escape byte for C string literal */
static int escape_byte(unsigned char byte, char* buf) {
    switch (byte) {
        case '\\': return sprintf(buf, "\\\\");
        case '"':  return sprintf(buf, "\\\"");
        case '\n': return sprintf(buf, "\\n");
        case '\r': return sprintf(buf, "\\r");
        case '\t': return sprintf(buf, "\\t");
        default:
            if (byte < 32 || byte > 126) {
                return sprintf(buf, "\\%03o", byte);
            } else {
                buf[0] = byte;
                buf[1] = '\0';
                return 1;
            }
    }
}

/* Write file content as C string literal(s) with chunking */
static void write_string_literal(FILE* out, const unsigned char* content, size_t length) {
    char escape_buf[8];
    int chunk_len = 0;
    int first_chunk = 1;

    fprintf(out, "      ");

    /* Handle empty files */
    if (length == 0) {
        fprintf(out, "\"\"");
        return;
    }

    for (size_t i = 0; i < length; i++) {
        int esc_len = escape_byte(content[i], escape_buf);

        if (chunk_len + esc_len > CHUNK_SIZE) {
            if (first_chunk) {
                fprintf(out, "\"\n      ");
                first_chunk = 0;
            } else {
                fprintf(out, "\"\n      ");
            }
            chunk_len = 0;
        }

        if (chunk_len == 0) {
            fprintf(out, "\"");
        }

        fprintf(out, "%s", escape_buf);
        chunk_len += esc_len;
    }

    if (chunk_len > 0 || length == 0) {
        fprintf(out, "\"");
    }
}

static int compare_files(const void* a, const void* b) {
    const FileEntry* fa = (const FileEntry*)a;
    const FileEntry* fb = (const FileEntry*)b;
    return strcmp(fa->vfs_path, fb->vfs_path);
}

/* Write uncompressed header (--no-compress mode) */
static int write_header_uncompressed(const char* output_path) {
    FILE* out = fopen(output_path, "w");
    if (!out) {
        fprintf(stderr, "Error: Could not create %s: %s\n", output_path, strerror(errno));
        return -1;
    }

    qsort(g_files, g_file_count, sizeof(FileEntry), compare_files);

    size_t total_size = 0;
    int hs_count = 0, runtime_count = 0, lib_count = 0;
    for (int i = 0; i < g_file_count; i++) {
        total_size += g_files[i].length;
        if (matches_pattern(g_files[i].vfs_path, "*.hs") ||
            matches_pattern(g_files[i].vfs_path, "*.hs-boot"))
            hs_count++;
        else if (matches_pattern(g_files[i].vfs_path, "*.a"))
            lib_count++;
        else
            runtime_count++;
    }

    fprintf(out, "/* Auto-generated by mhs-embed --no-compress - DO NOT EDIT */\n");
    fprintf(out, "/* %d embedded files (%d .hs, %d runtime, %d libs, %zu bytes total) */\n\n",
            g_file_count, hs_count, runtime_count, lib_count, total_size);
    fprintf(out, "#ifndef MHS_EMBEDDED_LIBS_H\n");
    fprintf(out, "#define MHS_EMBEDDED_LIBS_H\n\n");
    fprintf(out, "#include <stddef.h>\n\n");

    fprintf(out, "typedef struct {\n");
    fprintf(out, "    const char* path;      /* VFS path, e.g., \"lib/Prelude.hs\" */\n");
    fprintf(out, "    const char* content;   /* File contents */\n");
    fprintf(out, "    size_t length;         /* Content length in bytes */\n");
    fprintf(out, "} EmbeddedFile;\n\n");

    fprintf(out, "static const EmbeddedFile embedded_files[] = {\n");

    for (int i = 0; i < g_file_count; i++) {
        FileEntry* entry = &g_files[i];
        fprintf(out, "    /* %s */\n", entry->full_path);
        fprintf(out, "    { \"%s\",\n", entry->vfs_path);
        write_string_literal(out, entry->content, entry->length);
        fprintf(out, ",\n");
        fprintf(out, "      %zu },\n\n", entry->length);
    }

    fprintf(out, "    /* Sentinel */\n");
    fprintf(out, "    { NULL, NULL, 0 }\n");
    fprintf(out, "};\n\n");

    fprintf(out, "#define EMBEDDED_FILE_COUNT %d\n\n", g_file_count);
    fprintf(out, "#endif /* MHS_EMBEDDED_LIBS_H */\n");

    fclose(out);

    printf("Generated: %s (%d files, %zu bytes)\n", output_path, g_file_count, total_size);
    return 0;
}

/* Write uncompressed pkg-mode header */
static int write_header_pkg_uncompressed(const char* output_path) {
    FILE* out = fopen(output_path, "w");
    if (!out) {
        fprintf(stderr, "Error: Could not create %s: %s\n", output_path, strerror(errno));
        return -1;
    }

    qsort(g_files, g_file_count, sizeof(FileEntry), compare_files);

    size_t total_size = 0;
    int pkg_count = 0, txt_count = 0, runtime_count = 0;
    for (int i = 0; i < g_file_count; i++) {
        total_size += g_files[i].length;
        switch (g_files[i].file_type) {
            case FILE_TYPE_PKG: pkg_count++; break;
            case FILE_TYPE_TXT: txt_count++; break;
            default: runtime_count++; break;
        }
    }

    fprintf(out, "/* Auto-generated by mhs-embed --pkg-mode --no-compress - DO NOT EDIT */\n");
    fprintf(out, "/* %d packages, %d txt files, %d runtime files */\n\n",
            pkg_count, txt_count, runtime_count);
    fprintf(out, "#ifndef MHS_EMBEDDED_PKGS_H\n");
    fprintf(out, "#define MHS_EMBEDDED_PKGS_H\n\n");
    fprintf(out, "#include <stddef.h>\n\n");

    /* Write byte arrays for each file */
    fprintf(out, "/* File data */\n");
    int data_idx = 0;
    for (int i = 0; i < g_file_count; i++) {
        FileEntry* entry = &g_files[i];
        fprintf(out, "/* %s */\n", entry->vfs_path);
        fprintf(out, "static const unsigned char pkg_data_%d[] = {\n", data_idx++);
        if (entry->length == 0) {
            fprintf(out, "    0  /* empty file placeholder */\n");
        } else {
            for (size_t j = 0; j < entry->length; j++) {
                if (j % 16 == 0) fprintf(out, "    ");
                fprintf(out, "0x%02x", entry->content[j]);
                if (j + 1 < entry->length) fprintf(out, ",");
                if (j % 16 == 15 || j + 1 == entry->length) fprintf(out, "\n");
            }
        }
        fprintf(out, "};\n\n");
    }

    /* Struct types - vfs.c expects separate arrays for pkg, txt, and runtime */
    fprintf(out, "typedef struct {\n");
    fprintf(out, "    const char* path;              /* VFS path */\n");
    fprintf(out, "    const unsigned char* content;  /* Binary data */\n");
    fprintf(out, "    size_t length;                 /* Content length */\n");
    fprintf(out, "} EmbeddedPackage;\n\n");

    fprintf(out, "typedef struct {\n");
    fprintf(out, "    const char* path;              /* Module.txt */\n");
    fprintf(out, "    const char* content;           /* Package name */\n");
    fprintf(out, "    size_t length;                 /* Content length */\n");
    fprintf(out, "} EmbeddedTxtFile;\n\n");

    fprintf(out, "typedef struct {\n");
    fprintf(out, "    const char* path;              /* Runtime source path */\n");
    fprintf(out, "    const unsigned char* content;  /* File content */\n");
    fprintf(out, "    size_t length;                 /* Content length */\n");
    fprintf(out, "} EmbeddedRuntimeFile;\n\n");

    /* Package files array */
    fprintf(out, "static const EmbeddedPackage embedded_packages[] = {\n");
    data_idx = 0;
    for (int i = 0; i < g_file_count; i++) {
        FileEntry* entry = &g_files[i];
        if (entry->file_type == FILE_TYPE_PKG) {
            fprintf(out, "    { \"%s\", pkg_data_%d, %zu },\n",
                    entry->vfs_path, data_idx, entry->length);
        }
        data_idx++;
    }
    fprintf(out, "    { NULL, NULL, 0 }\n");
    fprintf(out, "};\n\n");

    /* Txt files array (cast to char* since they're text) */
    fprintf(out, "static const EmbeddedTxtFile embedded_txt_files[] = {\n");
    data_idx = 0;
    for (int i = 0; i < g_file_count; i++) {
        FileEntry* entry = &g_files[i];
        if (entry->file_type == FILE_TYPE_TXT) {
            fprintf(out, "    { \"%s\", (const char*)pkg_data_%d, %zu },\n",
                    entry->vfs_path, data_idx, entry->length);
        }
        data_idx++;
    }
    fprintf(out, "    { NULL, NULL, 0 }\n");
    fprintf(out, "};\n\n");

    /* Runtime files array */
    fprintf(out, "static const EmbeddedRuntimeFile embedded_runtime_files[] = {\n");
    data_idx = 0;
    for (int i = 0; i < g_file_count; i++) {
        FileEntry* entry = &g_files[i];
        if (entry->file_type == FILE_TYPE_RUNTIME) {
            fprintf(out, "    { \"%s\", pkg_data_%d, %zu },\n",
                    entry->vfs_path, data_idx, entry->length);
        }
        data_idx++;
    }
    fprintf(out, "    { NULL, NULL, 0 }\n");
    fprintf(out, "};\n\n");

    fprintf(out, "#define EMBEDDED_PACKAGE_COUNT %d\n", pkg_count);
    fprintf(out, "#define EMBEDDED_TXT_COUNT %d\n", txt_count);
    fprintf(out, "#define EMBEDDED_RUNTIME_COUNT %d\n\n", runtime_count);
    fprintf(out, "#endif /* MHS_EMBEDDED_PKGS_H */\n");

    fclose(out);

    printf("Generated: %s (pkg-mode, uncompressed)\n", output_path);
    printf("  Packages: %d, Txt: %d, Runtime: %d\n", pkg_count, txt_count, runtime_count);
    printf("  Total: %zu bytes\n", total_size);
    return 0;
}

/*-----------------------------------------------------------
 * Output generation - Compressed mode
 *-----------------------------------------------------------*/

#ifndef MHS_EMBED_NO_ZSTD
static void write_byte_array(FILE* out, const char* name,
                             const unsigned char* data, size_t len) {
    fprintf(out, "static const unsigned char %s[] = {\n", name);

    for (size_t i = 0; i < len; i++) {
        if (i % 16 == 0) fprintf(out, "    ");
        fprintf(out, "0x%02x", data[i]);
        if (i + 1 < len) fprintf(out, ",");
        if (i % 16 == 15 || i + 1 == len) fprintf(out, "\n");
    }

    fprintf(out, "};\n\n");
}

static int write_header_compressed(const char* output_path,
                                   const unsigned char* dict, size_t dict_len) {
    FILE* out = fopen(output_path, "w");
    if (!out) {
        fprintf(stderr, "Error: Could not create %s: %s\n", output_path, strerror(errno));
        return -1;
    }

    qsort(g_files, g_file_count, sizeof(FileEntry), compare_files);

    size_t total_original = 0;
    size_t total_compressed = 0;
    int valid_count = 0;

    for (int i = 0; i < g_file_count; i++) {
        if (g_files[i].compressed) {
            total_original += g_files[i].length;
            total_compressed += g_files[i].compressed_len;
            valid_count++;
        }
    }

    size_t total_embedded = total_compressed + dict_len;
    double ratio = total_embedded > 0 ?
        (double)total_original / total_embedded : 0;

    fprintf(out, "/* Auto-generated by mhs-embed - DO NOT EDIT */\n");
    fprintf(out, "/* %d files, zstd compressed with dictionary */\n", valid_count);
    fprintf(out, "/* Original: %zu bytes -> Embedded: %zu bytes (%.2fx) */\n\n",
            total_original, total_embedded, ratio);

    fprintf(out, "#ifndef MHS_EMBEDDED_ZSTD_H\n");
    fprintf(out, "#define MHS_EMBEDDED_ZSTD_H\n\n");
    fprintf(out, "#include <stddef.h>\n");
    fprintf(out, "#include <stdint.h>\n\n");

    if (dict && dict_len > 0) {
        fprintf(out, "/* Zstd dictionary for text file decompression */\n");
        write_byte_array(out, "embedded_zstd_dict", dict, dict_len);
        fprintf(out, "#define EMBEDDED_DICT_SIZE %zu\n\n", dict_len);
    } else {
        fprintf(out, "static const unsigned char embedded_zstd_dict[] = {0};\n");
        fprintf(out, "#define EMBEDDED_DICT_SIZE 0\n\n");
    }

    fprintf(out, "typedef struct {\n");
    fprintf(out, "    const char* path;              /* VFS path */\n");
    fprintf(out, "    const unsigned char* data;     /* Compressed data */\n");
    fprintf(out, "    uint32_t compressed_size;      /* Compressed size */\n");
    fprintf(out, "    uint32_t original_size;        /* Original size */\n");
    fprintf(out, "    uint8_t use_dict;              /* 1 if compressed with dictionary */\n");
    fprintf(out, "} EmbeddedFileZstd;\n\n");

    fprintf(out, "/* Compressed file data */\n");
    int array_idx = 0;
    for (int i = 0; i < g_file_count; i++) {
        if (!g_files[i].compressed) continue;

        char var_name[64];
        snprintf(var_name, sizeof(var_name), "file_data_%d", array_idx);

        fprintf(out, "/* %s (%zu -> %zu) */\n",
                g_files[i].vfs_path, g_files[i].length, g_files[i].compressed_len);
        write_byte_array(out, var_name, g_files[i].compressed, g_files[i].compressed_len);

        array_idx++;
    }

    fprintf(out, "static const EmbeddedFileZstd embedded_files_zstd[] = {\n");
    array_idx = 0;
    for (int i = 0; i < g_file_count; i++) {
        if (!g_files[i].compressed) continue;

        fprintf(out, "    { \"%s\", file_data_%d, %zu, %zu, %d },\n",
                g_files[i].vfs_path, array_idx,
                g_files[i].compressed_len, g_files[i].length,
                g_files[i].use_dict ? 1 : 0);
        array_idx++;
    }
    fprintf(out, "    { NULL, NULL, 0, 0, 0 }  /* Sentinel */\n");
    fprintf(out, "};\n\n");

    fprintf(out, "#define EMBEDDED_FILE_COUNT %d\n", valid_count);
    fprintf(out, "#define EMBEDDED_TOTAL_COMPRESSED %zuU\n", total_compressed);
    fprintf(out, "#define EMBEDDED_TOTAL_ORIGINAL %zuU\n\n", total_original);

    fprintf(out, "#endif /* MHS_EMBEDDED_ZSTD_H */\n");

    fclose(out);

    printf("\nGenerated: %s\n", output_path);
    printf("  Files: %d\n", valid_count);
    printf("  Original: %zu bytes\n", total_original);
    printf("  Compressed: %zu bytes\n", total_compressed);
    printf("  Dictionary: %zu bytes\n", dict_len);
    printf("  Total embedded: %zu bytes (%.1f%% of original)\n",
           total_embedded, 100.0 * total_embedded / total_original);

    return 0;
}

static int write_header_pkg_compressed(const char* output_path,
                                       const unsigned char* dict, size_t dict_len) {
    FILE* out = fopen(output_path, "w");
    if (!out) {
        fprintf(stderr, "Error: Could not create %s: %s\n", output_path, strerror(errno));
        return -1;
    }

    qsort(g_files, g_file_count, sizeof(FileEntry), compare_files);

    size_t total_original = 0;
    size_t total_compressed = 0;
    int valid_count = 0;
    int pkg_count = 0, txt_count = 0, runtime_count = 0;

    for (int i = 0; i < g_file_count; i++) {
        if (g_files[i].compressed) {
            total_original += g_files[i].length;
            total_compressed += g_files[i].compressed_len;
            valid_count++;
            switch (g_files[i].file_type) {
                case FILE_TYPE_PKG: pkg_count++; break;
                case FILE_TYPE_TXT: txt_count++; break;
                case FILE_TYPE_RUNTIME: runtime_count++; break;
            }
        }
    }

    size_t total_embedded = total_compressed + dict_len;
    double ratio = total_embedded > 0 ?
        (double)total_original / total_embedded : 0;

    fprintf(out, "/* Auto-generated by mhs-embed --pkg-mode - DO NOT EDIT */\n");
    fprintf(out, "/* %d packages, %d txt files, %d runtime files */\n",
            pkg_count, txt_count, runtime_count);
    fprintf(out, "/* Original: %zu bytes -> Embedded: %zu bytes (%.2fx) */\n\n",
            total_original, total_embedded, ratio);

    fprintf(out, "#ifndef MHS_EMBEDDED_PKGS_ZSTD_H\n");
    fprintf(out, "#define MHS_EMBEDDED_PKGS_ZSTD_H\n\n");
    fprintf(out, "#include <stddef.h>\n");
    fprintf(out, "#include <stdint.h>\n\n");

    if (dict && dict_len > 0) {
        fprintf(out, "/* Zstd dictionary for decompression */\n");
        write_byte_array(out, "embedded_pkg_zstd_dict", dict, dict_len);
        fprintf(out, "#define EMBEDDED_PKG_DICT_SIZE %zu\n\n", dict_len);
    } else {
        fprintf(out, "static const unsigned char embedded_pkg_zstd_dict[] = {0};\n");
        fprintf(out, "#define EMBEDDED_PKG_DICT_SIZE 0\n\n");
    }

    fprintf(out, "typedef struct {\n");
    fprintf(out, "    const char* path;              /* VFS path */\n");
    fprintf(out, "    const unsigned char* data;     /* Compressed data */\n");
    fprintf(out, "    uint32_t compressed_size;      /* Compressed size */\n");
    fprintf(out, "    uint32_t original_size;        /* Original size */\n");
    fprintf(out, "    uint8_t file_type;             /* 0=pkg, 1=txt, 2=runtime */\n");
    fprintf(out, "} EmbeddedFilePkgZstd;\n\n");

    fprintf(out, "#define PKG_FILE_TYPE_PKG     0\n");
    fprintf(out, "#define PKG_FILE_TYPE_TXT     1\n");
    fprintf(out, "#define PKG_FILE_TYPE_RUNTIME 2\n\n");

    fprintf(out, "/* Compressed file data */\n");
    int array_idx = 0;
    for (int i = 0; i < g_file_count; i++) {
        if (!g_files[i].compressed) continue;

        char var_name[64];
        snprintf(var_name, sizeof(var_name), "pkgzstd_data_%d", array_idx);

        fprintf(out, "/* %s (%zu -> %zu) */\n",
                g_files[i].vfs_path, g_files[i].length, g_files[i].compressed_len);
        write_byte_array(out, var_name, g_files[i].compressed, g_files[i].compressed_len);

        array_idx++;
    }

    fprintf(out, "static const EmbeddedFilePkgZstd embedded_files_pkg_zstd[] = {\n");
    array_idx = 0;
    for (int i = 0; i < g_file_count; i++) {
        if (!g_files[i].compressed) continue;

        int out_type;
        switch (g_files[i].file_type) {
            case FILE_TYPE_PKG: out_type = 0; break;
            case FILE_TYPE_TXT: out_type = 1; break;
            default: out_type = 2; break;
        }

        fprintf(out, "    { \"%s\", pkgzstd_data_%d, %zu, %zu, %d },\n",
                g_files[i].vfs_path, array_idx,
                g_files[i].compressed_len, g_files[i].length,
                out_type);
        array_idx++;
    }
    fprintf(out, "    { NULL, NULL, 0, 0, 0 }  /* Sentinel */\n");
    fprintf(out, "};\n\n");

    fprintf(out, "#define EMBEDDED_PKG_ZSTD_PACKAGE_COUNT %d\n", pkg_count);
    fprintf(out, "#define EMBEDDED_PKG_ZSTD_TXT_COUNT %d\n", txt_count);
    fprintf(out, "#define EMBEDDED_PKG_ZSTD_RUNTIME_COUNT %d\n", runtime_count);
    fprintf(out, "#define EMBEDDED_PKG_ZSTD_TOTAL_COUNT %d\n", valid_count);
    fprintf(out, "#define EMBEDDED_PKG_ZSTD_TOTAL_COMPRESSED %zuU\n", total_compressed);
    fprintf(out, "#define EMBEDDED_PKG_ZSTD_TOTAL_ORIGINAL %zuU\n\n", total_original);

    fprintf(out, "#endif /* MHS_EMBEDDED_PKGS_ZSTD_H */\n");

    fclose(out);

    printf("\nGenerated: %s (pkg-mode)\n", output_path);
    printf("  Packages: %d\n", pkg_count);
    printf("  Txt files: %d\n", txt_count);
    printf("  Runtime: %d\n", runtime_count);
    printf("  Original: %zu bytes\n", total_original);
    printf("  Compressed: %zu bytes\n", total_compressed);
    printf("  Dictionary: %zu bytes\n", dict_len);
    printf("  Total embedded: %zu bytes (%.1f%% of original)\n",
           total_embedded, 100.0 * total_embedded / total_original);

    return 0;
}
#endif /* MHS_EMBED_NO_ZSTD */

/*-----------------------------------------------------------
 * Cleanup and main
 *-----------------------------------------------------------*/

static void free_files(void) {
    for (int i = 0; i < g_file_count; i++) {
        free(g_files[i].vfs_path);
        free(g_files[i].full_path);
        free(g_files[i].content);
        free(g_files[i].compressed);
    }
    g_file_count = 0;
}

static void print_usage(const char* prog) {
    printf("Usage: %s <output.h> [libdir ...] [options]\n\n", prog);
    printf("Embed files into C headers for MicroHs standalone binaries.\n\n");
    printf("Options:\n");
    printf("  --no-compress       Disable zstd compression (plain C strings)\n");
    printf("  --runtime <dir>     Embed runtime C/H files from <dir>\n");
    printf("  --lib <file>        Embed a library file (.a) in lib/ (repeatable)\n");
    printf("  --header <file>     Embed a header file in src/runtime/ (repeatable)\n");
#ifndef MHS_EMBED_NO_ZSTD
    printf("  --dict-size <bytes> Dictionary size (default: %d)\n", DEFAULT_DICT_SIZE);
    printf("  --level <1-22>      Compression level (default: %d)\n", DEFAULT_COMP_LEVEL);
#endif
    printf("  --help              Show this help\n");
    printf("\nPackage mode (--pkg-mode):\n");
    printf("  --pkg-mode            Output in pkg format with file types\n");
    printf("  --pkg <vfs>=<file>    Embed a .pkg file (repeatable)\n");
    printf("  --txt-dir <dir>       Collect .txt module mapping files\n");
    printf("  --music-modules <spec> Add synthetic .txt (pkg:mod1,mod2,...)\n");
    printf("\nExamples:\n");
    printf("  # Source mode with compression (default)\n");
    printf("  %s mhs_embedded_zstd.h lib/ --runtime src/runtime/\n\n", prog);
    printf("  # Source mode without compression\n");
    printf("  %s mhs_embedded.h lib/ --no-compress\n\n", prog);
    printf("  # Package mode with compression\n");
    printf("  %s out.h --pkg-mode \\\n", prog);
    printf("    --pkg packages/base.pkg=/path/base.pkg \\\n");
    printf("    --txt-dir /path/to/mcabal/mhs-0.15.2.0\n");
}

int main(int argc, char** argv) {
    if (argc < 2) {
        print_usage(argv[0]);
        return 1;
    }

    if (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0) {
        print_usage(argv[0]);
        return 0;
    }

    const char* output_path = argv[1];
    const char* runtime_dir = NULL;
    const char* txt_dir = NULL;

    const char* lib_files[64];
    const char* header_files[64];
    const char* pkg_files[64];
    const char* music_modules[64];
    int lib_count = 0, header_count = 0, pkg_count = 0, music_count = 0;

    /* Parse arguments */
    for (int i = 2; i < argc; i++) {
        if (strcmp(argv[i], "--no-compress") == 0) {
            g_no_compress = 1;
        } else if (strcmp(argv[i], "--runtime") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: --runtime requires an argument\n");
                return 1;
            }
            runtime_dir = argv[++i];
        } else if (strcmp(argv[i], "--lib") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: --lib requires an argument\n");
                return 1;
            }
            if (lib_count >= 64) {
                fprintf(stderr, "Error: Too many --lib arguments\n");
                return 1;
            }
            lib_files[lib_count++] = argv[++i];
        } else if (strcmp(argv[i], "--header") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: --header requires an argument\n");
                return 1;
            }
            if (header_count >= 64) {
                fprintf(stderr, "Error: Too many --header arguments\n");
                return 1;
            }
            header_files[header_count++] = argv[++i];
#ifndef MHS_EMBED_NO_ZSTD
        } else if (strcmp(argv[i], "--dict-size") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: --dict-size requires an argument\n");
                return 1;
            }
            g_dict_size = atol(argv[++i]);
        } else if (strcmp(argv[i], "--level") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: --level requires an argument\n");
                return 1;
            }
            g_comp_level = atoi(argv[++i]);
            if (g_comp_level < 1 || g_comp_level > 22) {
                fprintf(stderr, "Error: --level must be 1-22\n");
                return 1;
            }
#endif
        } else if (strcmp(argv[i], "--pkg-mode") == 0) {
            g_pkg_mode = 1;
        } else if (strcmp(argv[i], "--pkg") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: --pkg requires an argument\n");
                return 1;
            }
            if (pkg_count >= 64) {
                fprintf(stderr, "Error: Too many --pkg arguments\n");
                return 1;
            }
            pkg_files[pkg_count++] = argv[++i];
        } else if (strcmp(argv[i], "--txt-dir") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: --txt-dir requires an argument\n");
                return 1;
            }
            txt_dir = argv[++i];
        } else if (strcmp(argv[i], "--music-modules") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: --music-modules requires an argument\n");
                return 1;
            }
            if (music_count >= 64) {
                fprintf(stderr, "Error: Too many --music-modules arguments\n");
                return 1;
            }
            music_modules[music_count++] = argv[++i];
        } else if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
            print_usage(argv[0]);
            return 0;
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Error: Unknown option %s\n", argv[i]);
            return 1;
        } else {
            printf("Collecting .hs files from %s...\n", argv[i]);
            collect_hs_files(argv[i]);
        }
    }

    if (runtime_dir) {
        printf("Collecting runtime files from %s...\n", runtime_dir);
        collect_runtime_files(runtime_dir);
    }

    if (header_count > 0) {
        printf("Embedding headers:\n");
        for (int i = 0; i < header_count; i++) {
            embed_single_file(header_files[i], "src/runtime");
        }
    }

    if (lib_count > 0) {
        printf("Embedding libraries:\n");
        for (int i = 0; i < lib_count; i++) {
            embed_single_file(lib_files[i], "lib");
        }
    }

    if (pkg_count > 0) {
        printf("Embedding packages:\n");
        for (int i = 0; i < pkg_count; i++) {
            add_pkg_file(pkg_files[i]);
        }
    }

    if (txt_dir) {
        printf("Collecting .txt module mappings from %s...\n", txt_dir);
        collect_txt_files(txt_dir);
    }

    if (music_count > 0) {
        printf("Adding synthetic .txt for music modules:\n");
        for (int i = 0; i < music_count; i++) {
            add_music_modules(music_modules[i]);
        }
    }

    if (g_file_count == 0) {
        fprintf(stderr, "Error: No files found\n");
        return 1;
    }

    printf("\nCollected %d files\n", g_file_count);

    int result;

    if (g_no_compress) {
        /* Uncompressed mode */
        if (g_pkg_mode) {
            result = write_header_pkg_uncompressed(output_path);
        } else {
            result = write_header_uncompressed(output_path);
        }
    } else {
#ifdef MHS_EMBED_NO_ZSTD
        fprintf(stderr, "Error: This build does not support compression. Use --no-compress.\n");
        result = 1;
#else
        /* Compressed mode */
        size_t dict_len = 0;
        unsigned char* dict = train_dictionary(&dict_len);

        if (compress_files(dict, dict_len) != 0) {
            free(dict);
            free_files();
            return 1;
        }

        if (g_pkg_mode) {
            result = write_header_pkg_compressed(output_path, dict, dict_len);
        } else {
            result = write_header_compressed(output_path, dict, dict_len);
        }

        free(dict);
#endif
    }

    free_files();
    return result;
}
