/*
 * embed_libs_zstd.c - Convert library files to C header with zstd compression
 *
 * Uses dictionary-based compression for optimal ratio on similar files.
 * Text files (.hs, .c, .h) are compressed with a trained dictionary.
 * Binary files (.a) are compressed without dictionary.
 *
 * Usage: embed_libs_zstd <output.h> <libdir1> [libdir2 ...] [options]
 *
 * Options:
 *   --runtime <dir>     Embed runtime C/H files from <dir>
 *   --lib <file>        Embed a library file (.a) in lib/
 *   --header <file>     Embed a header file in src/runtime/
 *   --dict-size <bytes> Dictionary size (default: 112KB)
 *   --level <1-22>      Compression level (default: 19)
 *
 * Compile:
 *   cc -O2 -o embed_libs_zstd embed_libs_zstd.c \
 *      ../thirdparty/zstd-1.5.7/zstd.c -I../thirdparty/zstd-1.5.7 -lpthread
 *
 * The generated header contains compressed data with a shared dictionary:
 *   - embedded_zstd_dict[]     - trained dictionary
 *   - embedded_files_zstd[]    - compressed file entries
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>
#include <errno.h>

/* Zstd includes */
#define ZSTD_STATIC_LINKING_ONLY
#include "zstd.h"
#include "zdict.h"

#define MAX_FILES 1024
#define MAX_PATH 4096
#define DEFAULT_DICT_SIZE (112 * 1024)  /* 112KB dictionary */
#define DEFAULT_COMP_LEVEL 19

typedef struct {
    char* vfs_path;           /* Path in VFS */
    char* full_path;          /* Full filesystem path */
    unsigned char* content;   /* Original content */
    size_t length;            /* Original length */
    unsigned char* compressed;/* Compressed content */
    size_t compressed_len;    /* Compressed length */
    int use_dict;             /* 1 if compressed with dictionary */
} FileEntry;

static FileEntry g_files[MAX_FILES];
static int g_file_count = 0;

static size_t g_dict_size = DEFAULT_DICT_SIZE;
static int g_comp_level = DEFAULT_COMP_LEVEL;

/* Forward declarations */
static int collect_files_recursive(const char* dir_path, const char* base_name,
                                   const char* pattern, int include_subdirs);
static int write_header(const char* output_path, const unsigned char* dict, size_t dict_len);
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
           matches_pattern(path, "*.h");
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

static int add_file(const char* vfs_path, const char* full_path) {
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

    return 0;
}

/*-----------------------------------------------------------
 * Directory traversal
 *-----------------------------------------------------------*/

static int collect_files_recursive(const char* dir_path, const char* base_name,
                                   const char* pattern, int include_subdirs) {
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
                collect_files_recursive(full_path, new_base, pattern, 1);
            }
        } else if (matches_pattern(entry->d_name, pattern)) {
            char vfs_path[MAX_PATH];
            if (base_name[0])
                snprintf(vfs_path, sizeof(vfs_path), "%s/%s", base_name, entry->d_name);
            else
                snprintf(vfs_path, sizeof(vfs_path), "%s", entry->d_name);
            add_file(vfs_path, full_path);
        }
    }

    closedir(dir);
    return 0;
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

    collect_files_recursive(dir_copy, base, "*.c", 1);
    collect_files_recursive(dir_copy, base, "*.h", 1);

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

    return add_file(vfs_path, file_path);
}

/*-----------------------------------------------------------
 * Zstd compression
 *-----------------------------------------------------------*/

/*
 * Train dictionary on text file samples
 */
static unsigned char* train_dictionary(size_t* out_dict_len) {
    /* Count text files and total size */
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

    /* Build sample buffer and sizes array */
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

    /* Train dictionary */
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

    /* Shrink to actual size */
    unsigned char* shrunk = realloc(dict, dict_len);
    return shrunk ? shrunk : dict;
}

/*
 * Compress all files
 */
static int compress_files(const unsigned char* dict, size_t dict_len) {
    /* Create compression context */
    ZSTD_CCtx* cctx = ZSTD_createCCtx();
    if (!cctx) {
        fprintf(stderr, "Error: Failed to create compression context\n");
        return -1;
    }

    /* Create dictionary if we have one */
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

        /* Allocate output buffer (worst case is slightly larger than input) */
        size_t bound = ZSTD_compressBound(entry->length);
        entry->compressed = malloc(bound);
        if (!entry->compressed) {
            fprintf(stderr, "Error: Memory allocation failed for %s\n", entry->vfs_path);
            continue;
        }

        /* Compress */
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

        /* Shrink buffer to actual size */
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

/*-----------------------------------------------------------
 * Output generation
 *-----------------------------------------------------------*/

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

static int compare_files(const void* a, const void* b) {
    const FileEntry* fa = (const FileEntry*)a;
    const FileEntry* fb = (const FileEntry*)b;
    return strcmp(fa->vfs_path, fb->vfs_path);
}

static int write_header(const char* output_path,
                        const unsigned char* dict, size_t dict_len) {
    FILE* out = fopen(output_path, "w");
    if (!out) {
        fprintf(stderr, "Error: Could not create %s: %s\n", output_path, strerror(errno));
        return -1;
    }

    /* Sort files */
    qsort(g_files, g_file_count, sizeof(FileEntry), compare_files);

    /* Calculate totals */
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

    /* Write header */
    fprintf(out, "/* Auto-generated by embed_libs_zstd - DO NOT EDIT */\n");
    fprintf(out, "/* %d files, zstd compressed with dictionary */\n", valid_count);
    fprintf(out, "/* Original: %zu bytes -> Embedded: %zu bytes (%.2fx) */\n\n",
            total_original, total_embedded, ratio);

    fprintf(out, "#ifndef MHS_EMBEDDED_ZSTD_H\n");
    fprintf(out, "#define MHS_EMBEDDED_ZSTD_H\n\n");
    fprintf(out, "#include <stddef.h>\n");
    fprintf(out, "#include <stdint.h>\n\n");

    /* Write dictionary */
    if (dict && dict_len > 0) {
        fprintf(out, "/* Zstd dictionary for text file decompression */\n");
        write_byte_array(out, "embedded_zstd_dict", dict, dict_len);
        fprintf(out, "#define EMBEDDED_DICT_SIZE %zu\n\n", dict_len);
    } else {
        fprintf(out, "static const unsigned char embedded_zstd_dict[] = {0};\n");
        fprintf(out, "#define EMBEDDED_DICT_SIZE 0\n\n");
    }

    /* File entry structure */
    fprintf(out, "typedef struct {\n");
    fprintf(out, "    const char* path;              /* VFS path */\n");
    fprintf(out, "    const unsigned char* data;     /* Compressed data */\n");
    fprintf(out, "    uint32_t compressed_size;      /* Compressed size */\n");
    fprintf(out, "    uint32_t original_size;        /* Original size */\n");
    fprintf(out, "    uint8_t use_dict;              /* 1 if compressed with dictionary */\n");
    fprintf(out, "} EmbeddedFileZstd;\n\n");

    /* Write compressed data arrays */
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

    /* Write file table */
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
    printf("Usage: %s <output.h> <libdir1> [libdir2 ...] [options]\n\n", prog);
    printf("Options:\n");
    printf("  --runtime <dir>     Embed runtime C/H files from <dir>\n");
    printf("  --lib <file>        Embed a library file (.a) in lib/ (repeatable)\n");
    printf("  --header <file>     Embed a header file in src/runtime/ (repeatable)\n");
    printf("  --dict-size <bytes> Dictionary size (default: %d)\n", DEFAULT_DICT_SIZE);
    printf("  --level <1-22>      Compression level (default: %d)\n", DEFAULT_COMP_LEVEL);
    printf("  --help              Show this help\n");
    printf("\nExamples:\n");
    printf("  %s mhs_embedded_zstd.h lib/ --runtime src/runtime/\n", prog);
    printf("  %s out.h lib/ --lib libfoo.a --level 22\n", prog);
}

int main(int argc, char** argv) {
    if (argc < 3) {
        print_usage(argv[0]);
        return 1;
    }

    if (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0) {
        print_usage(argv[0]);
        return 0;
    }

    const char* output_path = argv[1];
    const char* runtime_dir = NULL;

    const char* lib_files[64];
    const char* header_files[64];
    int lib_count = 0, header_count = 0;

    /* Parse arguments */
    for (int i = 2; i < argc; i++) {
        if (strcmp(argv[i], "--runtime") == 0) {
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

    if (g_file_count == 0) {
        fprintf(stderr, "Error: No files found\n");
        return 1;
    }

    printf("\nCollected %d files\n", g_file_count);

    /* Train dictionary */
    size_t dict_len = 0;
    unsigned char* dict = train_dictionary(&dict_len);

    /* Compress all files */
    if (compress_files(dict, dict_len) != 0) {
        free(dict);
        free_files();
        return 1;
    }

    /* Write output */
    int result = write_header(output_path, dict, dict_len);

    free(dict);
    free_files();

    return result;
}
