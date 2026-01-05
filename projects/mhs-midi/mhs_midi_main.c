/* mhs_midi_main.c - Self-contained entry point for mhs-midi
 *
 * This provides a self-contained mhs-midi binary that:
 * 1. Auto-detects MHSDIR if not set
 * 2. Auto-adds the MIDI library include path
 * 3. Supports repl, compile, and run modes
 *
 * Usage:
 *   mhs-midi                     # Start REPL (default)
 *   mhs-midi repl                # Start REPL (explicit)
 *   mhs-midi compile File.hs     # Compile to executable
 *   mhs-midi run File.hs         # Compile and run
 *   mhs-midi [mhs-options]       # Pass options to MicroHs
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>
#include <sys/stat.h>

#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

#ifdef __linux__
#include <linux/limits.h>
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

/* Forward declaration of MicroHs main */
int mhs_main(int argc, char **argv);

#ifdef MHS_STANDALONE
/* VFS functions for standalone build */
int vfs_init(void);
const char* vfs_get_temp_dir(void);
#endif

/* Get the directory containing the executable */
static int get_exe_dir(char *buf, size_t size) {
#ifdef __APPLE__
    uint32_t bufsize = (uint32_t)size;
    if (_NSGetExecutablePath(buf, &bufsize) == 0) {
        char *dir = dirname(buf);
        strncpy(buf, dir, size - 1);
        buf[size - 1] = '\0';
        return 0;
    }
#elif defined(__linux__)
    ssize_t len = readlink("/proc/self/exe", buf, size - 1);
    if (len > 0) {
        buf[len] = '\0';
        char *dir = dirname(buf);
        strncpy(buf, dir, size - 1);
        buf[size - 1] = '\0';
        return 0;
    }
#endif
    return -1;
}

/* Check if a directory exists */
static int dir_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0 && S_ISDIR(st.st_mode);
}

/* Check if a file exists */
static int file_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0 && S_ISREG(st.st_mode);
}

/* Try to find MHSDIR relative to executable */
static int find_mhsdir(char *buf, size_t size, const char *exe_dir) {
    /* Try various relative locations */
    const char *candidates[] = {
        /* Development: build/mhs-midi -> thirdparty/MicroHs */
        "../thirdparty/MicroHs",
        /* Installed: bin/mhs-midi -> share/mhs-midi/MicroHs */
        "../share/mhs-midi/MicroHs",
        /* Installed: bin/mhs-midi -> lib/mhs-midi/MicroHs */
        "../lib/mhs-midi/MicroHs",
        NULL
    };

    for (int i = 0; candidates[i]; i++) {
        snprintf(buf, size, "%s/%s", exe_dir, candidates[i]);

        /* Check if lib/Prelude.hs exists */
        char prelude_path[PATH_MAX];
        snprintf(prelude_path, sizeof(prelude_path), "%s/lib/Prelude.hs", buf);

        if (file_exists(prelude_path)) {
            /* Normalize the path */
            char *resolved = realpath(buf, NULL);
            if (resolved) {
                strncpy(buf, resolved, size - 1);
                buf[size - 1] = '\0';
                free(resolved);
                return 0;
            }
        }
    }
    return -1;
}

/* Try to find MIDI lib directory relative to executable */
static int find_midi_lib(char *buf, size_t size, const char *exe_dir) {
    const char *candidates[] = {
        /* Development: build/mhs-midi -> projects/mhs-midi/lib */
        "../projects/mhs-midi/lib",
        /* Installed: bin/mhs-midi -> share/mhs-midi/lib */
        "../share/mhs-midi/lib",
        /* Installed: bin/mhs-midi -> lib/mhs-midi/lib */
        "../lib/mhs-midi/lib",
        NULL
    };

    for (int i = 0; candidates[i]; i++) {
        snprintf(buf, size, "%s/%s", exe_dir, candidates[i]);

        /* Check if Midi.hs exists */
        char midi_path[PATH_MAX];
        snprintf(midi_path, sizeof(midi_path), "%s/Midi.hs", buf);

        if (file_exists(midi_path)) {
            char *resolved = realpath(buf, NULL);
            if (resolved) {
                strncpy(buf, resolved, size - 1);
                buf[size - 1] = '\0';
                free(resolved);
                return 0;
            }
        }
    }
    return -1;
}

static void print_usage(const char *prog) {
    printf("mhs-midi - MicroHs with MIDI support\n\n");
    printf("Usage:\n");
    printf("  %s                     Start interactive REPL (default)\n", prog);
    printf("  %s repl [options]      Start REPL with options\n", prog);
    printf("  %s [mhs-options]       Pass options directly to MicroHs\n", prog);
    printf("  %s --help              Show this help\n", prog);
    printf("  %s --version           Show version\n", prog);
    printf("\nEnvironment:\n");
    printf("  MHSDIR                 Path to MicroHs directory (auto-detected if not set)\n");
    printf("\nExamples:\n");
    printf("  %s                     Start REPL with Midi, Music modules available\n", prog);
    printf("  %s -r MyFile.hs        Run a Haskell file\n", prog);
    printf("  %s -oMyProg MyFile.hs  Compile to executable\n", prog);
    printf("\nThe REPL automatically imports: Midi, Music, MusicPerform, MidiPerform, Async\n");
}

int main(int argc, char **argv) {
    char exe_dir[PATH_MAX];
    char mhsdir[PATH_MAX];
    char midi_lib[PATH_MAX];
    char include_arg[PATH_MAX + 3];

    /* Check for --help before anything else */
    if (argc >= 2 && (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0)) {
        print_usage(argv[0]);
        return 0;
    }

    /* Get executable directory */
    if (get_exe_dir(exe_dir, sizeof(exe_dir)) != 0) {
        fprintf(stderr, "Warning: Could not determine executable directory\n");
        exe_dir[0] = '\0';
    }

    /* Set MHSDIR if not already set */
    if (!getenv("MHSDIR")) {
#ifdef MHS_STANDALONE
        /* Check for MHS_USE_FILESYSTEM to bypass embedded libs (for debugging) */
        if (getenv("MHS_USE_FILESYSTEM")) {
            /* Use regular filesystem like non-standalone version */
            if (exe_dir[0] && find_mhsdir(mhsdir, sizeof(mhsdir), exe_dir) == 0) {
                setenv("MHSDIR", mhsdir, 1);
                fprintf(stderr, "Using filesystem MHSDIR: %s\n", mhsdir);
            } else {
                fprintf(stderr, "Error: MHS_USE_FILESYSTEM set but cannot find MicroHs directory.\n");
                return 1;
            }
        } else {
            /* Standalone build: use VFS with embedded libraries */
            if (vfs_init() != 0) {
                fprintf(stderr, "Error: Failed to initialize VFS\n");
                return 1;
            }
            const char* vfs_root = vfs_get_temp_dir();
            setenv("MHSDIR", vfs_root, 1);
            fprintf(stderr, "Using embedded libraries (VFS)\n");

            /* VFS initialized, stdin remains normal */
        }
#else
        if (exe_dir[0] && find_mhsdir(mhsdir, sizeof(mhsdir), exe_dir) == 0) {
            setenv("MHSDIR", mhsdir, 1);
        } else {
            fprintf(stderr, "Error: Cannot find MicroHs directory.\n");
            fprintf(stderr, "Set MHSDIR environment variable or ensure proper installation.\n");
            return 1;
        }
#endif
    }

    /* Find MIDI library path */
    int have_midi_lib = 0;
#ifndef MHS_STANDALONE
    /* Non-standalone: need to find MIDI lib on filesystem */
    if (exe_dir[0] && find_midi_lib(midi_lib, sizeof(midi_lib), exe_dir) == 0) {
        have_midi_lib = 1;
        snprintf(include_arg, sizeof(include_arg), "-i%s", midi_lib);
    }
#endif
    /* Standalone: MIDI lib is embedded via VFS, no -i flag needed */

    /* Skip 'repl' command if present (for compatibility with old script) */
    int arg_offset = 0;
    if (argc >= 2 && strcmp(argv[1], "repl") == 0) {
        arg_offset = 1;
    }

    /* Build new argv with additional arguments */
    /* We add: -C (cache), -i<midi_lib> */
    int extra_args = 1 + (have_midi_lib ? 1 : 0);
    int new_argc = argc - arg_offset + extra_args;
    char **new_argv = malloc((new_argc + 1) * sizeof(char *));

    if (!new_argv) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        return 1;
    }

    int j = 0;
    new_argv[j++] = argv[0];
    new_argv[j++] = "-C";  /* Enable caching for faster startup */

    if (have_midi_lib) {
        new_argv[j++] = include_arg;
    }

    /* Copy remaining arguments */
    for (int i = 1 + arg_offset; i < argc; i++) {
        new_argv[j++] = argv[i];
    }
    new_argv[j] = NULL;

    /* Call MicroHs main */
    int result = mhs_main(new_argc, new_argv);

    free(new_argv);
    return result;
}
