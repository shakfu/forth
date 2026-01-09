/* mhs_midi_main.c - Entry point for mhs-midi (non-standalone)
 *
 * This provides the mhs-midi binary that:
 * 1. Auto-detects MHSDIR if not set
 * 2. Auto-adds the MIDI library include path
 * 3. Supports repl, compile, and run modes
 *
 * For the standalone version with embedded libraries, see mhs_midi_standalone_main.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <direct.h>
#define PATH_MAX MAX_PATH
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#else
#include <unistd.h>
#include <libgen.h>
#endif

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

/* Get the directory containing the executable */
static int get_exe_dir(char *buf, size_t size) {
#ifdef _WIN32
    DWORD len = GetModuleFileNameA(NULL, buf, (DWORD)size);
    if (len > 0 && len < size) {
        /* Find last backslash and truncate */
        char *last_sep = strrchr(buf, '\\');
        if (!last_sep) last_sep = strrchr(buf, '/');
        if (last_sep) {
            *last_sep = '\0';
            return 0;
        }
    }
#elif defined(__APPLE__)
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

/* Check if a file exists */
static int file_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0 && S_ISREG(st.st_mode);
}

/* Resolve a path to its absolute form */
static char* resolve_path(const char *path) {
#ifdef _WIN32
    char *resolved = malloc(PATH_MAX);
    if (resolved && _fullpath(resolved, path, PATH_MAX)) {
        return resolved;
    }
    free(resolved);
    return NULL;
#else
    return realpath(path, NULL);
#endif
}

/* Cross-platform setenv */
static int set_env(const char *name, const char *value) {
#ifdef _WIN32
    return _putenv_s(name, value);
#else
    return setenv(name, value, 1);
#endif
}

/* Try to find MHSDIR relative to executable */
static int find_mhsdir(char *buf, size_t size, const char *exe_dir) {
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
            char *resolved = resolve_path(buf);
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
            char *resolved = resolve_path(buf);
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
    printf("%s - MicroHs with MIDI support\n\n", prog);
    printf("Usage:\n");
    printf("  %s                     Start interactive REPL (default)\n", prog);
    printf("  %s [mhs-options]       Pass options directly to MicroHs\n", prog);
    printf("  %s --help              Show this help\n", prog);
    printf("\nEnvironment:\n");
    printf("  MHSDIR                 Path to MicroHs directory (auto-detected if not set)\n");
    printf("\nExamples:\n");
    printf("  %s                     Start REPL\n", prog);
    printf("  %s -r MyFile.hs        Run a Haskell file\n", prog);
    printf("  %s -oMyProg MyFile.hs  Compile to executable\n", prog);
    printf("\nAvailable modules: Midi, Music, MusicPerform, MidiPerform, Async\n");
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
        if (exe_dir[0] && find_mhsdir(mhsdir, sizeof(mhsdir), exe_dir) == 0) {
            set_env("MHSDIR", mhsdir);
        } else {
            fprintf(stderr, "Error: Cannot find MicroHs directory.\n");
            fprintf(stderr, "Set MHSDIR environment variable or ensure proper installation.\n");
            return 1;
        }
    }

    /* Find MIDI library path */
    int have_midi_lib = 0;
    if (exe_dir[0] && find_midi_lib(midi_lib, sizeof(midi_lib), exe_dir) == 0) {
        have_midi_lib = 1;
        snprintf(include_arg, sizeof(include_arg), "-i%s", midi_lib);
    }

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
