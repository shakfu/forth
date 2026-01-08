/* mhs_midi_standalone_main.c - Entry point for mhs-midi-standalone
 *
 * This provides the self-contained mhs-midi-standalone binary that:
 * 1. Has all MicroHs and MIDI libraries embedded via VFS
 * 2. Requires no external files (MHSDIR not needed)
 * 3. Supports repl, compile, and run modes
 *
 * Compile options:
 *   -DVFS_USE_PKG    Use precompiled .pkg files for fast startup (~0.5s vs ~19s)
 *   (default)        Use embedded .hs source files (slower startup)
 *
 * For compilation to executable (-o without .c), files are extracted
 * to a temp directory since cc needs real filesystem access. Linker
 * flags are automatically added to link against embedded MIDI libraries.
 *
 * For the non-standalone version requiring MHSDIR, see mhs_midi_main.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vfs.h"

/* Forward declaration of MicroHs main */
int mhs_main(int argc, char **argv);

static void print_usage(const char *prog) {
    printf("%s - Self-contained MicroHs with MIDI support\n\n", prog);
    printf("Usage:\n");
    printf("  %s                     Start interactive REPL (default)\n", prog);
    printf("  %s [mhs-options]       Pass options directly to MicroHs\n", prog);
    printf("  %s --help              Show this help\n", prog);
    printf("\n");
    printf("\nExamples:\n");
    printf("  %s                     Start REPL\n", prog);
    printf("  %s -r MyFile.hs        Run a Haskell file\n", prog);
    printf("  %s -oMyProg MyFile.hs  Compile to executable\n", prog);
    printf("  %s -oMyProg.c MyFile.hs  Output C code only\n", prog);
    printf("\nAvailable MIDI modules: Midi, Music, MusicPerform, MidiPerform, Async\n");
}

/* Check if we need to extract files for compilation.
 * Returns 1 if -o is specified with a non-.c output (executable compilation).
 */
static int needs_extraction(int argc, char **argv) {
    for (int i = 1; i < argc; i++) {
        /* Check for -oFILE or -o FILE */
        if (strncmp(argv[i], "-o", 2) == 0) {
            const char *output = NULL;
            if (argv[i][2] != '\0') {
                /* -oFILE form */
                output = argv[i] + 2;
            } else if (i + 1 < argc) {
                /* -o FILE form */
                output = argv[i + 1];
            }
            if (output) {
                size_t len = strlen(output);
                /* Check if output ends with .c */
                if (len >= 2 && strcmp(output + len - 2, ".c") == 0) {
                    return 0;  /* C output, VFS works fine */
                }
                return 1;  /* Executable output, needs extraction */
            }
        }
    }
    return 0;  /* No -o flag, VFS works fine */
}

int main(int argc, char **argv) {
    char *temp_dir = NULL;
    int linking_midi = 0;

    /* Check for --help before anything else */
    if (argc >= 2 && (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0)) {
        print_usage(argv[0]);
        return 0;
    }

    /* Initialize VFS with embedded libraries */
    vfs_init();

    /* Check if we're compiling to an executable (cc needs real files) */
    if (needs_extraction(argc, argv)) {
        temp_dir = vfs_extract_to_temp();
        if (!temp_dir) {
            fprintf(stderr, "Error: Failed to extract embedded files\n");
            return 1;
        }
        /* Set MHSDIR to temp directory for cc to find runtime files */
        setenv("MHSDIR", temp_dir, 1);
        linking_midi = 1;
    } else {
        /* Use VFS - set MHSDIR to virtual root */
        setenv("MHSDIR", vfs_get_temp_dir(), 1);
    }

    /* Skip 'repl' command if present (for compatibility with old script) */
    int arg_offset = 0;
    if (argc >= 2 && strcmp(argv[1], "repl") == 0) {
        arg_offset = 1;
    }

    /* Build new argv with -C flag for caching, plus linker flags if needed */
    /* Extra args: -C, and if linking: -optl flags for each library + frameworks */
    /* With VFS_USE_PKG: also add -a and -p flags for package loading */
#ifdef __APPLE__
    /* macOS: 4 libraries + 3 frameworks + C++ runtime = 8 -optl pairs = 16 args */
    #define LINK_EXTRA_ARGS 16
#else
    /* Linux: 4 libraries + ALSA + C++ runtime = 6 -optl pairs = 12 args */
    #define LINK_EXTRA_ARGS 12
#endif

#ifdef VFS_USE_PKG
    /* Package mode: -C + -a/mhs-embedded + -pbase + -pmusic = 4 extra args */
    #define PKG_EXTRA_ARGS 3
#else
    #define PKG_EXTRA_ARGS 0
#endif

    int extra_args = 1;  /* -C */
    extra_args += PKG_EXTRA_ARGS;
    if (linking_midi) {
        extra_args += LINK_EXTRA_ARGS;
    }

    int new_argc = argc - arg_offset + extra_args;
    char **new_argv = malloc((new_argc + 1) * sizeof(char *));

    /* Buffers for library path arguments */
    char lib_libremidi[512];
    char lib_midi_ffi[512];
    char lib_music_theory[512];
    char lib_midi_file[512];

    if (!new_argv) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        if (temp_dir) vfs_cleanup_temp(temp_dir);
        return 1;
    }

    int j = 0;
    new_argv[j++] = argv[0];
    new_argv[j++] = "-C";  /* Enable caching for faster startup */

#ifdef VFS_USE_PKG
    /* Package mode: add package search path and preload packages */
    new_argv[j++] = "-a/mhs-embedded";  /* Package search path */
    new_argv[j++] = "-pbase";           /* Preload base package */
    new_argv[j++] = "-pmusic";          /* Preload music package */
#endif

    /* Add linker flags for MIDI libraries if compiling to executable */
    if (linking_midi) {
        /* Build library paths */
        snprintf(lib_midi_ffi, sizeof(lib_midi_ffi), "%s/lib/libmidi_ffi.a", temp_dir);
        snprintf(lib_music_theory, sizeof(lib_music_theory), "%s/lib/libmusic_theory.a", temp_dir);
        snprintf(lib_midi_file, sizeof(lib_midi_file), "%s/lib/libmidi_file.a", temp_dir);
        snprintf(lib_libremidi, sizeof(lib_libremidi), "%s/lib/liblibremidi.a", temp_dir);

        /* Add -optl flags for each library */
        new_argv[j++] = "-optl";
        new_argv[j++] = lib_midi_ffi;
        new_argv[j++] = "-optl";
        new_argv[j++] = lib_music_theory;
        new_argv[j++] = "-optl";
        new_argv[j++] = lib_midi_file;
        new_argv[j++] = "-optl";
        new_argv[j++] = lib_libremidi;

#ifdef __APPLE__
        /* macOS frameworks */
        new_argv[j++] = "-optl";
        new_argv[j++] = "-framework";
        new_argv[j++] = "-optl";
        new_argv[j++] = "CoreMIDI";
        new_argv[j++] = "-optl";
        new_argv[j++] = "-framework";
        new_argv[j++] = "-optl";
        new_argv[j++] = "CoreFoundation";
        new_argv[j++] = "-optl";
        new_argv[j++] = "-framework";
        new_argv[j++] = "-optl";
        new_argv[j++] = "CoreAudio";
        /* C++ standard library (libremidi is C++) - Clang uses libc++ */
        new_argv[j++] = "-optl";
        new_argv[j++] = "-lc++";
#else
        /* Linux: ALSA */
        new_argv[j++] = "-optl";
        new_argv[j++] = "-lasound";
        /* C++ standard library (libremidi is C++) - GCC uses libstdc++ */
        new_argv[j++] = "-optl";
        new_argv[j++] = "-lstdc++";
#endif
    }

    /* Copy remaining arguments */
    for (int i = 1 + arg_offset; i < argc; i++) {
        new_argv[j++] = argv[i];
    }
    new_argv[j] = NULL;

    /* Update argc to match */
    new_argc = j;

    /* Call MicroHs main */
    int result = mhs_main(new_argc, new_argv);

    free(new_argv);

    /* Clean up temp directory if we extracted */
    if (temp_dir) {
        vfs_cleanup_temp(temp_dir);
    }

    return result;
}
