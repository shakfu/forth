# Changelog

All notable changes to midi-langs are documented in this file.

## [Unreleased]

### Added

- **Unified CI workflow**: New `build-all.yml` combines all build workflows into a single workflow
  - `build-unix` job: Linux and macOS builds (from build-test.yml)
  - `build-windows` job: Windows builds for all languages except mhs-midi
  - `build-windows-mhs` job: Windows build specifically for mhs-midi (requires MicroHs)
  - `mhs-standalone-variants` job: macOS builds for mhs-midi standalone variants (src, src-zstd, pkg, pkg-zstd)
  - `collect-artifacts` job: Aggregates all artifacts into single `midi-langs-all` download
  - Triggered manually via `workflow_dispatch`

- **mhs-midi Windows support**: New GitHub workflow for building mhs-midi on Windows
  - `build-windows-mhs.yml` workflow builds MicroHs compiler and mhs-midi REPL
  - Cross-platform Python script `mhs-patch-xffi.py` replaces sed for patching mhs.c

- **BUILD_MHS_MIDI CMake option**: Control whether mhs-midi targets are built
  - Defaults to ON; set `-DBUILD_MHS_MIDI=OFF` to skip mhs-midi (avoids MicroHs build at configure time)
  - Used by Windows CI to skip mhs-midi in `build-windows` job (separate `build-windows-mhs` handles it)

### Fixed

- **Windows build compatibility**: All MIDI language implementations now build and test on Windows
  - Added `WIN32_LEAN_AND_MEAN` before `<windows.h>` includes to prevent winsock.h/winsock2.h conflicts
  - Removed `struct timespec` definitions (Windows UCRT already provides it in `<time.h>`)
  - Added `#ifndef CLOCK_MONOTONIC` guards with `QueryPerformanceCounter`-based timing implementation
  - Fixed `getopt.h` unavailability in pktpy-midi with Windows-specific argument parsing
  - Disabled pocketpy threading on Windows (`PK_ENABLE_THREADS=0`) to avoid MSVC C11 atomics issues
  - Patched s7.c: fixed `nil_string` bug in Windows `g_uname()` and added `<time.h>` include

- **mhs-midi Windows compatibility**: mhs-midi REPL now builds on Windows
  - `mhs_midi_main.c`: Added `GetModuleFileNameA()`, `_fullpath()`, `_putenv_s()` for Windows
  - `midi_ffi.c`: Added `useconds_t` typedef, `Sleep()`, and `QueryPerformanceCounter()` for Windows
  - `CMakeLists.txt`: Fixed `.exe` extension for MHS_COMPILER, added Windows runtime includes and `winmm` library
  - Moved `find_package(Python3)` before mhs-midi target to fix target generation
  - Standalone variants (mhs-midi-src/pkg/zstd) remain Unix-only due to fmemopen dependency

- **Windows CI tests**: Shell-based tests now properly excluded on Windows
  - Wrapped all `sh -c` and `.sh` script tests in `if(NOT WIN32)` blocks
  - Cross-platform tests (direct executable calls) remain available on all platforms

## [0.1.8]

### Added

- **mhs-midi-pkg-zstd**: Combined package embedding with zstd compression
  - New build variant: fastest startup (precompiled packages) + smallest binary (compression)
  - Binary size: 3.0 MB (vs 4.8 MB uncompressed packages, 3.3 MB source)
  - Extends `scripts/embed_libs_zstd.c` with pkg-mode support:
    - `--pkg-mode` flag for pkg-zstd output format
    - `--pkg <vfs>=<file>` to embed .pkg files
    - `--txt-dir <dir>` to collect .txt module mapping files
    - `--music-modules <pkg:mod1,mod2>` for synthetic txt entries
  - Updated `vfs.c` with combined `VFS_USE_PKG + VFS_USE_ZSTD` mode
  - All mhs-midi variants now available:
    - `mhs-midi-src` (3.3M) - source embedding, slowest startup
    - `mhs-midi-src-zstd` (1.3M) - compressed source, smallest binary
    - `mhs-midi-pkg` (4.8M) - package embedding, fastest startup
    - `mhs-midi-pkg-zstd` (3.0M) - compressed packages, best balance

- **alda-midi built-in synthesizer**: TinySoundFont + miniaudio integration for direct audio output
  - No external synth required - just provide a SoundFont (.sf2) file
  - CLI: `-sf, --soundfont PATH` to use built-in synth
  - REPL commands:
    - `sf-load PATH` - Load soundfont and switch to built-in synth
    - `sf-list` - List soundfont presets
    - `midi` - Switch to MIDI output
    - `builtin` - Switch back to built-in synth
  - Fallback model: TSF takes priority when enabled, otherwise MIDI output
  - Thread-safe audio rendering with mutex-protected TSF calls
  - 44.1kHz stereo float output via miniaudio
  - New files: `tsf_backend.h`, `tsf_backend.c`
  - Dependencies: `thirdparty/TinySoundFont/`, `thirdparty/miniaudio/`

- **mhs-midi-standalone precompiled packages**: Optional `.pkg` embedding for fast cold start
  - New CMake option: `-DMHS_USE_PKG=ON` embeds precompiled MicroHs packages
  - Cold start reduced from ~20s to ~1s (20x faster first run)
  - Hybrid approach: embeds packages AND runtime files for full compilation support
  - Embedded content:
    - 2 `.pkg` files (base + music packages, ~2.7 MB)
    - 190 `.txt` module mapping files for package lookup
    - 33 runtime source files for standalone executable compilation
    - Static libraries (libremidi, midi_ffi, music_theory, midi_file)
  - VFS intercepts `opendir`/`readdir` for virtual package directory listing
  - New script: `scripts/embed_pkgs.py` for package embedding
  - Self-contained build: packages built locally in `build/` (no `~/.mcabal` required)
  - Documentation: `docs/mhs-midi/mhs-pkg-build.md`

- **mhs-midi-standalone zstd compression**: Optional build-time compression for smaller binaries
  - New CMake option: `-DMHS_USE_ZSTD=ON` enables zstd dictionary compression
  - Binary size reduced from 3.2 MB to 1.3 MB (59% smaller)
  - Embedded files compressed 5.2x (2.5 MB to 367 KB including 112 KB dictionary)
  - Dictionary-trained compression exploits patterns in Haskell source files
  - Decompression on demand with caching (~50ms startup overhead)
  - New files:
    - `scripts/embed_libs_zstd.c` - C tool for zstd compression with dictionary training
  - Consolidated `vfs.c` now handles all modes (default, ZSTD, PKG) via `#ifdef` conditionals
  - Uses `zstddeclib.c` (decompress-only, 900 KB) at runtime instead of full zstd library
  - Documentation added to `docs/mhs-midi/mhs-standalones.md`

- **CI matrix for mhs-midi-standalone build variations**: GitHub Actions tests all four configurations
  - default, MHS_USE_PKG, MHS_USE_ZSTD, and combined PKG+ZSTD
  - Each configuration builds and runs `mhs_standalone_compile` test
  - Reports binary size for each variation

### Fixed

- **Windows build compatibility**: All MIDI language implementations now build and test on Windows
  - Added `WIN32_LEAN_AND_MEAN` before `<windows.h>` includes to prevent winsock.h/winsock2.h conflicts
  - Removed `struct timespec` definitions (Windows UCRT already provides it in `<time.h>`)
  - Added `#ifndef CLOCK_MONOTONIC` guards with `QueryPerformanceCounter`-based timing implementation
  - Fixed `getopt.h` unavailability in pktpy-midi with Windows-specific argument parsing
  - Disabled pocketpy threading on Windows (`PK_ENABLE_THREADS=0`) to avoid MSVC C11 atomics issues
  - Patched s7.c: fixed `nil_string` bug in Windows `g_uname()` and added `<time.h>` include
  - Files modified: `forth_midi.h`, `lua-midi/midi_module.c`, `lua-midi/scheduler.c`, `pktpy-midi/midi_module.c`, `pktpy-midi/scheduler.c`, `pktpy-midi/main.c`, `s7-midi/midi_module.c`, `s7-midi/scheduler.c`, `alda-midi/src/midi_backend.c`, `alda-midi/src/tsf_backend.c`, `thirdparty/s7/s7.c`

- **Windows CI tests**: Shell-based tests now properly excluded on Windows
  - Wrapped all `sh -c` and `.sh` script tests in `if(NOT WIN32)` blocks
  - Cross-platform tests (direct executable calls) remain available on all platforms
  - Windows CI now runs: unit tests, demo examples, and direct executable tests

- **vfs.c**: Fixed ZSTD mode directory operations using wrong type names
  - Added `#elif defined(VFS_USE_ZSTD)` blocks for `vfs_opendir` and `vfs_readdir`
  - ZSTD mode now correctly uses `EmbeddedFileZstd` and `embedded_files_zstd`

- **mhs-midi-standalone Linux linking**: Fixed compilation to executable on Linux
  - Use `-lstdc++` instead of `-lc++` (GCC uses libstdc++, not Clang's libc++)
  - Added `-lm` for math library (required on Linux, implicit on macOS)
  - Added `-Wl,--no-as-needed` to fix linker ordering issues (GCC processes libraries left-to-right)

- **alda-midi cross-platform mutex**: Fixed pthread dependency for Windows
  - Added cross-platform mutex abstraction using `CRITICAL_SECTION` on Windows
  - POSIX systems continue to use `pthread_mutex_t`

- **alda-midi cross-platform sleep**: Fixed unistd.h dependency for Windows
  - Added `usleep()` macro using `Sleep()` on Windows
  - POSIX systems continue to use `unistd.h`

### Changed

- **VFS consolidation**: Consolidated VFS implementations into single `vfs.c`
  - Renamed `vfs_unified.c` to `vfs.c` (handles all modes via `#ifdef`)
  - Deleted unused files: old `vfs.c`, `vfs_zstd.c`, `vfs_zstd.h`, `vfs_unified.h`
  - Updated documentation in `mhs-standalones.md` and `self-contained.md`

- **mhs-midi Windows exclusion**: Disabled mhs-midi builds on Windows
  - VFS requires `fmemopen()` which has no Windows equivalent
  - Added `if(NOT WIN32)` wrapper in top-level CMakeLists.txt
  - Updated `build-windows.yml` workflow
  - Added `docs/mhs-midi/windows-support.md` with workarounds for Windows users

## [0.1.7]

### Added

- **mhs-midi-standalone**: Self-contained binary with embedded Haskell libraries
  - No external files required - 273 files embedded (~2.5MB):
    - 240 Haskell source files (MicroHs stdlib + MIDI libs)
    - 28 runtime C/H files for compilation
    - 4 static libraries (libremidi, midi_ffi, music_theory, midi_file)
    - midi_ffi.h header for MIDI FFI
  - Pure memory-based VFS using `fmemopen()` for zero-copy file access
  - Can compile MIDI programs to standalone executables:
    - `./mhs-midi-standalone -oMyProg MyFile.hs` produces self-contained binary
    - Automatic temp extraction and linker flag injection for executable compilation
    - Platform-specific linking (CoreMIDI/CoreAudio on macOS, ALSA on Linux)
  - Build: `cmake --build build --target mhs-midi-standalone`
  - New files: `vfs.c/h`, `mhs_ffi_override.c`, `mhs_midi_standalone_main.c`
  - New scripts: `scripts/embed_libs.py`, `scripts/patch_eval_vfs.py`
  - New test: `mhs_standalone_compile` - verifies compilation to executable

- **CI/CD**: GitHub Actions workflow (`build-test.yml`) for cross-platform builds
  - Builds on Ubuntu, macOS, and Windows
  - Artifact uploads for each platform (`midi-langs-linux`, `midi-langs-macos`, `midi-langs-windows`)
  - Combined artifact collection (`midi-langs-all`)
  - MIDI-dependent tests automatically skipped on Linux CI (no virtual MIDI ports)

- **mhs-midi install rules**: CMake install support for mhs-midi binaries
  - `cmake --install build --component mhs-midi` installs to prefix
  - Binaries to `bin/`, MicroHs libs to `share/mhs-midi/MicroHs/`, MIDI libs to `share/mhs-midi/lib/`
  - Auto-detection works from installed location (no MHSDIR needed)

### Changed

- **Build System**: MicroHs now builds automatically during CMake configuration
  - No longer requires manual `make -C thirdparty/MicroHs` step
  - Uses `make` on Unix, `nmake -f Makefile.windows` on Windows
  - Simplified Makefile - removed MicroHs build dependency

- **Cross-platform**: Readline now optional on Windows for all MIDI languages
  - forth-midi, lua-midi, pktpy-midi, s7-midi, alda-midi
  - Simple fallback provides basic input (no line editing/history/completion)
  - Linux/macOS still require readline (`libreadline-dev` on Linux)
  - Windows argument parsing uses simple fallback instead of getopt

### Fixed

- **CI**: Linux builds now install `libreadline-dev` for readline support
- **mhs-midi**: Help output now uses `argv[0]` instead of hardcoded executable name

## [0.1.6]

### Added

- **scripts/ctidy.py**: Run clang-tidy on all relevant projects

- **tests**: README example tests in `tests/examples/`
  - `demo_alda.alda` - Alda quick example
  - `demo_forth.4th` - Forth quick example
  - `demo_lua.lua` - Lua quick example
  - `DemoMhs.hs` - MicroHs quick example
  - `demo_pktpy.py` - PocketPy quick example
  - `demo_s7.scm` - s7 Scheme quick example

### Fixed

- **alda-midi**: Fixed clang-tidy warnings in interpreter, scanner, parser, and AST modules
  - `interpreter.c`: Added `ftell` error check to prevent `malloc(0)` on error
  - `interpreter.c`: Added defensive bounds check for `fread` return value
  - `interpreter.c`: Fixed narrowing conversion from `tolower()` (`int` to `char`)
  - `scanner.c`: Added missing `default` case to switch statement
  - `ast.c`, `parser.c`: Added explicit `(void*)` casts for `free(char**)`
- **forth-midi**: Fixed clang-tidy warnings in generative, interpreter, and recording modules
  - `generative.c`: Fixed implicit widening of multiplication result used as pointer offset
  - `interpreter.c`: Added missing `default` cases to switch statements
  - `interpreter.c`: Removed dead store to `last_pitch` and unused `result` variable
  - `recording.c`: Added missing `default` case to switch statement
- **lua-midi**: Added missing `default` cases to switch statements in `midi_module.c`
- **s7-midi**: Added missing `default` cases to switch statements in `midi_module.c`
- **pktpy-midi**: Fixed file reading bug in `main.c` (`ftell` error check, bounds check for `fread`)
- **pktpy-midi**: Added missing `default` case to switch statement in `midi_module.c`
- **mhs-midi**: Fixed `mhs-midi compile/run` to link music_theory and midi_file libraries
- **docs**: Updated README.md "Common Features" section to clarify alda-midi doesn't use music_theory.c

## [0.1.5]

### Added

- **alda-midi**: New Alda music language interpreter with MIDI output
  - `projects/alda-midi/` - Complete project structure
  - Full Alda language parser (38 tokens, 28 AST node types, ~2,500 LOC)
  - Core features:
    - Part declarations: `piano:`, `violin:`, `trumpet/trombone:`
    - Notes with durations: `c4` (quarter), `d8` (eighth), `e2` (half)
    - Chords: `c/e/g` (simultaneous notes)
    - Rests: `r4` (quarter rest)
    - Octave control: `o4`, `>` (up), `<` (down)
    - Ties: `c4~4` (tied notes)
    - Dotted notes: `c4.`, `c4..`
  - Attributes (S-expressions):
    - `(tempo 120)` - Set tempo in BPM
    - `(volume 80)` - Set volume 0-100
    - `(quant 90)` - Set quantization (articulation)
    - `(panning 50)` - Set pan position
    - Dynamics: `(ppp)`, `(pp)`, `(p)`, `(mp)`, `(mf)`, `(f)`, `(ff)`, `(fff)`
  - Voices for polyphony:
    - `V1:` - Voice 1 (independent timing)
    - `V2:` - Voice 2
    - `V0:` - Merge voices (continue at max tick)
  - GM instrument support (128 instruments with 400+ name aliases)
  - Auto channel assignment (1-16, skips channel 10 for non-percussion)
  - Event scheduler with tick-based timing (480 ticks per quarter)
  - File playback: `./build/alda_midi song.alda`
  - Interactive REPL with readline support:
    - Non-blocking playback using libuv (REPL remains responsive)
    - Concurrent mode for polyphonic playback of separately-entered parts
    - Commands: `help`, `quit`/`exit`, `list`, `stop`, `panic`, `concurrent`, `sequential`
    - State persistence across inputs
    - Command history with up/down arrows
  - CLI options:
    - `-v, --verbose` - Verbose output
    - `-l, --list` - List MIDI ports
    - `-p, --port N` - Use hardware port by index
    - `-o, --output NAME` - Use port matching name
    - `--virtual NAME` - Create virtual port
    - `--no-sleep` - Disable timing delays (for testing)
    - `-s, --sequential` - Use sequential playback mode
  - Auto-connects to first available MIDI port (falls back to virtual "AldaMIDI" if none)
  - Concurrent playback mode enabled by default for natural polyphony
  - Test suite with integration and quick tests
- **scripts/fluidsynth-gm.py**: Helper script to start FluidSynth with GM SoundFont
  - Platform-specific driver auto-detection (coreaudio/coremidi, pulseaudio/alsa_seq, dsound/winmidi)
  - Environment variables: `ALDA_SF2_DIR`, `ALDA_SF2_DEFAULT`
  - Options: `--list`, `-g/--gain`, `-a/--audio-driver`, `-m/--midi-driver`

### Changed

- **pktpy-midi**: Added proper CLI with getopt_long
  - `-e EXPR` - Execute Python statement
  - `-l, --list` - List available MIDI output ports
  - `-v, --version` - Show version information
  - `-h, --help` - Show help message
  - Existing `--profile` and `--debug` options preserved

- **alda-midi**: Concurrent mode is now the default
  - Multiple parts entered in REPL play simultaneously (polyphony)
  - Use `-s, --sequential` or `sequential` command for sequential mode
  - Sequential mode waits for each input to finish before accepting next
- **alda-midi**: Auto-port selection
  - Automatically connects to first available MIDI port
  - Falls back to virtual port "AldaMIDI" only if no hardware ports exist
  - FluidSynth now works out of the box without `--port`

### Fixed

- **pktpy-midi**: `yield from` now works correctly with async scheduler
  - PocketPy 2.1.6's `yield from` implementation properly delegates to sub-generators
  - Users can now write idiomatic `yield from midi.play(...)` instead of explicit `for` loops
  - Updated documentation and examples to use `yield from` syntax

- **pktpy-midi**: Tempo now affects all durations in the C layer
  - Added `global_tempo_bpm` and `scale_duration_for_tempo()` in C layer
  - `midi.set_tempo(bpm)` now updates both Python constants AND C-layer tempo
  - All durations passed to `MidiOut.note()` and `MidiOut.chord()` are scaled by `120/current_bpm`
  - At 60 BPM, all durations double; at 240 BPM, they halve

- **pktpy-midi**: MidiOut destructor now sends All Notes Off
  - `MidiOut_dtor` sends CC 123 on all 16 channels before freeing the handle
  - Prevents hanging notes when MidiOut objects are garbage collected
  - Matches behavior of `__exit__` context manager

- **alda-midi**: Tempo changes now affect timing during playback
  - Added `ALDA_EVT_TEMPO` event type to schedule tempo changes at tick positions
  - Both synchronous and async playback paths update tempo dynamically
  - `(tempo 120) c4 (tempo 60) d4` now plays d4 at half speed as expected

- **alda-midi**: Panning now emits MIDI CC 10 and accepts 0-127 range
  - Added `alda_schedule_pan()` to schedule pan events at current tick position
  - Fixed input range from 0-100 to 0-127 as documented
  - `(panning 0)` pans left, `(panning 64)` centers, `(panning 127)` pans right

- **alda-midi**: Accidentals `#` and `b` now parse correctly
  - Scanner now recognizes `#` as sharp token instead of comment start
  - Implemented pending token mechanism for suffix accidentals (`cs`, `db`, etc.)
  - Both `c#`/`d#` and `cs`/`db` notations work as documented

- **alda-midi**: Stop command now silences all notes
  - `stop` REPL command now calls `alda_midi_all_notes_off()` before cancelling timers
  - Prevents hanging notes when stopping playback mid-phrase

- **lua-midi**: Tempo now affects all durations in the C layer
  - Added `global_tempo_bpm` and `scale_duration_for_tempo()` in C layer
  - `midi.set_tempo(bpm)` now updates both Lua constants AND C-layer tempo
  - All durations in `midiout:note()`, `midiout:chord()`, `midiout:arpeggio()`, and `midi.sleep()` are scaled
  - At 60 BPM, all durations double; at 240 BPM, they halve

- **lua-midi**: Replay scripts now use wall-clock timing
  - Replaced busy-polling `os.clock()` loop with delta-sleep approach
  - Generated `play_timed()` function uses `midi.sleep()` for accurate timing
  - Playback timing now consistent regardless of CPU load

- **mhs-midi**: Parallel compositions now respect individual note durations
  - Rewrote `performPar` in `MusicPerform.hs` and `Async.hs` to use threads
  - Each parallel branch runs in its own `forkIO` thread with `MVar` completion signaling
  - A quarter note now releases after 480ms even when played alongside a half note

- **mhs-midi**: Exported `.hs` recordings now reproduce original timing
  - Updated `midi_save_hs` to generate `replay` function with delta delays
  - Events sleep for `(currentTime - lastTime)` ms instead of fixed 10ms
  - Playback timing now matches original performance

- **mhs-midi**: Euclidean rhythms now maintain proper step spacing
  - Fixed `euclideanWith` to advance clock by `dur` for both hits and misses
  - Pattern now spans `steps * dur` as expected
  - Pulses evenly distributed instead of collapsed together

- **s7-midi**: Tempo now affects all durations in the C layer
  - Added `global_tempo_bpm` and `scale_duration_for_tempo()` in C layer
  - `(set-tempo! bpm)` now updates both Scheme globals AND C-layer tempo
  - All durations in `midi-note`, `midi-chord`, and `midi-sleep` are scaled
  - At 60 BPM, all durations double; at 240 BPM, they halve

- **s7-midi**: Replay scripts now use proper timing
  - Updated `save-midi` to generate `play-timed` function with delta delays
  - Notes sleep for `(start-ms - last-time)` before each note-on
  - Playback timing now matches original recorded performance

- **s7-midi**: Async `make-note-voice` now sends note-off
  - Fixed `make-note-voice` to track `pending-off` state
  - First call sends note-on and returns duration
  - Second call sends note-off and returns `#f`
  - Async voices no longer leave notes hanging

- **forth-midi**: `bpm:=` now calculates correct quarter note duration
  - Fixed tempo math from `60000 / value / 2` to `60000 / value`
  - At 120 BPM, quarter note is now 500ms as documented (was 250ms)

- **forth-midi**: Recorded sequences now respect gate/articulation
  - Added `gate` parameter to `record_note` and `record_chord`
  - Note-off fires at `dur * gate / 100`, matching live playback
  - Staccato passages are now exported correctly instead of as legato

- **forth-midi**: `play-chord` stack order fixed and uses channel/gate
  - Changed pop order to match documented `( p1...pN N vel dur -- )`
  - Now uses `effective_channel()` instead of hard-coded channel 1
  - Now respects gate setting for articulation consistency

- **Sequential `run()` calls now work correctly**: lua-midi, pktpy-midi
  - Fixed bug where second `run()` call would hang after first completed
  - Root cause: `uv_timer_start` from main thread wasn't noticed by event loop thread
  - Fix: Added `uv_async_send(&sched.wake_async)` after starting timers in `spawn()`
  - Added regression tests: lua-midi (Test 52), pktpy-midi (Test 53), s7-midi (Test 44)
- **pktpy-midi**: Fixed crash in `midi.list_ports()`
  - Root cause: Incorrect use of pocketpy tuple API - was using `py_peek()` and `py_tuple_getitem()` instead of using the return value of `py_newtuple()` directly
  - Fix: Use `py_newtuple()` return value to access tuple slots directly via array indexing

### Added

- **forth-midi**: Implemented documented sequence API words
  - `seq-note` ( time pitch vel dur -- ) - Add note at time using default channel
  - `seq-note-ch` ( time ch pitch vel dur -- ) - Add note with specific channel
  - `seq-add` ( packed-note time -- ) - Add packed note at time
  - Documentation in `docs/forth-midi/sequences.md` now matches implementation

- **forth-midi Loop Control**: Standard Forth loop constructs for structured iteration
  - `do ... loop` - Counted loops with index on return stack
  - `i`, `j` - Access inner and outer loop indices
  - `+loop` - Increment loop by value from stack (e.g., `2 +loop` for step-2)
  - `leave` - Exit loop early
  - `begin ... until` - Loop until flag is true
  - `begin ... while ... repeat` - Loop while flag is true
  - Supports nested loops with proper index tracking
  - 7 new tests for loop control

- **pktpy-midi yield_ms helper**: Cleaner syntax for generator-based voices
  - `midi.ms(n)` - Return milliseconds value for yielding
  - `midi.yield_ms(n)` - Alias for midi.ms
  - Usage: `yield midi.ms(100)` instead of `for ms in midi.wait(100): yield ms`
  - 2 new tests (Tests 54-55)

- **forth-midi MIDI Input**: Polling-based MIDI input with lock-free message queue
  - `midi-input-list` - List available MIDI input ports
  - `midi-input-open` - Open hardware input port by index
  - `midi-input-virtual` - Create virtual MIDI input port
  - `midi-input-close` - Close the current input port
  - `midi-input?` - Check if messages are pending (returns flag)
  - `midi-input@` - Read next message (returns status data1 data2 flag)
  - `midi-input-flush` - Discard all pending messages
  - Lock-free ring buffer for thread-safe callback-to-main-thread communication
  - Single producer (libremidi callback), single consumer (main thread) pattern
  - 6 new tests for MIDI input

- **forth-midi Consistent MIDI Port Naming**: Output words now mirror input naming
  - `midi-output-list` - List available MIDI output ports
  - `midi-output-open` - Open hardware output port by index
  - `midi-output-virtual` - Create virtual MIDI output port
  - `midi-output-open-as` - Create named virtual output port
  - `midi-output-close` - Close the current output port
  - Backward-compatible aliases: `midi-list`, `midi-open-port`, `midi-open`, `midi-open-as`, `midi-close`
  - 4 new tests for MIDI output words

- **mhs-midi Async Scheduler**: Concurrent voice playback using native Haskell threads
  - `spawn name action` - Spawn a new voice with a name, runs in its own thread
  - `run` - Block until all spawned voices complete
  - `stop voiceId` - Stop a specific voice by ID
  - `stopAll` - Stop all active voices
  - `voices` - Get count of active voices
  - `status` - Get scheduler status `(running, count, names)`
  - Async note helpers:
    - `asyncNote` - Play a note asynchronously
    - `asyncChord` - Play a chord asynchronously
    - `asyncPerform` - Perform a Music value asynchronously
  - Pure Haskell implementation using `forkIO`, `MVar`, `ThreadId`
  - No C FFI required for scheduling (unlike other languages that use libuv)
  - Re-exported from `MusicPerform` module for convenience
  - Full test coverage (6 async tests)

- **pktpy-midi Async Tests**: 20 new tests for async scheduler (Tests 28-47)
  - Function existence tests: spawn, run, stop, voices, status
  - Functional tests: spawn/run, voice_id return, multiple voices
  - Control tests: stop specific voice, stop all voices
  - Helper tests: midi.play, midi.play_chord, midi.wait generators
  - Edge case tests: run with no voices, voices count after complete

- **Async Models Documentation**: Cross-language comparison guide
  - New `docs/async-models.md` documenting all five async implementations
  - Comparison tables for threading model, blocking behavior, ease of use
  - Side-by-side code examples showing same musical idea in all languages
  - Guidance on when to use which language for async tasks

- **Example Compositions**: Runnable examples for each language in `examples/`
  - `melody_forth.4th` - Concise notation, word definitions
  - `concurrent_lua.lua` - Coroutine-based concurrent voices
  - `generative_pktpy.py` - Generator patterns with concurrent voices
  - `functional_s7.scm` - Thunk-based functional composition
  - `pure_mhs.hs` - Pure functional Music DSL

- **Non-blocking `poll()` for REPL Responsiveness**: lua-midi, pktpy-midi, s7-midi
  - Added `poll()` function to process ready voices without blocking
  - Returns true/`#t` if voices still active, false/`#f` when done
  - Allows REPL to stay responsive during playback
  - Usage: `while poll() do ... end` (lua), `while midi.poll(): ...` (pktpy), `(let loop () (when (poll) (loop)))` (s7)
  - Full test coverage: lua-midi (Tests 47-51), pktpy-midi (Tests 48-52), s7-midi (Tests 39-43)

- **s7-midi Async Scheduler**: Non-blocking concurrent playback using libuv and thunk-based cooperative multitasking
  - `(spawn thunk [name])` - Create a new voice from a procedure
  - `(run)` - Run scheduler until all voices complete
  - `(stop [voice-id])` - Stop a specific voice or all voices
  - `(voices)` - Get count of active voices
  - `(scheduler-status)` - Get scheduler status alist
  - Thunk-based design: each voice is a procedure that returns:
    - Number (ms to wait before next call)
    - `#f` (voice is complete)
  - Voice builder helpers in prelude:
    - `(make-sequence-voice steps)` - Create voice from `(action . delay)` pairs
    - `(make-note-voice pitch vel dur)` - Single note voice
    - `(make-melody-voice pitches vel dur)` - Sequential notes
    - `(make-chord-voice pitches vel dur)` - Chord voice
    - `(make-repeat-voice thunk n delay)` - Repeat thunk n times
    - `(make-loop-voice thunk delay)` - Loop forever
  - Async playback conveniences:
    - `(async-note pitch [vel] [dur])` - Spawn note voice
    - `(async-chord pitches [vel] [dur])` - Spawn chord voice
    - `(async-melody pitches [vel] [dur])` - Spawn melody voice
  - Up to 16 concurrent voices supported
  - Single-threaded libuv event loop (runs on main thread during `(run)`)
  - Full test coverage (8 new tests for async functionality)

- **pktpy-midi Async Scheduler**: Non-blocking concurrent playback using libuv and Python generators
  - `spawn(func, [name])` - Create a new voice from a generator function
  - `run()` - Run scheduler until all voices complete
  - `stop([voice_id])` - Stop a specific voice or all voices
  - `voices()` - Get count of active voices
  - `status()` - Get scheduler status dict (running, active)
  - Async note helpers for use inside spawned voices:
    - `midi.play(out, pitch, vel, dur, ch)` - Play note (generator)
    - `midi.play_chord(out, pitches, vel, dur, ch)` - Play chord (generator)
    - `midi.play_arp(out, pitches, vel, dur, spacing, ch)` - Arpeggiate (generator)
    - `midi.wait(ms)` - Wait for ms (generator)
  - Up to 16 concurrent voices supported
  - Thread-safe design with dedicated libuv event loop thread
  - `yield from` works correctly in PocketPy 2.1.6 for idiomatic generator delegation
  - See [yield-from.md](docs/pktpy-midi/yield-from.md) for usage examples

- **lua-midi Async Scheduler**: Non-blocking concurrent playback using libuv and Lua coroutines
  - `spawn(func, [name])` - Create a new voice (coroutine) from a function
  - `yield_ms(ms)` - Pause the current voice for N milliseconds (non-blocking)
  - `run()` - Run scheduler until all voices complete
  - `stop([voice_id])` - Stop a specific voice or all voices
  - `voices()` - Get count of active voices
  - `scheduler.status()` - Get detailed scheduler status table
  - Async note helpers for use inside spawned voices:
    - `play(pitch, vel, dur, ch)` - Play note with non-blocking wait
    - `play_chord(pitches, vel, dur, ch)` - Play chord with non-blocking wait
    - `play_arp(pitches, vel, dur, ch)` - Arpeggiate with non-blocking wait
  - Up to 16 concurrent voices supported
  - Thread-safe design with dedicated libuv event loop thread
  - Full test coverage (16 new tests)

- **forth-midi Async Sequence Playback**: Non-blocking playback using libuv
  - `seq-play&` - Play current sequence asynchronously (REPL remains responsive)
  - `seq-loop&` - Play current sequence in a continuous loop
  - `seq-stop` - Stop current sequence's async playback
  - `seq-stop-all` - Stop all async playback
  - `seq-playing?` - Check if any async playback is active (returns flag)
  - `seq-active` - Get count of active async players
  - Up to 8 simultaneous sequences can play concurrently
  - Thread-safe design with dedicated event loop thread
  - Added libuv dependency (`thirdparty/libuv/`)

- **forth-midi Sequence Recording Mode**: Record notes directly into sequences
  - `N seq-start` - Begin recording into sequence N (creates if needed)
  - `N seq-end` - End recording into sequence N
  - Notes, chords, and rests played between seq-start/seq-end are captured with timing
  - Dynamics and duration changes apply during recording

- **forth-midi Script/Batch Mode**: CLI flags for non-interactive execution
  - `--script FILE` - Run FILE and exit (no REPL, returns non-zero on error)
  - `--no-sleep` - Disable all sleep/delay calls (for CI/testing)
  - `--help` - Show usage information
  - Files can still be loaded before REPL with positional arguments

- **forth-midi Sequence Export**: Save sequences to files
  - `seq-write-mid filename` - Write current sequence to standard MIDI file
  - `seq-save filename` - Export sequence as .4th source file with seq-note commands

- **forth-midi Context Introspection**: Debug helper
  - `ctx@` - Display current context defaults (channel, velocity, duration, gate, bpm, pitch, midi status)

- **forth-midi Load Diagnostics**: Better error messages when loading files
  - Errors now show `filename:line:` prefix when loading .4th files
  - Helps locate problems in scripts quickly

- **forth-midi MIDI Port Substring Search**: Enhanced midi-open
  - `midi-open` now searches hardware ports by substring before creating virtual port
  - Example: `midi-open` finds "IAC Driver" if available, otherwise creates virtual port

- **Testing Infrastructure**:
  - C unit tests for forth-midi (`test_forth_midi_unit.c`)
    - Tests for `parse_pitch()` with various note names, sharps, flats, articulation suffixes
    - Tests for packed note encoding/decoding
    - Tests for stack operations
    - Tests for tick/ms timing conversions
  - Golden .4th test programs in `tests/golden/`:
    - `test_basic.4th` - Arithmetic, comparisons, stack ops, conditionals, word definitions
    - `test_notation.4th` - Pitch parsing, octave commands, parameter settings
    - `test_blocks.4th` - Anonymous blocks with `{ } n *`
    - `test_generative.4th` - Random, seed operations
    - `test_sequences.4th` - Sequence creation and manipulation
  - Static library `forth_midi_lib` for linking unit tests

- **MIDI File I/O**: Read and write standard MIDI files (.mid) across all language implementations
  - Common C library `midi_file.h/cpp` wrapping libremidi's reader/writer classes
  - **s7-midi**: `(write-mid filename)` and `(read-mid filename)` functions
  - **pktpy-midi**: `midi.write_mid(filename)` and `midi.read_mid(filename)` functions
  - **lua-midi**: `midi.write_mid(filename)` and `midi.read_mid(filename)` functions
  - **forth-midi**: `write-mid filename` and `read-mid filename` commands
  - Write functions save captured MIDI events to standard Type 1 MIDI files
  - Read functions parse MIDI files and return metadata (tracks, ppqn, tempo, duration, format) and events
  - Round-trip tested: write then read back produces consistent data
  - Integrated with existing recording/capture systems in each language

- **s7-midi Readline Support**: Interactive REPL with readline and autocomplete
  - Tab completion for MIDI functions (`midi-open`, `midi-note`, `midi-chord`, etc.)
  - Tab completion for Scheme keywords (`define`, `lambda`, `let`, `if`, etc.)
  - Tab completion for prelude functions (`major`, `minor`, `scale`, `transpose`, etc.)
  - Tab completion for pitch names (`c4`, `cs4`, `db4`, etc.)
  - Tab completion for dynamics (`ppp` through `fff`) and durations (`whole`, `half`, etc.)
  - Tab completion for scale definitions (`scale-major`, `scale-blues`, etc.)
  - Command history with up/down arrow navigation
  - Graceful fallback to basic REPL if readline unavailable

- **lua-midi Readline Autocomplete**: Added tab completion to existing readline support
  - Tab completion for Lua keywords (`function`, `local`, `if`, `for`, etc.)
  - Tab completion for Lua standard library (`print`, `pairs`, `ipairs`, `math`, etc.)
  - Tab completion for MIDI module functions (`midi.open`, `midi.note`, `midi.scales`, etc.)
  - Tab completion for MidiOut methods (`:note`, `:chord`, `:arpeggio`, etc.)
  - Tab completion for prelude functions (`open`, `n`, `ch`, `major`, `minor`, etc.)
  - Tab completion for pitch names (`c4`, `cs4`, `db4`, etc.)
  - Tab completion for dynamics and durations
  - Tab completion for scale names (`major`, `dorian`, `blues`, etc.)

- **pktpy-midi Readline Support**: Interactive REPL with readline and autocomplete
  - Tab completion for Python keywords (`def`, `class`, `if`, `for`, `import`, etc.)
  - Tab completion for Python builtins (`print`, `len`, `range`, `list`, etc.)
  - Tab completion for MIDI module functions (`midi.open`, `midi.note`, `midi.scale`, etc.)
  - Tab completion for MidiOut methods (`.note`, `.chord`, `.arpeggio`, etc.)
  - Tab completion for pitch names (`midi.c4`, `midi.cs4`, etc.)
  - Tab completion for dynamics (`midi.ppp` through `midi.fff`) and durations
  - Tab completion for scale constants (`midi.SCALE_MAJOR`, `midi.SCALE_BLUES`, etc.)
  - Tab completion for CC constants (`midi.CC_MODULATION`, `midi.CC_VOLUME`, etc.)
  - Command history with up/down arrow navigation
  - Graceful fallback to pocketpy's built-in REPL if readline unavailable

- **mhs-midi Pure Generative Algorithms**: Music.hs now includes pure generative functions
  - Pure PRNG: `nextRandom`, `randomRange`, `randomList` (Linear Congruential Generator)
  - Deterministic: `euclideanRhythm`, `arpUp`, `arpDown`, `arpUpDown`, `retrograde`, `invert`
  - Seed-based random: `shuffle`, `pick`, `pickN`, `randomWalk`, `drunkWalk`
  - All functions are pure (no IO) and use explicit seeds for reproducibility

- **mhs-midi Generative Music**: New generative functions in MidiPerform module
  - Random selection: `pick`, `chance`, `oneOf`, `maybeDo`
  - Random sequences: `scramble`, `randomNote`, `randomMelody`
  - Algorithmic patterns: `walk`, `drunk`, `euclidean`
  - Scales for generative use: `major`, `minor`, `pentatonic`, `blues`, `dorian`, etc.
  - Random number FFI: `midiSeedRandom`, `midiRandom`, `midiRandomRange`

### Changed

- **forth-midi Unified Bracket Syntax**: Sequences and lists now use single `[ ]` syntax
  - Removed `[[` and `]]` list notation - use `[ ]` for both sequences and lists
  - Sequences can now hold plain numbers in addition to pitches, chords, rests, dynamics, and durations
  - Updated generative operations to work with sequences:
    - `shuffle` - Fisher-Yates shuffle on sequence elements
    - `reverse` - reverse sequence in place
    - `pick` - pick random element from sequence
    - `pick-n` - pick n random elements, returns new sequence
    - `invert` - invert pitches around axis
    - `arp-up-down` - create sequence with middle reversed appended
    - `random-walk` - outputs sequence instead of stack values
    - `drunk-walk` - takes scale sequence as input, outputs sequence
    - `weighted-pick` - pick from sequence with value/weight pairs
  - All generative operations maintain backward compatibility with alternatives syntax (`c4|e4|g4`)
  - Removed `LIST_MARKER` constant (no longer needed)
  - Removed dead code: `op_list_begin`, `op_list_end`, `op_list_print`, `op_list_len`

- **forth-midi Named Parameters and New Features**:
  - Named parameter syntax: `vel=100` (one-shot), `ch:=2` (persistent)
  - Gate parameter: `gate!`, `gate@`, `gate=80` - percentage of duration to sound (1-100)
  - New sequence operations:
    - `concat` - concatenate two sequences
  - Polymorphic `transpose` - works on both packed notes and bracket sequences
  - Notes and chords now use effective parameters (support one-shot overrides and gate)

- **mhs-midi Module Refactoring**: Clean separation of concerns
  - `Music.hs` - Pure music theory + DSL (no IO, no MIDI concepts)
    - Event type now `ENote Pitch Velocity Duration` (removed Channel)
    - Constructors: `note`, `rest`, `chord`, `line` (no hardcoded defaults)
    - Combinators: `(+:+)`, `(|||)`, `timesM`
    - Transformations: `transpose`, `louder`, `softer`, `stretch`, `compress`
  - `Midi.hs` - Pure FFI bindings only
  - `MusicPerform.hs` - Bridge from pure Music to MIDI
    - `perform :: Music -> IO ()` - perform on channel 1
    - `performOn :: Channel -> Music -> IO ()` - perform on specific channel
    - Microtonal: `centsToBend`, `pitchBendCents`
  - `MidiPerform.hs` - Immediate MIDI playback with generative functions
    - Direct IO: `note`, `chord`, `rest`, `melody`, `arpeggio`
    - Generative: `pick`, `drunk`, `walk`, `euclidean`, `scramble`
    - REPL helpers: `open`, `close`, `panic`, `ports`
  - Renamed `MidiRepl.hs` to `MidiPerform.hs`
  - Deleted `MidiPrelude.hs` (merged into MusicPerform.hs)

### Added

- **Common Music Theory Library**: Shared C library for music-related utilities
  - `projects/common/music_theory.h` - Header with API and constants
  - `projects/common/music_theory.c` - Implementation
  - Pitch parsing: `music_parse_pitch()` converts note names ("C4", "C#4", "Db5") to MIDI numbers
  - Pitch formatting: `music_pitch_to_name()` converts MIDI numbers back to note names
  - Chord intervals: `CHORD_MAJOR`, `CHORD_MINOR`, `CHORD_DIM`, `CHORD_AUG`, `CHORD_DOM7`, `CHORD_MAJ7`, `CHORD_MIN7`, `CHORD_DIM7`, `CHORD_HALF_DIM7`, `CHORD_SUS2`, `CHORD_SUS4`
  - Chord building: `music_build_chord()` builds chords from root + intervals
  - Scale intervals (35 scales):
    - Diatonic modes: `SCALE_MAJOR`, `SCALE_DORIAN`, `SCALE_PHRYGIAN`, `SCALE_LYDIAN`, `SCALE_MIXOLYDIAN`, `SCALE_MINOR`, `SCALE_LOCRIAN`
    - Minor variants: `SCALE_HARMONIC_MINOR`, `SCALE_MELODIC_MINOR`
    - Pentatonic: `SCALE_PENTATONIC_MAJOR`, `SCALE_PENTATONIC_MINOR`
    - Symmetric: `SCALE_WHOLE_TONE`, `SCALE_CHROMATIC`, `SCALE_DIMINISHED_HW`, `SCALE_DIMINISHED_WH`, `SCALE_AUGMENTED`
    - Bebop: `SCALE_BEBOP_DOMINANT`, `SCALE_BEBOP_MAJOR`, `SCALE_BEBOP_MINOR`
    - Exotic: `SCALE_HUNGARIAN_MINOR`, `SCALE_DOUBLE_HARMONIC`, `SCALE_NEAPOLITAN_MAJOR`, `SCALE_NEAPOLITAN_MINOR`, `SCALE_PHRYGIAN_DOMINANT`, `SCALE_PERSIAN`, `SCALE_ALTERED`, `SCALE_ENIGMATIC`
    - Japanese: `SCALE_HIRAJOSHI`, `SCALE_IN_SEN`, `SCALE_IWATO`, `SCALE_KUMOI`
    - World: `SCALE_BLUES`, `SCALE_EGYPTIAN`, `SCALE_ROMANIAN_MINOR`, `SCALE_SPANISH_8_TONE`
    - Arabic Maqamat (12-TET): `SCALE_MAQAM_HIJAZ`, `SCALE_MAQAM_NAHAWAND`, `SCALE_MAQAM_NIKRIZ`, `SCALE_MAQAM_ATHAR_KURD`, `SCALE_MAQAM_SHAWQ_AFZA`, `SCALE_MAQAM_JIHARKAH`
    - Indian Ragas (12-TET): `SCALE_RAGA_BHAIRAV`, `SCALE_RAGA_TODI`, `SCALE_RAGA_MARWA`, `SCALE_RAGA_PURVI`, `SCALE_RAGA_CHARUKESHI`, `SCALE_RAGA_ASAVARI`, `SCALE_RAGA_BILAWAL`, `SCALE_RAGA_KHAMAJ`, `SCALE_RAGA_KALYAN`, `SCALE_RAGA_BHIMPALASI`, `SCALE_RAGA_DARBARI`
  - Microtonal scales (cents-based, for quarter tones and other microtonal intervals):
    - Arabic Maqamat: `SCALE_MAQAM_BAYATI_CENTS`, `SCALE_MAQAM_RAST_CENTS`, `SCALE_MAQAM_SABA_CENTS`, `SCALE_MAQAM_SIKAH_CENTS`, `SCALE_MAQAM_HUZAM_CENTS`, `SCALE_MAQAM_IRAQ_CENTS`, `SCALE_MAQAM_BASTANIKAR_CENTS`
    - Turkish Makamlar: `SCALE_MAKAM_USSAK_CENTS`, `SCALE_MAKAM_HUSEYNI_CENTS`
    - Indian: `SCALE_SHRUTI_CENTS` (22-shruti scale)
  - Microtonal functions:
    - `MicrotonalNote` struct - combines MIDI note with pitch bend offset
    - `music_cents_to_note()` - convert cents interval to MIDI note + bend
    - `music_cents_to_bend()` - convert cents offset to MIDI pitch bend value
    - `music_build_microtonal_scale()` - build scale with pitch bend values
    - `music_microtonal_degree()` - get nth degree of a microtonal scale
  - Scale functions:
    - `music_build_scale()` - build scale pitches from root + intervals
    - `music_scale_degree()` - get nth degree of a scale (supports extended degrees like 9, 11, 13)
    - `music_in_scale()` - check if a pitch belongs to a scale (any octave)
    - `music_quantize_to_scale()` - snap a pitch to the nearest scale tone
  - Dynamics parsing: `music_parse_dynamics()` converts "ppp"-"fff" to velocity values
  - Duration calculation: `music_duration_ms()` calculates note duration from beats and BPM
  - Constants: `DYN_PPP`-`DYN_FFF`, `DUR_WHOLE`-`DUR_SIXTEENTH`, `NOTE_C0`-`NOTE_G9`, `CC_*`

- **Prelude System**: Extracted prelude code from C files into native language source files
  - `projects/s7-midi/prelude.scm` - Scheme prelude (pitch constants, chord builders, etc.)
  - `projects/lua-midi/prelude.lua` - Lua prelude
  - `projects/pktpy-midi/prelude.py` - Python prelude
  - Preludes are converted to C headers (`*_prelude.h`) at build time

- **scripts/prelude2c.py**: Generic script to convert prelude files to C headers
  - Supports `.scm`, `.lua`, `.py`, `.hs` file extensions
  - Handles language-specific comment stripping
  - Generates C string constants for embedding in interpreters
  - Usage: `./scripts/prelude2c.py projects/s7-midi/prelude.scm`

- **Makefile target**: `make preludes` to regenerate all prelude headers
  - Automatic dependency tracking (only regenerates when source changes)
  - Integrated into build process (runs before `configure`)

### Changed

- **forth-midi**: Refactored to use common library
  - `parse_pitch()` now wraps `music_parse_pitch()` with articulation suffix handling
  - Dynamics functions use `DYN_*` constants from music_theory.h
- **lua-midi**: Refactored to use common `music_parse_pitch()` instead of local implementation
  - Added scale support with 55 12-TET scales and 10 microtonal scales
  - New FFI functions: `midi.build_scale()`, `midi.scale_degree()`, `midi.in_scale()`, `midi.quantize()`
  - New MidiOut method: `m:pitch_bend(cents, [ch])` for microtonal playback
  - Scale intervals in `midi.scales` table (major, minor, dorian, blues, pentatonic, maqam_hijaz, raga_bhairav, etc.)
  - Microtonal scales in `midi.scales_cents` table (maqam_bayati, maqam_rast, shruti, etc.)
  - Helper functions: `scale(root, name)`, `degree(root, name, n)`, `in_scale()`, `quantize()`
  - Helper: `midi.cents_to_note(root, cents)` for microtonal interval calculation
- **s7-midi**: Refactored to use common `music_parse_pitch()` instead of local implementation
  - Added scale support with 55 12-TET scales and 10 microtonal scales
  - New FFI functions: `build-scale`, `scale-degree`, `in-scale?`, `quantize-to-scale`
  - New function: `midi-pitch-bend` for microtonal playback
  - Scale intervals as `scale-*` variables (scale-major, scale-dorian, scale-blues, etc.)
  - Microtonal scales as `scale-*-cents` variables (scale-maqam-bayati-cents, etc.)
  - Helper functions: `(scale root 'name)`, `(degree root 'name n)`, `(quantize pitch root 'name)`
  - `*scales*` alist for name-based scale lookup
  - Helper: `(cents-to-note root cents)` for microtonal interval calculation
- **pktpy-midi**: Refactored to use common `music_parse_pitch()` instead of local implementation
  - Added scale support with 55 12-TET scales and 10 microtonal scales
  - New FFI functions: `midi.build_scale()`, `midi.scale_degree()`, `midi.in_scale()`, `midi.quantize_to_scale()`
  - New MidiOut method: `m.pitch_bend(cents, [channel])` for microtonal playback
  - Scale intervals as `midi.SCALE_*` tuples (SCALE_MAJOR, SCALE_DORIAN, SCALE_BLUES, etc.)
  - Microtonal scales as `midi.SCALE_*_CENTS` tuples (SCALE_MAQAM_BAYATI_CENTS, etc.)
  - Lookup dictionaries: `midi.scales` and `midi.scales_cents` for name-based access
  - Helper functions: `midi.scale(root, name)`, `midi.degree(root, name, n)`, `midi.quantize(pitch, root, name)`
  - Helper: `midi.cents_to_note(root, cents)` for microtonal interval calculation
- **mhs-midi**: Added scale support with 55 12-TET scales and 10 microtonal scales
  - New FFI function: `midi_cents_to_bend` for microtonal pitch bend calculation
  - Scale intervals as `scale*` constants (scaleMajor, scaleDorian, scaleBlues, etc.)
  - Microtonal scales as `scale*Cents` constants (scaleMaqamBayatiCents, etc.)
  - Scale functions: `buildScale`, `scaleDegree`, `inScale`, `quantize`
  - Microtonal helpers: `centsToBend`, `centsToNote`, `pitchBendCents`
  - Pure Haskell implementation for scale operations (no FFI needed for computation)
- **forth-midi**: Added scale support with 49 12-TET scales
  - Scale constant words: `scale-major`, `scale-minor`, `scale-dorian`, `scale-blues`, `scale-pentatonic`, etc.
  - Scale operations: `scale` (build scale pitches), `degree` (get nth degree), `in-scale?` (membership check), `quantize` (snap to scale)
  - Utility: `scales` word lists all 49 available scales
  - Microtonal helpers: `cents>bend` (convert cents to pitch bend), `pb-cents` (send pitch bend in cents)
  - Updated `help` output with Scales and Microtonal sections
  - New documentation: `docs/forth-midi/scales.md`

## [0.1.4]

### Added

- **lua_midi**: New Lua-based MIDI language using [Lua 5.5](https://www.lua.org/)
  - `projects/lua_midi/` - Complete project structure
  - `midi_module.c` - Lua FFI bindings wrapping libremidi
  - Lua 5.5 interpreter (embedded from `thirdparty/lua-5.5.0/`)
  - Core features:
    - `midi.open()` - Virtual port with default name "luaMIDI"
    - `midi.open("name")` - Virtual port with custom name
    - `midi.open(index)` - Hardware port by index
    - `midi.list_ports()` - List available MIDI ports
    - `m:close()` - Close MIDI port
    - `m:is_open()` - Check if port is open
  - Note playing:
    - `m:note(pitch, [vel], [dur], [ch])` - Play single note
    - `m:chord(pitches, [vel], [dur], [ch])` - Play chord
    - `m:arpeggio(pitches, [vel], [dur], [ch])` - Arpeggiate
  - REPL convenience functions (use global `midi._out` port):
    - `open()`, `open("name")`, `open(index)` - Open port and set default
    - `close()` - Close default port
    - `n(pitch, [vel], [dur], [ch])` - Play note on default port
    - `ch(pitches, [vel], [dur], [ch])` - Play chord on default port
    - `arp(pitches, [vel], [dur], [ch])` - Arpeggiate on default port
  - Pitch helpers:
    - `midi.note("C4")` - Parse note names to MIDI numbers
    - `midi.c0`-`midi.c8`, `midi.cs0`-`midi.cs8`, etc. - Pitch constants
    - `midi.transpose(pitch, semitones)`, `midi.octave_up()`, `midi.octave_down()`
  - Dynamics: `midi.ppp` through `midi.fff` (velocity values 16-127)
  - Durations: `midi.whole`, `midi.half`, `midi.quarter`, `midi.eighth`, `midi.sixteenth`, `midi.dotted(dur)`
  - Chord builders: `midi.major()`, `midi.minor()`, `midi.dim()`, `midi.aug()`, `midi.dom7()`, `midi.maj7()`, `midi.min7()`
  - Tempo: `midi.set_tempo(bpm)`, `midi.get_tempo()`, `midi.bpm(tempo)`
  - Timing: `midi.sleep(ms)`, `midi.rest([dur])`
  - Low-level: `m:note_on`, `m:note_off`, `m:cc`, `m:program`, `m:all_notes_off`
  - Test suite with 26 tests

- **s7_midi**: New Scheme-based MIDI language using [s7](https://ccrma.stanford.edu/software/snd/snd/s7.html)
  - `projects/s7_midi/` - Complete project structure
  - `midi_module.c` - s7 FFI bindings wrapping libremidi
  - s7 Scheme interpreter (embedded from `thirdparty/s7/`)
  - Core features:
    - `(midi-open)` - Virtual port with default name "s7MIDI"
    - `(midi-open "name")` - Virtual port with custom name
    - `(midi-open index)` - Hardware port by index
    - `(midi-list-ports)` - List available MIDI ports
    - `(midi-close m)` - Close MIDI port
    - `(midi-out? x)`, `(midi-open? m)` - Type predicates
  - Note playing:
    - `(midi-note m pitch [vel] [dur] [ch])` - Play single note
    - `(midi-chord m pitches [vel] [dur] [ch])` - Play chord
    - `(midi-arpeggio m pitches [vel] [dur] [ch])` - Arpeggiate
  - REPL convenience functions (use global `*midi*` port):
    - `(open)`, `(open "name")`, `(open index)` - Open port and set `*midi*`
    - `(close)` - Close `*midi*` port
    - `(n pitch [vel] [dur] [ch])` - Play note on `*midi*`
    - `(ch pitches [vel] [dur] [ch])` - Play chord on `*midi*`
    - `(arp pitches [vel] [dur] [ch])` - Arpeggiate on `*midi*`
  - Pitch helpers:
    - `(note "C4")` or `(note 'c4)` - Parse note names to MIDI numbers
    - `c0`-`c8`, `cs0`-`cs8`, etc. - Pitch constants
    - `(transpose pitch semitones)`, `(octave-up)`, `(octave-down)`
  - Dynamics: `ppp` through `fff` (velocity values 16-127)
  - Durations: `whole`, `half`, `quarter`, `eighth`, `sixteenth`, `(dotted dur)`
  - Chord builders: `(major root)`, `(minor root)`, `(dim root)`, `(aug root)`, `(dom7 root)`, `(maj7 root)`, `(min7 root)`
  - Tempo: `(set-tempo! bpm)`, `(get-tempo)`, `(bpm tempo)`
  - Timing: `(midi-sleep ms)`, `(rest [dur])`
  - Low-level: `midi-note-on`, `midi-note-off`, `midi-cc`, `midi-program`, `midi-all-notes-off`
  - Test suite with 26 tests

- **pktpy_midi**: New Python-based MIDI language using [PocketPy](https://pocketpy.dev)
  - `projects/pktpy_midi/` - Complete project structure
  - `midi_module.c` - C bindings for libremidi with Pythonic API
  - PocketPy v2.1.6 embedded interpreter
  - Core features:
    - `midi.open()` - Virtual port with default name "pktpyMIDI"
    - `midi.open("name")` - Virtual port with custom name
    - `midi.open(index)` - Hardware port by index
    - `midi.list_ports()` - List available MIDI ports
    - Context manager support (`with midi.open() as m:`)
  - Note playing:
    - `MidiOut.note(pitch, velocity, duration, channel)` - Play single note
    - `MidiOut.chord(pitches, velocity, duration, channel)` - Play chord
    - `MidiOut.arpeggio(pitches, velocity, note_duration, spacing, channel)` - Arpeggiate
  - Pitch helpers:
    - `midi.note("C4")` - Parse note names to MIDI numbers
    - `midi.c4`, `midi.cs4`, etc. - Pitch constants (c0-b8)
    - `midi.transpose(pitch, semitones)` - Transpose pitch
    - `midi.octave_up()`, `midi.octave_down()` - Octave shifts
  - Dynamics: `midi.ppp` through `midi.fff` (velocity values 16-127)
  - Durations: `midi.whole`, `midi.half`, `midi.quarter`, `midi.eighth`, `midi.sixteenth`
  - Chord builders: `midi.major()`, `midi.minor()`, `midi.dim()`, `midi.aug()`, `midi.dom7()`, `midi.maj7()`, `midi.min7()`
  - Tempo: `midi.set_tempo(bpm)`, `midi.get_tempo()`, `midi.bpm(tempo)`
  - Timing: `midi.sleep(ms)`, `midi.rest(duration)`
  - CC helpers: `modulation()`, `volume()`, `pan()`, `sustain()`
  - Low-level: `note_on`, `note_off`, `cc`, `program_change`, `all_notes_off`
  - Test suite with 23 tests

- **mhs-midi REPL**: Interactive Haskell REPL with MIDI FFI support
  - `./scripts/mhs-midi-repl` - Start REPL with caching for fast startup
  - `./scripts/mhs-midi-repl -r File.hs` - Run a Haskell file (interpreted)
  - `./scripts/mhs-midi-compile File.hs -o output` - Compile to standalone executable
  - Enables `import Midi` for interactive MIDI programming
  - Uses MicroHs `xffi_table` extension mechanism for FFI injection
  - Documentation in `docs/mhs-midi/`:
    - API reference with all functions and types
    - Examples from basic to advanced
    - Architecture overview of FFI integration

## [0.1.3]

### Added

- **mhs-midi**: New Haskell-based MIDI language using [MicroHs](https://github.com/augustss/MicroHs)
  - `projects/mhs_midi/` - Complete project structure
  - `midi_ffi.c/h` - C FFI bindings wrapping libremidi
  - `lib/Midi.hs` - High-level Haskell MIDI library with:
    - Pitch names: `c0`-`c8`, `d0`-`d8`, etc. (with sharps `cs4`, `fs4`)
    - Durations: `whole`, `half`, `quarter`, `eighth`, `sixteenth`, `dotted`
    - Dynamics: `ppp`, `pp`, `p`, `mp`, `mf`, `ff`, `fff`
    - Note playing: `play`, `playNote`, `playChord`, `chord`
    - Sequences: `melody`, `arpeggio`, `times`
    - MIDI control: `midiOpen`, `midiOpenVirtual`, `midiClose`, `midiCC`, `midiProgram`
  - Example programs:
    - `hello_midi` - C major scale
    - `chords` - I-IV-V-I chord progression
    - `melody_example` - Twinkle Twinkle Little Star
    - `arpeggio` - Arpeggiated patterns
    - `list_ports` - List available MIDI ports

- **MicroHs Integration**
  - Added `thirdparty/MicroHs/` dependency
  - CMake integration for compiling Haskell to native executables
  - FFI bindings using MicroHs `foreign import ccall`

## [0.1.2]

### Added

- **MIDI Capture System**: Record MIDI events with timing
  - `capture` - Start capturing all MIDI output with timestamps
  - `save-midi filename` - Save captured events as sequence file
  - Pairs note-on/off events, converts timing to ticks
  - Output files can be loaded and transformed with sequence operations

- **Command Recording**: Record interactive sessions
  - `rec` - Start recording input commands
  - `save filename` - Save recorded commands to .4th file
  - Control commands (rec, stop, save, load, capture) are excluded from recordings

- **File Loading**: Execute .4th files
  - `load filename` - Load and execute a Forth file
  - Supports nested loading (up to 8 levels deep)
  - Skips empty lines and Forth comments (`\`)

- **CMake Build System**
  - Converted from Makefile to CMake with CTest integration
  - Makefile frontend preserved for convenience (`make`, `make test`, `make clean`)
  - Executables output to `build/` directory
  - Quick tests labeled for fast feedback during development

- **Comprehensive Test Suite**: 86 tests covering all features
  - Pitch parsing, arithmetic, stack operations
  - Word definitions, conditionals, anonymous blocks
  - Alternatives, probability, packed notes
  - File loading, recording, MIDI capture
  - Error handling

## [0.1.1]

### Added

- **Concise Notation**: Primary note input system
  - Pitch names with octaves: `c4,` `c#4,` `db4,`
  - Comma triggers note playback
  - Chords with parentheses: `(c4 e4 g4),`
  - Rests: `r,`
  - Default parameters: `ch!`, `vel!`, `dur!`
  - Explicit parameters: `ch pitch vel dur,`

- **Dynamics**: Velocity control with musical markings
  - `ppp` (16), `pp` (32), `p` (48), `mp` (64)
  - `mf` (80), `f` (96), `ff` (112), `fff` (127)

- **Articulation**: Note modifiers
  - Staccato: `c4.,` (50% duration)
  - Accent: `c4>,` (+20 velocity)
  - Tenuto: `c4-,` (full duration)

- **Relative Intervals**: Movement from last pitch
  - Semitone steps: `+2,` `-3,`
  - Octave jumps: `^,` `v,`

- **Generative Features**
  - Alternatives: `c4|e4|g4,` (random selection)
  - Probability: `c4 75%,` (chance to play)
  - Random: `random` (push 0-99)

- **Control Flow**
  - Conditionals: `if ... then`, `if ... else ... then`
  - Anonymous blocks: `{ ... } n *`
  - Word definitions: `: name ... ;`
  - Loops: `name n times`

- **Sequences**: MIDI event storage and manipulation
  - Create/select: `seq-new`, `seq`, `seq@`
  - Add notes: `seq-note`, `seq-note-ch`, `seq-add`
  - Playback: `seq-play`, `seq-show`, `seq-length`
  - Transform: `seq-transpose`, `seq-reverse`, `seq-stretch`
  - Clear: `seq-clear`

- **Packed Notes**: Single-value note representation
  - Create: `note` (pitch vel ch dur)
  - Play: `note!`
  - Extract: `pitch@`, `vel@`, `ch@`, `dur@`
  - Transform: `transpose`
  - Debug: `note.`

- **Chord Builders**: Build chords from root note
  - Triads: `major`, `minor`, `dim`, `aug`
  - Sevenths: `dom7`, `maj7`, `min7`
  - Playback: `play-chord`, `chord>seq`, `arp>seq`

- **Duration Constants**
  - `whole` (1920), `half` (960), `quarter` (480)
  - `eighth` (240), `sixteenth` (120)

- **MIDI Output**
  - Virtual port: `midi-open`, `midi-open-as <name>`
  - Hardware ports: `midi-list`, `midi-open-port`, `midi-close`
  - Control: `cc`, `pc`, `pb`, `panic`

- **Timing**
  - Tempo: `bpm!`, `bpm@`
  - Sleep: `ms`

- **Stack Operations**
  - Arithmetic: `+`, `-`, `*`, `/`
  - Stack: `dup`, `drop`, `swap`, `over`, `rot`, `clear`
  - Comparison: `=`, `<`, `>`
  - Bitwise: `and`, `or`, `xor`, `not`
  - Output: `.`, `.s`, `cr`, `space`

## [0.1.0]

### Added

- Initial Forth interpreter (`forth.c`)
- Basic MIDI integration with libremidi
- Note-on/note-off commands (later replaced by concise notation)
- libremidi v5.3.1 dependency
