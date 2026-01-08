# CMakeLists.txt for mhs-midi
# Builds MIDI examples using MicroHs and libremidi

# Paths
set(MHS_DIR ${CMAKE_SOURCE_DIR}/thirdparty/MicroHs)
set(MHS_COMPILER ${MHS_DIR}/bin/mhs)
set(MHS_RUNTIME ${MHS_DIR}/src/runtime)
set(MHS_LIB ${MHS_DIR}/lib)
set(MIDI_LIB_DIR ${CMAKE_CURRENT_SOURCE_DIR}/lib)
set(EXAMPLES_DIR ${CMAKE_CURRENT_SOURCE_DIR}/examples)

# Build MicroHs compiler if it doesn't exist
if(NOT EXISTS ${MHS_COMPILER})
    message(STATUS "Building MicroHs compiler...")
    if(WIN32)
        execute_process(
            COMMAND nmake -f Makefile.windows
            WORKING_DIRECTORY ${MHS_DIR}
            RESULT_VARIABLE MHS_BUILD_RESULT
        )
    else()
        execute_process(
            COMMAND make
            WORKING_DIRECTORY ${MHS_DIR}
            RESULT_VARIABLE MHS_BUILD_RESULT
        )
    endif()
    if(NOT MHS_BUILD_RESULT EQUAL 0)
        message(FATAL_ERROR "Failed to build MicroHs compiler")
    endif()
endif()

# Verify MicroHs compiler exists after build attempt
if(NOT EXISTS ${MHS_COMPILER})
    message(FATAL_ERROR "MicroHs compiler not found at ${MHS_COMPILER}")
endif()

# Create a static library for the MIDI FFI
add_library(midi_ffi STATIC midi_ffi.c)
target_include_directories(midi_ffi PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/thirdparty/libremidi/include
)
target_link_libraries(midi_ffi PUBLIC music_theory midi_file)

# Platform-specific dependencies for midi_ffi
if(APPLE)
    target_link_libraries(midi_ffi PRIVATE
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(midi_ffi PRIVATE ${ALSA_LIBRARIES})
    endif()
endif()

# Function to create an mhs-midi executable target
# NAME: target name for the executable
# MODULE: Haskell module name (e.g., HelloMidi)
function(add_mhs_midi_example NAME MODULE)
    set(HS_SOURCE ${EXAMPLES_DIR}/${MODULE}.hs)
    set(C_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${NAME}.c)
    set(EXE_OUTPUT ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${NAME})

    # Custom command to compile Haskell to C using MicroHs
    add_custom_command(
        OUTPUT ${C_OUTPUT}
        COMMAND ${CMAKE_COMMAND} -E env "MHSDIR=${MHS_DIR}"
            ${MHS_COMPILER}
            -i${MIDI_LIB_DIR}
            -i${EXAMPLES_DIR}
            ${MODULE}
            -C
            -o${C_OUTPUT}
        DEPENDS ${HS_SOURCE} ${MIDI_LIB_DIR}/Midi.hs ${MIDI_LIB_DIR}/Music.hs ${MIDI_LIB_DIR}/MusicPerform.hs ${MIDI_LIB_DIR}/Async.hs
        WORKING_DIRECTORY ${MHS_DIR}
        COMMENT "Compiling ${MODULE}.hs with MicroHs"
        VERBATIM
    )

    # Create the executable from generated C code
    add_executable(${NAME} ${C_OUTPUT})

    # Include directories for MicroHs runtime
    target_include_directories(${NAME} PRIVATE
        ${MHS_RUNTIME}
        ${MHS_RUNTIME}/unix
        ${CMAKE_CURRENT_SOURCE_DIR}
    )

    # Link with MicroHs runtime, FFI, and libremidi
    target_sources(${NAME} PRIVATE
        ${MHS_RUNTIME}/eval.c
        ${MHS_RUNTIME}/main.c
    )

    target_link_libraries(${NAME} PRIVATE
        midi_ffi
        m
    )

    # Platform-specific
    if(APPLE)
        target_link_libraries(${NAME} PRIVATE
            "-framework CoreMIDI"
            "-framework CoreFoundation"
            "-framework CoreAudio"
        )
    elseif(UNIX)
        find_package(ALSA)
        if(ALSA_FOUND)
            target_link_libraries(${NAME} PRIVATE ${ALSA_LIBRARIES})
        endif()
    endif()

    # Ensure midi_ffi is built before we try to link
    add_dependencies(${NAME} midi_ffi)

    # Strip in Release mode
    add_strip_command(${NAME})
endfunction()

# Build example executables (target_name, ModuleName)
# NOTE: Dependencies are added between targets to prevent parallel MicroHs
# compilations, which corrupt the shared .mhscache file
if(BUILD_EXAMPLES)
add_mhs_midi_example(hello_midi HelloMidi)
add_mhs_midi_example(chords Chords)
add_mhs_midi_example(melody_example Melody)
add_mhs_midi_example(arpeggio Arpeggio)
add_mhs_midi_example(list_ports ListPorts)
add_mhs_midi_example(async_test AsyncTest)

# Serialize MicroHs compilations to avoid .mhscache corruption
# Each target depends on the previous one
add_dependencies(chords hello_midi)
add_dependencies(melody_example chords)
add_dependencies(arpeggio melody_example)
add_dependencies(list_ports arpeggio)
add_dependencies(async_test list_ports)

# Custom target to build all mhs-midi examples
add_custom_target(mhs-midi-examples
    DEPENDS hello_midi chords melody_example arpeggio list_ports async_test
)
endif()

# ============================================
# mhs-midi: Interactive REPL with MIDI support
# ============================================

# Strip xffi_table definition from mhs.c to avoid multiple definition error
# Our midi_ffi_wrappers.c provides its own xffi_table with MIDI FFI functions
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/mhs_repl.c
    COMMAND sed -e "s/const struct ffi_entry .xffi_table = imp_table;/\\/\\/ xffi_table defined in midi_ffi_wrappers.c/"
            ${MHS_DIR}/generated/mhs.c > ${CMAKE_CURRENT_BINARY_DIR}/mhs_repl.c
    DEPENDS ${MHS_DIR}/generated/mhs.c
    COMMENT "Creating mhs_repl.c with xffi_table removed"
    VERBATIM
)

# Build the mhs-midi REPL executable
add_executable(mhs-midi
    ${CMAKE_CURRENT_BINARY_DIR}/mhs_repl.c
    ${CMAKE_CURRENT_SOURCE_DIR}/mhs_midi_main.c
    ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi_wrappers.c
    ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi.c
    ${MHS_RUNTIME}/eval.c
)

target_include_directories(mhs-midi PRIVATE
    ${MHS_RUNTIME}
    ${MHS_RUNTIME}/unix
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/thirdparty/libremidi/include
)

target_link_libraries(mhs-midi PRIVATE
    music_theory
    midi_file
    m
)

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(mhs-midi PRIVATE
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(mhs-midi PRIVATE ${ALSA_LIBRARIES})
    endif()
endif()

add_strip_command(mhs-midi)

# ============================================
# mhs-midi standalone variants
# ============================================
#
# Builds 3 standalone variants:
#   - mhs-midi-src:       source embedding (default)
#   - mhs-midi-src-zstd:  source embedding + zstd compression
#   - mhs-midi-pkg:       precompiled .pkg embedding (fast startup)

find_package(Python3 REQUIRED COMPONENTS Interpreter)
set(ZSTD_DIR ${CMAKE_SOURCE_DIR}/thirdparty/zstd-1.5.7)
set(MHS_VERSION "0.15.2.0")

# ============================================
# Shared generated files for all standalone variants
# ============================================

# Create modified mhs.c for standalone (removes xffi_table definition)
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/mhs_standalone.c
    COMMAND sed -e "s/const struct ffi_entry .xffi_table = imp_table;/\\/\\/ xffi_table defined in midi_ffi_wrappers.c/"
            ${MHS_DIR}/generated/mhs.c > ${CMAKE_CURRENT_BINARY_DIR}/mhs_standalone.c
    DEPENDS ${MHS_DIR}/generated/mhs.c
    COMMENT "Creating mhs_standalone.c with xffi_table removed"
    VERBATIM
)

# Create patched eval.c that renames mhs_fopen to mhs_fopen_orig for VFS override
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/eval_vfs.c
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/scripts/patch_eval_vfs.py
            ${MHS_RUNTIME}/eval.c
            ${CMAKE_CURRENT_BINARY_DIR}/eval_vfs.c
    DEPENDS ${MHS_RUNTIME}/eval.c ${CMAKE_SOURCE_DIR}/scripts/patch_eval_vfs.py
    COMMENT "Creating eval_vfs.c with mhs_fopen renamed for VFS override"
    VERBATIM
)

# Build zstd embedding tool (shared by src-zstd and pkg-zstd)
set(EMBED_LIBS_ZSTD ${CMAKE_BINARY_DIR}/embed_libs_zstd)
add_custom_command(
    OUTPUT ${EMBED_LIBS_ZSTD}
    COMMAND ${CMAKE_C_COMPILER} -O2
        -o ${EMBED_LIBS_ZSTD}
        ${CMAKE_SOURCE_DIR}/scripts/embed_libs_zstd.c
        ${ZSTD_DIR}/zstd.c
        -I${ZSTD_DIR}
        -lpthread
    DEPENDS ${CMAKE_SOURCE_DIR}/scripts/embed_libs_zstd.c
    COMMENT "Building embed_libs_zstd tool"
    VERBATIM
)

# ============================================
# Generate embedded headers for source-based variants
# ============================================

# mhs-midi-src: uncompressed source embedding
# Uses mhs_embedded_libs.h (expected by vfs.c default mode)
set(EMBEDDED_SRC_HEADER ${CMAKE_CURRENT_BINARY_DIR}/mhs_embedded_libs.h)
add_custom_command(
    OUTPUT ${EMBEDDED_SRC_HEADER}
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/scripts/embed_libs.py
        ${EMBEDDED_SRC_HEADER}
        ${MHS_LIB}
        ${MIDI_LIB_DIR}
        --runtime ${MHS_RUNTIME}
        --header ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi.h
        --lib ${CMAKE_BINARY_DIR}/thirdparty/libremidi/liblibremidi.a
        --lib ${CMAKE_BINARY_DIR}/projects/mhs-midi/libmidi_ffi.a
        --lib ${CMAKE_BINARY_DIR}/projects/common/libmusic_theory.a
        --lib ${CMAKE_BINARY_DIR}/projects/common/libmidi_file.a
    DEPENDS
        ${CMAKE_SOURCE_DIR}/scripts/embed_libs.py
        ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi.h
        libremidi
        midi_ffi
        music_theory
        midi_file
    COMMENT "Generating mhs_embedded_src.h (uncompressed source)"
    VERBATIM
)

# mhs-midi-src-zstd: compressed source embedding
# Uses mhs_embedded_zstd.h (expected by vfs.c VFS_USE_ZSTD mode)
set(EMBEDDED_SRC_ZSTD_HEADER ${CMAKE_CURRENT_BINARY_DIR}/mhs_embedded_zstd.h)
add_custom_command(
    OUTPUT ${EMBEDDED_SRC_ZSTD_HEADER}
    COMMAND ${EMBED_LIBS_ZSTD}
        ${EMBEDDED_SRC_ZSTD_HEADER}
        ${MHS_LIB}
        ${MIDI_LIB_DIR}
        --runtime ${MHS_RUNTIME}
        --header ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi.h
        --lib ${CMAKE_BINARY_DIR}/thirdparty/libremidi/liblibremidi.a
        --lib ${CMAKE_BINARY_DIR}/projects/mhs-midi/libmidi_ffi.a
        --lib ${CMAKE_BINARY_DIR}/projects/common/libmusic_theory.a
        --lib ${CMAKE_BINARY_DIR}/projects/common/libmidi_file.a
    DEPENDS
        ${EMBED_LIBS_ZSTD}
        ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi.h
        libremidi
        midi_ffi
        music_theory
        midi_file
    COMMENT "Generating mhs_embedded_src_zstd.h (compressed source)"
    VERBATIM
)

# ============================================
# Generate embedded headers for package-based variants
# ============================================

# Build base.pkg locally (keeps source tree clean)
set(LOCAL_MCABAL "${CMAKE_BINARY_DIR}/mcabal")
set(BASE_DIR "${LOCAL_MCABAL}/mhs-${MHS_VERSION}")
set(BASE_PKG "${BASE_DIR}/packages/base-${MHS_VERSION}.pkg")
set(LOCAL_MCABAL_STAMP "${LOCAL_MCABAL}/.installed")
set(MHS_BASE_BUILD "${CMAKE_BINARY_DIR}/mhs-base-src")
set(DIST_BASE_PKG "${MHS_BASE_BUILD}/dist-mcabal/base-${MHS_VERSION}.pkg")

add_custom_command(
    OUTPUT ${DIST_BASE_PKG}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${MHS_BASE_BUILD}
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${MHS_LIB} ${MHS_BASE_BUILD}
    COMMAND ${CMAKE_COMMAND} -E chdir ${MHS_BASE_BUILD}
        ${CMAKE_COMMAND} -E env
        "PATH=${MHS_DIR}/bin:$ENV{PATH}"
        "MHSDIR=${MHS_DIR}"
        ${MHS_DIR}/bin/mcabal build
    DEPENDS ${MHS_COMPILER}
    COMMENT "Building base-${MHS_VERSION}.pkg in build/mhs-base-src"
    VERBATIM
)

# Install base.pkg locally and copy runtime files
set(MHS_DATA_DIR "${BASE_DIR}/packages/mhs-${MHS_VERSION}/data")
add_custom_command(
    OUTPUT ${LOCAL_MCABAL_STAMP}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${LOCAL_MCABAL}/bin
    COMMAND ${CMAKE_COMMAND} -E make_directory ${BASE_DIR}/packages
    COMMAND ${CMAKE_COMMAND} -E make_directory ${MHS_DATA_DIR}/src/runtime
    COMMAND ${CMAKE_COMMAND} -E copy ${MHS_DIR}/bin/mhs ${LOCAL_MCABAL}/bin/
    COMMAND ${CMAKE_COMMAND} -E copy ${MHS_DIR}/bin/cpphs ${LOCAL_MCABAL}/bin/
    COMMAND ${CMAKE_COMMAND} -E copy ${MHS_DIR}/bin/mcabal ${LOCAL_MCABAL}/bin/
    COMMAND ${CMAKE_COMMAND} -E env "MHSDIR=${MHS_DIR}"
        ${MHS_COMPILER} -Q ${DIST_BASE_PKG} ${BASE_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${MHS_RUNTIME} ${MHS_DATA_DIR}/src/runtime
    COMMAND ${CMAKE_COMMAND} -E copy ${MHS_DIR}/targets.conf ${MHS_DATA_DIR}/
    COMMAND ${CMAKE_COMMAND} -E touch ${LOCAL_MCABAL_STAMP}
    DEPENDS ${MHS_COMPILER} ${DIST_BASE_PKG}
    COMMENT "Installing MicroHs packages to build/mcabal"
    VERBATIM
)
add_custom_target(mhs-local-install DEPENDS ${LOCAL_MCABAL_STAMP})

# Generate music.pkg
set(MUSIC_PKG ${CMAKE_CURRENT_BINARY_DIR}/music-0.1.0.pkg)
add_custom_command(
    OUTPUT ${MUSIC_PKG}
    COMMAND ${CMAKE_COMMAND} -E env "MHSDIR=${MHS_DIR}"
        ${MHS_COMPILER} -Pmusic-0.1.0 -i${MIDI_LIB_DIR}
        Async Midi MidiPerform Music MusicPerform
        -o ${MUSIC_PKG}
    DEPENDS
        ${MIDI_LIB_DIR}/Async.hs
        ${MIDI_LIB_DIR}/Midi.hs
        ${MIDI_LIB_DIR}/MidiPerform.hs
        ${MIDI_LIB_DIR}/Music.hs
        ${MIDI_LIB_DIR}/MusicPerform.hs
    WORKING_DIRECTORY ${MHS_DIR}
    COMMENT "Building music-0.1.0.pkg"
    VERBATIM
)

# mhs-midi-pkg: uncompressed package embedding
# Uses mhs_embedded_pkgs.h (expected by vfs.c VFS_USE_PKG mode)
set(EMBEDDED_PKG_HEADER ${CMAKE_CURRENT_BINARY_DIR}/mhs_embedded_pkgs.h)
add_custom_command(
    OUTPUT ${EMBEDDED_PKG_HEADER}
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/scripts/embed_pkgs.py
        ${EMBEDDED_PKG_HEADER}
        --base-pkg ${BASE_PKG}
        --music-pkg ${MUSIC_PKG}
        --base-dir ${BASE_DIR}
        --lib lib/liblibremidi.a=${CMAKE_BINARY_DIR}/thirdparty/libremidi/liblibremidi.a
        --lib lib/libmidi_ffi.a=${CMAKE_BINARY_DIR}/projects/mhs-midi/libmidi_ffi.a
        --lib lib/libmusic_theory.a=${CMAKE_BINARY_DIR}/projects/common/libmusic_theory.a
        --lib lib/libmidi_file.a=${CMAKE_BINARY_DIR}/projects/common/libmidi_file.a
        --header include/midi_ffi.h=${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi.h
    DEPENDS
        ${CMAKE_SOURCE_DIR}/scripts/embed_pkgs.py
        ${LOCAL_MCABAL_STAMP}
        ${MUSIC_PKG}
        ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi.h
        libremidi
        midi_ffi
        music_theory
        midi_file
    COMMENT "Generating mhs_embedded_pkg.h (uncompressed packages)"
    VERBATIM
)

# mhs-midi-pkg-zstd: compressed package embedding (smallest + fast startup)
# Uses mhs_embedded_pkgs_zstd.h (expected by vfs.c VFS_USE_PKG + VFS_USE_ZSTD mode)
set(EMBEDDED_PKG_ZSTD_HEADER ${CMAKE_CURRENT_BINARY_DIR}/mhs_embedded_pkgs_zstd.h)
add_custom_command(
    OUTPUT ${EMBEDDED_PKG_ZSTD_HEADER}
    COMMAND ${EMBED_LIBS_ZSTD}
        ${EMBEDDED_PKG_ZSTD_HEADER}
        --pkg-mode
        --pkg packages/base-${MHS_VERSION}.pkg=${BASE_PKG}
        --pkg packages/music-0.1.0.pkg=${MUSIC_PKG}
        --txt-dir ${BASE_DIR}
        --music-modules music-0.1.0.pkg:Async,Midi,MidiPerform,Music,MusicPerform
        --lib ${CMAKE_BINARY_DIR}/thirdparty/libremidi/liblibremidi.a
        --lib ${CMAKE_BINARY_DIR}/projects/mhs-midi/libmidi_ffi.a
        --lib ${CMAKE_BINARY_DIR}/projects/common/libmusic_theory.a
        --lib ${CMAKE_BINARY_DIR}/projects/common/libmidi_file.a
        --header ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi.h
    DEPENDS
        ${EMBED_LIBS_ZSTD}
        ${LOCAL_MCABAL_STAMP}
        ${MUSIC_PKG}
        ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi.h
        libremidi
        midi_ffi
        music_theory
        midi_file
    COMMENT "Generating mhs_embedded_pkgs_zstd.h (compressed packages)"
    VERBATIM
)

# ============================================
# Function to create a standalone variant target
# ============================================

function(add_mhs_standalone_variant TARGET_NAME EMBEDDED_HEADER)
    set(options USE_ZSTD USE_PKG)
    cmake_parse_arguments(VARIANT "${options}" "" "" ${ARGN})

    set(VARIANT_SOURCES
        ${CMAKE_CURRENT_BINARY_DIR}/mhs_standalone.c
        ${CMAKE_CURRENT_BINARY_DIR}/eval_vfs.c
        ${CMAKE_CURRENT_SOURCE_DIR}/mhs_midi_standalone_main.c
        ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi_wrappers.c
        ${CMAKE_CURRENT_SOURCE_DIR}/mhs_ffi_override.c
        ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi.c
        ${CMAKE_CURRENT_SOURCE_DIR}/vfs.c
        ${EMBEDDED_HEADER}
    )

    if(VARIANT_USE_ZSTD)
        list(APPEND VARIANT_SOURCES ${ZSTD_DIR}/zstddeclib.c)
    endif()

    add_executable(${TARGET_NAME} ${VARIANT_SOURCES})

    target_include_directories(${TARGET_NAME} PRIVATE
        ${MHS_RUNTIME}
        ${MHS_RUNTIME}/unix
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_CURRENT_BINARY_DIR}
        ${CMAKE_SOURCE_DIR}/thirdparty/libremidi/include
    )

    if(VARIANT_USE_PKG)
        target_compile_definitions(${TARGET_NAME} PRIVATE VFS_USE_PKG)
    endif()

    if(VARIANT_USE_ZSTD)
        target_compile_definitions(${TARGET_NAME} PRIVATE VFS_USE_ZSTD)
        target_include_directories(${TARGET_NAME} PRIVATE ${ZSTD_DIR})
    endif()

    target_link_libraries(${TARGET_NAME} PRIVATE
        music_theory
        midi_file
        m
    )

    # Platform-specific dependencies
    if(APPLE)
        target_link_libraries(${TARGET_NAME} PRIVATE
            "-framework CoreMIDI"
            "-framework CoreFoundation"
            "-framework CoreAudio"
        )
    elseif(UNIX)
        if(ALSA_FOUND)
            target_link_libraries(${TARGET_NAME} PRIVATE ${ALSA_LIBRARIES})
        endif()
    endif()

    add_strip_command(${TARGET_NAME})
endfunction()

# ============================================
# Create all standalone variant targets
# ============================================

# mhs-midi-src: source embedding (default standalone behavior)
add_mhs_standalone_variant(mhs-midi-src ${EMBEDDED_SRC_HEADER})

# mhs-midi-src-zstd: source embedding + zstd compression
add_mhs_standalone_variant(mhs-midi-src-zstd ${EMBEDDED_SRC_ZSTD_HEADER} USE_ZSTD)

# mhs-midi-pkg: package embedding (fast startup)
add_mhs_standalone_variant(mhs-midi-pkg ${EMBEDDED_PKG_HEADER} USE_PKG)

# mhs-midi-pkg-zstd: compressed package embedding (smallest + fast startup)
add_mhs_standalone_variant(mhs-midi-pkg-zstd ${EMBEDDED_PKG_ZSTD_HEADER} USE_PKG USE_ZSTD)

# Backward compatibility: mhs-midi-standalone as alias to mhs-midi-src
add_custom_target(mhs-midi-standalone DEPENDS mhs-midi-src)

# Convenience target to build all variants
add_custom_target(mhs-midi-all
    DEPENDS mhs-midi mhs-midi-src mhs-midi-src-zstd mhs-midi-pkg mhs-midi-pkg-zstd
)

# ============================================
# Installation rules
# ============================================

# Install binaries
install(TARGETS mhs-midi mhs-midi-src mhs-midi-src-zstd mhs-midi-pkg mhs-midi-pkg-zstd
    RUNTIME DESTINATION bin
    COMPONENT mhs-midi
)

# Install MicroHs library files (needed for compilation)
install(DIRECTORY ${MHS_LIB}/
    DESTINATION share/mhs-midi/MicroHs/lib
    COMPONENT mhs-midi
    FILES_MATCHING PATTERN "*.hs" PATTERN "*.hs-boot"
)

# Install MicroHs runtime files (needed for compilation to executable)
install(DIRECTORY ${MHS_RUNTIME}/
    DESTINATION share/mhs-midi/MicroHs/src/runtime
    COMPONENT mhs-midi
    FILES_MATCHING PATTERN "*.c" PATTERN "*.h"
)

# Install MIDI Haskell library files
install(DIRECTORY ${MIDI_LIB_DIR}/
    DESTINATION share/mhs-midi/lib
    COMPONENT mhs-midi
    FILES_MATCHING PATTERN "*.hs"
)
