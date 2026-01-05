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
# mhs-midi-standalone: Self-contained binary with embedded libraries
# ============================================

# Generate embedded library header
find_package(Python3 REQUIRED COMPONENTS Interpreter)

set(EMBEDDED_LIBS_HEADER ${CMAKE_CURRENT_BINARY_DIR}/mhs_embedded_libs.h)

add_custom_command(
    OUTPUT ${EMBEDDED_LIBS_HEADER}
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/scripts/embed_libs.py
        ${EMBEDDED_LIBS_HEADER}
        ${MHS_LIB}
        ${MIDI_LIB_DIR}
        --runtime ${MHS_RUNTIME}
        --header ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi.h
    DEPENDS
        ${CMAKE_SOURCE_DIR}/scripts/embed_libs.py
        ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi.h
    COMMENT "Generating embedded library header with runtime"
    VERBATIM
)

# Create modified mhs.c for standalone (same as mhs_repl.c)
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/mhs_standalone.c
    COMMAND sed -e "s/const struct ffi_entry .xffi_table = imp_table;/\\/\\/ xffi_table defined in midi_ffi_wrappers.c/"
            ${MHS_DIR}/generated/mhs.c > ${CMAKE_CURRENT_BINARY_DIR}/mhs_standalone.c
    DEPENDS ${MHS_DIR}/generated/mhs.c
    COMMENT "Creating mhs_standalone.c with xffi_table removed"
    VERBATIM
)

# Create patched eval.c that renames mhs_fopen to mhs_fopen_orig
# This allows our mhs_ffi_override.c to intercept fopen calls for VFS
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/eval_vfs.c
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/scripts/patch_eval_vfs.py
            ${MHS_RUNTIME}/eval.c
            ${CMAKE_CURRENT_BINARY_DIR}/eval_vfs.c
    DEPENDS ${MHS_RUNTIME}/eval.c ${CMAKE_SOURCE_DIR}/scripts/patch_eval_vfs.py
    COMMENT "Creating eval_vfs.c with mhs_fopen renamed for VFS override"
    VERBATIM
)

# Build the self-contained mhs-midi-standalone executable
# Uses patched eval_vfs.c so mhs_ffi_override.c can intercept fopen
# Uses mhs_midi_standalone_main.c (separate from mhs_midi_main.c)
add_executable(mhs-midi-standalone
    ${CMAKE_CURRENT_BINARY_DIR}/mhs_standalone.c
    ${CMAKE_CURRENT_BINARY_DIR}/eval_vfs.c
    ${CMAKE_CURRENT_SOURCE_DIR}/mhs_midi_standalone_main.c
    ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi_wrappers.c
    ${CMAKE_CURRENT_SOURCE_DIR}/mhs_ffi_override.c
    ${CMAKE_CURRENT_SOURCE_DIR}/midi_ffi.c
    ${CMAKE_CURRENT_SOURCE_DIR}/vfs.c
    ${EMBEDDED_LIBS_HEADER}
)

target_include_directories(mhs-midi-standalone PRIVATE
    ${MHS_RUNTIME}
    ${MHS_RUNTIME}/unix
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_SOURCE_DIR}/thirdparty/libremidi/include
)

target_link_libraries(mhs-midi-standalone PRIVATE
    music_theory
    midi_file
    m
)

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(mhs-midi-standalone PRIVATE
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    if(ALSA_FOUND)
        target_link_libraries(mhs-midi-standalone PRIVATE ${ALSA_LIBRARIES})
    endif()
endif()

add_strip_command(mhs-midi-standalone)
