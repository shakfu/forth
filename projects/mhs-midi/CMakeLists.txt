# CMakeLists.txt for mhs-midi
# Builds MIDI examples using MicroHs and libremidi

# Paths
set(MHS_DIR ${CMAKE_SOURCE_DIR}/thirdparty/MicroHs)
set(MHS_COMPILER ${MHS_DIR}/bin/mhs)
set(MHS_RUNTIME ${MHS_DIR}/src/runtime)
set(MHS_LIB ${MHS_DIR}/lib)
set(MIDI_LIB_DIR ${CMAKE_CURRENT_SOURCE_DIR}/lib)
set(EXAMPLES_DIR ${CMAKE_CURRENT_SOURCE_DIR}/examples)

# Check that MicroHs compiler exists
if(NOT EXISTS ${MHS_COMPILER})
    message(FATAL_ERROR "MicroHs compiler not found at ${MHS_COMPILER}. Please build MicroHs first.")
endif()

# Create a static library for the MIDI FFI
add_library(midi_ffi STATIC midi_ffi.c)
target_include_directories(midi_ffi PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/thirdparty/libremidi/include
)
target_link_libraries(midi_ffi PRIVATE libremidi)

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
        DEPENDS ${HS_SOURCE} ${MIDI_LIB_DIR}/Midi.hs ${MIDI_LIB_DIR}/MidiRepl.hs ${MIDI_LIB_DIR}/Music.hs
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
        libremidi
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
add_mhs_midi_example(hello_midi HelloMidi)
add_mhs_midi_example(chords Chords)
add_mhs_midi_example(melody_example Melody)
add_mhs_midi_example(arpeggio Arpeggio)
add_mhs_midi_example(list_ports ListPorts)

# Custom target to build all mhs-midi examples
add_custom_target(mhs-midi-examples
    DEPENDS hello_midi chords melody_example arpeggio list_ports
)

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
    libremidi
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
