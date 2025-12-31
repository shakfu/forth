# Core MIDI Forth source files (without main.c for testing)
set(FORTH_MIDI_CORE_SOURCES
    context.c
    stack.c
    primitives.c
    midi_core.c
    notation.c
    generative.c
    sequences.c
    packed_notes.c
    patterns.c
    scales.c
    recording.c
    dictionary.c
    interpreter.c
    readline_comp.c
)

# Create a static library for unit testing
add_library(forth_midi_lib STATIC ${FORTH_MIDI_CORE_SOURCES})
target_compile_options(forth_midi_lib PRIVATE -Wall -Wextra)
target_link_libraries(forth_midi_lib PUBLIC music_theory midi_file)

# Main executable
add_executable(forth_midi main.c)
target_link_libraries(forth_midi PRIVATE forth_midi_lib)

target_compile_options(forth_midi PRIVATE -Wall -Wextra)

# Link with readline
find_library(READLINE_LIBRARY readline)
if(READLINE_LIBRARY)
    target_link_libraries(forth_midi PRIVATE ${READLINE_LIBRARY})
    target_link_libraries(forth_midi_lib PUBLIC ${READLINE_LIBRARY})
else()
    message(FATAL_ERROR "readline library not found")
endif()

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(forth_midi_lib PUBLIC
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(forth_midi_lib PUBLIC ${ALSA_LIBRARIES})
    endif()
endif()

add_strip_command(forth_midi)

install(TARGETS forth_midi RUNTIME DESTINATION bin)
