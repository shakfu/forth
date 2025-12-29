# Modular MIDI Forth source files
set(FORTH_MIDI_SOURCES
    main.c
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

add_executable(forth_midi ${FORTH_MIDI_SOURCES})

target_compile_options(forth_midi PRIVATE -Wall -Wextra)

# Link with libremidi and music_theory
target_link_libraries(forth_midi PRIVATE libremidi music_theory)

# Link with readline
find_library(READLINE_LIBRARY readline)
if(READLINE_LIBRARY)
    target_link_libraries(forth_midi PRIVATE ${READLINE_LIBRARY})
else()
    message(FATAL_ERROR "readline library not found")
endif()

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(forth_midi PRIVATE
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(forth_midi PRIVATE ${ALSA_LIBRARIES})
    endif()
endif()

add_strip_command(forth_midi)

install(TARGETS forth_midi RUNTIME DESTINATION bin)
