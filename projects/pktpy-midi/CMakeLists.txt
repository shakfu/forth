add_executable(pktpy_midi
    main.c
    midi_module.c
    pocketpy.c
)

target_compile_options(pktpy_midi PRIVATE -Wall -Wextra -Wno-unused-parameter)

# Check for readline
find_library(READLINE_LIB readline)
if(READLINE_LIB)
    target_compile_definitions(pktpy_midi PRIVATE USE_READLINE)
    target_link_libraries(pktpy_midi PRIVATE ${READLINE_LIB})
endif()

# Link with libremidi and music_theory
target_link_libraries(pktpy_midi PRIVATE libremidi music_theory)

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(pktpy_midi PRIVATE
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(pktpy_midi PRIVATE ${ALSA_LIBRARIES})
    endif()
endif()

add_strip_command(pktpy_midi)

install(TARGETS pktpy_midi RUNTIME DESTINATION bin)
