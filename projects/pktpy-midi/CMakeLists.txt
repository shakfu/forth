add_executable(pktpy_midi
    main.c
    midi_module.c
    pocketpy.c
)

target_compile_options(pktpy_midi PRIVATE -Wall -Wextra -Wno-unused-parameter)

# Link with libremidi
target_link_libraries(pktpy_midi PRIVATE libremidi)

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
