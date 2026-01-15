# Joy-MIDI - Joy interpreter with MIDI extensions

# Include the core Joy interpreter library
add_subdirectory(joy)

# Main executable
add_executable(joy_midi
    main.c
    joy_midi.c
    midi_primitives.c
    music_context.c
    music_notation.c
)

# Link against joy_core (the standalone Joy interpreter)
target_link_libraries(joy_midi PRIVATE joy_core music_theory libremidi)

# Include paths
target_include_directories(joy_midi PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/joy
    ${CMAKE_SOURCE_DIR}/projects/common
    ${CMAKE_SOURCE_DIR}/thirdparty/libremidi/include
)

# Compiler warnings (GCC/Clang only)
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(joy_midi PRIVATE -Wall -Wextra -Wno-unused-parameter)
endif()

# Platform-specific dependencies for MIDI
if(APPLE)
    target_link_libraries(joy_midi PRIVATE
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(joy_midi PRIVATE ${ALSA_LIBRARIES})
    endif()
endif()

add_strip_command(joy_midi)

# Copy prelude.joy to build directory next to executable
add_custom_command(TARGET joy_midi POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${CMAKE_CURRENT_SOURCE_DIR}/prelude.joy
        $<TARGET_FILE_DIR:joy_midi>/prelude.joy
    COMMENT "Copying prelude.joy to build directory"
)

install(TARGETS joy_midi RUNTIME DESTINATION bin)
install(FILES prelude.joy DESTINATION bin)
