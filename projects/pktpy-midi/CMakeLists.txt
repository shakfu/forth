add_executable(pktpy_midi
    main.c
    midi_module.c
    scheduler.c
    ${CMAKE_SOURCE_DIR}/thirdparty/pocketpy/pocketpy.c
)

target_include_directories(pktpy_midi PRIVATE
    ${CMAKE_SOURCE_DIR}/thirdparty/libuv/include
    ${CMAKE_SOURCE_DIR}/thirdparty/pocketpy
)

# Compiler warnings (GCC/Clang only)
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(pktpy_midi PRIVATE -Wall -Wextra -Wno-unused-parameter)
endif()

# Check for readline (optional on Windows)
find_library(READLINE_LIB readline)
if(READLINE_LIB)
    target_compile_definitions(pktpy_midi PRIVATE USE_READLINE)
    target_link_libraries(pktpy_midi PRIVATE ${READLINE_LIB})
else()
    if(NOT WIN32)
        message(FATAL_ERROR "readline library not found (install libreadline-dev on Linux)")
    endif()
endif()

# Link with music_theory, midi_file, and libuv for async scheduler
target_link_libraries(pktpy_midi PRIVATE music_theory midi_file uv_a)

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
