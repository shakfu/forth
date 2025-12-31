set(S7_DIR ${CMAKE_SOURCE_DIR}/thirdparty/s7)

add_executable(s7_midi
    main.c
    midi_module.c
    ${S7_DIR}/s7.c
)

target_include_directories(s7_midi PRIVATE ${S7_DIR})

# Use -Os for size optimization (s7.c is very large)
# Disable some s7 features to reduce size
target_compile_options(s7_midi PRIVATE -Wall -Wextra -Wno-unused-parameter -Os)
target_compile_definitions(s7_midi PRIVATE
    WITH_SYSTEM_EXTRAS=0
    WITH_C_LOADER=0
    DISABLE_AUTOLOAD=1
)

# Check for readline
find_library(READLINE_LIB readline)
if(READLINE_LIB)
    target_compile_definitions(s7_midi PRIVATE USE_READLINE)
    target_link_libraries(s7_midi PRIVATE ${READLINE_LIB})
endif()

# s7 requires math and dl libraries
target_link_libraries(s7_midi PRIVATE music_theory midi_file m)

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(s7_midi PRIVATE
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    target_link_libraries(s7_midi PRIVATE dl)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(s7_midi PRIVATE ${ALSA_LIBRARIES})
    endif()
endif()

add_strip_command(s7_midi)

install(TARGETS s7_midi RUNTIME DESTINATION bin)
