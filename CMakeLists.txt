cmake_minimum_required(VERSION 3.16)
project(midi_langs_project VERSION 1.0.0 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)

# Default to Release build if not specified
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

# Strip binaries in Release mode
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O2")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O2")

if(UNIX AND NOT APPLE)
    # Linux: strip debug symbols via linker
    add_link_options($<$<CONFIG:Release>:-s>)
endif()

# Macro to add strip post-build step (for macOS)
macro(add_strip_command target)
    if(APPLE AND CMAKE_BUILD_TYPE STREQUAL "Release")
        add_custom_command(TARGET ${target} POST_BUILD
            COMMAND strip $<TARGET_FILE:${target}>
            COMMENT "Stripping ${target}"
        )
    endif()
endmacro()

# Output executables to build directory root
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

# Build libremidi as a subdirectory
set(LIBREMIDI_EXAMPLES OFF CACHE BOOL "" FORCE)
set(LIBREMIDI_TESTS OFF CACHE BOOL "" FORCE)
add_subdirectory(thirdparty/libremidi)

# Add project subdirectories
add_subdirectory(projects/forth)
add_subdirectory(projects/forth-midi)
add_subdirectory(projects/mhs-midi)
add_subdirectory(projects/pktpy-midi)
add_subdirectory(projects/s7-midi)
add_subdirectory(projects/lua-midi)

# Enable testing
enable_testing()
add_subdirectory(tests)
