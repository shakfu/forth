cmake_minimum_required(VERSION 3.16)
project(midi_langs_project VERSION 1.0.0 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)

# Default to Release build if not specified
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

option(BUILD_EXAMPLES "Build language standalone examples" OFF)
option(BUILD_MHS_MIDI "Build mhs-midi targets (requires MicroHs)" ON)
# Option to enable address and undefined behavior sanitizers
option(ENABLE_SANITIZERS "Enable AddressSanitizer and UndefinedBehaviorSanitizer" OFF)

if(ENABLE_SANITIZERS)
    message(STATUS "Sanitizers enabled")
    if(MSVC)
        # MSVC sanitizer flags
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /fsanitize=address")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /fsanitize=address")
    else()
        set(SANITIZER_FLAGS "-fsanitize=address,undefined -fno-omit-frame-pointer")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${SANITIZER_FLAGS}")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${SANITIZER_FLAGS}")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${SANITIZER_FLAGS}")
    endif()
endif()

# Optimization flags for Release mode
if(MSVC)
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /O2")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /O2")
else()
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O2")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O2")
endif()

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

# Build libuv as a subdirectory (for async event loop)
set(LIBUV_BUILD_SHARED OFF CACHE BOOL "" FORCE)
set(LIBUV_BUILD_TESTS OFF CACHE BOOL "" FORCE)
add_subdirectory(thirdparty/libuv)

# Build common music theory library
add_subdirectory(projects/common)

# Add project subdirectories
add_subdirectory(projects/forth-midi)
if(BUILD_MHS_MIDI)
    add_subdirectory(projects/mhs-midi)
endif()
add_subdirectory(projects/pktpy-midi)
add_subdirectory(projects/s7-midi)
add_subdirectory(projects/lua-midi)
add_subdirectory(projects/alda-midi)
add_subdirectory(projects/joy-midi)

# ============================================
# Alias targets for consistent naming
# ============================================
# Provides hyphenated target names (e.g., forth-midi instead of forth_midi)

add_custom_target(forth-midi DEPENDS forth_midi)
add_custom_target(lua-midi DEPENDS lua_midi)
add_custom_target(pktpy-midi DEPENDS pktpy_midi)
add_custom_target(s7-midi DEPENDS s7_midi)
add_custom_target(alda-midi DEPENDS alda_midi)
add_custom_target(joy-midi DEPENDS joy_midi)

# Convenience target to build all midi-langs
add_custom_target(all-midi-langs
    DEPENDS forth_midi lua_midi pktpy_midi s7_midi alda_midi joy_midi
)
if(BUILD_MHS_MIDI)
    add_dependencies(all-midi-langs mhs-midi)
    if(NOT WIN32)
        # Standalone variants require fmemopen (not available on Windows)
        add_dependencies(all-midi-langs mhs-midi-src mhs-midi-src-zstd mhs-midi-pkg mhs-midi-pkg-zstd)
    endif()
endif()

# Enable testing
enable_testing()
add_subdirectory(tests)
