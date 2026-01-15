# joy_core - Core Joy language interpreter library
# This is a standalone Joy interpreter compatible with standard Joy unit tests

add_library(joy_core STATIC
    joy_runtime.c
    joy_primitives.c
    joy_parser.c
)

target_include_directories(joy_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Link math library on Unix
if(UNIX)
    target_link_libraries(joy_core PUBLIC m)
endif()

# Readline support (enabled by default)
if(NOT WIN32)
    find_library(READLINE_LIB readline)
    if(READLINE_LIB)
        target_compile_definitions(joy_core PUBLIC HAVE_READLINE)
        target_link_libraries(joy_core PUBLIC ${READLINE_LIB})
        message(STATUS "joy_core: readline support enabled")
    else()
        message(STATUS "joy_core: readline not found, using basic input")
    endif()
endif()

# Compiler warnings
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(joy_core PRIVATE -Wall -Wextra -Wno-unused-parameter)
endif()
