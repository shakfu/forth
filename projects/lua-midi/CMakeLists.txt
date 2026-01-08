set(LUA_DIR ${CMAKE_SOURCE_DIR}/thirdparty/lua-5.5.0/src)

# Lua core sources
set(LUA_CORE_SOURCES
    ${LUA_DIR}/lapi.c
    ${LUA_DIR}/lcode.c
    ${LUA_DIR}/lctype.c
    ${LUA_DIR}/ldebug.c
    ${LUA_DIR}/ldo.c
    ${LUA_DIR}/ldump.c
    ${LUA_DIR}/lfunc.c
    ${LUA_DIR}/lgc.c
    ${LUA_DIR}/llex.c
    ${LUA_DIR}/lmem.c
    ${LUA_DIR}/lobject.c
    ${LUA_DIR}/lopcodes.c
    ${LUA_DIR}/lparser.c
    ${LUA_DIR}/lstate.c
    ${LUA_DIR}/lstring.c
    ${LUA_DIR}/ltable.c
    ${LUA_DIR}/ltm.c
    ${LUA_DIR}/lundump.c
    ${LUA_DIR}/lvm.c
    ${LUA_DIR}/lzio.c
)

# Lua library sources
set(LUA_LIB_SOURCES
    ${LUA_DIR}/lauxlib.c
    ${LUA_DIR}/lbaselib.c
    ${LUA_DIR}/lcorolib.c
    ${LUA_DIR}/ldblib.c
    ${LUA_DIR}/liolib.c
    ${LUA_DIR}/lmathlib.c
    ${LUA_DIR}/loadlib.c
    ${LUA_DIR}/loslib.c
    ${LUA_DIR}/lstrlib.c
    ${LUA_DIR}/ltablib.c
    ${LUA_DIR}/lutf8lib.c
    ${LUA_DIR}/linit.c
)

add_executable(lua_midi
    main.c
    midi_module.c
    scheduler.c
    ${LUA_CORE_SOURCES}
    ${LUA_LIB_SOURCES}
)

target_include_directories(lua_midi PRIVATE
    ${LUA_DIR}
    ${CMAKE_SOURCE_DIR}/thirdparty/libuv/include
)

# Compiler warnings (GCC/Clang only)
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(lua_midi PRIVATE -Wall -Wextra -Wno-unused-parameter)
endif()

# Check for readline (optional on Windows)
find_library(READLINE_LIB readline)
if(READLINE_LIB)
    target_compile_definitions(lua_midi PRIVATE USE_READLINE)
    target_link_libraries(lua_midi PRIVATE ${READLINE_LIB})
else()
    if(NOT WIN32)
        message(FATAL_ERROR "readline library not found (install libreadline-dev on Linux)")
    endif()
endif()

# Lua requires libuv for async scheduler
target_link_libraries(lua_midi PRIVATE music_theory midi_file uv_a)

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(lua_midi PRIVATE
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    # Math library and dl needed on Unix
    target_link_libraries(lua_midi PRIVATE dl m)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(lua_midi PRIVATE ${ALSA_LIBRARIES})
    endif()
endif()

add_strip_command(lua_midi)

install(TARGETS lua_midi RUNTIME DESTINATION bin)
