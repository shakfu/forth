# Common music theory library

add_library(music_theory STATIC
    music_theory.c
    music_theory.h
)

target_include_directories(music_theory PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

# Set standard
set_target_properties(music_theory PROPERTIES
    C_STANDARD 11
    C_STANDARD_REQUIRED ON
)

# MIDI file I/O library (C++ wrapper for libremidi reader/writer)
add_library(midi_file STATIC
    midi_file.cpp
    midi_file.h
)

target_include_directories(midi_file PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(midi_file PUBLIC
    libremidi
)

set_target_properties(midi_file PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED ON
)
