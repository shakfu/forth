# C unit tests for forth_midi
add_executable(test_forth_midi_unit test_forth_midi_unit.c)
target_link_libraries(test_forth_midi_unit PRIVATE forth_midi_lib)
target_include_directories(test_forth_midi_unit PRIVATE ${CMAKE_SOURCE_DIR}/projects/forth-midi)

add_test(
    NAME forth_midi_unit_tests
    COMMAND test_forth_midi_unit
)
set_tests_properties(forth_midi_unit_tests PROPERTIES LABELS "quick")

# Test using the comprehensive shell script
add_test(
    NAME forth_midi_test_suite
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_forth_midi.sh $<TARGET_FILE:forth_midi>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

# Set test properties
set_tests_properties(forth_midi_test_suite PROPERTIES
    TIMEOUT 60
    LABELS "integration"
)

# Individual quick tests for faster feedback during development
add_test(
    NAME forth_midi_smoke
    COMMAND sh -c "echo 'midi-list' | $<TARGET_FILE:forth_midi>"
)

add_test(
    NAME forth_midi_arithmetic
    COMMAND sh -c "echo '3 4 + .' | $<TARGET_FILE:forth_midi> | grep -q '7'"
)

add_test(
    NAME forth_midi_pitch_parsing
    COMMAND sh -c "echo 'c4 .' | $<TARGET_FILE:forth_midi> | grep -q '60'"
)

add_test(
    NAME forth_midi_conditionals
    COMMAND sh -c "echo '1 if 42 . then' | $<TARGET_FILE:forth_midi> | grep -q '42'"
)

add_test(
    NAME forth_midi_word_definitions
    COMMAND sh -c "echo ': double 2 * ; 7 double .' | $<TARGET_FILE:forth_midi> | grep -q '14'"
)

add_test(
    NAME forth_midi_blocks
    COMMAND sh -c "echo '{ 1 . } 3 *' | $<TARGET_FILE:forth_midi> | grep -q '1 1 1'"
)

# Label quick tests
set_tests_properties(
    forth_midi_smoke
    forth_midi_arithmetic
    forth_midi_pitch_parsing
    forth_midi_conditionals
    forth_midi_word_definitions
    forth_midi_blocks
    PROPERTIES LABELS "quick"
)

# pktpy_midi tests
add_test(
    NAME pktpy_midi_test_suite
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_pktpy_midi.sh $<TARGET_FILE:pktpy_midi>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

set_tests_properties(pktpy_midi_test_suite PROPERTIES
    TIMEOUT 60
    LABELS "integration"
)

# Quick pktpy_midi tests
add_test(
    NAME pktpy_midi_smoke
    COMMAND sh -c "echo 'import midi; print(\"ok\")' | $<TARGET_FILE:pktpy_midi> | grep -q 'ok'"
)

add_test(
    NAME pktpy_midi_note_parsing
    COMMAND sh -c "echo 'import midi; print(midi.note(\"C4\"))' | $<TARGET_FILE:pktpy_midi> | grep -q '60'"
)

add_test(
    NAME pktpy_midi_virtual_port
    COMMAND sh -c "echo 'import midi; m = midi.open(); print(m.is_open)' | $<TARGET_FILE:pktpy_midi> | grep -q 'True'"
)

set_tests_properties(
    pktpy_midi_smoke
    pktpy_midi_note_parsing
    pktpy_midi_virtual_port
    PROPERTIES LABELS "quick"
)

# s7_midi tests
add_test(
    NAME s7_midi_test_suite
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_s7_midi.sh $<TARGET_FILE:s7_midi>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

set_tests_properties(s7_midi_test_suite PROPERTIES
    TIMEOUT 60
    LABELS "integration"
)

# Quick s7_midi tests
add_test(
    NAME s7_midi_smoke
    COMMAND sh -c "$<TARGET_FILE:s7_midi> -e '(+ 1 2)' | grep -q '3'"
)

add_test(
    NAME s7_midi_note_parsing
    COMMAND sh -c "$<TARGET_FILE:s7_midi> -e '(note \"C4\")' | grep -q '60'"
)

add_test(
    NAME s7_midi_pitch_constants
    COMMAND sh -c "$<TARGET_FILE:s7_midi> -e 'c4' | grep -q '60'"
)

add_test(
    NAME s7_midi_virtual_port
    COMMAND sh -c "$<TARGET_FILE:s7_midi> -e '(begin (define m (midi-open)) (midi-out? m))' | grep -q '#t'"
)

set_tests_properties(
    s7_midi_smoke
    s7_midi_note_parsing
    s7_midi_pitch_constants
    s7_midi_virtual_port
    PROPERTIES LABELS "quick"
)

# lua_midi tests
add_test(
    NAME lua_midi_test_suite
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_lua_midi.sh $<TARGET_FILE:lua_midi>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

set_tests_properties(lua_midi_test_suite PROPERTIES
    TIMEOUT 60
    LABELS "integration"
)

# Quick lua_midi tests
add_test(
    NAME lua_midi_smoke
    COMMAND sh -c "$<TARGET_FILE:lua_midi> -e 'print(1+2)' | grep -q '3'"
)

add_test(
    NAME lua_midi_note_parsing
    COMMAND sh -c "$<TARGET_FILE:lua_midi> -e 'print(midi.note(\"C4\"))' | grep -q '60'"
)

add_test(
    NAME lua_midi_pitch_constants
    COMMAND sh -c "$<TARGET_FILE:lua_midi> -e 'print(midi.c4)' | grep -q '60'"
)

add_test(
    NAME lua_midi_virtual_port
    COMMAND sh -c "$<TARGET_FILE:lua_midi> -e 'local m = midi.open(); print(m:is_open()); m:close()' | grep -q 'true'"
)

set_tests_properties(
    lua_midi_smoke
    lua_midi_note_parsing
    lua_midi_pitch_constants
    lua_midi_virtual_port
    PROPERTIES LABELS "quick"
)

# mhs-midi tests
add_test(
    NAME mhs_midi_test_suite
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_mhs_midi.sh $<TARGET_FILE:mhs-midi>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

set_tests_properties(mhs_midi_test_suite PROPERTIES
    TIMEOUT 120
    LABELS "integration"
)

# Golden .4th test programs for forth_midi
add_test(
    NAME forth_midi_golden_basic
    COMMAND sh -c "$<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_basic.4th | grep -q '999' && ! $<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_basic.4th | grep -q ' 0 '"
)

add_test(
    NAME forth_midi_golden_notation
    COMMAND sh -c "$<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_notation.4th | grep -q '999' && ! $<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_notation.4th | grep -q ' 0 '"
)

add_test(
    NAME forth_midi_golden_blocks
    COMMAND sh -c "$<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_blocks.4th | grep -q '999' && ! $<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_blocks.4th | grep -q ' 0 '"
)

add_test(
    NAME forth_midi_golden_generative
    COMMAND sh -c "$<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_generative.4th | grep -q '999' && ! $<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_generative.4th | grep -q ' 0 '"
)

add_test(
    NAME forth_midi_golden_sequences
    COMMAND sh -c "$<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_sequences.4th | grep -q '999' && ! $<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_sequences.4th | grep -q ' 0 '"
)

add_test(
    NAME forth_midi_golden_async
    COMMAND sh -c "$<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_async.4th | grep -q '999' && ! $<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_async.4th | grep -q ' 0 '"
)

add_test(
    NAME forth_midi_golden_seq_record
    COMMAND sh -c "$<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_seq_record.4th | grep -q '999' && ! $<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_seq_record.4th | grep -q ' 0 '"
)

set_tests_properties(
    forth_midi_golden_basic
    forth_midi_golden_notation
    forth_midi_golden_blocks
    forth_midi_golden_generative
    forth_midi_golden_sequences
    forth_midi_golden_async
    forth_midi_golden_seq_record
    PROPERTIES LABELS "golden"
)

# alda_midi tests
add_test(
    NAME alda_midi_test_suite
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_alda_midi.sh $<TARGET_FILE:alda_midi>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

set_tests_properties(alda_midi_test_suite PROPERTIES
    TIMEOUT 60
    LABELS "integration"
)

# Quick alda_midi tests
add_test(
    NAME alda_midi_smoke
    COMMAND sh -c "$<TARGET_FILE:alda_midi> --help | grep -q 'Alda'"
)

add_test(
    NAME alda_midi_basic
    COMMAND $<TARGET_FILE:alda_midi> --no-sleep ${CMAKE_CURRENT_SOURCE_DIR}/alda/basic.alda
)

set_tests_properties(
    alda_midi_smoke
    alda_midi_basic
    PROPERTIES LABELS "quick"
)

# README example tests - verify examples from README.md work
# These use --no-sleep to run quickly without actual audio delays

add_test(
    NAME demo_example_alda
    COMMAND $<TARGET_FILE:alda_midi> --no-sleep ${CMAKE_CURRENT_SOURCE_DIR}/examples/demo_alda.alda
)

add_test(
    NAME demo_example_forth
    COMMAND $<TARGET_FILE:forth_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/examples/demo_forth.4th
)

add_test(
    NAME demo_example_lua
    COMMAND $<TARGET_FILE:lua_midi> ${CMAKE_CURRENT_SOURCE_DIR}/examples/demo_lua.lua
)

add_test(
    NAME demo_example_pktpy
    COMMAND $<TARGET_FILE:pktpy_midi> ${CMAKE_CURRENT_SOURCE_DIR}/examples/demo_pktpy.py
)

add_test(
    NAME demo_example_s7
    COMMAND $<TARGET_FILE:s7_midi> ${CMAKE_CURRENT_SOURCE_DIR}/examples/demo_s7.scm
)

add_test(
    NAME demo_example_mhs
    COMMAND ${CMAKE_SOURCE_DIR}/scripts/mhs-midi run ${CMAKE_CURRENT_SOURCE_DIR}/examples/DemoMhs.hs
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)

set_tests_properties(
    demo_example_alda
    demo_example_forth
    demo_example_lua
    demo_example_pktpy
    demo_example_s7
    PROPERTIES LABELS "quick"
)

set_tests_properties(
    demo_example_mhs
    PROPERTIES
        LABELS "integration"
        TIMEOUT 120
)
