# Joy-MIDI unit tests from pyjoy-lang
# Each .joy file becomes a separate ctest

# Make test runner executable
set(JOY_TEST_RUNNER "${CMAKE_CURRENT_SOURCE_DIR}/run_single_joy_test.sh")

# Find all .joy test files
file(GLOB JOY_TEST_FILES "${CMAKE_CURRENT_SOURCE_DIR}/joy/*.joy")

# Add each test file as a separate ctest
foreach(TEST_FILE ${JOY_TEST_FILES})
    get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)

    add_test(
        NAME joy_unit_${TEST_NAME}
        COMMAND bash ${JOY_TEST_RUNNER} $<TARGET_FILE:joy_midi> ${TEST_FILE}
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    )

    # Label for filtering
    set_tests_properties(joy_unit_${TEST_NAME} PROPERTIES
        LABELS "joy_unit"
        TIMEOUT 10
    )
endforeach()

# Summary target to count tests
list(LENGTH JOY_TEST_FILES JOY_TEST_COUNT)
message(STATUS "Added ${JOY_TEST_COUNT} Joy unit tests")

# Timing verification test (verifies parallel parts synchronization)
add_test(
    NAME joy_midi_timing
    COMMAND bash ${CMAKE_CURRENT_SOURCE_DIR}/timing_test.sh $<TARGET_FILE:joy_midi>
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)
set_tests_properties(joy_midi_timing PROPERTIES
    LABELS "joy_midi"
    TIMEOUT 10
)
