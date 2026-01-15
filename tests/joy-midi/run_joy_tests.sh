#!/bin/bash
# Run Joy unit tests for joy-midi
# Usage: run_joy_tests.sh <joy_midi_binary> <test_dir>

JOY_MIDI="$1"
TEST_DIR="$2"

if [ -z "$JOY_MIDI" ] || [ -z "$TEST_DIR" ]; then
    echo "Usage: $0 <joy_midi_binary> <test_dir>"
    exit 1
fi

if [ ! -x "$JOY_MIDI" ]; then
    echo "Error: $JOY_MIDI is not executable"
    exit 1
fi

if [ ! -d "$TEST_DIR" ]; then
    echo "Error: $TEST_DIR is not a directory"
    exit 1
fi

PASSED=0
FAILED=0
SKIPPED=0

# Process each .joy file
for testfile in "$TEST_DIR"/*.joy; do
    [ -f "$testfile" ] || continue

    testname=$(basename "$testfile")

    # Extract test lines (skip comments, libload, and error tests)
    # Comments are (* ... *) or lines starting with #
    test_lines=$(sed -e 's/(\*.*\*)//' \
                     -e '/^[[:space:]]*$/d' \
                     -e '/libload/d' \
                     -e '/^[[:space:]]*#/d' \
                     -e '/^[[:space:]]*(\*/,/\*)/d' \
                     "$testfile" | grep -v '^$')

    if [ -z "$test_lines" ]; then
        ((SKIPPED++))
        continue
    fi

    # Run each test line
    while IFS= read -r line; do
        # Skip empty lines
        [ -z "$line" ] && continue

        # Strip trailing . (Joy statement terminator)
        line=$(echo "$line" | sed 's/\.[[:space:]]*$//')

        # Run the test with timeout
        result=$(echo "$line" | timeout 2 "$JOY_MIDI" 2>&1 | tail -1)

        # Check if result contains true (last element on stack should be true)
        # The stack output may have multiple elements, we check for "true" anywhere
        if echo "$result" | grep -qw "true"; then
            ((PASSED++))
        else
            echo "FAIL: $testname: $line"
            echo "  Got: $result"
            ((FAILED++))
        fi
    done <<< "$test_lines"
done

echo ""
echo "Results: $PASSED passed, $FAILED failed, $SKIPPED skipped"

if [ $FAILED -gt 0 ]; then
    exit 1
fi
exit 0
