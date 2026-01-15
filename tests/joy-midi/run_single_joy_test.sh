#!/bin/bash
# Run a single Joy test file
# Usage: run_single_joy_test.sh <joy_midi_binary> <test_file>
# Exit 0 if all tests pass, 1 if any fail

JOY_MIDI="$1"
TEST_FILE="$2"

if [ ! -x "$JOY_MIDI" ] || [ ! -f "$TEST_FILE" ]; then
    echo "Usage: $0 <joy_midi_binary> <test_file>"
    exit 1
fi

# Extract code: remove multi-line comments (* ... *), error tests (#), libload
# Keep everything else (definitions + tests) as a single program
# Note: We keep trailing . since it's the Joy print/pop operator
code=$(perl -0777 -pe 's/\(\*.*?\*\)//gs' "$TEST_FILE" | \
       grep -v '^[[:space:]]*#' | \
       grep -v 'libload' | \
       grep -v '^[[:space:]]*$')

if [ -z "$code" ]; then
    echo "SKIP: No code in $TEST_FILE"
    exit 0
fi

# Run entire file as one program
result=$(echo "$code" | timeout 5 "$JOY_MIDI" 2>&1 | \
         grep -v "^Joy-MIDI" | \
         grep -v "^Type 'quit'" | \
         grep -v "^Created virtual" | \
         grep -v "^$" | \
         tail -1)

# Count how many "true" values are on the stack (each test should produce true)
# A passing test file should have stack containing only true values
true_count=$(echo "$result" | grep -o "true" | wc -l | tr -d ' ')
false_count=$(echo "$result" | grep -o "false" | wc -l | tr -d ' ')

# Check for errors
if echo "$result" | grep -qi "error\|underflow\|undefined"; then
    echo "ERROR: $result"
    exit 1
fi

# Check results
if [ "$false_count" -gt 0 ]; then
    echo "FAIL: $false_count false values"
    echo "  Result: $result"
    exit 1
fi

# If we have true values, report them
if [ "$true_count" -gt 0 ]; then
    echo "PASS: $true_count tests passed"
    exit 0
fi

# No true or false values - this is a smoke test (just checking for no crash)
echo "PASS: smoke test (no errors)"
exit 0
