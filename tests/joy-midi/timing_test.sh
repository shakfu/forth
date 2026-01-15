#!/bin/bash
# Automated timing verification test for Joy-MIDI
# Verifies that parallel parts are correctly synchronized

set -e

JOY_MIDI="${1:-./build/joy_midi}"
TEST_FILE="tests/joy-midi/timing_test.joy"

# Run the timing test and capture output
OUTPUT=$("$JOY_MIDI" "$TEST_FILE" 2>&1)

echo "Running timing verification..."

# Test 1: Verify parallel parts sync (both channels at same time)
# Check that t=0 has both ch=1 and ch=2
if echo "$OUTPUT" | grep -q "t=   0 ch=1" && echo "$OUTPUT" | grep -q "t=   0 ch=2"; then
    echo "PASS: Test 1 - Both channels start at t=0"
else
    echo "FAIL: Test 1 - Channels not synchronized at t=0"
    exit 1
fi

# Check that t=500 has both channels
if echo "$OUTPUT" | grep -q "t= 500 ch=1" && echo "$OUTPUT" | grep -q "t= 500 ch=2"; then
    echo "PASS: Test 1 - Both channels at t=500"
else
    echo "FAIL: Test 1 - Channels not synchronized at t=500"
    exit 1
fi

# Test 2: Verify mixed durations (quarters at 500ms, eighths at 250ms)
# Check for eighth note interval (250ms)
if echo "$OUTPUT" | grep -q "t=2200 ch=2"; then
    echo "PASS: Test 2 - Eighth note at t=2200 (250ms after t=1950)"
else
    echo "FAIL: Test 2 - Eighth note timing incorrect"
    exit 1
fi

# Check for quarter note interval (500ms)
if echo "$OUTPUT" | grep -q "t=2450 ch=1"; then
    echo "PASS: Test 2 - Quarter note at t=2450 (500ms after t=1950)"
else
    echo "FAIL: Test 2 - Quarter note timing incorrect"
    exit 1
fi

# Test 3: Verify composition (phrase_b follows phrase_a)
# phrase_b should start after phrase_a ends
if echo "$OUTPUT" | grep -q "t=4875 ch=1 pitch= 67"; then
    echo "PASS: Test 3 - phrase_b g4 (pitch=67) at correct offset"
else
    echo "FAIL: Test 3 - Sequence composition incorrect"
    exit 1
fi

echo ""
echo "All timing tests PASSED"
