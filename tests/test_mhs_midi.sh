#!/bin/bash
# test_mhs_midi.sh - Run mhs-midi unit tests
#
# Usage: test_mhs_midi.sh <mhs-midi-binary>

set -e

MHS_MIDI="${1:-./build/mhs-midi}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
MHS_DIR="$PROJECT_DIR/thirdparty/MicroHs"
MIDI_LIB="$PROJECT_DIR/projects/mhs-midi/lib"
EXAMPLES_DIR="$PROJECT_DIR/projects/mhs-midi/examples"

if [ ! -f "$MHS_MIDI" ]; then
    echo "Error: mhs-midi not found at $MHS_MIDI"
    exit 1
fi

echo "Running mhs-midi unit tests..."
echo "Binary: $MHS_MIDI"
echo ""

# Run the Music.hs test module
OUTPUT=$(env MHSDIR="$MHS_DIR" "$MHS_MIDI" -r -C -i"$MIDI_LIB" TestMusic 2>&1)
EXIT_CODE=$?

echo "$OUTPUT"

# Check for Music test failures
if ! echo "$OUTPUT" | grep -q "ALL TESTS PASSED"; then
    echo ""
    echo "Music test suite failed!"
    exit 1
fi

echo ""
echo "Music tests passed."
echo ""

# Run the Async test module
echo "Running Async tests..."
ASYNC_OUTPUT=$(env MHSDIR="$MHS_DIR" "$MHS_MIDI" -r -C -i"$MIDI_LIB" -i"$EXAMPLES_DIR" AsyncTest 2>&1)
ASYNC_EXIT=$?

echo "$ASYNC_OUTPUT"

# Check for Async test failures
if ! echo "$ASYNC_OUTPUT" | grep -q "All tests passed"; then
    echo ""
    echo "Async test suite failed!"
    exit 1
fi

echo ""
echo "All test suites passed!"
exit 0
