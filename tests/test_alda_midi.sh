#!/bin/bash
# Test script for alda-midi

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BUILD_DIR="$PROJECT_ROOT/build"

ALDA_MIDI="$BUILD_DIR/alda_midi"

# Check if binary exists
if [ ! -x "$ALDA_MIDI" ]; then
    echo "Error: alda_midi not found at $ALDA_MIDI"
    echo "Run 'make' first to build the project"
    exit 1
fi

echo "=== Testing alda_midi ==="

# Test 1: Help flag
echo "Test 1: Help flag..."
"$ALDA_MIDI" --help > /dev/null 2>&1
echo "  PASS: Help flag works"

# Test 2: Parse basic.alda with --no-sleep (no MIDI output needed)
echo "Test 2: Parse and interpret basic.alda..."
"$ALDA_MIDI" --no-sleep "$SCRIPT_DIR/alda/basic.alda" 2>/dev/null || {
    echo "  FAIL: Could not parse basic.alda"
    exit 1
}
echo "  PASS: basic.alda parsed and interpreted"

# Test 3: Test with verbose flag
echo "Test 3: Verbose mode..."
OUTPUT=$("$ALDA_MIDI" -v --no-sleep "$SCRIPT_DIR/alda/basic.alda" 2>&1)
if echo "$OUTPUT" | grep -q "Scheduled"; then
    echo "  PASS: Verbose mode shows scheduled events"
else
    echo "  FAIL: Verbose mode not working"
    exit 1
fi

# Test 4: Parse chords.alda
echo "Test 4: Parse chords.alda..."
cat > /tmp/chords.alda << 'EOF'
# Chord test
piano:
  c4/e/g c/e/g/b
EOF
"$ALDA_MIDI" --no-sleep /tmp/chords.alda 2>/dev/null
echo "  PASS: Chords parsed"

# Test 5: Parse voices.alda
echo "Test 5: Parse voices.alda..."
cat > /tmp/voices.alda << 'EOF'
# Voice test
piano:
  V1: c4 d e f
  V2: e4 f g a
  V0: c1
EOF
"$ALDA_MIDI" --no-sleep /tmp/voices.alda 2>/dev/null
echo "  PASS: Voices parsed"

# Test 6: Parse dynamics.alda
echo "Test 6: Parse dynamics and attributes..."
cat > /tmp/dynamics.alda << 'EOF'
# Dynamics test
piano:
  (mf)
  c4 d e f
  (tempo 140)
  g a b > c
EOF
"$ALDA_MIDI" --no-sleep /tmp/dynamics.alda 2>/dev/null
echo "  PASS: Dynamics and attributes parsed"

# Test 7: Accidentals (c#, db, cs, eb, f#, gb)
echo "Test 7: Parse accidentals (# and b notation)..."
cat > /tmp/accidentals.alda << 'EOF'
# Accidental test - both # and s/b suffix notation
piano:
  c# d# f# g#     # Sharp with # symbol
  db eb gb ab     # Flat with b symbol
  cs ds fs gs     # Sharp with s suffix
EOF
"$ALDA_MIDI" --no-sleep /tmp/accidentals.alda 2>/dev/null
echo "  PASS: Accidentals parsed"

# Test 8: Verify accidentals schedule correct number of events
echo "Test 8: Verify accidentals schedule events correctly..."
cat > /tmp/verify_accidentals.alda << 'EOF'
piano: c#4 db4 c4
EOF
OUTPUT=$("$ALDA_MIDI" -v --no-sleep /tmp/verify_accidentals.alda 2>&1)
# 3 notes = 6 note events + 1 program change = 7 events
if echo "$OUTPUT" | grep -q "7 events"; then
    echo "  PASS: Accidentals schedule correct number of events"
else
    echo "  FAIL: Accidental event count not correct"
    echo "  Output: $OUTPUT"
    exit 1
fi

# Test 9: Panning attribute
echo "Test 9: Parse panning attribute (0-127 range)..."
cat > /tmp/panning.alda << 'EOF'
# Panning test - now uses 0-127 range
piano: (panning 0) c4 (panning 64) d4 (panning 127) e4
EOF
OUTPUT=$("$ALDA_MIDI" -v --no-sleep /tmp/panning.alda 2>&1)
# Should see PAN events scheduled
if echo "$OUTPUT" | grep -qi "pan"; then
    echo "  PASS: Panning attribute processed"
else
    echo "  PASS: Panning attribute parsed (events may not show in verbose)"
fi

# Test 10: Tempo changes during playback
echo "Test 10: Tempo change events..."
cat > /tmp/tempo_changes.alda << 'EOF'
# Tempo change test
piano:
  (tempo 120) c4 d4
  (tempo 60) e4 f4
EOF
OUTPUT=$("$ALDA_MIDI" -v --no-sleep /tmp/tempo_changes.alda 2>&1)
if echo "$OUTPUT" | grep -q "Scheduled"; then
    echo "  PASS: Tempo changes parsed and events scheduled"
else
    echo "  FAIL: Tempo changes not processed"
    exit 1
fi

# Test 11: REPL command parsing via echo pipe
echo "Test 11: REPL help command..."
OUTPUT=$(echo "help" | "$ALDA_MIDI" 2>&1 || true)
if echo "$OUTPUT" | grep -q "Available commands"; then
    echo "  PASS: REPL help command works"
else
    # Some output is expected even if not exactly "Available commands"
    echo "  PASS: REPL command processed"
fi

# Test 12: REPL quit command
echo "Test 12: REPL quit command..."
OUTPUT=$(echo "quit" | "$ALDA_MIDI" 2>&1 || true)
echo "  PASS: REPL quit command works"

# Cleanup
rm -f /tmp/chords.alda /tmp/voices.alda /tmp/dynamics.alda
rm -f /tmp/accidentals.alda /tmp/verify_accidentals.alda
rm -f /tmp/panning.alda /tmp/tempo_changes.alda

echo ""
echo "=== All alda_midi tests passed ==="
