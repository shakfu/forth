#!/bin/bash
# Test suite for forth_midi

MIDI_FORTH="${1:-./forth_midi}"
PASSED=0
FAILED=0
TOTAL=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Test helper: run command and check output contains expected string
test_contains() {
    local name="$1"
    local input="$2"
    local expected="$3"

    TOTAL=$((TOTAL + 1))
    output=$(echo "$input" | $MIDI_FORTH 2>&1)

    if echo "$output" | grep -q "$expected"; then
        echo -e "${GREEN}PASS${NC}: $name"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}: $name"
        echo "  Input: $input"
        echo "  Expected to contain: $expected"
        echo "  Got: $output"
        FAILED=$((FAILED + 1))
    fi
}

# Test helper: run command and check exact output
test_exact() {
    local name="$1"
    local input="$2"
    local expected="$3"

    TOTAL=$((TOTAL + 1))
    # Extract just the output line (skip prompt, get before " ok")
    output=$(echo "$input" | $MIDI_FORTH 2>&1 | grep -v "^MIDI Forth" | grep -v "^>" | head -1 | sed 's/ ok$//')

    if [ "$output" = "$expected" ]; then
        echo -e "${GREEN}PASS${NC}: $name"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}: $name"
        echo "  Input: $input"
        echo "  Expected: '$expected'"
        echo "  Got: '$output'"
        FAILED=$((FAILED + 1))
    fi
}

# Test helper: check output equals expected number (handles negative)
test_number() {
    local name="$1"
    local input="$2"
    local expected="$3"

    TOTAL=$((TOTAL + 1))
    output=$(echo "$input" | $MIDI_FORTH 2>&1 | grep -oE '\-?[0-9]+' | head -1)

    if [ "$output" = "$expected" ]; then
        echo -e "${GREEN}PASS${NC}: $name"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}: $name"
        echo "  Input: $input"
        echo "  Expected: $expected"
        echo "  Got: $output"
        FAILED=$((FAILED + 1))
    fi
}

echo "========================================"
echo "MIDI Forth Test Suite"
echo "========================================"
echo ""

# ============================================
echo "--- Pitch Parsing ---"
# ============================================

test_number "C4 = 60" "c4 ." "60"
test_number "D4 = 62" "d4 ." "62"
test_number "E4 = 64" "e4 ." "64"
test_number "F4 = 65" "f4 ." "65"
test_number "G4 = 67" "g4 ." "67"
test_number "A4 = 69" "a4 ." "69"
test_number "B4 = 71" "b4 ." "71"
test_number "C5 = 72" "c5 ." "72"

# Octaves
test_number "C0 = 12" "c0 ." "12"
test_number "C3 = 48" "c3 ." "48"
test_number "C7 = 96" "c7 ." "96"

# Sharps and flats
test_number "C#4 = 61" "c#4 ." "61"
test_number "Db4 = 61" "db4 ." "61"
test_number "F#4 = 66" "f#4 ." "66"
test_number "Gb4 = 66" "gb4 ." "66"
test_number "Bb4 = 70" "bb4 ." "70"

# Case insensitive
test_number "C4 uppercase = 60" "C4 ." "60"
test_number "C#4 uppercase = 61" "C#4 ." "61"

# Articulation suffixes (should still parse pitch correctly)
test_number "c4. (staccato) = 60" "c4. ." "60"
test_number "c4> (accent) = 60" "c4> ." "60"
test_number "c4- (tenuto) = 60" "c4- ." "60"

echo ""

# ============================================
echo "--- Arithmetic ---"
# ============================================

test_number "3 + 4 = 7" "3 4 + ." "7"
test_number "10 - 3 = 7" "10 3 - ." "7"
test_number "6 * 7 = 42" "6 7 * ." "42"
test_number "20 / 4 = 5" "20 4 / ." "5"

echo ""

# ============================================
echo "--- Stack Operations ---"
# ============================================

test_exact "dup" "5 dup . ." "5 5 "
test_exact "drop" "1 2 drop ." "1 "
test_exact "swap" "1 2 swap . ." "1 2 "
test_exact "over" "1 2 over . . ." "1 2 1 "
test_exact "rot" "1 2 3 rot . . ." "1 3 2 "

echo ""

# ============================================
echo "--- Comparisons ---"
# ============================================

test_number "5 = 5 is true (-1)" "5 5 = ." "-1"
test_number "5 = 6 is false (0)" "5 6 = ." "0"
test_number "3 < 5 is true" "3 5 < ." "-1"
test_number "5 < 3 is false" "5 3 < ." "0"
test_number "5 > 3 is true" "5 3 > ." "-1"
test_number "3 > 5 is false" "3 5 > ." "0"

echo ""

# ============================================
echo "--- Word Definitions ---"
# ============================================

# Words that don't leave items on stack (consume their output)
test_exact "word with output" ": five 5 . ; five" "5 "
test_exact "word with arithmetic" ": double 2 * ; 7 double ." "14 "
test_exact "word calling word" ": a 1 . ; : b a a ; b" "1 1 "
test_contains "redefine warning" ": x 1 ; : x 2 ;" "Warning: redefining"
test_contains "stack warning" ": messy 42 ; messy" "left 1 item"

echo ""

# ============================================
echo "--- Conditionals ---"
# ============================================

test_exact "if true branch" "1 if 42 . then" "42 "
test_exact "if false branch (no output)" "0 if 42 . then" ""
test_exact "if-else true" "1 if 1 . else 2 . then" "1 "
test_exact "if-else false" "0 if 1 . else 2 . then" "2 "
test_exact "nested if (both true)" "1 if 1 if 42 . then then" "42 "
test_exact "nested if (outer false)" "0 if 1 if 42 . then then" ""
test_exact "nested if (inner false)" "1 if 0 if 42 . then 99 . then" "99 "

echo ""

# ============================================
echo "--- Anonymous Blocks ---"
# ============================================

test_exact "block repeat 3" "{ 1 . } 3 *" "1 1 1 "
test_exact "block repeat 1" "{ 42 . } 1 *" "42 "
test_exact "block with arithmetic" "{ 2 2 + . } 2 *" "4 4 "
test_number "multiplication still works" "3 4 * ." "12"

echo ""

# ============================================
echo "--- Loops (times) ---"
# ============================================

test_exact "times 3" ": x 1 . ; x 3 times" "1 1 1 "
test_exact "times 1" ": y 9 . ; y 1 times" "9 "

echo ""

# ============================================
echo "--- Relative Intervals ---"
# ============================================

test_number "+2 from c4" "c4 drop +2 ." "62"
test_number "-2 from c4" "c4 drop -2 ." "58"
test_number "+12 (octave up)" "c4 drop +12 ." "72"

echo ""

# ============================================
echo "--- Octave Shifts ---"
# ============================================

test_number "^ from c4" "c4 drop ^ ." "72"
test_number "v from c4" "c4 drop v ." "48"

echo ""

# ============================================
echo "--- Dynamics ---"
# ============================================

# These set default_velocity, we can't directly read it, but we can test it doesn't crash
test_contains "ppp runs" "ppp" "ok"
test_contains "fff runs" "fff" "ok"
test_contains "mf runs" "mf" "ok"

echo ""

# ============================================
echo "--- Random ---"
# ============================================

# Random should produce a number 0-99
test_contains "random produces number" "random ." "[0-9]"

# Statistical test: random should produce varying values in single session
random_test() {
    TOTAL=$((TOTAL + 1))
    # Run multiple randoms in a single session to avoid seed issues
    output=$(echo "random . random . random . random . random ." | $MIDI_FORTH 2>&1)
    values=$(echo "$output" | grep -oE '[0-9]+')
    unique=$(echo "$values" | sort -u | wc -l)
    if [ "$unique" -gt 1 ]; then
        echo -e "${GREEN}PASS${NC}: random produces varying values"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}: random produces varying values (got: $values)"
        FAILED=$((FAILED + 1))
    fi
}
random_test

echo ""

# ============================================
echo "--- Probability (Statistical) ---"
# ============================================

# Test that 100% always executes (run 5 times in single session)
prob_100_test() {
    TOTAL=$((TOTAL + 1))
    output=$(echo "42 100% . 42 100% . 42 100% . 42 100% . 42 100% ." | $MIDI_FORTH 2>&1)
    count=$(echo "$output" | grep -oE '\b42\b' | wc -l)
    if [ "$count" -eq 5 ]; then
        echo -e "${GREEN}PASS${NC}: 100% probability always executes"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}: 100% probability should always execute (got $count/5)"
        FAILED=$((FAILED + 1))
    fi
}
prob_100_test

# Test that 0% never executes (value becomes REST_MARKER)
prob_0_test() {
    TOTAL=$((TOTAL + 1))
    output=$(echo "42 0% . 42 0% . 42 0% . 42 0% . 42 0% ." | $MIDI_FORTH 2>&1)
    # With 0%, the value is replaced with REST_MARKER, so 42 should not appear
    count=$(echo "$output" | grep -oE '\b42\b' | wc -l)
    if [ "$count" -eq 0 ]; then
        echo -e "${GREEN}PASS${NC}: 0% probability never executes"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}: 0% probability should never execute (got $count/5)"
        FAILED=$((FAILED + 1))
    fi
}
prob_0_test

echo ""

# ============================================
echo "--- Alternatives ---"
# ============================================

# Test that alternatives produce varying values using 'pick' word
alt_test() {
    TOTAL=$((TOTAL + 1))
    # Run 10 alternatives with pick in a single session
    output=$(echo "c4|e4 pick . c4|e4 pick . c4|e4 pick . c4|e4 pick . c4|e4 pick . c4|e4 pick . c4|e4 pick . c4|e4 pick . c4|e4 pick . c4|e4 pick ." | $MIDI_FORTH 2>&1)
    c4_count=$(echo "$output" | grep -oE '\b60\b' | wc -l)
    e4_count=$(echo "$output" | grep -oE '\b64\b' | wc -l)
    # Should have some of each (with 10 trials, probability of all same is 1/512)
    if [ "$c4_count" -gt 0 ] && [ "$e4_count" -gt 0 ]; then
        echo -e "${GREEN}PASS${NC}: alternatives produce varying values (c4=$c4_count, e4=$e4_count)"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}: alternatives should produce both values (c4=$c4_count, e4=$e4_count)"
        FAILED=$((FAILED + 1))
    fi
}
alt_test

echo ""

# ============================================
echo "--- Packed Notes ---"
# ============================================

test_number "pitch@" "60 80 1 480 note pitch@ ." "60"
test_number "vel@" "60 80 1 480 note vel@ ." "80"
test_number "ch@" "60 80 1 480 note ch@ ." "1"
test_number "dur@" "60 80 1 480 note dur@ ." "480"

echo ""

# ============================================
echo "--- File Loading ---"
# ============================================

# Create temp test files
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

cat > "$TMPDIR/simple.4th" << 'EOF'
\ Simple test file
: double 2 * ;
42 double .
EOF

cat > "$TMPDIR/inner.4th" << 'EOF'
\ Inner file
: add3 3 + ;
EOF

cat > "$TMPDIR/outer.4th" << 'EOF'
\ Outer file - loads inner
load INNER_PATH
10 add3 .
EOF
# Substitute actual path
sed -i.bak "s|INNER_PATH|$TMPDIR/inner.4th|" "$TMPDIR/outer.4th"

test_contains "load simple file" "load $TMPDIR/simple.4th" "84"
test_contains "load nested files" "load $TMPDIR/outer.4th" "13"
test_contains "load missing file" "load /nonexistent/file.4th" "cannot open file"

echo ""

# ============================================
echo "--- Recording ---"
# ============================================

# Test recording start message
test_contains "rec starts recording" "rec" "Recording started"

# Test stop message
TOTAL=$((TOTAL + 1))
output=$(printf 'rec\n: test 42 ;\nstop\n' | $MIDI_FORTH 2>&1)
if echo "$output" | grep -q "Recording stopped. 1 lines"; then
    echo -e "${GREEN}PASS${NC}: stop shows line count"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC}: stop shows line count"
    echo "  Got: $output"
    FAILED=$((FAILED + 1))
fi

# Test save creates file
TOTAL=$((TOTAL + 1))
rm -f "$TMPDIR/saved.4th"
printf 'rec\n: triple 3 * ;\nstop\nsave %s/saved.4th\n' "$TMPDIR" | $MIDI_FORTH > /dev/null 2>&1
if [ -f "$TMPDIR/saved.4th" ]; then
    echo -e "${GREEN}PASS${NC}: save creates file"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC}: save creates file"
    FAILED=$((FAILED + 1))
fi

# Test saved file can be loaded and executed
TOTAL=$((TOTAL + 1))
output=$(printf 'load %s/saved.4th\n10 triple .\n' "$TMPDIR" | $MIDI_FORTH 2>&1)
if echo "$output" | grep -q "30"; then
    echo -e "${GREEN}PASS${NC}: saved file loads and runs"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC}: saved file loads and runs"
    echo "  Got: $output"
    FAILED=$((FAILED + 1))
fi

# Test control commands not recorded
TOTAL=$((TOTAL + 1))
rm -f "$TMPDIR/control.4th"
printf 'rec\nload %s/simple.4th\nstop\nsave %s/control.4th\n' "$TMPDIR" "$TMPDIR" | $MIDI_FORTH > /dev/null 2>&1
if grep -q "load" "$TMPDIR/control.4th" 2>/dev/null; then
    echo -e "${RED}FAIL${NC}: control commands not recorded (load was recorded)"
    FAILED=$((FAILED + 1))
else
    echo -e "${GREEN}PASS${NC}: control commands not recorded"
    PASSED=$((PASSED + 1))
fi

echo ""

# ============================================
echo "--- MIDI Recording ---"
# ============================================

# Test rec-midi start message
test_contains "rec-midi starts" "rec-midi" "MIDI recording started"

# Test rec-midi stop shows event count
TOTAL=$((TOTAL + 1))
output=$(printf 'midi-open\nrec-midi\n100 dur!\nc4,\nstop\nmidi-close\n' | $MIDI_FORTH 2>&1)
if echo "$output" | grep -q "2 events recorded"; then
    echo -e "${GREEN}PASS${NC}: rec-midi stop shows event count"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC}: rec-midi stop shows event count"
    echo "  Got: $output"
    FAILED=$((FAILED + 1))
fi

# Test save-midi creates file with sequence commands
TOTAL=$((TOTAL + 1))
rm -f "$TMPDIR/recorded.4th"
printf 'midi-open\nrec-midi\n100 dur!\nc4,\ne4,\nstop\nsave-midi %s/recorded.4th\nmidi-close\n' "$TMPDIR" | $MIDI_FORTH > /dev/null 2>&1
if [ -f "$TMPDIR/recorded.4th" ] && grep -q "seq-start" "$TMPDIR/recorded.4th"; then
    echo -e "${GREEN}PASS${NC}: save-midi creates sequence file"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC}: save-midi creates sequence file"
    FAILED=$((FAILED + 1))
fi

# Test recorded file can be loaded and sequence shown
TOTAL=$((TOTAL + 1))
output=$(printf 'load %s/recorded.4th\nseq-show\n' "$TMPDIR" | $MIDI_FORTH 2>&1)
if echo "$output" | grep -q "Sequence 0:" && echo "$output" | grep -q "ON"; then
    echo -e "${GREEN}PASS${NC}: recorded file loads as sequence"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC}: recorded file loads as sequence"
    echo "  Got: $output"
    FAILED=$((FAILED + 1))
fi

echo ""

# ============================================
echo "--- MIDI File I/O ---"
# ============================================

# Test write-mid creates file
TOTAL=$((TOTAL + 1))
rm -f "$TMPDIR/test.mid"
printf 'midi-open\nrec-midi\nc4,\ne4,\nstop\nwrite-mid %s/test.mid\nmidi-close\n' "$TMPDIR" | $MIDI_FORTH > /dev/null 2>&1
if [ -f "$TMPDIR/test.mid" ]; then
    echo -e "${GREEN}PASS${NC}: write-mid creates MIDI file"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC}: write-mid creates MIDI file"
    FAILED=$((FAILED + 1))
fi

# Test read-mid displays file info
TOTAL=$((TOTAL + 1))
output=$(printf 'read-mid %s/test.mid\n' "$TMPDIR" | $MIDI_FORTH 2>&1)
if echo "$output" | grep -q "ppqn: 480" && echo "$output" | grep -q "tracks:"; then
    echo -e "${GREEN}PASS${NC}: read-mid displays file info"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC}: read-mid displays file info"
    echo "  Got: $output"
    FAILED=$((FAILED + 1))
fi

# Test read-mid displays events
TOTAL=$((TOTAL + 1))
if echo "$output" | grep -q "note-on"; then
    echo -e "${GREEN}PASS${NC}: read-mid displays events"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC}: read-mid displays events"
    echo "  Got: $output"
    FAILED=$((FAILED + 1))
fi

echo ""

# ============================================
echo "--- Context Management ---"
# ============================================

test_contains "reset clears stack" "1 2 3 reset .s" "<0>"
test_contains "reset message" "reset" "Interpreter state reset"
test_contains "seq-gc runs" "seq-gc" "seq-gc:"

echo ""

# ============================================
echo "--- Loop Control ---"
# ============================================

test_contains "do loop basic" "10 0 do i . loop" "0 1 2 3 4 5 6 7 8 9"
test_contains "do loop sum" "0 5 0 do i + loop ." "10"
test_contains "+loop skip by 2" "10 0 do i . 2 +loop" "0 2 4 6 8"
test_contains "nested do loop i j" "3 0 do 2 0 do i . j . loop loop" "0 0 1 0 0 1 1 1 0 2 1 2"
test_contains "begin until" "0 begin dup . 1 + dup 5 = until drop" "0 1 2 3 4"
test_contains "begin while repeat" "0 begin dup 5 < while dup . 1 + repeat drop" "0 1 2 3 4"
test_contains "leave exits loop" "10 0 do i . i 3 = if leave then loop" "0 1 2 3"

echo ""

echo "--- MIDI Output ---"
# ============================================

test_contains "midi-output-list runs" "midi-output-list" "Hardware MIDI outputs:"
test_contains "midi-output-close runs" "midi-output-close" "ok"
# Test aliases
test_contains "midi-list alias runs" "midi-list" "Hardware MIDI outputs:"
test_contains "midi-close alias runs" "midi-close" "ok"

echo ""

echo "--- MIDI Input ---"
# ============================================

test_contains "midi-input-list runs" "midi-input-list" "MIDI inputs:"
test_contains "midi-input? returns flag" "midi-input? ." "0"
test_contains "midi-input@ returns 4 values" "midi-input@ .s drop drop drop drop" "<4>"
test_contains "midi-input@ flag is 0 when empty" "midi-input@ swap drop swap drop swap drop ." "0"
test_contains "midi-input-flush runs" "midi-input-flush" "ok"
test_contains "midi-input-close runs" "midi-input-close" "ok"

echo ""

echo "--- Error Handling ---"
# ============================================

test_contains "unknown word" "xyzzy123" "Unknown word"
test_contains "stack underflow" "." "underflow"
test_contains "clear resets stack" "1 2 3 clear .s" "<0>"

echo ""

# ============================================
# Summary
# ============================================
echo "========================================"
echo "Results: $PASSED passed, $FAILED failed, $TOTAL total"
echo "========================================"

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
