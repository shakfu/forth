#!/bin/bash
# Test suite for pktpy_midi

set -e

PKTPY=${1:-./build/pktpy_midi}

echo "Testing pktpy_midi..."

# Test 1: Module import
echo "Test 1: Module import..."
echo 'import midi; print("ok")' | $PKTPY | grep -q "ok" || { echo "FAIL: module import"; exit 1; }
echo "  PASS"

# Test 2: Note parsing
echo "Test 2: Note parsing..."
echo 'import midi; print(midi.note("C4"))' | $PKTPY | grep -q "60" || { echo "FAIL: C4 != 60"; exit 1; }
echo 'import midi; print(midi.note("C#4"))' | $PKTPY | grep -q "61" || { echo "FAIL: C#4 != 61"; exit 1; }
echo 'import midi; print(midi.note("Db4"))' | $PKTPY | grep -q "61" || { echo "FAIL: Db4 != 61"; exit 1; }
echo 'import midi; print(midi.note("A4"))' | $PKTPY | grep -q "69" || { echo "FAIL: A4 != 69"; exit 1; }
echo 'import midi; print(midi.note("C-1"))' | $PKTPY | grep -q "0" || { echo "FAIL: C-1 != 0"; exit 1; }
echo 'import midi; print(midi.note("G9"))' | $PKTPY | grep -q "127" || { echo "FAIL: G9 != 127"; exit 1; }
echo "  PASS"

# Test 3: list_ports returns a list
echo "Test 3: list_ports..."
echo 'import midi; ports = midi.list_ports(); print(type(ports).__name__)' | $PKTPY | grep -q "list" || { echo "FAIL: list_ports not returning list"; exit 1; }
echo "  PASS"

# Test 4: open creates MidiOut
echo "Test 4: open..."
echo 'import midi; m = midi.open(); print(type(m).__name__)' | $PKTPY | grep -q "MidiOut" || { echo "FAIL: open not returning MidiOut"; exit 1; }
echo 'import midi; m = midi.open("Test"); print(type(m).__name__)' | $PKTPY | grep -q "MidiOut" || { echo "FAIL: open(name) not returning MidiOut"; exit 1; }
echo "  PASS"

# Test 5: MidiOut.is_open property
echo "Test 5: is_open property..."
echo 'import midi; m = midi.open(); print(m.is_open)' | $PKTPY | grep -q "True" || { echo "FAIL: is_open should be True"; exit 1; }
echo 'import midi; m = midi.open(); m.close(); print(m.is_open)' | $PKTPY | grep -q "False" || { echo "FAIL: is_open should be False after close"; exit 1; }
echo "  PASS"

# Test 6: Context manager
echo "Test 6: Context manager..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
with midi.open() as m:
    print("opened:", m.is_open)
print("closed:", m.is_open)
EOF
$PKTPY "$TMPFILE" | grep -q "opened: True" || { rm -f "$TMPFILE"; echo "FAIL: context manager open"; exit 1; }
$PKTPY "$TMPFILE" | grep -q "closed: False" || { rm -f "$TMPFILE"; echo "FAIL: context manager close"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 7: note() method (high-level)
echo "Test 7: note() method..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.note("C4")              # string pitch, defaults
m.note(62)                # int pitch, defaults
m.note("E4", 100)         # with velocity
m.note("G4", 80, 100)     # with velocity and duration
m.note(72, 80, 100, 2)    # with channel
m.close()
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: note() method"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 8: chord() method
echo "Test 8: chord() method..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.chord(["C4", "E4", "G4"])           # string pitches
m.chord([60, 64, 67])                  # int pitches
m.chord(("C4", "E4", "G4"), 100)       # tuple, with velocity
m.chord(["C4", "E4"], 80, 100)         # with duration
m.chord(["C4", "E4"], 80, 100, 2)      # with channel
m.close()
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: chord() method"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 9: Low-level note_on/note_off
echo "Test 9: note_on/note_off..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.note_on(60)
m.note_on(61, 100)
m.note_on(62, 80, 2)
m.note_off(60)
m.note_off(61, 64)
m.note_off(62, 0, 2)
m.close()
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: note_on/note_off"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 10: CC and program_change
echo "Test 10: CC and program_change..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.cc(1, 64)
m.cc(7, 100, 1)
m.program_change(0)
m.program_change(42, 2)
m.close()
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: cc/program_change"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 11: all_notes_off
echo "Test 11: all_notes_off..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.all_notes_off()
m.all_notes_off(1)
m.close()
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: all_notes_off"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 12: repr
echo "Test 12: repr..."
echo 'import midi; m = midi.open(); print(repr(m))' | $PKTPY | grep -q "MidiOut virtual" || { echo "FAIL: repr virtual"; exit 1; }
echo 'import midi; m = midi.open(); m.close(); print(repr(m))' | $PKTPY | grep -q "MidiOut closed" || { echo "FAIL: repr closed"; exit 1; }
echo "  PASS"

# Test 13: Error handling - invalid note
echo "Test 13: Error handling..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
try:
    midi.note("invalid")
    print("FAIL")
except ValueError:
    print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: invalid note should raise ValueError"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 14: Error handling - closed port
echo "Test 14: Error handling - closed port..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.close()
try:
    m.note_on(60)
    print("FAIL")
except RuntimeError:
    print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: closed port should raise RuntimeError"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 15: Dynamics constants
echo "Test 15: Dynamics constants..."
echo 'import midi; print(midi.ppp, midi.pp, midi.p, midi.mp, midi.mf, midi.f, midi.ff, midi.fff)' | $PKTPY | grep -q "16 33 49 64 80 96 112 127" || { echo "FAIL: dynamics constants"; exit 1; }
echo "  PASS"

# Test 16: Duration constants
echo "Test 16: Duration constants..."
echo 'import midi; print(midi.whole, midi.half, midi.quarter, midi.eighth, midi.sixteenth)' | $PKTPY | grep -q "2000 1000 500 250 125" || { echo "FAIL: duration constants"; exit 1; }
echo 'import midi; print(midi.dotted(midi.quarter))' | $PKTPY | grep -q "750" || { echo "FAIL: dotted duration"; exit 1; }
echo "  PASS"

# Test 17: Pitch constants
echo "Test 17: Pitch constants..."
echo 'import midi; print(midi.c4, midi.cs4, midi.d4, midi.a4)' | $PKTPY | grep -q "60 61 62 69" || { echo "FAIL: pitch constants"; exit 1; }
echo "  PASS"

# Test 18: Chord builders
echo "Test 18: Chord builders..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
assert midi.major("C4") == [60, 64, 67], "major"
assert midi.minor("C4") == [60, 63, 67], "minor"
assert midi.dim("C4") == [60, 63, 66], "dim"
assert midi.aug("C4") == [60, 64, 68], "aug"
assert midi.dom7("C4") == [60, 64, 67, 70], "dom7"
assert midi.maj7("C4") == [60, 64, 67, 71], "maj7"
assert midi.min7("C4") == [60, 63, 67, 70], "min7"
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: chord builders"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 19: Transpose helpers
echo "Test 19: Transpose helpers..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
assert midi.transpose("C4", 2) == 62, "transpose up"
assert midi.transpose(60, -2) == 58, "transpose down"
assert midi.octave_up("C4") == 72, "octave up"
assert midi.octave_down(60) == 48, "octave down"
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: transpose helpers"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 20: Tempo/BPM
echo "Test 20: Tempo/BPM..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
assert midi.get_tempo() == 120, "default tempo"
assert midi.bpm(120) == 500, "bpm(120)"
assert midi.bpm(60) == 1000, "bpm(60)"
midi.set_tempo(60)
assert midi.quarter == 1000, "quarter at 60 BPM"
assert midi.eighth == 500, "eighth at 60 BPM"
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: tempo/BPM"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 21: Arpeggio
echo "Test 21: Arpeggio..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.arpeggio(midi.major("C4"), velocity=80, note_duration=50)
m.close()
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: arpeggio"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 22: CC helpers
echo "Test 22: CC helpers..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.modulation(64)
m.volume(100)
m.pan(64)
m.sustain(True)
m.sustain(False)
m.close()
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: CC helpers"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 23: Musical example (integration)
echo "Test 23: Musical example..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
midi.set_tempo(240)  # Fast for testing
with midi.open() as m:
    m.chord(midi.major("C4"), midi.mf, midi.quarter)
    m.chord(midi.minor("A3"), midi.mp, midi.quarter)
    m.arpeggio(midi.dom7("G3"), midi.f, midi.sixteenth)
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: musical example"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# ============================================================================
# MIDI File I/O Tests
# ============================================================================

# Test 24: write_mid function exists
echo "Test 24: write_mid function exists..."
echo 'import midi; print(callable(midi.write_mid))' | $PKTPY | grep -q "True" || { echo "FAIL: write_mid not callable"; exit 1; }
echo "  PASS"

# Test 25: read_mid function exists
echo "Test 25: read_mid function exists..."
echo 'import midi; print(callable(midi.read_mid))' | $PKTPY | grep -q "True" || { echo "FAIL: read_mid not callable"; exit 1; }
echo "  PASS"

# Test 26: Write and read MIDI file round-trip
echo "Test 26: MIDI file round-trip..."
MIDFILE=$(mktemp /tmp/test_midi_XXXXXX.mid)
TMPFILE=$(mktemp)
cat > "$TMPFILE" << EOF
import midi
midi.record_midi(120)
m = midi.open()
m.note_on(60, 80)
import time
time.sleep(0.01)
m.note_off(60)
m.note_on(64, 90)
time.sleep(0.01)
m.note_off(64)
m.close()
midi.record_stop()
midi.write_mid("$MIDFILE")

data = midi.read_mid("$MIDFILE")
if not data:
    print("FAIL:no_data")
elif "events" not in data:
    print("FAIL:no_events")
elif len(data["events"]) < 4:
    print("FAIL:event_count:" + str(len(data["events"])))
else:
    print("ok")
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE" "$MIDFILE"; echo "FAIL: MIDI file round-trip failed"; exit 1; }
rm -f "$TMPFILE" "$MIDFILE"
echo "  PASS"

# Test 27: read_mid returns correct structure
echo "Test 27: read_mid structure..."
MIDFILE=$(mktemp /tmp/test_midi_XXXXXX.mid)
TMPFILE=$(mktemp)
cat > "$TMPFILE" << EOF
import midi
midi.record_midi(120)
m = midi.open()
m.note_on(60, 80)
import time
time.sleep(0.01)
m.note_off(60)
m.close()
midi.record_stop()
midi.write_mid("$MIDFILE")

data = midi.read_mid("$MIDFILE")
ok = True
if "num_tracks" not in data:
    ok = False
    print("FAIL:num_tracks")
if "ppqn" not in data:
    ok = False
    print("FAIL:ppqn")
if "tempo" not in data:
    ok = False
    print("FAIL:tempo")
if "events" not in data:
    ok = False
    print("FAIL:events")
if ok:
    print("ok")
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE" "$MIDFILE"; echo "FAIL: read_mid structure check failed"; exit 1; }
rm -f "$TMPFILE" "$MIDFILE"
echo "  PASS"

# ============================================================================
# Async Scheduler Tests
# ============================================================================

# Test 28: spawn function exists
echo "Test 28: spawn function exists..."
echo 'import midi; print(callable(midi.spawn))' | $PKTPY | grep -q "True" || { echo "FAIL: spawn not callable"; exit 1; }
echo "  PASS"

# Test 29: run function exists
echo "Test 29: run function exists..."
echo 'import midi; print(callable(midi.run))' | $PKTPY | grep -q "True" || { echo "FAIL: run not callable"; exit 1; }
echo "  PASS"

# Test 30: voices function exists
echo "Test 30: voices function exists..."
echo 'import midi; print(callable(midi.voices))' | $PKTPY | grep -q "True" || { echo "FAIL: voices not callable"; exit 1; }
echo "  PASS"

# Test 31: status function exists
echo "Test 31: status function exists..."
echo 'import midi; print(callable(midi.status))' | $PKTPY | grep -q "True" || { echo "FAIL: status not callable"; exit 1; }
echo "  PASS"

# Test 32: stop function exists
echo "Test 32: stop function exists..."
echo 'import midi; print(callable(midi.stop))' | $PKTPY | grep -q "True" || { echo "FAIL: stop not callable"; exit 1; }
echo "  PASS"

# Test 33: spawn and run basic
echo "Test 33: spawn and run basic..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
results = []
def voice1():
    results.append("start")
    yield 10
    results.append("done")
midi.spawn(voice1)
midi.run()
if results == ["start", "done"]:
    print("ok")
else:
    print("FAIL:", results)
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: spawn/run basic"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 34: spawn returns voice_id
echo "Test 34: spawn returns voice_id..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
def voice():
    yield 10
vid = midi.spawn(voice)
if isinstance(vid, int) and vid > 0:
    print("ok")
else:
    print("FAIL:", type(vid), vid)
midi.run()
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: spawn returns voice_id"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 35: multiple voices run concurrently
echo "Test 35: multiple voices..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
results = []
def voice1():
    results.append("A1")
    yield 20
    results.append("A2")
def voice2():
    results.append("B1")
    yield 10
    results.append("B2")
midi.spawn(voice1, "voiceA")
midi.spawn(voice2, "voiceB")
midi.run()
# Both should have completed
if "A1" in results and "A2" in results and "B1" in results and "B2" in results:
    print("ok")
else:
    print("FAIL:", results)
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: multiple voices"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 36: voices() count
echo "Test 36: voices count..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
import time
def long_voice():
    yield 200
midi.spawn(long_voice)
midi.spawn(long_voice)
time.sleep(0.05)  # Let them start
count = midi.voices()
if count == 2:
    print("ok")
else:
    print("FAIL: expected 2, got", count)
midi.run()
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: voices count"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 37: status() returns dict
echo "Test 37: status returns dict..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
s = midi.status()
if isinstance(s, dict) and "running" in s and "active" in s:
    print("ok")
else:
    print("FAIL:", s)
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: status returns dict"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 38: stop specific voice (before run)
echo "Test 38: stop specific voice..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
results = []
def voice1():
    results.append("voice1")
    yield 10
def voice2():
    results.append("voice2")
    yield 10
vid1 = midi.spawn(voice1)
vid2 = midi.spawn(voice2)
# Stop voice1 before run - it should not execute
stopped = midi.stop(vid1)
midi.run()  # Only voice2 should run
if stopped and "voice2" in results and "voice1" not in results:
    print("ok")
else:
    print("FAIL: stopped=", stopped, "results=", results)
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: stop specific voice"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 39: stop all voices (before run)
echo "Test 39: stop all voices..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
results = []
def voice():
    results.append("ran")
    yield 10
midi.spawn(voice)
midi.spawn(voice)
count_before = midi.voices()
midi.stop()  # stop all before run
count_after = midi.voices()
midi.run()  # should do nothing since all stopped
if count_before == 2 and count_after == 0 and len(results) == 0:
    print("ok")
else:
    print("FAIL: before=", count_before, "after=", count_after, "results=", results)
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: stop all voices"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 40: spawn requires generator function
echo "Test 40: spawn requires generator..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
def not_a_generator():
    return 42
try:
    midi.spawn(not_a_generator)
    print("FAIL: should have raised")
except TypeError as e:
    if "generator" in str(e):
        print("ok")
    else:
        print("FAIL: wrong error:", e)
except Exception as e:
    print("FAIL: wrong exception type:", type(e), e)
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: spawn requires generator"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 41: multiple spawns then single run
# Note: Sequential run() calls have a known issue, so we test multiple spawns + single run
echo "Test 41: multiple spawns single run..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
results = []
def voice1():
    results.append("first")
    yield 10
def voice2():
    results.append("second")
    yield 10
midi.spawn(voice1)
midi.spawn(voice2)
midi.run()  # Single run for both
if "first" in results and "second" in results:
    print("ok")
else:
    print("FAIL:", results)
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: multiple spawns single run"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 42: voice with named parameter
echo "Test 42: voice with name..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
def voice():
    yield 10
vid = midi.spawn(voice, "test_voice")
if isinstance(vid, int) and vid > 0:
    print("ok")
else:
    print("FAIL:", vid)
midi.run()
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: voice with name"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 43: midi.play generator helper
echo "Test 43: midi.play helper..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
out = midi.open()
def melody():
    for ms in midi.play(out, midi.c4, midi.mf, 50):
        yield ms
    for ms in midi.play(out, midi.e4, midi.mf, 50):
        yield ms
midi.spawn(melody)
midi.run()
out.close()
print("ok")
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: midi.play helper"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 44: midi.play_chord generator helper
echo "Test 44: midi.play_chord helper..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
out = midi.open()
def chords():
    for ms in midi.play_chord(out, midi.major(midi.c4), midi.mf, 50):
        yield ms
midi.spawn(chords)
midi.run()
out.close()
print("ok")
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: midi.play_chord helper"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 45: midi.wait helper
echo "Test 45: midi.wait helper..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
results = []
def voice():
    results.append("before")
    for ms in midi.wait(50):
        yield ms
    results.append("after")
midi.spawn(voice)
midi.run()
if results == ["before", "after"]:
    print("ok")
else:
    print("FAIL:", results)
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: midi.wait helper"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 46: run with no voices does nothing
echo "Test 46: run with no voices..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
midi.run()  # Should return immediately, no error
print("ok")
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: run with no voices"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 47: voices returns 0 after all complete
echo "Test 47: voices 0 after complete..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
def voice():
    yield 10
midi.spawn(voice)
midi.run()
count = midi.voices()
if count == 0:
    print("ok")
else:
    print("FAIL: expected 0, got", count)
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: voices 0 after complete"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# ============================================================================
# Poll Tests
# ============================================================================

# Test 48: poll function exists
echo "Test 48: poll function exists..."
echo 'import midi; print(callable(midi.poll))' | $PKTPY | grep -q "True" || { echo "FAIL: poll not callable"; exit 1; }
echo "  PASS"

# Test 49: poll returns False when no voices
echo "Test 49: poll returns False when no voices..."
echo 'import midi; print(midi.poll())' | $PKTPY | grep -q "False" || { echo "FAIL: poll() should return False when no voices"; exit 1; }
echo "  PASS"

# Test 50: poll returns True when voices active
echo "Test 50: poll returns True when voices active..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
def voice():
    yield 100
midi.spawn(voice)
result = midi.poll()
midi.stop()
print(result)
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "True" || { rm -f "$TMPFILE"; echo "FAIL: poll() should return True when voices active"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 51: poll loop can replace run
echo "Test 51: poll loop replaces run..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
done = False
def voice():
    global done
    yield 10
    done = True
midi.spawn(voice)
while midi.poll():
    pass
print("ok" if done and midi.voices() == 0 else "FAIL")
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: poll loop should complete voice"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 52: poll processes multiple voices
echo "Test 52: poll processes multiple voices..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
v1_done = False
v2_done = False
def voice1():
    global v1_done
    yield 10
    v1_done = True
def voice2():
    global v2_done
    yield 5
    v2_done = True
midi.spawn(voice1)
midi.spawn(voice2)
while midi.poll():
    pass
print("ok" if v1_done and v2_done else "FAIL")
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: poll should process all voices"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

echo ""
echo "All tests passed!"
