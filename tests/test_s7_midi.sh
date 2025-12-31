#!/bin/bash
# Test suite for s7_midi

set -e

S7MIDI=${1:-./build/s7_midi}

echo "Testing s7_midi..."

# Test 1: Basic evaluation
echo "Test 1: Basic evaluation..."
result=$($S7MIDI -e '(+ 1 2)')
[ "$result" = "3" ] || { echo "FAIL: (+ 1 2) != 3, got $result"; exit 1; }
echo "  PASS"

# Test 2: Note parsing
echo "Test 2: Note parsing..."
result=$($S7MIDI -e '(note "C4")')
[ "$result" = "60" ] || { echo "FAIL: C4 != 60, got $result"; exit 1; }
result=$($S7MIDI -e '(note "C#4")')
[ "$result" = "61" ] || { echo "FAIL: C#4 != 61, got $result"; exit 1; }
result=$($S7MIDI -e '(note "A4")')
[ "$result" = "69" ] || { echo "FAIL: A4 != 69, got $result"; exit 1; }
echo "  PASS"

# Test 3: Pitch constants
echo "Test 3: Pitch constants..."
result=$($S7MIDI -e 'c4')
[ "$result" = "60" ] || { echo "FAIL: c4 != 60, got $result"; exit 1; }
result=$($S7MIDI -e 'cs4')
[ "$result" = "61" ] || { echo "FAIL: cs4 != 61, got $result"; exit 1; }
result=$($S7MIDI -e 'a4')
[ "$result" = "69" ] || { echo "FAIL: a4 != 69, got $result"; exit 1; }
echo "  PASS"

# Test 4: Dynamics constants
echo "Test 4: Dynamics constants..."
result=$($S7MIDI -e 'ppp')
[ "$result" = "16" ] || { echo "FAIL: ppp != 16, got $result"; exit 1; }
result=$($S7MIDI -e 'mf')
[ "$result" = "80" ] || { echo "FAIL: mf != 80, got $result"; exit 1; }
result=$($S7MIDI -e 'fff')
[ "$result" = "127" ] || { echo "FAIL: fff != 127, got $result"; exit 1; }
echo "  PASS"

# Test 5: Duration constants
echo "Test 5: Duration constants..."
result=$($S7MIDI -e 'quarter')
[ "$result" = "500" ] || { echo "FAIL: quarter != 500, got $result"; exit 1; }
result=$($S7MIDI -e 'whole')
[ "$result" = "2000" ] || { echo "FAIL: whole != 2000, got $result"; exit 1; }
result=$($S7MIDI -e '(dotted quarter)')
[ "$result" = "750" ] || { echo "FAIL: dotted quarter != 750, got $result"; exit 1; }
echo "  PASS"

# Test 6: Tempo functions
echo "Test 6: Tempo functions..."
result=$($S7MIDI -e '(get-tempo)')
[ "$result" = "120" ] || { echo "FAIL: default tempo != 120, got $result"; exit 1; }
result=$($S7MIDI -e '(bpm 120)')
[ "$result" = "500" ] || { echo "FAIL: bpm 120 != 500, got $result"; exit 1; }
result=$($S7MIDI -e '(bpm 60)')
[ "$result" = "1000" ] || { echo "FAIL: bpm 60 != 1000, got $result"; exit 1; }
echo "  PASS"

# Test 7: Chord builders
echo "Test 7: Chord builders..."
result=$($S7MIDI -e '(major c4)')
[ "$result" = "(60 64 67)" ] || { echo "FAIL: major c4 != (60 64 67), got $result"; exit 1; }
result=$($S7MIDI -e '(minor c4)')
[ "$result" = "(60 63 67)" ] || { echo "FAIL: minor c4 != (60 63 67), got $result"; exit 1; }
result=$($S7MIDI -e '(dim c4)')
[ "$result" = "(60 63 66)" ] || { echo "FAIL: dim c4 != (60 63 66), got $result"; exit 1; }
result=$($S7MIDI -e '(aug c4)')
[ "$result" = "(60 64 68)" ] || { echo "FAIL: aug c4 != (60 64 68), got $result"; exit 1; }
result=$($S7MIDI -e '(dom7 c4)')
[ "$result" = "(60 64 67 70)" ] || { echo "FAIL: dom7 c4 != (60 64 67 70), got $result"; exit 1; }
result=$($S7MIDI -e '(maj7 c4)')
[ "$result" = "(60 64 67 71)" ] || { echo "FAIL: maj7 c4 != (60 64 67 71), got $result"; exit 1; }
result=$($S7MIDI -e '(min7 c4)')
[ "$result" = "(60 63 67 70)" ] || { echo "FAIL: min7 c4 != (60 63 67 70), got $result"; exit 1; }
echo "  PASS"

# Test 8: Transpose helpers
echo "Test 8: Transpose helpers..."
result=$($S7MIDI -e '(transpose c4 2)')
[ "$result" = "62" ] || { echo "FAIL: transpose c4 2 != 62, got $result"; exit 1; }
result=$($S7MIDI -e '(octave-up c4)')
[ "$result" = "72" ] || { echo "FAIL: octave-up c4 != 72, got $result"; exit 1; }
result=$($S7MIDI -e '(octave-down c4)')
[ "$result" = "48" ] || { echo "FAIL: octave-down c4 != 48, got $result"; exit 1; }
echo "  PASS"

# Test 9: midi-list-ports returns list
echo "Test 9: midi-list-ports..."
result=$($S7MIDI -e '(list? (midi-list-ports))')
[ "$result" = "#t" ] || { echo "FAIL: midi-list-ports not returning list"; exit 1; }
echo "  PASS"

# Test 10: midi-open creates midi-out
echo "Test 10: midi-open..."
result=$($S7MIDI -e '(begin (define m (midi-open)) (midi-out? m))')
[ "$result" = "#t" ] || { echo "FAIL: midi-open not returning midi-out"; exit 1; }
result=$($S7MIDI -e '(begin (define m (midi-open "Test")) (midi-out? m))')
[ "$result" = "#t" ] || { echo "FAIL: midi-open with name not returning midi-out"; exit 1; }
echo "  PASS"

# Test 11: midi-open? property
echo "Test 11: midi-open? property..."
result=$($S7MIDI -e '(begin (define m (midi-open)) (midi-open? m))')
[ "$result" = "#t" ] || { echo "FAIL: midi-open? should be #t"; exit 1; }
result=$($S7MIDI -e '(begin (define m (midi-open)) (midi-close m) (midi-open? m))')
[ "$result" = "#f" ] || { echo "FAIL: midi-open? should be #f after close"; exit 1; }
echo "  PASS"

# Test 12: midi-note
echo "Test 12: midi-note..."
result=$($S7MIDI -e '(begin (define m (midi-open)) (midi-note m c4) (midi-close m) "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: midi-note failed"; exit 1; }
result=$($S7MIDI -e '(begin (define m (midi-open)) (midi-note m 60 80 100) (midi-close m) "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: midi-note with params failed"; exit 1; }
echo "  PASS"

# Test 13: midi-chord
echo "Test 13: midi-chord..."
result=$($S7MIDI -e '(begin (define m (midi-open)) (midi-chord m (major c4)) (midi-close m) "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: midi-chord failed"; exit 1; }
result=$($S7MIDI -e '(begin (define m (midi-open)) (midi-chord m (list 60 64 67) 80 100) (midi-close m) "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: midi-chord with params failed"; exit 1; }
echo "  PASS"

# Test 14: midi-note-on/off
echo "Test 14: midi-note-on/off..."
result=$($S7MIDI -e '(begin (define m (midi-open)) (midi-note-on m 60) (midi-note-off m 60) (midi-close m) "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: note-on/off failed"; exit 1; }
echo "  PASS"

# Test 15: midi-cc
echo "Test 15: midi-cc..."
result=$($S7MIDI -e '(begin (define m (midi-open)) (midi-cc m 1 64) (midi-close m) "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: midi-cc failed"; exit 1; }
echo "  PASS"

# Test 16: midi-program
echo "Test 16: midi-program..."
result=$($S7MIDI -e '(begin (define m (midi-open)) (midi-program m 0) (midi-close m) "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: midi-program failed"; exit 1; }
echo "  PASS"

# Test 17: midi-all-notes-off
echo "Test 17: midi-all-notes-off..."
result=$($S7MIDI -e '(begin (define m (midi-open)) (midi-all-notes-off m) (midi-close m) "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: midi-all-notes-off failed"; exit 1; }
echo "  PASS"

# Test 18: midi-arpeggio
echo "Test 18: midi-arpeggio..."
result=$($S7MIDI -e '(begin (define m (midi-open)) (midi-arpeggio m (major c4) mf 50) (midi-close m) "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: midi-arpeggio failed"; exit 1; }
echo "  PASS"

# Test 19: rest
echo "Test 19: rest..."
result=$($S7MIDI -e '(begin (rest 10) "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: rest failed"; exit 1; }
echo "  PASS"

# Test 20: Error handling - invalid note
echo "Test 20: Error handling..."
result=$($S7MIDI -e '(note "invalid")' 2>&1 || true)
echo "$result" | grep -q "invalid-note" || { echo "FAIL: invalid note should raise error"; exit 1; }
echo "  PASS"

# Test 21: Musical example
echo "Test 21: Musical example..."
result=$($S7MIDI -e '(begin
  (define m (midi-open))
  (set-tempo! 240)
  (midi-chord m (major c4) mf quarter)
  (midi-chord m (minor a3) mp quarter)
  (midi-arpeggio m (dom7 g3) f sixteenth)
  (midi-close m)
  "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: musical example failed, got $result"; exit 1; }
echo "  PASS"

# Test 22: REPL convenience functions - *midi* global
echo "Test 22: REPL convenience - *midi* global..."
result=$($S7MIDI -e '*midi*')
[ "$result" = "#f" ] || { echo "FAIL: *midi* should be #f initially, got $result"; exit 1; }
result=$($S7MIDI -e '(begin (open) (midi-out? *midi*))')
[ "$result" = "#t" ] || { echo "FAIL: (open) should set *midi* to midi-out"; exit 1; }
echo "  PASS"

# Test 23: REPL convenience functions - n (note)
echo "Test 23: REPL convenience - n..."
result=$($S7MIDI -e '(begin (open) (n c4) (close) "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: (n c4) failed"; exit 1; }
result=$($S7MIDI -e '(begin (open) (n c4 mf quarter) (close) "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: (n c4 mf quarter) failed"; exit 1; }
echo "  PASS"

# Test 24: REPL convenience functions - ch (chord)
echo "Test 24: REPL convenience - ch..."
result=$($S7MIDI -e '(begin (open) (ch (major c4)) (close) "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: (ch (major c4)) failed"; exit 1; }
echo "  PASS"

# Test 25: REPL convenience functions - arp (arpeggio)
echo "Test 25: REPL convenience - arp..."
result=$($S7MIDI -e '(begin (open) (arp (major c4) mf 50) (close) "ok")')
[ "$result" = "\"ok\"" ] || { echo "FAIL: (arp (major c4)) failed"; exit 1; }
echo "  PASS"

# Test 26: REPL convenience - error without open
echo "Test 26: REPL convenience - error without open..."
result=$($S7MIDI -e '(n c4)' 2>&1 || true)
echo "$result" | grep -q "no-midi-port\|No MIDI port" || { echo "FAIL: n without open should raise error"; exit 1; }
echo "  PASS"

# ============================================================================
# MIDI File I/O Tests
# ============================================================================

# Test 27: write-mid function exists
echo "Test 27: write-mid function exists..."
result=$($S7MIDI -e '(procedure? write-mid)')
[ "$result" = "#t" ] || { echo "FAIL: write-mid not a procedure"; exit 1; }
echo "  PASS"

# Test 28: read-mid function exists
echo "Test 28: read-mid function exists..."
result=$($S7MIDI -e '(procedure? read-mid)')
[ "$result" = "#t" ] || { echo "FAIL: read-mid not a procedure"; exit 1; }
echo "  PASS"

# Test 29: Write and read MIDI file round-trip
echo "Test 29: MIDI file round-trip..."
TMPFILE=$(mktemp /tmp/test_midi_XXXXXX.mid)
result=$($S7MIDI -e "(begin
  (record-midi 120)
  (define m (midi-open))
  (midi-note-on m 60 80)
  (midi-sleep 10)
  (midi-note-off m 60)
  (midi-note-on m 64 90)
  (midi-sleep 10)
  (midi-note-off m 64)
  (midi-close m)
  (record-stop)
  (write-mid \"$TMPFILE\")

  (define data (read-mid \"$TMPFILE\"))
  (if (not data)
      \"FAIL:no_data\"
      (let ((events (cdr (assq 'events data))))
        (if (not events)
            \"FAIL:no_events\"
            (if (< (length events) 4)
                (string-append \"FAIL:event_count:\" (number->string (length events)))
                \"ok\")))))" 2>&1)
rm -f "$TMPFILE"
echo "$result" | grep -q "ok" || { echo "FAIL: MIDI file round-trip failed: $result"; exit 1; }
echo "  PASS"

# Test 30: read-mid returns correct structure
echo "Test 30: read-mid structure..."
TMPFILE=$(mktemp /tmp/test_midi_XXXXXX.mid)
result=$($S7MIDI -e "(begin
  (record-midi 120)
  (define m (midi-open))
  (midi-note-on m 60 80)
  (midi-sleep 10)
  (midi-note-off m 60)
  (midi-close m)
  (record-stop)
  (write-mid \"$TMPFILE\")

  (define data (read-mid \"$TMPFILE\"))
  (define ok #t)
  (if (not (assq 'num-tracks data)) (begin (set! ok #f) (display \"FAIL:num-tracks\")))
  (if (not (assq 'ppqn data)) (begin (set! ok #f) (display \"FAIL:ppqn\")))
  (if (not (assq 'tempo data)) (begin (set! ok #f) (display \"FAIL:tempo\")))
  (if (not (assq 'events data)) (begin (set! ok #f) (display \"FAIL:events\")))
  (if ok \"ok\" \"FAIL\"))" 2>&1)
rm -f "$TMPFILE"
echo "$result" | grep -q "ok" || { echo "FAIL: read-mid structure check failed: $result"; exit 1; }
echo "  PASS"

# ============================================================================
# Async scheduler tests
# ============================================================================

# Test 31: spawn returns voice id
echo "Test 31: spawn returns voice id..."
result=$($S7MIDI -e "(number? (spawn (lambda () #f)))" 2>&1)
echo "$result" | grep -q "#t" || { echo "FAIL: spawn should return number, got: $result"; exit 1; }
echo "  PASS"

# Test 32: voices count
echo "Test 32: voices count..."
result=$($S7MIDI -e "(begin (spawn (lambda () #f)) (voices))" 2>&1)
echo "$result" | grep -q "1" || { echo "FAIL: voices should return 1, got: $result"; exit 1; }
echo "  PASS"

# Test 33: run completes all voices
echo "Test 33: run completes all voices..."
result=$($S7MIDI -e "(begin (spawn (lambda () #f)) (run) (voices))" 2>&1)
echo "$result" | grep -q "0" || { echo "FAIL: voices after run should be 0, got: $result"; exit 1; }
echo "  PASS"

# Test 34: multi-step voice
echo "Test 34: multi-step voice..."
result=$($S7MIDI -e "(begin
  (define count 0)
  (spawn (lambda ()
           (set! count (+ count 1))
           (if (<= count 3) 10 #f)))
  (run)
  count)" 2>&1)
echo "$result" | grep -q "4" || { echo "FAIL: multi-step voice count should be 4, got: $result"; exit 1; }
echo "  PASS"

# Test 35: multiple voices
echo "Test 35: multiple concurrent voices..."
result=$($S7MIDI -e "(begin
  (define a 0)
  (define b 0)
  (spawn (lambda () (set! a (+ a 1)) (if (<= a 2) 10 #f)))
  (spawn (lambda () (set! b (+ b 1)) (if (<= b 3) 10 #f)))
  (run)
  (list a b))" 2>&1)
echo "$result" | grep -q "(3 4)" || { echo "FAIL: multi-voice counts should be (3 4), got: $result"; exit 1; }
echo "  PASS"

# Test 36: scheduler-status
echo "Test 36: scheduler-status..."
result=$($S7MIDI -e "(begin (spawn (lambda () #f) \"test\") (scheduler-status))" 2>&1)
echo "$result" | grep -q "test" || { echo "FAIL: scheduler-status should contain voice name, got: $result"; exit 1; }
echo "  PASS"

# Test 37: stop voice
echo "Test 37: stop voice..."
result=$($S7MIDI -e "(begin
  (define v (spawn (lambda () 1000)))  ; would wait 1 second
  (stop v)
  (voices))" 2>&1)
echo "$result" | grep -q "0" || { echo "FAIL: voices after stop should be 0, got: $result"; exit 1; }
echo "  PASS"

# Test 38: sequential spawn/run cycles
echo "Test 38: sequential spawn/run cycles..."
result=$($S7MIDI -e "(begin
  (spawn (lambda () #f))
  (run)
  (spawn (lambda () #f))
  (run)
  (display \"ok\"))" 2>&1)
echo "$result" | grep -q "ok" || { echo "FAIL: sequential spawn/run failed: $result"; exit 1; }
echo "  PASS"

# ============================================================================
# Poll Tests
# ============================================================================

# Test 39: poll function exists
echo "Test 39: poll function exists..."
result=$($S7MIDI -e '(procedure? poll)')
[ "$result" = "#t" ] || { echo "FAIL: poll not a procedure, got $result"; exit 1; }
echo "  PASS"

# Test 40: poll returns #f when no voices
echo "Test 40: poll returns #f when no voices..."
result=$($S7MIDI -e '(poll)')
[ "$result" = "#f" ] || { echo "FAIL: poll should return #f when no voices, got $result"; exit 1; }
echo "  PASS"

# Test 41: poll returns #t when voices active
echo "Test 41: poll returns #t when voices active..."
result=$($S7MIDI -e "(begin (spawn (lambda () 100)) (poll))" 2>&1)
echo "$result" | grep -q "#t" || { echo "FAIL: poll should return #t when voices active, got: $result"; exit 1; }
echo "  PASS"

# Test 42: poll loop can replace run
echo "Test 42: poll loop replaces run..."
result=$($S7MIDI -e "(begin
  (define done #f)
  (spawn (lambda () (set! done #t) #f))
  (let loop () (when (poll) (loop)))
  (list done (voices)))" 2>&1)
echo "$result" | grep -q "(#t 0)" || { echo "FAIL: poll loop should complete voice, got: $result"; exit 1; }
echo "  PASS"

# Test 43: poll processes multiple voices
echo "Test 43: poll processes multiple voices..."
result=$($S7MIDI -e "(begin
  (define v1 #f)
  (define v2 #f)
  (spawn (lambda () (set! v1 #t) #f))
  (spawn (lambda () (set! v2 #t) #f))
  (let loop () (when (poll) (loop)))
  (and v1 v2))" 2>&1)
echo "$result" | grep -q "#t" || { echo "FAIL: poll should process all voices, got: $result"; exit 1; }
echo "  PASS"

echo ""
echo "All tests passed!"
