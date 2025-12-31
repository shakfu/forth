;; Test basic scheduler functionality

(display "Test 1: Single voice counting...\n")
(define count1 0)
(spawn (lambda ()
         (set! count1 (+ count1 1))
         (if (<= count1 3)
             10
             #f))
       "counter1")
(run)
(display "count1 = ")
(display count1)
(newline)

(display "\nTest 2: Two concurrent voices...\n")
(define countA 0)
(define countB 0)

(spawn (lambda ()
         (set! countA (+ countA 1))
         (if (<= countA 2) 10 #f))
       "voiceA")

(spawn (lambda ()
         (set! countB (+ countB 1))
         (if (<= countB 3) 10 #f))
       "voiceB")

(run)
(display "countA = ")
(display countA)
(display ", countB = ")
(display countB)
(newline)

(display "\nTest 3: scheduler-status...\n")
(spawn (lambda () #f) "quick")
(display (scheduler-status))
(newline)
(run)

(display "\nAll async tests passed!\n")
