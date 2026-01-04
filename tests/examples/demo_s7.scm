;; README.md example for s7-midi
;; Tests the quick example from the project README

(open)

;; Functional transformations
(define melody (scale c4 'dorian))
(define bass (invert melody c4))  ; melodic inversion

;; Thunk-based concurrent voices
(spawn (make-melody-voice melody mf eighth) "melody")
(spawn (make-melody-voice bass ff quarter) "bass")

;; Euclidean rhythm generator
(spawn (lambda ()
  (euclidean 3 8  ; 3 hits over 8 steps
    (lambda () (n c2 ff sixteenth))))
  "rhythm")

(run)
(close)
