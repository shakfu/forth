;; Dynamics (velocity values)
(define ppp 16)
(define pp 33)
(define p 49)
(define mp 64)
(define mf 80)
(define f 96)
(define ff 112)
(define fff 127)

;; Durations (milliseconds at 120 BPM)
(define whole 2000)
(define half 1000)
(define quarter 500)
(define eighth 250)
(define sixteenth 125)

(define (dotted dur) (floor (* dur 1.5)))

;; Tempo
(define *bpm* 120)

(define (set-tempo! bpm)
  (set! *bpm* bpm)
  (set-tempo-c! bpm)  ; Update C layer for duration scaling
  (let ((beat-ms (floor (/ 60000 bpm))))
    (set! quarter beat-ms)
    (set! half (* beat-ms 2))
    (set! whole (* beat-ms 4))
    (set! eighth (floor (/ beat-ms 2)))
    (set! sixteenth (floor (/ beat-ms 4)))))

(define (get-tempo) *bpm*)

(define (bpm tempo) (floor (/ 60000 tempo)))

;; Pitch constants
(define c0 12) (define cs0 13) (define db0 13) (define d0 14)
(define ds0 15) (define eb0 15) (define e0 16) (define f0 17)
(define fs0 18) (define gb0 18) (define g0 19) (define gs0 20)
(define ab0 20) (define a0 21) (define as0 22) (define bb0 22) (define b0 23)

(define c1 24) (define cs1 25) (define db1 25) (define d1 26)
(define ds1 27) (define eb1 27) (define e1 28) (define f1 29)
(define fs1 30) (define gb1 30) (define g1 31) (define gs1 32)
(define ab1 32) (define a1 33) (define as1 34) (define bb1 34) (define b1 35)

(define c2 36) (define cs2 37) (define db2 37) (define d2 38)
(define ds2 39) (define eb2 39) (define e2 40) (define f2 41)
(define fs2 42) (define gb2 42) (define g2 43) (define gs2 44)
(define ab2 44) (define a2 45) (define as2 46) (define bb2 46) (define b2 47)

(define c3 48) (define cs3 49) (define db3 49) (define d3 50)
(define ds3 51) (define eb3 51) (define e3 52) (define f3 53)
(define fs3 54) (define gb3 54) (define g3 55) (define gs3 56)
(define ab3 56) (define a3 57) (define as3 58) (define bb3 58) (define b3 59)

(define c4 60) (define cs4 61) (define db4 61) (define d4 62)
(define ds4 63) (define eb4 63) (define e4 64) (define f4 65)
(define fs4 66) (define gb4 66) (define g4 67) (define gs4 68)
(define ab4 68) (define a4 69) (define as4 70) (define bb4 70) (define b4 71)

(define c5 72) (define cs5 73) (define db5 73) (define d5 74)
(define ds5 75) (define eb5 75) (define e5 76) (define f5 77)
(define fs5 78) (define gb5 78) (define g5 79) (define gs5 80)
(define ab5 80) (define a5 81) (define as5 82) (define bb5 82) (define b5 83)

(define c6 84) (define cs6 85) (define db6 85) (define d6 86)
(define ds6 87) (define eb6 87) (define e6 88) (define f6 89)
(define fs6 90) (define gb6 90) (define g6 91) (define gs6 92)
(define ab6 92) (define a6 93) (define as6 94) (define bb6 94) (define b6 95)

(define c7 96) (define cs7 97) (define db7 97) (define d7 98)
(define ds7 99) (define eb7 99) (define e7 100) (define f7 101)
(define fs7 102) (define gb7 102) (define g7 103) (define gs7 104)
(define ab7 104) (define a7 105) (define as7 106) (define bb7 106) (define b7 107)

(define c8 108) (define cs8 109) (define db8 109) (define d8 110)
(define ds8 111) (define eb8 111) (define e8 112) (define f8 113)
(define fs8 114) (define gb8 114) (define g8 115) (define gs8 116)
(define ab8 116) (define a8 117) (define as8 118) (define bb8 118) (define b8 119)

;; ============================================================================
;; Scale intervals (semitones from root)
;; ============================================================================

;; Diatonic modes
(define scale-major '(0 2 4 5 7 9 11))
(define scale-ionian scale-major)
(define scale-dorian '(0 2 3 5 7 9 10))
(define scale-phrygian '(0 1 3 5 7 8 10))
(define scale-lydian '(0 2 4 6 7 9 11))
(define scale-mixolydian '(0 2 4 5 7 9 10))
(define scale-minor '(0 2 3 5 7 8 10))
(define scale-aeolian scale-minor)
(define scale-locrian '(0 1 3 5 6 8 10))

;; Other minor scales
(define scale-harmonic-minor '(0 2 3 5 7 8 11))
(define scale-melodic-minor '(0 2 3 5 7 9 11))

;; Pentatonic
(define scale-pentatonic '(0 2 4 7 9))
(define scale-pentatonic-major scale-pentatonic)
(define scale-pentatonic-minor '(0 3 5 7 10))

;; Blues
(define scale-blues '(0 3 5 6 7 10))

;; Symmetric
(define scale-whole-tone '(0 2 4 6 8 10))
(define scale-chromatic '(0 1 2 3 4 5 6 7 8 9 10 11))
(define scale-diminished-hw '(0 1 3 4 6 7 9 10))
(define scale-diminished-wh '(0 2 3 5 6 8 9 11))
(define scale-augmented '(0 3 4 7 8 11))

;; Bebop
(define scale-bebop-dominant '(0 2 4 5 7 9 10 11))
(define scale-bebop-major '(0 2 4 5 7 8 9 11))
(define scale-bebop-minor '(0 2 3 5 7 8 9 10))

;; Exotic/World
(define scale-hungarian-minor '(0 2 3 6 7 8 11))
(define scale-double-harmonic '(0 1 4 5 7 8 11))
(define scale-neapolitan-major '(0 1 3 5 7 9 11))
(define scale-neapolitan-minor '(0 1 3 5 7 8 11))
(define scale-phrygian-dominant '(0 1 4 5 7 8 10))
(define scale-persian '(0 1 4 5 6 8 11))
(define scale-altered '(0 1 3 4 6 8 10))
(define scale-enigmatic '(0 1 4 6 8 10 11))

;; Japanese
(define scale-hirajoshi '(0 2 3 7 8))
(define scale-in-sen '(0 1 5 7 10))
(define scale-iwato '(0 1 5 6 10))
(define scale-kumoi '(0 2 3 7 9))

;; Other world scales
(define scale-egyptian '(0 2 5 7 10))
(define scale-romanian-minor '(0 2 3 6 7 9 10))
(define scale-spanish-8-tone '(0 1 3 4 5 6 8 10))

;; Arabic Maqamat (12-TET approximations)
(define scale-maqam-hijaz '(0 1 4 5 7 8 10))
(define scale-maqam-nahawand '(0 2 3 5 7 8 11))
(define scale-maqam-nikriz '(0 2 3 6 7 9 10))
(define scale-maqam-athar-kurd '(0 1 3 5 6 8 10))
(define scale-maqam-shawq-afza '(0 2 3 6 7 9 11))
(define scale-maqam-jiharkah '(0 2 4 5 7 9 10))

;; Indian Ragas (12-TET approximations)
(define scale-raga-bhairav '(0 1 4 5 7 8 11))
(define scale-raga-todi '(0 1 3 6 7 8 11))
(define scale-raga-marwa '(0 1 4 6 7 9 11))
(define scale-raga-purvi '(0 1 4 6 7 8 11))
(define scale-raga-charukeshi '(0 2 4 5 7 8 10))
(define scale-raga-asavari '(0 2 3 5 7 8 10))
(define scale-raga-bilawal '(0 2 4 5 7 9 11))
(define scale-raga-khamaj '(0 2 4 5 7 9 10))
(define scale-raga-kalyan '(0 2 4 6 7 9 11))
(define scale-raga-bhimpalasi '(0 3 5 7 10))
(define scale-raga-darbari '(0 2 3 5 7 8 9))

;; ============================================================================
;; Microtonal scales (cents-based, for use with pitch-bend)
;; ============================================================================

;; Arabic Maqamat with quarter tones
(define scale-maqam-bayati-cents '(0 150 300 500 700 800 1000))
(define scale-maqam-rast-cents '(0 200 350 500 700 900 1050))
(define scale-maqam-saba-cents '(0 150 300 400 500 700 800))
(define scale-maqam-sikah-cents '(0 150 350 500 650 850 1000))
(define scale-maqam-huzam-cents '(0 150 350 500 700 850 1050))
(define scale-maqam-iraq-cents '(0 150 350 500 700 850 1000))
(define scale-maqam-bastanikar-cents '(0 150 350 500 700 800 1000))

;; Turkish Makamlar
(define scale-makam-ussak-cents '(0 150 300 500 700 800 1000))
(define scale-makam-huseyni-cents '(0 150 300 500 700 900 1000))

;; Indian 22-Shruti scale
(define scale-shruti-cents '(0 90 112 182 204 294 316 386 408 498 520 590
                             612 702 792 814 884 906 996 1018 1088 1110))

;; ============================================================================
;; Scale name lookup table
;; ============================================================================

(define *scales*
  (list
    (cons 'major scale-major)
    (cons 'ionian scale-ionian)
    (cons 'dorian scale-dorian)
    (cons 'phrygian scale-phrygian)
    (cons 'lydian scale-lydian)
    (cons 'mixolydian scale-mixolydian)
    (cons 'minor scale-minor)
    (cons 'aeolian scale-aeolian)
    (cons 'locrian scale-locrian)
    (cons 'harmonic-minor scale-harmonic-minor)
    (cons 'melodic-minor scale-melodic-minor)
    (cons 'pentatonic scale-pentatonic)
    (cons 'pentatonic-major scale-pentatonic-major)
    (cons 'pentatonic-minor scale-pentatonic-minor)
    (cons 'blues scale-blues)
    (cons 'whole-tone scale-whole-tone)
    (cons 'chromatic scale-chromatic)
    (cons 'diminished-hw scale-diminished-hw)
    (cons 'diminished-wh scale-diminished-wh)
    (cons 'augmented scale-augmented)
    (cons 'bebop-dominant scale-bebop-dominant)
    (cons 'bebop-major scale-bebop-major)
    (cons 'bebop-minor scale-bebop-minor)
    (cons 'hungarian-minor scale-hungarian-minor)
    (cons 'double-harmonic scale-double-harmonic)
    (cons 'neapolitan-major scale-neapolitan-major)
    (cons 'neapolitan-minor scale-neapolitan-minor)
    (cons 'phrygian-dominant scale-phrygian-dominant)
    (cons 'persian scale-persian)
    (cons 'altered scale-altered)
    (cons 'enigmatic scale-enigmatic)
    (cons 'hirajoshi scale-hirajoshi)
    (cons 'in-sen scale-in-sen)
    (cons 'iwato scale-iwato)
    (cons 'kumoi scale-kumoi)
    (cons 'egyptian scale-egyptian)
    (cons 'romanian-minor scale-romanian-minor)
    (cons 'spanish-8-tone scale-spanish-8-tone)
    (cons 'maqam-hijaz scale-maqam-hijaz)
    (cons 'maqam-nahawand scale-maqam-nahawand)
    (cons 'maqam-nikriz scale-maqam-nikriz)
    (cons 'maqam-athar-kurd scale-maqam-athar-kurd)
    (cons 'maqam-shawq-afza scale-maqam-shawq-afza)
    (cons 'maqam-jiharkah scale-maqam-jiharkah)
    (cons 'raga-bhairav scale-raga-bhairav)
    (cons 'raga-todi scale-raga-todi)
    (cons 'raga-marwa scale-raga-marwa)
    (cons 'raga-purvi scale-raga-purvi)
    (cons 'raga-charukeshi scale-raga-charukeshi)
    (cons 'raga-asavari scale-raga-asavari)
    (cons 'raga-bilawal scale-raga-bilawal)
    (cons 'raga-khamaj scale-raga-khamaj)
    (cons 'raga-kalyan scale-raga-kalyan)
    (cons 'raga-bhimpalasi scale-raga-bhimpalasi)
    (cons 'raga-darbari scale-raga-darbari)))

;; ============================================================================
;; Scale helper functions
;; ============================================================================

(define (get-scale name)
  "Look up scale intervals by name symbol"
  (let ((entry (assq name *scales*)))
    (if entry (cdr entry)
        (error 'unknown-scale "Unknown scale name" name))))

(define (scale root name)
  "Build a scale from root and scale name. E.g. (scale c4 'major)"
  (build-scale root (get-scale name)))

(define (degree root name n)
  "Get the nth degree of a named scale. E.g. (degree c4 'major 5)"
  (scale-degree root (get-scale name) n))

(define (in-scale-named? pitch root name)
  "Check if pitch is in named scale. E.g. (in-scale-named? e4 c4 'major)"
  (in-scale? pitch root (get-scale name)))

(define (quantize pitch root name)
  "Quantize pitch to named scale. E.g. (quantize fs4 c4 'major)"
  (quantize-to-scale pitch root (get-scale name)))

;; Microtonal helper: convert cents interval to note + bend
(define (cents-to-note root cents)
  "Convert cents interval to (note . bend-cents) pair"
  (let* ((semitones (floor (/ cents 100)))
         (bend-cents (modulo cents 100)))
    (if (> bend-cents 50)
        (cons (+ root semitones 1) (- bend-cents 100))
        (cons (+ root semitones) bend-cents))))

;; ============================================================================
;; Chord builders
;; ============================================================================

(define (major root)
  (let ((r (if (number? root) root (note root))))
    (list r (+ r 4) (+ r 7))))

(define (minor root)
  (let ((r (if (number? root) root (note root))))
    (list r (+ r 3) (+ r 7))))

(define (dim root)
  (let ((r (if (number? root) root (note root))))
    (list r (+ r 3) (+ r 6))))

(define (aug root)
  (let ((r (if (number? root) root (note root))))
    (list r (+ r 4) (+ r 8))))

(define (dom7 root)
  (let ((r (if (number? root) root (note root))))
    (list r (+ r 4) (+ r 7) (+ r 10))))

(define (maj7 root)
  (let ((r (if (number? root) root (note root))))
    (list r (+ r 4) (+ r 7) (+ r 11))))

(define (min7 root)
  (let ((r (if (number? root) root (note root))))
    (list r (+ r 3) (+ r 7) (+ r 10))))

;; Transpose helpers
(define (transpose pitch semitones)
  (let ((p (if (number? pitch) pitch (note pitch))))
    (+ p semitones)))

(define (octave-up pitch) (transpose pitch 12))
(define (octave-down pitch) (transpose pitch -12))

;; Arpeggio helper
(define (midi-arpeggio m pitches . args)
  (let ((vel (if (>= (length args) 1) (car args) mf))
        (dur (if (>= (length args) 2) (cadr args) eighth))
        (ch (if (>= (length args) 3) (caddr args) 1)))
    (for-each (lambda (p) (midi-note m p vel dur ch)) pitches)))

;; Rest helper
(define (rest . args)
  (let ((dur (if (null? args) quarter (car args))))
    (midi-sleep dur)))

;; ============================================================================
;; REPL convenience functions (use global *midi* output)
;; ============================================================================
(define *midi* #f)

(define (open . args)
  "Open MIDI port and set as default. (open) or (open name) or (open index)"
  (if *midi* (midi-close *midi*))
  (set! *midi* (if (null? args) (midi-open) (midi-open (car args))))
  *midi*)

(define (close)
  "Close default MIDI port"
  (if *midi* (begin (midi-close *midi*) (set! *midi* #f))))

(define (n pitch . args)
  "Play note on default port: (n pitch [vel] [dur] [ch])"
  (if (not *midi*) (error 'no-midi-port "No MIDI port open. Use (open) first."))
  (let ((vel (if (>= (length args) 1) (car args) mf))
        (dur (if (>= (length args) 2) (cadr args) quarter))
        (ch (if (>= (length args) 3) (caddr args) 1)))
    (midi-note *midi* pitch vel dur ch)))

(define (ch pitches . args)
  "Play chord on default port: (ch pitches [vel] [dur] [channel])"
  (if (not *midi*) (error 'no-midi-port "No MIDI port open. Use (open) first."))
  (let ((vel (if (>= (length args) 1) (car args) mf))
        (dur (if (>= (length args) 2) (cadr args) quarter))
        (channel (if (>= (length args) 3) (caddr args) 1)))
    (midi-chord *midi* pitches vel dur channel)))

(define (arp pitches . args)
  "Arpeggiate on default port: (arp pitches [vel] [dur] [ch])"
  (if (not *midi*) (error 'no-midi-port "No MIDI port open. Use (open) first."))
  (let ((vel (if (>= (length args) 1) (car args) mf))
        (dur (if (>= (length args) 2) (cadr args) eighth))
        (channel (if (>= (length args) 3) (caddr args) 1)))
    (midi-arpeggio *midi* pitches vel dur channel)))

(define (play-notes ns)
	"Play a list of notes"
	(if (not *midi*) (error 'no-midi-port "No MIDI port open. Use (open) first."))
	(map n ns))

;; ============================================================================
;; Generative Music Functions
;; ============================================================================

;; Pure PRNG using Linear Congruential Generator (same constants as glibc)
;; Returns (value . next-seed)
(define (next-random seed)
  (let* ((next-seed (modulo (+ (* seed 1103515245) 12345) 2147483648))
         (value (floor (/ next-seed 65536))))
    (cons value next-seed)))

;; Generate random int in range [lo, hi]. Returns (value . next-seed)
(define (random-range seed lo hi)
  (if (>= lo hi)
      (cons lo seed)
      (let* ((result (next-random seed))
             (r (car result))
             (next-seed (cdr result)))
        (cons (+ lo (modulo r (+ 1 (- hi lo)))) next-seed))))

;; Generate n random ints in range [lo, hi]. Returns (list . next-seed)
(define (random-list seed n lo hi)
  (let loop ((s seed) (count n) (acc '()))
    (if (<= count 0)
        (cons (reverse acc) s)
        (let* ((result (random-range s lo hi))
               (r (car result))
               (next-s (cdr result)))
          (loop next-s (- count 1) (cons r acc))))))

;; Euclidean rhythm using Bjorklund's algorithm
;; Returns a list of booleans where #t = hit, #f = rest
(define (euclidean hits steps)
  (cond
    ((<= hits 0) (make-list steps #f))
    ((>= hits steps) (make-list steps #t))
    (else
     (let loop ((seqs (map list (make-list hits #t)))
                (remainder (map list (make-list (- steps hits) #f))))
       (if (<= (length remainder) 1)
           ;; Flatten and return
           (apply append (append seqs remainder))
           ;; Bjorklund step
           (let* ((min-len (min (length seqs) (length remainder)))
                  (new-seqs (map (lambda (s r) (append s r))
                                 (take seqs min-len)
                                 (take remainder min-len)))
                  (new-remainder (append (drop seqs min-len)
                                        (drop remainder min-len))))
             (loop new-seqs new-remainder)))))))

;; Helper: take first n elements
(define (take lst n)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;; Helper: drop first n elements
(define (drop lst n)
  (if (or (<= n 0) (null? lst))
      lst
      (drop (cdr lst) (- n 1))))

;; Helper: make a list of n copies of x
(define (make-list n x)
  (if (<= n 0)
      '()
      (cons x (make-list (- n 1) x))))

;; Arpeggio patterns
(define (arp-up lst)
  "Ascending arpeggio pattern - returns copy of list"
  (map (lambda (x) x) lst))

(define (arp-down lst)
  "Descending arpeggio pattern - returns reversed list"
  (reverse lst))

(define (arp-up-down lst)
  "Up-down arpeggio pattern (no repeated top note)"
  (if (<= (length lst) 1)
      lst
      (append lst (reverse (cdr (reverse (cdr lst)))))))

;; Retrograde - reverse a list
(define (retrograde lst)
  "Reverse a list of pitches"
  (reverse lst))

;; Melodic inversion around an axis pitch
(define (invert lst axis)
  "Melodic inversion around an axis pitch"
  (map (lambda (pitch) (- (* 2 axis) pitch)) lst))

;; Fisher-Yates shuffle using seed. Returns shuffled list.
(define (shuffle seed lst)
  (let ((vec (list->vector lst)))
    (let loop ((s seed) (i (- (vector-length vec) 1)))
      (if (<= i 0)
          (vector->list vec)
          (let* ((result (random-range s 0 i))
                 (j (car result))
                 (next-s (cdr result))
                 (tmp (vector-ref vec i)))
            (vector-set! vec i (vector-ref vec j))
            (vector-set! vec j tmp)
            (loop next-s (- i 1)))))))

;; Pick one element from a list using seed
(define (pick seed lst)
  (if (null? lst)
      #f
      (let* ((result (random-range seed 0 (- (length lst) 1)))
             (idx (car result)))
        (list-ref lst idx))))

;; Pick n elements from a list (with replacement)
(define (pick-n seed n lst)
  (if (null? lst)
      '()
      (let loop ((s seed) (count n) (acc '()))
        (if (<= count 0)
            (reverse acc)
            (let* ((result (random-range s 0 (- (length lst) 1)))
                   (idx (car result))
                   (next-s (cdr result)))
              (loop next-s (- count 1) (cons (list-ref lst idx) acc)))))))

;; Random walk - start from a pitch, take n steps of max size
;; Returns list of pitches
(define (random-walk seed start max-step n)
  (let loop ((s seed) (pitch start) (count n) (acc '()))
    (if (<= count 0)
        (reverse acc)
        (let* ((result (random-range s (- max-step) max-step))
               (step (car result))
               (next-s (cdr result))
               (new-pitch (max 0 (min 127 (+ pitch step)))))
          (loop next-s new-pitch (- count 1) (cons pitch acc))))))

;; Drunk walk constrained to scale pitches
;; Returns list of pitches from the scale
(define (drunk-walk seed start scale-pitches max-degrees n)
  (if (null? scale-pitches)
      '()
      (let* ((find-closest
              (lambda (p pitches)
                (let loop ((ps pitches) (idx 0) (best-idx 0) (best-dist 999999))
                  (if (null? ps)
                      best-idx
                      (let ((dist (abs (- p (car ps)))))
                        (if (< dist best-dist)
                            (loop (cdr ps) (+ idx 1) idx dist)
                            (loop (cdr ps) (+ idx 1) best-idx best-dist)))))))
             (scale-len (length scale-pitches))
             (start-idx (find-closest start scale-pitches)))
        (let loop ((s seed) (idx start-idx) (count n) (acc '()))
          (if (<= count 0)
              (reverse acc)
              (let* ((result (random-range s (- max-degrees) max-degrees))
                     (step (car result))
                     (next-s (cdr result))
                     (new-idx (max 0 (min (- scale-len 1) (+ idx step)))))
                (loop next-s new-idx (- count 1)
                      (cons (list-ref scale-pitches idx) acc))))))))

;; Weighted random selection
;; weights is a list of (value . weight) pairs
(define (weighted-pick seed weights)
  (let ((total (apply + (map cdr weights))))
    (if (<= total 0)
        #f
        (let* ((result (random-range seed 1 total))
               (r (car result)))
          (let loop ((ws weights) (cumulative 0))
            (if (null? ws)
                (caar (reverse weights))
                (let ((new-cumulative (+ cumulative (cdar ws))))
                  (if (<= r new-cumulative)
                      (caar ws)
                      (loop (cdr ws) new-cumulative)))))))))

;; Probability gate - returns (result . next-seed) with given probability (0-100)
(define (chance seed probability)
  (let* ((result (random-range seed 0 99))
         (r (car result))
         (next-seed (cdr result)))
    (cons (< r probability) next-seed)))

;; ============================================================================
;; Async Scheduler Helpers
;; ============================================================================
;; The scheduler uses a thunk-based cooperative model:
;; - (spawn thunk [name]) creates a voice from a procedure
;; - The procedure takes no args and returns: number (ms to wait) or #f (done)
;; - (run) runs until all voices complete
;; - (stop [id]) stops one or all voices
;; - (voices) returns count of active voices

;; Create a voice that executes a sequence of (action . delay) pairs
;; action is a thunk to call, delay is ms to wait after
(define (make-sequence-voice steps)
  "Create a voice from a list of (action . delay) pairs"
  (let ((remaining steps))
    (lambda ()
      (if (null? remaining)
          #f
          (let ((step (car remaining)))
            (set! remaining (cdr remaining))
            ((car step))  ; Execute the action
            (cdr step)))))) ; Return delay

;; Create a voice that plays a note on the default port
(define (make-note-voice pitch vel dur)
  "Create a voice that plays a single note"
  (let ((pending-off #f))
    (lambda ()
      (cond
        ;; Send note-off if pending
        (pending-off
         (midi-note-off *midi* pitch 1)
         #f)  ; Done after note-off
        ;; Start the note
        (else
         (if (not *midi*)
             (error 'no-midi-port "No MIDI port open"))
         (midi-note-on *midi* pitch vel 1)
         (set! pending-off #t)
         dur)))))

;; Create a voice that plays a melody (list of pitches with same duration)
(define (make-melody-voice pitches vel dur)
  "Create a voice that plays a sequence of notes"
  (let ((remaining pitches)
        (pending-off #f))
    (lambda ()
      (cond
        ;; First, send note-off if pending
        (pending-off
         (midi-note-off *midi* pending-off 1)
         (set! pending-off #f)
         ;; If more notes, start next immediately
         (if (null? remaining)
             #f
             0))
        ;; No pending off, start next note
        ((null? remaining)
         #f)
        (else
         (let ((pitch (car remaining)))
           (set! remaining (cdr remaining))
           (midi-note-on *midi* pitch vel 1)
           (set! pending-off pitch)
           dur))))))

;; Create a voice that plays a chord
(define (make-chord-voice pitches vel dur)
  "Create a voice that plays a chord (all notes at once)"
  (let ((started #f))
    (lambda ()
      (if started
          ;; Send all note-offs
          (begin
            (for-each (lambda (p) (midi-note-off *midi* p 1)) pitches)
            #f)
          ;; Send all note-ons
          (begin
            (set! started #t)
            (for-each (lambda (p) (midi-note-on *midi* p vel 1)) pitches)
            dur)))))

;; Play a note asynchronously on default port
(define (async-note pitch . args)
  "Spawn a voice to play a note: (async-note pitch [vel] [dur])"
  (if (not *midi*) (error 'no-midi-port "No MIDI port open"))
  (let ((vel (if (>= (length args) 1) (car args) mf))
        (dur (if (>= (length args) 2) (cadr args) quarter)))
    (spawn (make-note-voice pitch vel dur))))

;; Play a chord asynchronously on default port
(define (async-chord pitches . args)
  "Spawn a voice to play a chord: (async-chord pitches [vel] [dur])"
  (if (not *midi*) (error 'no-midi-port "No MIDI port open"))
  (let ((vel (if (>= (length args) 1) (car args) mf))
        (dur (if (>= (length args) 2) (cadr args) quarter)))
    (spawn (make-chord-voice pitches vel dur))))

;; Play a melody asynchronously
(define (async-melody pitches . args)
  "Spawn a voice to play a melody: (async-melody pitches [vel] [dur])"
  (if (not *midi*) (error 'no-midi-port "No MIDI port open"))
  (let ((vel (if (>= (length args) 1) (car args) mf))
        (dur (if (>= (length args) 2) (cadr args) eighth)))
    (spawn (make-melody-voice pitches vel dur))))

;; Create a repeating voice that calls a thunk n times with delay between
(define (make-repeat-voice thunk n delay)
  "Create a voice that calls thunk n times"
  (let ((count n))
    (lambda ()
      (if (<= count 0)
          #f
          (begin
            (set! count (- count 1))
            (thunk)
            delay)))))

;; Create a looping voice that repeats forever until stopped
(define (make-loop-voice thunk delay)
  "Create a voice that loops forever"
  (lambda ()
    (thunk)
    delay))

