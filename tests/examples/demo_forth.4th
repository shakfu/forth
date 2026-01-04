\ README.md example for forth-midi
\ Tests the quick example from the project README

midi-virtual

\ Concise notation with probability and articulation
mf c4, e4. g4> c5-,          \ staccato, accent, tenuto
c4|e4|g4, 75%,               \ random selection, 75% chance
(c4 e4 g4),                  \ chord

\ Generative pattern with anonymous block
{ c4, d4, e4, } 4 *          \ repeat block 4 times

\ Async sequence playback
seq-new 0 seq-start
  c4, e4, g4,
0 seq-end
seq-play&                    \ non-blocking playback

\ Wait a moment for async playback
50 ms

midi-close
