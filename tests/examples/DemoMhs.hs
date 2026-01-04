-- README.md example for mhs-midi (adapted to actual API)
-- Tests the quick example from the project README

module DemoMhs(main) where

import MusicPerform

main = do
    midiOpenVirtual "demo"
    -- Pure functional Music DSL with combinators
    let melody = line [c4, e4, g4, c5] mf eighth
        bass   = note c3 ff whole
        piece  = melody ||| bass  -- parallel composition

    perform piece

    -- Concurrent voices with native threads
    spawn "arp" $ perform (line (arpUp [c4, e4, g4]) mp sixteenth)
    spawn "pad" $ perform (chord [c3, g3, e4] pp whole)
    run
    midiClose
