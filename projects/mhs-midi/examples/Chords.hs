-- | Chords.hs - Chord progression example
-- Plays a simple I-IV-V-I progression in C major
module Chords(main) where

import Midi

-- Define some common chords
cMajor, fMajor, gMajor :: [Pitch]
cMajor = [c4, e4, g4]      -- C major: C E G
fMajor = [f4, a4, c5]      -- F major: F A C
gMajor = [g4, b4, d5]      -- G major: G B D

main :: IO ()
main = do
    putStrLn "MHS-MIDI: Chord Progression Example"

    ok <- midiOpenVirtual "MhsMidi"
    if not ok
        then putStrLn "Failed to open MIDI"
        else do
            putStrLn "Playing I-IV-V-I progression..."

            -- I-IV-V-I progression
            chord cMajor half       -- I  (C major)
            chord fMajor half       -- IV (F major)
            chord gMajor half       -- V  (G major)
            chord cMajor whole      -- I  (C major)

            rest quarter
            putStrLn "Done!"
            midiClose
