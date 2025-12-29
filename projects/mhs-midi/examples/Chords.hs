-- | Chords.hs - Chord progression example
-- Plays a simple I-IV-V-I progression in C major
module Chords(main) where

import MusicPerform

-- Define chords as pure Music
cMajorChord, fMajorChord, gMajorChord :: Music
cMajorChord = chord [c4, e4, g4] mf half   -- C major: C E G
fMajorChord = chord [f4, a4, c5] mf half   -- F major: F A C
gMajorChord = chord [g4, b4, d5] mf half   -- G major: G B D

-- I-IV-V-I progression as pure Music
progression :: Music
progression = cMajorChord
          +:+ fMajorChord
          +:+ gMajorChord
          +:+ chord [c4, e4, g4] mf whole  -- final I chord (whole note)

main :: IO ()
main = do
    putStrLn "MHS-MIDI: Chord Progression Example"

    ok <- midiOpenVirtual "mhsMIDI"
    if not ok
        then putStrLn "Failed to open MIDI"
        else do
            putStrLn "Playing I-IV-V-I progression..."

            -- Perform the pure Music
            perform progression

            midiSleep quarter
            putStrLn "Done!"
            midiClose
