-- | Arpeggio.hs - Arpeggio patterns example
module Arpeggio(main) where

import MusicPerform

-- Chord notes
cMajorNotes, aminorNotes, fMajorNotes, gMajorNotes :: [Pitch]
cMajorNotes = [c4, e4, g4, c5]          -- C major arpeggio
aminorNotes = [a3, c4, e4, a4]          -- A minor arpeggio
fMajorNotes = [f3, a3, c4, f4]          -- F major arpeggio
gMajorNotes = [g3, b3, d4, g4]          -- G major arpeggio

-- Arpeggio up then down as pure Music
arpeggioUpDown :: [Pitch] -> Music
arpeggioUpDown notes = line notes mf sixteenth
                   +:+ line (reverse notes) mf sixteenth

-- Full progression as pure Music
arpeggioProgression :: Music
arpeggioProgression = arpeggioUpDown cMajorNotes
                  +:+ arpeggioUpDown aminorNotes
                  +:+ arpeggioUpDown fMajorNotes
                  +:+ arpeggioUpDown gMajorNotes

main :: IO ()
main = do
    putStrLn "MHS-MIDI: Arpeggio Example"

    ok <- midiOpenVirtual "mhsMIDI"
    if not ok
        then putStrLn "Failed to open MIDI"
        else do
            putStrLn "Playing arpeggiated chord progression..."

            -- Play progression twice using timesM
            perform (timesM 2 arpeggioProgression)

            midiSleep quarter
            putStrLn "Done!"
            midiClose
