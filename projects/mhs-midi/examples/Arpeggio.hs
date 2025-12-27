-- | Arpeggio.hs - Arpeggio patterns example
module Arpeggio(main) where

import Midi

-- Chord arpeggios
cMajorArp, aminorArp, fMajorArp, gMajorArp :: [Pitch]
cMajorArp = [c4, e4, g4, c5]          -- C major arpeggio
aminorArp = [a3, c4, e4, a4]          -- A minor arpeggio
fMajorArp = [f3, a3, c4, f4]          -- F major arpeggio
gMajorArp = [g3, b3, d4, g4]          -- G major arpeggio

-- Play arpeggio up then down
arpeggioUpDown :: [Pitch] -> IO ()
arpeggioUpDown notes = do
    arpeggio notes sixteenth mf            -- Up
    arpeggio (reverse notes) sixteenth mf  -- Down

main :: IO ()
main = do
    putStrLn "MHS-MIDI: Arpeggio Example"

    ok <- midiOpenVirtual "MhsMidi"
    if not ok
        then putStrLn "Failed to open MIDI"
        else do
            putStrLn "Playing arpeggiated chord progression..."

            -- Repeat the progression 2 times
            times 2 $ do
                arpeggioUpDown cMajorArp
                arpeggioUpDown aminorArp
                arpeggioUpDown fMajorArp
                arpeggioUpDown gMajorArp

            rest quarter
            putStrLn "Done!"
            midiClose
