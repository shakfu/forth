-- | Melody.hs - Melody example with dynamics
-- Plays a simple melody with varying dynamics
module Melody(main) where

import Midi

-- Twinkle Twinkle Little Star melody
twinkle :: [(Pitch, Duration)]
twinkle =
    [ (c4, quarter), (c4, quarter), (g4, quarter), (g4, quarter)
    , (a4, quarter), (a4, quarter), (g4, half)
    , (f4, quarter), (f4, quarter), (e4, quarter), (e4, quarter)
    , (d4, quarter), (d4, quarter), (c4, half)
    ]

main :: IO ()
main = do
    putStrLn "MHS-MIDI: Melody Example"

    ok <- midiOpenVirtual "MhsMidi"
    if not ok
        then putStrLn "Failed to open MIDI"
        else do
            putStrLn "Playing Twinkle Twinkle Little Star..."

            -- Play melody with medium-soft dynamics
            melody twinkle mp

            rest quarter

            -- Play it again louder
            putStrLn "Now louder..."
            melody twinkle ff

            rest quarter
            putStrLn "Done!"
            midiClose
