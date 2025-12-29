-- | Melody.hs - Melody example with dynamics
-- Plays a simple melody with varying dynamics
module Melody(main) where

import MusicPerform

-- Twinkle Twinkle Little Star as pure Music
-- Build from (pitch, duration) pairs
twinkle :: Velocity -> Music
twinkle vel = MSeq
    [ note c4 vel quarter, note c4 vel quarter, note g4 vel quarter, note g4 vel quarter
    , note a4 vel quarter, note a4 vel quarter, note g4 vel half
    , note f4 vel quarter, note f4 vel quarter, note e4 vel quarter, note e4 vel quarter
    , note d4 vel quarter, note d4 vel quarter, note c4 vel half
    ]

main :: IO ()
main = do
    putStrLn "MHS-MIDI: Melody Example"

    ok <- midiOpenVirtual "mhsMIDI"
    if not ok
        then putStrLn "Failed to open MIDI"
        else do
            putStrLn "Playing Twinkle Twinkle Little Star..."

            -- Play melody with medium-soft dynamics
            perform (twinkle mp)

            midiSleep quarter

            -- Play it again louder
            putStrLn "Now louder..."
            perform (twinkle ff)

            midiSleep quarter
            putStrLn "Done!"
            midiClose
