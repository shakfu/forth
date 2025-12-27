-- | HelloMidi.hs - Simple MIDI example
-- Plays a C major scale using a virtual MIDI port
module HelloMidi(main) where

import Midi

main :: IO ()
main = do
    putStrLn "MHS-MIDI: Hello MIDI Example"

    -- Open virtual MIDI port
    ok <- midiOpenVirtual "MhsMidi"
    if not ok
        then putStrLn "Failed to open virtual MIDI port"
        else do
            putStrLn "Virtual MIDI port 'MhsMidi' opened"
            putStrLn "Playing C major scale..."

            -- Play C major scale
            playNote c4 quarter
            playNote d4 quarter
            playNote e4 quarter
            playNote f4 quarter
            playNote g4 quarter
            playNote a4 quarter
            playNote b4 quarter
            playNote c5 half

            rest quarter

            putStrLn "Done!"
            midiClose
