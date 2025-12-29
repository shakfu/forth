-- | HelloMidi.hs - Simple MIDI example
-- Plays a C major scale using a virtual MIDI port
module HelloMidi(main) where

import MusicPerform

-- C major scale as pure Music
cMajorScale :: Music
cMajorScale = line [c4, d4, e4, f4, g4, a4, b4] mf quarter
          +:+ note c5 mf half

main :: IO ()
main = do
    putStrLn "MHS-MIDI: Hello MIDI Example"

    -- Open virtual MIDI port
    ok <- midiOpenVirtual "mhsMIDI"
    if not ok
        then putStrLn "Failed to open virtual MIDI port"
        else do
            putStrLn "Virtual MIDI port 'mhsMIDI' opened"
            putStrLn "Playing C major scale..."

            -- Perform the pure Music
            perform cMajorScale

            midiSleep quarter

            putStrLn "Done!"
            midiClose
