-- | Music Performance for MicroHs
--
-- IO functions for performing pure Music via MIDI.
-- Imports and re-exports Music (pure), Midi (IO), and Async (concurrent).
--
-- Example:
--   import MusicPerform
--   main = do
--       midiOpenVirtual "myApp"
--       perform (line [c4, e4, g4] mf quarter)
--       perform (chord [c4, e4, g4] ff half)
--       midiClose
--
-- Async example:
--   import MusicPerform
--   main = do
--       midiOpenVirtual "myApp"
--       spawn "melody" $ perform (line [c4, e4, g4] mf quarter)
--       spawn "bass" $ perform (note c2 f whole)
--       run  -- Wait for all voices
--       midiClose
--
module MusicPerform (
    -- * Re-exports
    module Music,
    module Midi,
    module Async,

    -- * Music DSL interpretation
    perform,
    performOn,

    -- * Microtonal IO
    centsToBend,
    pitchBendCents,
) where

import Music
import Midi
import Async

-----------------------------------------------------------
-- Microtonal IO
-----------------------------------------------------------

-- | Convert cents offset to MIDI pitch bend value
centsToBend :: Int -> IO Int
centsToBend = midiCentsToBend

-- | Send pitch bend in cents on a channel
pitchBendCents :: Channel -> Int -> IO ()
pitchBendCents channel cents = do
    bend <- centsToBend cents
    midiPitchBend channel bend

-----------------------------------------------------------
-- Music DSL Interpretation
-----------------------------------------------------------

-- | Perform music on channel 1
perform :: Music -> IO ()
perform = performOn 1

-- | Perform music on a specific MIDI channel
performOn :: Channel -> Music -> IO ()
performOn ch music = go music
  where
    go (MEvent ev) = performEvent ch ev
    go (MSeq ms) = mapM_ go ms
    go (MPar ms) = performPar ch ms

-- | Perform a single event
performEvent :: Channel -> Event -> IO ()
performEvent ch (ENote pit v d) = do
    midiNoteOn ch pit v
    midiSleep d
    midiNoteOff ch pit
performEvent _ (ERest d) = midiSleep d

-- | Perform parallel music (all start together, wait for longest)
performPar :: Channel -> [Music] -> IO ()
performPar ch ms = do
    let events = concatMap collectEvents ms
        notes = [ev | ev@(ENote _ _ _) <- events]
        maxDur = maximum (0 : [d | ENote _ _ d <- notes])
    mapM_ (\(ENote pit v _) -> midiNoteOn ch pit v) notes
    midiSleep maxDur
    mapM_ (\(ENote pit _ _) -> midiNoteOff ch pit) notes
