-- | MIDI Prelude - handy functions with pitch-last parameter order
--
-- Designed for partial application:
--   note :: Channel -> Velocity -> Duration -> Pitch -> IO ()
--   n = note 1 mf quarter   -- pre-configured
--   n c4                    -- just supply pitch
--   loud = note 1 fff quarter
--   loud c4
--
module MidiPrelude (
    -- * Re-export Midi
    module Midi,

    -- * Ergonomic note functions (pitch last)
    note,
    notes,

    -- * Pre-configured defaults
    n,
    ch,

    -- * Quick setup
    open,
    close,
) where

import Midi

-- | Play a single note. Pitch is last for partial application.
--
-- Examples:
--   note 1 mf quarter c4        -- explicit
--   let loud = note 1 fff quarter in loud c4
--   let fast = note 1 mf eighth in fast c4 >> fast e4 >> fast g4
note :: Channel -> Velocity -> Duration -> Pitch -> IO ()
note chan vel dur pit = do
    midiNoteOn chan pit vel
    midiSleep dur
    midiNoteOff chan pit

-- | Play multiple notes as a chord. Pitches last for partial application.
--
-- Examples:
--   notes 1 mf quarter [c4, e4, g4]
--   let crd = notes 1 mf half in crd [c4, e4, g4]
notes :: Channel -> Velocity -> Duration -> [Pitch] -> IO ()
notes chan vel dur pits = do
    mapM_ (\pit -> midiNoteOn chan pit vel) pits
    midiSleep dur
    mapM_ (\pit -> midiNoteOff chan pit) pits

-- | Default note: channel 1, mf velocity, quarter duration
--
-- Examples:
--   n c4
--   n c4 >> n e4 >> n g4
n :: Pitch -> IO ()
n = note 1 mf quarter

-- | Default chord: channel 1, mf velocity, quarter duration
--
-- Examples:
--   ch [c4, e4, g4]
ch :: [Pitch] -> IO ()
ch = notes 1 mf quarter

-- | Quick setup: open virtual port named "MicroHs"
open :: IO Bool
open = midiOpenVirtual "MicroHs"

-- | Close MIDI port (alias for midiClose)
close :: IO ()
close = midiClose
