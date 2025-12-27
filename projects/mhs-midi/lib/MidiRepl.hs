-- | MIDI REPL - ergonomic functions for interactive use
--
-- All functions return IO () for REPL friendliness (no Show constraint errors).
--
-- Designed for partial application with pitch LAST:
--   note :: Channel -> Velocity -> Duration -> Pitch -> IO ()
--   n = note 1 mf quarter
--   n c4
--   loud = note 1 fff quarter
--   loud c4
--
module MidiRepl (
    -- * Re-export Midi
    module Midi,

    -- * Ergonomic note functions (pitch last)
    note,
    notes,

    -- * Pre-configured defaults
    n,
    ch,

    -- * Quick setup (REPL friendly - all return IO ())
    open,
    close,
    panic,
    ports,
) where

import Midi

-- | Play a single note. Pitch is last for partial application.
--
-- Examples:
--   note 1 mf quarter c4
--   let loud = note 1 fff quarter in loud c4
--   let fast = note 1 mf eighth in mapM_ fast [c4, e4, g4]
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
--   mapM_ n [c4, e4, g4]
n :: Pitch -> IO ()
n = note 1 mf quarter

-- | Default chord: channel 1, mf velocity, quarter duration
--
-- Examples:
--   ch [c4, e4, g4]
ch :: [Pitch] -> IO ()
ch = notes 1 mf quarter

-- | Open virtual MIDI port "MicroHs". Prints status.
open :: IO ()
open = do
    ok <- midiOpenVirtual "MicroHs"
    if ok
        then putStrLn "MIDI open"
        else putStrLn "MIDI failed to open"

-- | Close MIDI port
close :: IO ()
close = midiClose

-- | All notes off (panic)
panic :: IO ()
panic = midiPanic

-- | List available MIDI ports
ports :: IO ()
ports = do
    n <- midiListPorts
    if n == 0
        then putStrLn "No MIDI ports found"
        else do
            putStrLn $ show n ++ " MIDI port(s):"
            printPorts 0 n
  where
    printPorts i total
        | i >= total = return ()
        | otherwise = do
            name <- midiPortName i
            putStrLn $ "  " ++ show i ++ ": " ++ name
            printPorts (i + 1) total
