-- | Music Performance for MicroHs
--
-- IO functions for playing music via MIDI.
-- Imports and re-exports Music (pure) and Midi (IO).
--
-- Example:
--   import MusicPerform
--   main = do
--       open
--       n c4 >> n e4 >> n g4
--       ch [c4, e4, g4]
--       perform (line [c4, e4, g4])
--       close
--
module MusicPerform (
    -- * Re-exports
    module Music,
    module Midi,

    -- * High-level note playing
    play,
    playNote,
    playChord,
    rest,

    -- * Sequences
    melody,
    chord,
    arpeggio,
    times,

    -- * Channel management
    withChannel,

    -- * Microtonal IO
    centsToBend,
    pitchBendCents,

    -- * Music DSL interpretation
    perform,
    performChan,

    -- * REPL helpers
    n,
    ch,
    noteR,
    notesR,
    open,
    close,
    panic,
    ports,
) where

import Music
import Midi

-----------------------------------------------------------
-- High-level note playing
-----------------------------------------------------------

-- | Play a note with pitch, velocity, and duration on default channel
play :: Pitch -> Velocity -> Duration -> IO ()
play pitch vel dur = do
    midiNoteOn defaultChannel pitch vel
    midiSleep dur
    midiNoteOff defaultChannel pitch

-- | Play a note with default velocity (mf)
playNote :: Pitch -> Duration -> IO ()
playNote pitch dur = play pitch mf dur

-- | Play a chord (multiple notes simultaneously)
playChord :: [Pitch] -> Velocity -> Duration -> IO ()
playChord pitches vel dur = do
    mapM_ (\pit -> midiNoteOn defaultChannel pit vel) pitches
    midiSleep dur
    mapM_ (\pit -> midiNoteOff defaultChannel pit) pitches

-- | Rest (silence) for duration
rest :: Duration -> IO ()
rest = midiSleep

-----------------------------------------------------------
-- Sequences
-----------------------------------------------------------

-- | Play a melody (sequence of notes)
melody :: [(Pitch, Duration)] -> Velocity -> IO ()
melody notes vel = mapM_ (\(pit, dur) -> play pit vel dur) notes

-- | Play notes as a chord
chord :: [Pitch] -> Duration -> IO ()
chord pitches dur = playChord pitches mf dur

-- | Play notes as an arpeggio
arpeggio :: [Pitch] -> Duration -> Velocity -> IO ()
arpeggio pitches noteDur vel = mapM_ (\pit -> play pit vel noteDur) pitches

-- | Repeat an action n times
times :: Int -> IO () -> IO ()
times 0 _ = return ()
times cnt action = action >> times (cnt - 1) action

-----------------------------------------------------------
-- Channel management
-----------------------------------------------------------

-- | Execute action with a specific channel
withChannel :: Channel -> (Channel -> IO ()) -> IO ()
withChannel ch action = action ch

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

-- | Perform music on default channel
perform :: Music -> IO ()
perform = performChan defaultChannel

-- | Perform music, using given channel as default
performChan :: Channel -> Music -> IO ()
performChan defCh music = go music
  where
    go (MEvent ev) = performEvent defCh ev
    go (MSeq ms) = mapM_ go ms
    go (MPar ms) = performPar defCh ms

-- | Perform a single event
performEvent :: Channel -> Event -> IO ()
performEvent defCh (ENote channel pit v d) = do
    let ch' = if channel == defaultChannel then defCh else channel
    midiNoteOn ch' pit v
    midiSleep d
    midiNoteOff ch' pit
performEvent _ (ERest d) = midiSleep d

-- | Perform parallel music (all start together, wait for longest)
performPar :: Channel -> [Music] -> IO ()
performPar defCh ms = do
    let events = concatMap collectEvents ms
        notes = [ev | ev@(ENote _ _ _ _) <- events]
        maxDur = maximum (0 : [d | ENote _ _ _ d <- notes])
    mapM_ (\(ENote channel pit v _) -> midiNoteOn channel pit v) notes
    midiSleep maxDur
    mapM_ (\(ENote channel pit _ _) -> midiNoteOff channel pit) notes

-----------------------------------------------------------
-- REPL helpers
-----------------------------------------------------------

-- | Play a single note. Pitch is last for partial application.
--
-- Examples:
--   noteR 1 mf quarter c4
--   let loud = noteR 1 fff quarter in loud c4
noteR :: Channel -> Velocity -> Duration -> Pitch -> IO ()
noteR channel vel dur pit = do
    midiNoteOn channel pit vel
    midiSleep dur
    midiNoteOff channel pit

-- | Play multiple notes as a chord. Pitches last for partial application.
--
-- Examples:
--   notesR 1 mf quarter [c4, e4, g4]
notesR :: Channel -> Velocity -> Duration -> [Pitch] -> IO ()
notesR channel vel dur pits = do
    mapM_ (\pit -> midiNoteOn channel pit vel) pits
    midiSleep dur
    mapM_ (\pit -> midiNoteOff channel pit) pits

-- | Default note: channel 1, mf velocity, quarter duration
--
-- Examples:
--   n c4
--   mapM_ n [c4, e4, g4]
n :: Pitch -> IO ()
n = noteR 1 mf quarter

-- | Default chord: channel 1, mf velocity, quarter duration
--
-- Examples:
--   ch [c4, e4, g4]
ch :: [Pitch] -> IO ()
ch = notesR 1 mf quarter

-- | Open virtual MIDI port "mhsMIDI". Prints status.
open :: IO ()
open = do
    ok <- midiOpenVirtual "mhsMIDI"
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
    cnt <- midiListPorts
    if cnt == 0
        then putStrLn "No MIDI ports found"
        else do
            putStrLn $ show cnt ++ " MIDI port(s):"
            printPorts 0 cnt
  where
    printPorts i total
        | i >= total = return ()
        | otherwise = do
            name <- midiPortName i
            putStrLn $ "  " ++ show i ++ ": " ++ name
            printPorts (i + 1) total
