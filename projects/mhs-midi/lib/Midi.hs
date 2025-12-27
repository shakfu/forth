-- | High-level MIDI library for MicroHs
-- Provides musical abstractions for MIDI output
module Midi (
    -- * Initialization
    midiInit,
    midiCleanup,
    midiListPorts,
    midiPortName,
    midiOpen,
    midiOpenVirtual,
    midiClose,
    midiIsOpen,

    -- * Low-level MIDI
    midiSend,
    midiNoteOn,
    midiNoteOff,
    midiCC,
    midiProgram,
    midiPitchBend,
    midiSleep,
    midiPanic,

    -- * Pitch names (MIDI note numbers)
    Pitch,
    c, cs, d, ds, e, f, fs, g, gs, a, as, b,
    c0, c1, c2, c3, c4, c5, c6, c7, c8,
    d0, d1, d2, d3, d4, d5, d6, d7, d8,
    e0, e1, e2, e3, e4, e5, e6, e7, e8,
    f0, f1, f2, f3, f4, f5, f6, f7, f8,
    g0, g1, g2, g3, g4, g5, g6, g7, g8,
    a0, a1, a2, a3, a4, a5, a6, a7, a8,
    b0, b1, b2, b3, b4, b5, b6, b7, b8,
    cs0, cs1, cs2, cs3, cs4, cs5, cs6, cs7, cs8,
    ds0, ds1, ds2, ds3, ds4, ds5, ds6, ds7, ds8,
    fs0, fs1, fs2, fs3, fs4, fs5, fs6, fs7, fs8,
    gs0, gs1, gs2, gs3, gs4, gs5, gs6, gs7, gs8,
    as0, as1, as2, as3, as4, as5, as6, as7, as8,
    -- Flats as aliases
    db, eb, gb, ab, bb,

    -- * Duration (milliseconds)
    Duration,
    whole, half, quarter, eighth, sixteenth,
    dotted,
    bpm,

    -- * Velocity
    Velocity,
    ppp, pp, p, mp, mf, ff, fff,

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

    -- * Channel
    Channel,
    withChannel,
    defaultChannel,
) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

-- FFI imports for MIDI functions
foreign import ccall "midi_ffi.h midi_init" c_midi_init :: IO CInt
foreign import ccall "midi_ffi.h midi_cleanup" c_midi_cleanup :: IO ()
foreign import ccall "midi_ffi.h midi_list_ports" c_midi_list_ports :: IO CInt
foreign import ccall "midi_ffi.h midi_port_name" c_midi_port_name :: CInt -> IO CString
foreign import ccall "midi_ffi.h midi_open" c_midi_open :: CInt -> IO CInt
foreign import ccall "midi_ffi.h midi_open_virtual" c_midi_open_virtual :: CString -> IO CInt
foreign import ccall "midi_ffi.h midi_close" c_midi_close :: IO ()
foreign import ccall "midi_ffi.h midi_is_open" c_midi_is_open :: IO CInt
foreign import ccall "midi_ffi.h midi_send" c_midi_send :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "midi_ffi.h midi_note_on" c_midi_note_on :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "midi_ffi.h midi_note_off" c_midi_note_off :: CInt -> CInt -> IO CInt
foreign import ccall "midi_ffi.h midi_cc" c_midi_cc :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "midi_ffi.h midi_program" c_midi_program :: CInt -> CInt -> IO CInt
foreign import ccall "midi_ffi.h midi_pitch_bend" c_midi_pitch_bend :: CInt -> CInt -> IO CInt
foreign import ccall "midi_ffi.h midi_sleep" c_midi_sleep :: CInt -> IO ()
foreign import ccall "midi_ffi.h midi_panic" c_midi_panic :: IO ()

-----------------------------------------------------------
-- MIDI initialization and port management
-----------------------------------------------------------

-- | Initialize MIDI system
midiInit :: IO Bool
midiInit = do
    r <- c_midi_init
    return (r == 0)

-- | Cleanup MIDI system
midiCleanup :: IO ()
midiCleanup = c_midi_cleanup

-- | List available MIDI ports, returns count
midiListPorts :: IO Int
midiListPorts = do
    n <- c_midi_list_ports
    return (fromIntegral n)

-- | Get port name by index
midiPortName :: Int -> IO String
midiPortName idx = do
    ptr <- c_midi_port_name (fromIntegral idx)
    if ptr == nullPtr
        then return ""
        else peekCString ptr

-- | Open MIDI port by index
midiOpen :: Int -> IO Bool
midiOpen idx = do
    r <- c_midi_open (fromIntegral idx)
    return (r == 0)

-- | Open virtual MIDI port
midiOpenVirtual :: String -> IO Bool
midiOpenVirtual name = do
    withCString name $ \cname -> do
        r <- c_midi_open_virtual cname
        return (r == 0)

-- | Close MIDI port
midiClose :: IO ()
midiClose = c_midi_close

-- | Check if MIDI is open
midiIsOpen :: IO Bool
midiIsOpen = do
    r <- c_midi_is_open
    return (r /= 0)

-----------------------------------------------------------
-- Low-level MIDI operations
-----------------------------------------------------------

-- | Send raw MIDI message
midiSend :: Int -> Int -> Int -> IO ()
midiSend status d1 d2 = do
    _ <- c_midi_send (fromIntegral status) (fromIntegral d1) (fromIntegral d2)
    return ()

-- | Send note on
midiNoteOn :: Int -> Int -> Int -> IO ()
midiNoteOn ch pitch vel = do
    _ <- c_midi_note_on (fromIntegral ch) (fromIntegral pitch) (fromIntegral vel)
    return ()

-- | Send note off
midiNoteOff :: Int -> Int -> IO ()
midiNoteOff ch pitch = do
    _ <- c_midi_note_off (fromIntegral ch) (fromIntegral pitch)
    return ()

-- | Send control change
midiCC :: Int -> Int -> Int -> IO ()
midiCC ch cc val = do
    _ <- c_midi_cc (fromIntegral ch) (fromIntegral cc) (fromIntegral val)
    return ()

-- | Send program change
midiProgram :: Int -> Int -> IO ()
midiProgram ch prog = do
    _ <- c_midi_program (fromIntegral ch) (fromIntegral prog)
    return ()

-- | Send pitch bend (-8192 to 8191)
midiPitchBend :: Int -> Int -> IO ()
midiPitchBend ch val = do
    _ <- c_midi_pitch_bend (fromIntegral ch) (fromIntegral val)
    return ()

-- | Sleep for milliseconds
midiSleep :: Int -> IO ()
midiSleep ms = c_midi_sleep (fromIntegral ms)

-- | Panic - all notes off
midiPanic :: IO ()
midiPanic = c_midi_panic

-----------------------------------------------------------
-- Musical types
-----------------------------------------------------------

type Pitch = Int
type Duration = Int  -- milliseconds
type Velocity = Int
type Channel = Int

-- Default channel
defaultChannel :: Channel
defaultChannel = 1

-----------------------------------------------------------
-- Pitch class (semitone offset within octave)
-----------------------------------------------------------

c, cs, d, ds, e, f, fs, g, gs, a, as, b :: Int
c  = 0
cs = 1
d  = 2
ds = 3
e  = 4
f  = 5
fs = 6
g  = 7
gs = 8
a  = 9
as = 10
b  = 11

-- Flat aliases
db, eb, gb, ab, bb :: Int
db = cs
eb = ds
gb = fs
ab = gs
bb = as

-----------------------------------------------------------
-- Pitches by octave (C4 = middle C = MIDI 60)
-----------------------------------------------------------

c0, c1, c2, c3, c4, c5, c6, c7, c8 :: Pitch
c0 = 12; c1 = 24; c2 = 36; c3 = 48; c4 = 60; c5 = 72; c6 = 84; c7 = 96; c8 = 108

d0, d1, d2, d3, d4, d5, d6, d7, d8 :: Pitch
d0 = 14; d1 = 26; d2 = 38; d3 = 50; d4 = 62; d5 = 74; d6 = 86; d7 = 98; d8 = 110

e0, e1, e2, e3, e4, e5, e6, e7, e8 :: Pitch
e0 = 16; e1 = 28; e2 = 40; e3 = 52; e4 = 64; e5 = 76; e6 = 88; e7 = 100; e8 = 112

f0, f1, f2, f3, f4, f5, f6, f7, f8 :: Pitch
f0 = 17; f1 = 29; f2 = 41; f3 = 53; f4 = 65; f5 = 77; f6 = 89; f7 = 101; f8 = 113

g0, g1, g2, g3, g4, g5, g6, g7, g8 :: Pitch
g0 = 19; g1 = 31; g2 = 43; g3 = 55; g4 = 67; g5 = 79; g6 = 91; g7 = 103; g8 = 115

a0, a1, a2, a3, a4, a5, a6, a7, a8 :: Pitch
a0 = 21; a1 = 33; a2 = 45; a3 = 57; a4 = 69; a5 = 81; a6 = 93; a7 = 105; a8 = 117

b0, b1, b2, b3, b4, b5, b6, b7, b8 :: Pitch
b0 = 23; b1 = 35; b2 = 47; b3 = 59; b4 = 71; b5 = 83; b6 = 95; b7 = 107; b8 = 119

-- Sharps by octave
cs0, cs1, cs2, cs3, cs4, cs5, cs6, cs7, cs8 :: Pitch
cs0 = 13; cs1 = 25; cs2 = 37; cs3 = 49; cs4 = 61; cs5 = 73; cs6 = 85; cs7 = 97; cs8 = 109

ds0, ds1, ds2, ds3, ds4, ds5, ds6, ds7, ds8 :: Pitch
ds0 = 15; ds1 = 27; ds2 = 39; ds3 = 51; ds4 = 63; ds5 = 75; ds6 = 87; ds7 = 99; ds8 = 111

fs0, fs1, fs2, fs3, fs4, fs5, fs6, fs7, fs8 :: Pitch
fs0 = 18; fs1 = 30; fs2 = 42; fs3 = 54; fs4 = 66; fs5 = 78; fs6 = 90; fs7 = 102; fs8 = 114

gs0, gs1, gs2, gs3, gs4, gs5, gs6, gs7, gs8 :: Pitch
gs0 = 20; gs1 = 32; gs2 = 44; gs3 = 56; gs4 = 68; gs5 = 80; gs6 = 92; gs7 = 104; gs8 = 116

as0, as1, as2, as3, as4, as5, as6, as7, as8 :: Pitch
as0 = 22; as1 = 34; as2 = 46; as3 = 58; as4 = 70; as5 = 82; as6 = 94; as7 = 106; as8 = 118

-----------------------------------------------------------
-- Durations (at 120 BPM by default)
-----------------------------------------------------------

-- | Whole note duration in ms at 120 BPM
whole :: Duration
whole = 2000

-- | Half note
half :: Duration
half = 1000

-- | Quarter note
quarter :: Duration
quarter = 500

-- | Eighth note
eighth :: Duration
eighth = 250

-- | Sixteenth note
sixteenth :: Duration
sixteenth = 125

-- | Dotted duration (1.5x)
dotted :: Duration -> Duration
dotted dur = dur + dur `div` 2

-- | Convert BPM to quarter note duration in ms
bpm :: Int -> Duration
bpm tempo = 60000 `div` tempo

-----------------------------------------------------------
-- Velocities (dynamics)
-----------------------------------------------------------

ppp, pp, p, mp, mf, ff, fff :: Velocity
ppp = 16
pp  = 33
p   = 49
mp  = 64
mf  = 80
ff  = 96
fff = 112

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
    -- Note on for all pitches
    mapM_ (\pit -> midiNoteOn defaultChannel pit vel) pitches
    midiSleep dur
    -- Note off for all pitches
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
times n action = action >> times (n - 1) action

-----------------------------------------------------------
-- Channel management
-----------------------------------------------------------

-- | Execute action with a specific channel
withChannel :: Channel -> (Channel -> IO ()) -> IO ()
withChannel ch action = action ch
