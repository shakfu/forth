-- | Low-level MIDI I/O for MicroHs
-- Pure FFI bindings with no music theory dependencies
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

    -- * Microtonal helper
    midiCentsToBend,

    -- * Random
    midiSeedRandom,
    midiRandom,
    midiRandomRange,

    -- * Recording
    midiRecordStart,
    midiRecordStop,
    midiSaveToFile,
    midiSaveToSource,
    midiRecordCount,
    midiRecordActive,

    -- * MIDI File I/O
    midiReadMid,
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
foreign import ccall "midi_ffi.h midi_cents_to_bend" c_midi_cents_to_bend :: CInt -> IO CInt
foreign import ccall "midi_ffi.h midi_sleep" c_midi_sleep :: CInt -> IO ()
foreign import ccall "midi_ffi.h midi_panic" c_midi_panic :: IO ()
foreign import ccall "midi_ffi.h midi_seed_random" c_midi_seed_random :: CInt -> IO ()
foreign import ccall "midi_ffi.h midi_random" c_midi_random :: IO CInt
foreign import ccall "midi_ffi.h midi_random_range" c_midi_random_range :: CInt -> CInt -> IO CInt
foreign import ccall "midi_ffi.h midi_record_start" c_midi_record_start :: CInt -> IO CInt
foreign import ccall "midi_ffi.h midi_record_stop" c_midi_record_stop :: IO CInt
foreign import ccall "midi_ffi.h midi_save_mid" c_midi_save_mid :: CString -> IO CInt
foreign import ccall "midi_ffi.h midi_save_hs" c_midi_save_hs :: CString -> IO CInt
foreign import ccall "midi_ffi.h midi_record_count" c_midi_record_count :: IO CInt
foreign import ccall "midi_ffi.h midi_record_active" c_midi_record_active :: IO CInt
foreign import ccall "midi_ffi.h midi_read_mid" c_midi_read_mid :: CString -> IO CInt

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

-- | Convert cents offset to MIDI pitch bend value
midiCentsToBend :: Int -> IO Int
midiCentsToBend cents = do
    r <- c_midi_cents_to_bend (fromIntegral cents)
    return (fromIntegral r)

-----------------------------------------------------------
-- Random number generation
-----------------------------------------------------------

-- | Seed the random number generator
midiSeedRandom :: Int -> IO ()
midiSeedRandom seed = c_midi_seed_random (fromIntegral seed)

-- | Get a random integer
midiRandom :: IO Int
midiRandom = do
    r <- c_midi_random
    return (fromIntegral r)

-- | Get a random integer in range [min, max]
midiRandomRange :: Int -> Int -> IO Int
midiRandomRange minVal maxVal = do
    r <- c_midi_random_range (fromIntegral minVal) (fromIntegral maxVal)
    return (fromIntegral r)

-----------------------------------------------------------
-- MIDI recording
-----------------------------------------------------------

-- | Start MIDI recording with given BPM
midiRecordStart :: Int -> IO Bool
midiRecordStart bpm = do
    r <- c_midi_record_start (fromIntegral bpm)
    return (r == 0)

-- | Stop MIDI recording, returns event count
midiRecordStop :: IO Int
midiRecordStop = do
    r <- c_midi_record_stop
    return (fromIntegral r)

-- | Save recorded MIDI to standard MIDI file (.mid)
midiSaveToFile :: String -> IO Bool
midiSaveToFile filename = do
    withCString filename $ \cname -> do
        r <- c_midi_save_mid cname
        return (r == 0)

-- | Save recorded MIDI as Haskell source file (.hs)
midiSaveToSource :: String -> IO Bool
midiSaveToSource filename = do
    withCString filename $ \cname -> do
        r <- c_midi_save_hs cname
        return (r == 0)

-- | Get current recording event count
midiRecordCount :: IO Int
midiRecordCount = do
    r <- c_midi_record_count
    return (fromIntegral r)

-- | Check if recording is active
midiRecordActive :: IO Bool
midiRecordActive = do
    r <- c_midi_record_active
    return (r /= 0)

-----------------------------------------------------------
-- MIDI File I/O
-----------------------------------------------------------

-- | Read and display MIDI file info
midiReadMid :: String -> IO Bool
midiReadMid filename = do
    withCString filename $ \cname -> do
        r <- c_midi_read_mid cname
        return (r == 0)
