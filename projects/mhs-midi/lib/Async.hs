-- | Async Scheduler for mhs-midi
--
-- Provides concurrent voice playback using Haskell threads.
-- Each voice runs in its own thread using forkIO.
--
-- Example:
--   import Async
--   main = do
--       midiOpenVirtual "myApp"
--       spawn "melody" $ do
--           midiNoteOn 1 60 80
--           midiSleep 500
--           midiNoteOff 1 60
--       spawn "bass" $ do
--           midiNoteOn 1 36 100
--           midiSleep 1000
--           midiNoteOff 1 36
--       run  -- Wait for all voices to complete
--       midiClose
--
module Async (
    -- * Types
    VoiceId,

    -- * Scheduler functions
    spawn,
    run,
    stop,
    stopAll,
    voices,
    status,

    -- * Async music helpers
    asyncNote,
    asyncChord,
    asyncPerform,
) where

import Control.Concurrent
import Control.Exception
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Midi
import Music

-----------------------------------------------------------
-- Types
-----------------------------------------------------------

-- | Voice identifier (wrapper around ThreadId)
type VoiceId = ThreadId

-- | Voice state tracked by scheduler
data VoiceState = VoiceState
    { vsThreadId :: ThreadId
    , vsName     :: String
    , vsRunning  :: MVar Bool  -- True while running
    }

-- | Global scheduler state
data SchedulerState = SchedulerState
    { ssVoices   :: MVar [VoiceState]  -- List of active voices
    , ssRunning  :: MVar Bool          -- Is run() active?
    }

-----------------------------------------------------------
-- Global state (initialized lazily)
-----------------------------------------------------------

{-# NOINLINE globalState #-}
globalState :: SchedulerState
globalState = unsafePerformIO $ do
    voices <- newMVar []
    running <- newMVar False
    return (SchedulerState voices running)

-----------------------------------------------------------
-- Scheduler functions
-----------------------------------------------------------

-- | Spawn a new voice with an optional name
spawn :: String -> IO () -> IO VoiceId
spawn name action = do
    runningVar <- newMVar True
    tid <- forkIO $ do
        -- Run the action, catching exceptions
        catch action (\e -> let _ = (e :: SomeException) in return ())
        -- Mark as done
        swapMVar runningVar False >> return ()
        -- Remove self from list
        myTid <- myThreadId
        modifyMVar_ (ssVoices globalState) $ \vs ->
            return [v | v <- vs, vsThreadId v /= myTid]

    -- Add to voice list with the thread ID
    let voice = VoiceState tid name runningVar
    modifyMVar_ (ssVoices globalState) $ \vs -> return (voice : vs)
    return tid

-- | Run scheduler until all voices complete
run :: IO ()
run = do
    swapMVar (ssRunning globalState) True >> return ()
    waitLoop
    swapMVar (ssRunning globalState) False >> return ()
  where
    waitLoop = do
        vs <- readMVar (ssVoices globalState)
        if null vs
            then return ()
            else do
                threadDelay 10000  -- 10ms poll
                waitLoop

-- | Stop a specific voice
stop :: VoiceId -> IO Bool
stop tid = do
    vs <- readMVar (ssVoices globalState)
    case [v | v <- vs, vsThreadId v == tid] of
        [] -> return False
        (v:_) -> do
            killThread tid
            swapMVar (vsRunning v) False >> return ()
            modifyMVar_ (ssVoices globalState) $ \vs' ->
                return [v' | v' <- vs', vsThreadId v' /= tid]
            return True

-- | Stop all voices
stopAll :: IO ()
stopAll = do
    vs <- takeMVar (ssVoices globalState)
    mapM_ (\v -> killThread (vsThreadId v)) vs
    putMVar (ssVoices globalState) []

-- | Get count of active voices
voices :: IO Int
voices = do
    vs <- readMVar (ssVoices globalState)
    return (length vs)

-- | Get scheduler status as (running, voiceCount, voiceNames)
status :: IO (Bool, Int, [String])
status = do
    isRunning <- readMVar (ssRunning globalState)
    vs <- readMVar (ssVoices globalState)
    return (isRunning, length vs, map vsName vs)

-----------------------------------------------------------
-- Async music helpers
-----------------------------------------------------------

-- | Play a note asynchronously
asyncNote :: Channel -> Pitch -> Velocity -> Duration -> IO VoiceId
asyncNote ch pit vel dur = spawn "" $ do
    midiNoteOn ch pit vel
    midiSleep dur
    midiNoteOff ch pit

-- | Play a chord asynchronously
asyncChord :: Channel -> [Pitch] -> Velocity -> Duration -> IO VoiceId
asyncChord ch pits vel dur = spawn "" $ do
    mapM_ (\p -> midiNoteOn ch p vel) pits
    midiSleep dur
    mapM_ (\p -> midiNoteOff ch p) pits

-- | Perform music asynchronously
asyncPerform :: Music -> IO VoiceId
asyncPerform music = spawn "" $ performOn 1 music

-- | Perform music on a specific channel (local implementation)
performOn :: Channel -> Music -> IO ()
performOn ch music = go music
  where
    go (MEvent ev) = performEvent ch ev
    go (MSeq ms) = mapM_ go ms
    go (MPar ms) = performPar ch ms

    performEvent ch' (ENote pit v d) = do
        midiNoteOn ch' pit v
        midiSleep d
        midiNoteOff ch' pit
    performEvent _ (ERest d) = midiSleep d

    -- Perform parallel music using threads so each note respects its duration
    performPar ch' ms = do
        doneVars <- mapM (\_ -> newEmptyMVar) ms
        mapM_ (\(m, doneVar) -> forkIO (go' m >> putMVar doneVar ())) (zip ms doneVars)
        mapM_ takeMVar doneVars
      where
        go' (MEvent ev) = performEvent ch' ev
        go' (MSeq subMs) = mapM_ go' subMs
        go' (MPar subMs) = performPar ch' subMs
