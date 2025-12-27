-- | Pure music representation with IO interpretation
--
-- Build music as pure data, transform it, then perform:
--
--   melody = line [c4, e4, g4]
--   bass = line [c2, g2]
--   piece = melody ||| bass
--   piece' = transpose 7 (stretch 2 piece)
--   perform piece'
--
module Music (
    -- * Core types
    Music(..),
    Event(..),

    -- * Note constructors
    note,
    noteV,
    noteD,
    noteVD,
    rest,

    -- * Chord constructors
    chord,
    chordV,
    chordD,
    chordVD,

    -- * Sequencing
    line,
    (+:+),
    (|||),
    times,

    -- * Transformations
    transpose,
    louder,
    softer,
    stretch,
    compress,
    withChan,

    -- * Interpretation
    perform,
    performChan,

    -- * Re-exports from Midi
    Pitch, Duration, Velocity, Channel,
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
    db, eb, gb, ab, bb,
    whole, half, quarter, eighth, sixteenth, dotted, bpm,
    ppp, pp, p, mp, mf, ff, fff,
    defaultChannel,
    midiOpenVirtual, midiOpen, midiClose, midiPanic,
) where

import Midi hiding (times, melody, chord, arpeggio, play, playNote, playChord, rest)

-- | A musical event (single point in time)
data Event
    = ENote Channel Pitch Velocity Duration
    | ERest Duration
    deriving (Eq, Show)

-- | Music is a tree of sequential and parallel events
data Music
    = MEvent Event
    | MSeq [Music]    -- sequential (one after another)
    | MPar [Music]    -- parallel (simultaneous)
    deriving (Eq, Show)

------------------------------------------------------------
-- Note constructors
------------------------------------------------------------

-- | Single note with default velocity (mf) and duration (quarter)
note :: Pitch -> Music
note p = MEvent (ENote defaultChannel p mf quarter)

-- | Note with custom velocity
noteV :: Velocity -> Pitch -> Music
noteV v p = MEvent (ENote defaultChannel p v quarter)

-- | Note with custom duration
noteD :: Duration -> Pitch -> Music
noteD d p = MEvent (ENote defaultChannel p mf d)

-- | Note with custom velocity and duration
noteVD :: Velocity -> Duration -> Pitch -> Music
noteVD v d p = MEvent (ENote defaultChannel p v d)

-- | Rest (silence)
rest :: Duration -> Music
rest d = MEvent (ERest d)

------------------------------------------------------------
-- Chord constructors
------------------------------------------------------------

-- | Chord with default velocity and duration
chord :: [Pitch] -> Music
chord ps = MPar [note p | p <- ps]

-- | Chord with custom velocity
chordV :: Velocity -> [Pitch] -> Music
chordV v ps = MPar [noteV v p | p <- ps]

-- | Chord with custom duration
chordD :: Duration -> [Pitch] -> Music
chordD d ps = MPar [noteD d p | p <- ps]

-- | Chord with custom velocity and duration
chordVD :: Velocity -> Duration -> [Pitch] -> Music
chordVD v d ps = MPar [noteVD v d p | p <- ps]

------------------------------------------------------------
-- Sequencing combinators
------------------------------------------------------------

-- | Sequence notes into a melody
line :: [Pitch] -> Music
line ps = MSeq [note p | p <- ps]

-- | Sequential composition
(+:+) :: Music -> Music -> Music
m1 +:+ m2 = MSeq [m1, m2]

-- | Parallel composition
(|||) :: Music -> Music -> Music
m1 ||| m2 = MPar [m1, m2]

-- | Repeat music n times
times :: Int -> Music -> Music
times n m = MSeq (replicate n m)

infixr 5 +:+
infixr 4 |||

------------------------------------------------------------
-- Transformations (pure)
------------------------------------------------------------

-- | Transpose all pitches by semitones
transpose :: Int -> Music -> Music
transpose n = mapEvents transposeEvent
  where
    transposeEvent (ENote ch p v d) = ENote ch (p + n) v d
    transposeEvent e = e

-- | Increase velocity by amount
louder :: Int -> Music -> Music
louder n = mapEvents louderEvent
  where
    louderEvent (ENote ch p v d) = ENote ch p (clamp (v + n)) d
    louderEvent e = e
    clamp x = max 0 (min 127 x)

-- | Decrease velocity by amount
softer :: Int -> Music -> Music
softer n = louder (-n)

-- | Stretch durations by factor (2 = twice as slow)
stretch :: Int -> Music -> Music
stretch factor = mapEvents stretchEvent
  where
    stretchEvent (ENote ch p v d) = ENote ch p v (d * factor)
    stretchEvent (ERest d) = ERest (d * factor)

-- | Compress durations by factor (2 = twice as fast)
compress :: Int -> Music -> Music
compress factor = mapEvents compressEvent
  where
    compressEvent (ENote ch p v d) = ENote ch p v (d `div` factor)
    compressEvent (ERest d) = ERest (d `div` factor)

-- | Set channel for all notes
withChan :: Channel -> Music -> Music
withChan ch = mapEvents setChan
  where
    setChan (ENote _ p v d) = ENote ch p v d
    setChan e = e

-- | Map a function over all events
mapEvents :: (Event -> Event) -> Music -> Music
mapEvents f (MEvent e) = MEvent (f e)
mapEvents f (MSeq ms) = MSeq [mapEvents f m | m <- ms]
mapEvents f (MPar ms) = MPar [mapEvents f m | m <- ms]

------------------------------------------------------------
-- Interpretation (IO)
------------------------------------------------------------

-- | Perform music on default channel
perform :: Music -> IO ()
perform = performChan defaultChannel

-- | Perform music, using given channel as default
performChan :: Channel -> Music -> IO ()
performChan defCh music = go music
  where
    go (MEvent e) = performEvent defCh e
    go (MSeq ms) = mapM_ go ms
    go (MPar ms) = performPar defCh ms

-- | Perform a single event
performEvent :: Channel -> Event -> IO ()
performEvent defCh (ENote ch p v d) = do
    let ch' = if ch == defaultChannel then defCh else ch
    midiNoteOn ch' p v
    midiSleep d
    midiNoteOff ch' p
performEvent _ (ERest d) = midiSleep d

-- | Perform parallel music (all start together, wait for longest)
performPar :: Channel -> [Music] -> IO ()
performPar defCh ms = do
    -- For true parallelism we'd need threads, but for chords
    -- we can handle the common case: parallel notes
    let events = concatMap collectEvents ms
        notes = [e | e@(ENote _ _ _ _) <- events]
        maxDur = maximum (0 : [d | ENote _ _ _ d <- notes])
    -- Note on for all
    mapM_ (\(ENote ch p v _) -> midiNoteOn ch p v) notes
    -- Wait for longest
    midiSleep maxDur
    -- Note off for all
    mapM_ (\(ENote ch p _ _) -> midiNoteOff ch p) notes

-- | Collect all events from music (flattening structure)
collectEvents :: Music -> [Event]
collectEvents (MEvent e) = [e]
collectEvents (MSeq ms) = concatMap collectEvents ms
collectEvents (MPar ms) = concatMap collectEvents ms
