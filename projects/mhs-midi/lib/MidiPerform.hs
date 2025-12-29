-- | MIDI Perform - Immediate MIDI playback with generative functions
--
-- Standalone module for immediate MIDI playback.
-- Does NOT import Music.hs, so musical terms (note, rest, chord, etc.)
-- refer directly to IO actions.
--
-- Example session:
--   > open
--   MIDI open
--   > note c4
--   > chord [c4, e4, g4]
--   > arpeggio [c4, e4, g4, c5]
--   > rest quarter
--   > melody [c4, d4, e4, f4, g4]
--   > drunk 16 c4 (major c4) 2
--   > close
--
module MidiPerform (
    -- * MIDI control
    open,
    openPort,
    close,
    panic,
    ports,

    -- * Note playing
    note,
    noteWith,
    chord,
    chordWith,
    arpeggio,
    arpeggioWith,

    -- * Timing
    rest,
    wait,

    -- * Sequences
    melody,
    melodyWith,
    times,

    -- * Generative
    pick,
    chance,
    oneOf,
    maybeDo,
    scramble,
    randomNote,
    randomMelody,
    walk,
    drunk,
    euclidean,
    seed,

    -- * Scales (for generative use)
    major, minor, pentatonic, blues, chromatic,
    dorian, phrygian, lydian, mixolydian,
    harmonicMinor, melodicMinor,

    -- * Low-level MIDI
    noteOn,
    noteOff,
    cc,
    program,
    bend,

    -- * Pitches
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
    db0, db1, db2, db3, db4, db5, db6, db7, db8,
    eb0, eb1, eb2, eb3, eb4, eb5, eb6, eb7, eb8,
    gb0, gb1, gb2, gb3, gb4, gb5, gb6, gb7, gb8,
    ab0, ab1, ab2, ab3, ab4, ab5, ab6, ab7, ab8,
    bb0, bb1, bb2, bb3, bb4, bb5, bb6, bb7, bb8,

    -- * Durations (ms)
    whole, half, quarter, eighth, sixteenth,
    dotted,

    -- * Velocities
    ppp, pp, p, mp, mf, ff, fff,

    -- * Types
    Pitch, Duration, Velocity, Channel,
) where

import Midi

-----------------------------------------------------------
-- Types
-----------------------------------------------------------

type Pitch = Int
type Duration = Int
type Velocity = Int
type Channel = Int

-----------------------------------------------------------
-- MIDI control
-----------------------------------------------------------

-- | Open virtual MIDI port "mhsMIDI"
open :: IO ()
open = do
    ok <- midiOpenVirtual "mhsMIDI"
    if ok
        then putStrLn "MIDI open"
        else putStrLn "MIDI failed to open"

-- | Open a specific MIDI port by index
openPort :: Int -> IO ()
openPort idx = do
    ok <- midiOpen idx
    if ok
        then putStrLn "MIDI port open"
        else putStrLn "Failed to open MIDI port"

-- | Close MIDI port
close :: IO ()
close = midiClose

-- | All notes off on all channels
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

-----------------------------------------------------------
-- Note playing (immediate IO)
-----------------------------------------------------------

-- | Play a note with default velocity (mf) and duration (quarter)
--
-- > note c4
-- > note (c4 + 12)  -- octave up
note :: Pitch -> IO ()
note pit = noteWith 1 mf quarter pit

-- | Play a note with channel, velocity, duration
--
-- > noteWith 1 ff half c4
-- > noteWith 2 pp eighth g5
noteWith :: Channel -> Velocity -> Duration -> Pitch -> IO ()
noteWith ch vel dur pit = do
    midiNoteOn ch pit vel
    midiSleep dur
    midiNoteOff ch pit

-- | Play a chord with default velocity (mf) and duration (quarter)
--
-- > chord [c4, e4, g4]
-- > chord [c3, c4, g4, e5]
chord :: [Pitch] -> IO ()
chord pits = chordWith 1 mf quarter pits

-- | Play a chord with channel, velocity, duration
--
-- > chordWith 1 ff half [c4, e4, g4]
chordWith :: Channel -> Velocity -> Duration -> [Pitch] -> IO ()
chordWith ch vel dur pits = do
    mapM_ (\pit -> midiNoteOn ch pit vel) pits
    midiSleep dur
    mapM_ (\pit -> midiNoteOff ch pit) pits

-- | Play notes as arpeggio with default velocity (mf) and note duration (sixteenth)
--
-- > arpeggio [c4, e4, g4, c5]
-- > arpeggio (reverse [c4, e4, g4, c5])
arpeggio :: [Pitch] -> IO ()
arpeggio pits = arpeggioWith 1 mf sixteenth pits

-- | Play notes as arpeggio with channel, velocity, note duration
--
-- > arpeggioWith 1 ff eighth [c4, e4, g4]
arpeggioWith :: Channel -> Velocity -> Duration -> [Pitch] -> IO ()
arpeggioWith ch vel dur pits = mapM_ (noteWith ch vel dur) pits

-----------------------------------------------------------
-- Timing
-----------------------------------------------------------

-- | Rest (silence) for a duration
--
-- > rest quarter
-- > rest (dotted half)
rest :: Duration -> IO ()
rest = midiSleep

-- | Alias for rest
wait :: Duration -> IO ()
wait = rest

-----------------------------------------------------------
-- Sequences
-----------------------------------------------------------

-- | Play a melody (sequence of pitches) with default velocity (mf) and duration (quarter)
--
-- > melody [c4, d4, e4, f4, g4]
melody :: [Pitch] -> IO ()
melody pits = melodyWith 1 mf quarter pits

-- | Play a melody with channel, velocity, duration per note
--
-- > melodyWith 1 mp eighth [c4, d4, e4, f4, g4, a4, b4, c5]
melodyWith :: Channel -> Velocity -> Duration -> [Pitch] -> IO ()
melodyWith ch vel dur pits = mapM_ (noteWith ch vel dur) pits

-- | Repeat an action n times
--
-- > times 4 (note c4)
-- > times 2 (chord [c4, e4, g4] >> rest quarter >> chord [g4, b4, d5])
times :: Int -> IO () -> IO ()
times 0 _ = return ()
times n action = action >> times (n - 1) action

-----------------------------------------------------------
-- Generative music
-----------------------------------------------------------

-- | Seed the random number generator
--
-- > seed 42
seed :: Int -> IO ()
seed = midiSeedRandom

-- | Pick a random element from a list and play it
--
-- > pick [c4, e4, g4]
-- > times 8 (pick [c4, d4, e4, f4, g4])
pick :: [Pitch] -> IO ()
pick [] = return ()
pick pits = do
    idx <- midiRandomRange 0 (length pits - 1)
    note (pits !! idx)

-- | Play with a probability (0-100)
--
-- > chance 50 (note c4)        -- 50% chance
-- > chance 75 (chord [c4, e4, g4])  -- 75% chance
chance :: Int -> IO () -> IO ()
chance pct action = do
    r <- midiRandomRange 1 100
    if r <= pct then action else return ()

-- | Randomly execute one of several actions
--
-- > oneOf [note c4, note e4, note g4]
-- > oneOf [chord [c4, e4, g4], arpeggio [c4, e4, g4]]
oneOf :: [IO ()] -> IO ()
oneOf [] = return ()
oneOf actions = do
    idx <- midiRandomRange 0 (length actions - 1)
    actions !! idx

-- | Maybe do something (50% chance)
--
-- > maybeDo (note c4)
maybeDo :: IO () -> IO ()
maybeDo = chance 50

-- | Play notes in random order
--
-- > scramble [c4, e4, g4, c5]
-- > scramble (major c4)
scramble :: [Pitch] -> IO ()
scramble [] = return ()
scramble pits = scrambleWith 1 mf quarter pits

-- | Scramble with parameters
scrambleWith :: Channel -> Velocity -> Duration -> [Pitch] -> IO ()
scrambleWith _ _ _ [] = return ()
scrambleWith ch vel dur pits = do
    shuffled <- shuffle pits
    mapM_ (noteWith ch vel dur) shuffled

-- | Shuffle a list randomly
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    idx <- midiRandomRange 0 (length xs - 1)
    let (before, chosen:after) = splitAt idx xs
    rest' <- shuffle (before ++ after)
    return (chosen : rest')

-- | Play a random note in a pitch range
--
-- > randomNote c4 c5
-- > times 8 (randomNote c3 c6)
randomNote :: Pitch -> Pitch -> IO ()
randomNote low high = do
    pit <- midiRandomRange low high
    note pit

-- | Generate and play a random melody
--
-- > randomMelody 8 c4 c5       -- 8 random notes between c4 and c5
-- > randomMelody 16 c3 c6      -- 16 random notes, wider range
randomMelody :: Int -> Pitch -> Pitch -> IO ()
randomMelody n low high = times n (randomNote low high)

-- | Random walk melody - each note is +/- maxStep from previous
--
-- > walk 8 c4 3          -- 8 notes, starting at c4, max step of 3 semitones
-- > walk 16 g4 5         -- longer walk with bigger steps
walk :: Int -> Pitch -> Int -> IO ()
walk n start maxStep = walkWith 1 mf quarter n start maxStep

-- | Random walk with parameters
walkWith :: Channel -> Velocity -> Duration -> Int -> Pitch -> Int -> IO ()
walkWith _ _ _ 0 _ _ = return ()
walkWith ch vel dur n pit maxStep = do
    noteWith ch vel dur pit
    step <- midiRandomRange (-maxStep) maxStep
    let nextPit = clamp 24 108 (pit + step)  -- keep in reasonable range
    walkWith ch vel dur (n - 1) nextPit maxStep

-- | Drunk walk - random walk constrained to a scale
--
-- > drunk 8 c4 (major c4) 2    -- walk on C major, max 2 scale degrees
-- > drunk 16 a4 (minor a4) 3   -- walk on A minor
drunk :: Int -> Pitch -> [Pitch] -> Int -> IO ()
drunk n start scale maxDegrees = drunkWith 1 mf quarter n start scale maxDegrees

-- | Drunk walk with parameters
drunkWith :: Channel -> Velocity -> Duration -> Int -> Pitch -> [Pitch] -> Int -> IO ()
drunkWith _ _ _ 0 _ _ _ = return ()
drunkWith _ _ _ _ _ [] _ = return ()
drunkWith ch vel dur n pit scale maxDegrees = do
    let idx = findNearest pit scale
    noteWith ch vel dur (scale !! idx)
    step <- midiRandomRange (-maxDegrees) maxDegrees
    let nextIdx = clamp 0 (length scale - 1) (idx + step)
    drunkWith ch vel dur (n - 1) (scale !! nextIdx) scale maxDegrees

-- | Find index of nearest pitch in a list
findNearest :: Pitch -> [Pitch] -> Int
findNearest _ [] = 0
findNearest pit pits = go 0 0 (abs (pit - head pits)) pits
  where
    go bestIdx _ _ [] = bestIdx
    go bestIdx idx bestDist (x:xs)
        | dist < bestDist = go idx (idx + 1) dist xs
        | otherwise = go bestIdx (idx + 1) bestDist xs
      where dist = abs (pit - x)

-- | Euclidean rhythm - distribute n hits over k steps
--
-- > euclidean 3 8 (note c4)     -- 3 hits over 8 steps: X..X..X.
-- > euclidean 5 8 (note c4)     -- 5 hits over 8 steps: X.XX.XX.
-- > euclidean 7 16 (chord [c4, e4, g4])
euclidean :: Int -> Int -> IO () -> IO ()
euclidean hits steps action = euclideanWith quarter action hits steps

-- | Euclidean rhythm with custom step duration
euclideanWith :: Duration -> IO () -> Int -> Int -> IO ()
euclideanWith dur action hits steps = mapM_ playStep pattern
  where
    pattern = euclideanPattern hits steps
    playStep True = action >> rest (dur - dur)  -- action includes its own duration
    playStep False = rest dur

-- | Generate euclidean rhythm pattern
euclideanPattern :: Int -> Int -> [Bool]
euclideanPattern hits steps
    | steps <= 0 = []
    | hits <= 0 = replicate steps False
    | hits >= steps = replicate steps True
    | otherwise = bjorklund hits (steps - hits)

-- | Bjorklund algorithm for euclidean rhythms
bjorklund :: Int -> Int -> [Bool]
bjorklund hits rests = concat $ bjorklundStep (replicate hits [True]) (replicate rests [False])

bjorklundStep :: [[Bool]] -> [[Bool]] -> [[Bool]]
bjorklundStep xs [] = xs
bjorklundStep [] ys = ys
bjorklundStep xs ys
    | length ys <= 1 = xs ++ ys
    | otherwise = bjorklundStep (zipWith (++) xs ys) (drop (length xs) ys ++ take (length xs - length ys) [])

-- | Clamp a value to a range
clamp :: Int -> Int -> Int -> Int
clamp lo hi x
    | x < lo = lo
    | x > hi = hi
    | otherwise = x

-----------------------------------------------------------
-- Scales (for generative use)
-----------------------------------------------------------

-- | Build a scale from root and intervals
buildScale :: Pitch -> [Int] -> [Pitch]
buildScale root intervals = [root + i | i <- scaleIntervals]
  where
    scaleIntervals = concatMap (\oct -> map (+ (oct * 12)) intervals) [0..3]

-- | Major scale from root (2 octaves)
--
-- > melody (major c4)
-- > scramble (major g3)
major :: Pitch -> [Pitch]
major root = buildScale root [0, 2, 4, 5, 7, 9, 11]

-- | Natural minor scale from root
minor :: Pitch -> [Pitch]
minor root = buildScale root [0, 2, 3, 5, 7, 8, 10]

-- | Pentatonic scale from root
pentatonic :: Pitch -> [Pitch]
pentatonic root = buildScale root [0, 2, 4, 7, 9]

-- | Blues scale from root
blues :: Pitch -> [Pitch]
blues root = buildScale root [0, 3, 5, 6, 7, 10]

-- | Chromatic scale from root
chromatic :: Pitch -> [Pitch]
chromatic root = buildScale root [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

-- | Dorian mode
dorian :: Pitch -> [Pitch]
dorian root = buildScale root [0, 2, 3, 5, 7, 9, 10]

-- | Phrygian mode
phrygian :: Pitch -> [Pitch]
phrygian root = buildScale root [0, 1, 3, 5, 7, 8, 10]

-- | Lydian mode
lydian :: Pitch -> [Pitch]
lydian root = buildScale root [0, 2, 4, 6, 7, 9, 11]

-- | Mixolydian mode
mixolydian :: Pitch -> [Pitch]
mixolydian root = buildScale root [0, 2, 4, 5, 7, 9, 10]

-- | Harmonic minor scale
harmonicMinor :: Pitch -> [Pitch]
harmonicMinor root = buildScale root [0, 2, 3, 5, 7, 8, 11]

-- | Melodic minor scale (ascending)
melodicMinor :: Pitch -> [Pitch]
melodicMinor root = buildScale root [0, 2, 3, 5, 7, 9, 11]

-----------------------------------------------------------
-- Low-level MIDI
-----------------------------------------------------------

-- | Send note on
noteOn :: Channel -> Pitch -> Velocity -> IO ()
noteOn = midiNoteOn

-- | Send note off
noteOff :: Channel -> Pitch -> IO ()
noteOff = midiNoteOff

-- | Send control change
cc :: Channel -> Int -> Int -> IO ()
cc = midiCC

-- | Send program change
program :: Channel -> Int -> IO ()
program = midiProgram

-- | Send pitch bend (-8192 to 8191)
bend :: Channel -> Int -> IO ()
bend = midiPitchBend

-----------------------------------------------------------
-- Pitches (MIDI note numbers)
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

-- Sharps
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

-- Flats (aliases for sharps)
db0, db1, db2, db3, db4, db5, db6, db7, db8 :: Pitch
db0 = cs0; db1 = cs1; db2 = cs2; db3 = cs3; db4 = cs4; db5 = cs5; db6 = cs6; db7 = cs7; db8 = cs8

eb0, eb1, eb2, eb3, eb4, eb5, eb6, eb7, eb8 :: Pitch
eb0 = ds0; eb1 = ds1; eb2 = ds2; eb3 = ds3; eb4 = ds4; eb5 = ds5; eb6 = ds6; eb7 = ds7; eb8 = ds8

gb0, gb1, gb2, gb3, gb4, gb5, gb6, gb7, gb8 :: Pitch
gb0 = fs0; gb1 = fs1; gb2 = fs2; gb3 = fs3; gb4 = fs4; gb5 = fs5; gb6 = fs6; gb7 = fs7; gb8 = fs8

ab0, ab1, ab2, ab3, ab4, ab5, ab6, ab7, ab8 :: Pitch
ab0 = gs0; ab1 = gs1; ab2 = gs2; ab3 = gs3; ab4 = gs4; ab5 = gs5; ab6 = gs6; ab7 = gs7; ab8 = gs8

bb0, bb1, bb2, bb3, bb4, bb5, bb6, bb7, bb8 :: Pitch
bb0 = as0; bb1 = as1; bb2 = as2; bb3 = as3; bb4 = as4; bb5 = as5; bb6 = as6; bb7 = as7; bb8 = as8

-----------------------------------------------------------
-- Durations (milliseconds at 120 BPM)
-----------------------------------------------------------

whole, half, quarter, eighth, sixteenth :: Duration
whole     = 2000
half      = 1000
quarter   = 500
eighth    = 250
sixteenth = 125

-- | Dotted duration (1.5x)
dotted :: Duration -> Duration
dotted dur = dur + dur `div` 2

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
