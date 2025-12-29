-- | Pure Music Theory for MicroHs
--
-- This module contains only pure functions and constants.
-- No IO - see MusicPerform for performance functions.
--
-- Provides:
-- - Pitch, duration, velocity constants
-- - 49 scales with operations
-- - Microtonal scales (cents-based)
-- - Pure music representation and transformations
--
module Music (
    -- * Musical types
    Pitch, Duration, Velocity, Channel,
    defaultChannel,

    -- * Pitch names (MIDI note numbers)
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
    whole, half, quarter, eighth, sixteenth,
    dotted,
    bpm,

    -- * Velocity (dynamics)
    ppp, pp, p, mp, mf, ff, fff,

    -- * Scales
    Scale,
    buildScale,
    scaleDegree,
    inScale,
    quantize,
    -- ** Diatonic modes
    scaleMajor, scaleMinor, scaleDorian, scalePhrygian,
    scaleLydian, scaleMixolydian, scaleLocrian,
    scaleIonian, scaleAeolian,
    -- ** Minor variants
    scaleHarmonicMinor, scaleMelodicMinor, scaleHarmonicMajor,
    -- ** Pentatonic and blues
    scalePentatonic, scalePentatonicMajor, scalePentatonicMinor,
    scaleBlues, scaleBluesMajor,
    -- ** Symmetric
    scaleWholeTone, scaleChromatic, scaleDiminished, scaleAugmented,
    -- ** Bebop
    scaleBebopDominant, scaleBebopMajor, scaleBebopMinor,
    -- ** World scales
    scaleHungarianMinor, scaleDoubleHarmonic, scaleGypsy,
    scaleHirajoshi, scaleInSen, scaleIwato, scaleKumoi,
    scalePersian, scaleAltered, scaleEnigmatic,
    scaleNeapolitanMajor, scaleNeapolitanMinor, scalePhrygianDominant,
    scaleEgyptian, scaleRomanianMinor, scaleSpanish8Tone,
    -- ** Arabic Maqamat (12-TET approximations)
    scaleMaqamHijaz, scaleMaqamNahawand, scaleMaqamNikriz,
    scaleMaqamAtharKurd, scaleMaqamShawqAfza, scaleMaqamJiharkah,
    -- ** Indian Ragas (12-TET approximations)
    scaleRagaBhairav, scaleRagaTodi, scaleRagaMarwa, scaleRagaPurvi,
    scaleRagaCharukeshi, scaleRagaAsavari, scaleRagaBilawal,
    scaleRagaKhamaj, scaleRagaKalyan, scaleRagaBhimpalasi, scaleRagaDarbari,

    -- * Microtonal (pure)
    centsToNote,
    -- ** Microtonal scales (cents-based)
    scaleMaqamBayatiCents, scaleMaqamRastCents, scaleMaqamSabaCents,
    scaleMaqamSikahCents, scaleMaqamHuzamCents, scaleMaqamIraqCents,
    scaleMaqamBastanikarCents,
    scaleMakamUssakCents, scaleMakamHuseyniCents,
    scaleShrutiCents,

    -- * Music DSL (pure)
    Music(..),
    Event(..),
    -- ** Constructors
    note,
    rest,
    chord,
    line,
    -- ** Combinators
    (+:+),
    (|||),
    timesM,
    -- ** Transformations
    transpose,
    louder,
    softer,
    stretch,
    compress,
    -- ** Utilities
    mapEvents,
    collectEvents,
    duration,
) where

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
-- Scales
-----------------------------------------------------------

-- | A scale is a list of semitone intervals from the root
type Scale = [Int]

-- | Build a scale from root pitch and intervals
buildScale :: Pitch -> Scale -> [Pitch]
buildScale root intervals = [root + i | i <- intervals, root + i >= 0, root + i <= 127]

-- | Get the nth degree of a scale (1-based)
-- Supports extended degrees (e.g., 9 = 2nd + octave)
scaleDegree :: Pitch -> Scale -> Int -> Pitch
scaleDegree root intervals deg
    | deg < 1   = root
    | otherwise = root + octaves * 12 + intervals !! idx
  where
    len = length intervals
    idx = (deg - 1) `mod` len
    octaves = (deg - 1) `div` len

-- | Check if a pitch belongs to a scale (in any octave)
inScale :: Pitch -> Pitch -> Scale -> Bool
inScale pitch root intervals = elemInt pc normalizedIntervals
  where
    pc = (pitch - root) `mod` 12
    normalizedIntervals = [i `mod` 12 | i <- intervals]
    elemInt :: Int -> [Int] -> Bool
    elemInt _ [] = False
    elemInt x (y:ys) = x == y || elemInt x ys

-- | Quantize a pitch to the nearest note in a scale
quantize :: Pitch -> Pitch -> Scale -> Pitch
quantize pitch root intervals = pitch + bestOffset
  where
    pc = (pitch - root) `mod` 12
    normalizedIntervals = [i `mod` 12 | i <- intervals]
    offsets = [i - pc | i <- normalizedIntervals]
    -- Find offset with smallest absolute value
    bestOffset = foldr1 (\o best -> if abs o < abs best then o else best) offsets

-----------------------------------------------------------
-- Scale constants (12-TET)
-----------------------------------------------------------

-- Diatonic modes
scaleMajor, scaleMinor, scaleDorian, scalePhrygian :: Scale
scaleLydian, scaleMixolydian, scaleLocrian :: Scale
scaleIonian, scaleAeolian :: Scale

scaleMajor      = [0, 2, 4, 5, 7, 9, 11]
scaleMinor      = [0, 2, 3, 5, 7, 8, 10]
scaleDorian     = [0, 2, 3, 5, 7, 9, 10]
scalePhrygian   = [0, 1, 3, 5, 7, 8, 10]
scaleLydian     = [0, 2, 4, 6, 7, 9, 11]
scaleMixolydian = [0, 2, 4, 5, 7, 9, 10]
scaleLocrian    = [0, 1, 3, 5, 6, 8, 10]
scaleIonian     = scaleMajor
scaleAeolian    = scaleMinor

-- Minor variants
scaleHarmonicMinor, scaleMelodicMinor, scaleHarmonicMajor :: Scale
scaleHarmonicMinor = [0, 2, 3, 5, 7, 8, 11]
scaleMelodicMinor  = [0, 2, 3, 5, 7, 9, 11]
scaleHarmonicMajor = [0, 2, 4, 5, 7, 8, 11]

-- Pentatonic and blues
scalePentatonic, scalePentatonicMajor, scalePentatonicMinor :: Scale
scaleBlues, scaleBluesMajor :: Scale
scalePentatonic      = [0, 2, 4, 7, 9]
scalePentatonicMajor = [0, 2, 4, 7, 9]
scalePentatonicMinor = [0, 3, 5, 7, 10]
scaleBlues           = [0, 3, 5, 6, 7, 10]
scaleBluesMajor      = [0, 2, 3, 4, 7, 9]

-- Symmetric scales
scaleWholeTone, scaleChromatic, scaleDiminished, scaleAugmented :: Scale
scaleWholeTone  = [0, 2, 4, 6, 8, 10]
scaleChromatic  = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
scaleDiminished = [0, 2, 3, 5, 6, 8, 9, 11]  -- whole-half
scaleAugmented  = [0, 3, 4, 7, 8, 11]

-- Bebop scales
scaleBebopDominant, scaleBebopMajor, scaleBebopMinor :: Scale
scaleBebopDominant = [0, 2, 4, 5, 7, 9, 10, 11]
scaleBebopMajor    = [0, 2, 4, 5, 7, 8, 9, 11]
scaleBebopMinor    = [0, 2, 3, 4, 5, 7, 9, 10]

-- World scales
scaleHungarianMinor, scaleDoubleHarmonic, scaleGypsy :: Scale
scaleHungarianMinor  = [0, 2, 3, 6, 7, 8, 11]
scaleDoubleHarmonic  = [0, 1, 4, 5, 7, 8, 11]
scaleGypsy           = [0, 2, 3, 6, 7, 8, 10]

scaleHirajoshi, scaleInSen, scaleIwato, scaleKumoi :: Scale
scaleHirajoshi = [0, 2, 3, 7, 8]
scaleInSen     = [0, 1, 5, 7, 10]
scaleIwato     = [0, 1, 5, 6, 10]
scaleKumoi     = [0, 2, 3, 7, 9]

scalePersian, scaleAltered, scaleEnigmatic :: Scale
scalePersian   = [0, 1, 4, 5, 6, 8, 11]
scaleAltered   = [0, 1, 3, 4, 6, 8, 10]
scaleEnigmatic = [0, 1, 4, 6, 8, 10, 11]

scaleNeapolitanMajor, scaleNeapolitanMinor, scalePhrygianDominant :: Scale
scaleNeapolitanMajor  = [0, 1, 3, 5, 7, 9, 11]
scaleNeapolitanMinor  = [0, 1, 3, 5, 7, 8, 11]
scalePhrygianDominant = [0, 1, 4, 5, 7, 8, 10]

scaleEgyptian, scaleRomanianMinor, scaleSpanish8Tone :: Scale
scaleEgyptian      = [0, 2, 5, 7, 10]
scaleRomanianMinor = [0, 2, 3, 6, 7, 9, 10]
scaleSpanish8Tone  = [0, 1, 3, 4, 5, 6, 8, 10]

-- Arabic Maqamat (12-TET approximations)
scaleMaqamHijaz, scaleMaqamNahawand, scaleMaqamNikriz :: Scale
scaleMaqamAtharKurd, scaleMaqamShawqAfza, scaleMaqamJiharkah :: Scale
scaleMaqamHijaz      = [0, 1, 4, 5, 7, 8, 10]
scaleMaqamNahawand   = [0, 2, 3, 5, 7, 8, 11]
scaleMaqamNikriz     = [0, 2, 3, 6, 7, 9, 10]
scaleMaqamAtharKurd  = [0, 2, 3, 6, 7, 8, 11]
scaleMaqamShawqAfza  = [0, 2, 3, 6, 7, 9, 11]
scaleMaqamJiharkah   = [0, 2, 4, 5, 7, 9, 10]

-- Indian Ragas (12-TET approximations)
scaleRagaBhairav, scaleRagaTodi, scaleRagaMarwa, scaleRagaPurvi :: Scale
scaleRagaCharukeshi, scaleRagaAsavari, scaleRagaBilawal :: Scale
scaleRagaKhamaj, scaleRagaKalyan, scaleRagaBhimpalasi, scaleRagaDarbari :: Scale
scaleRagaBhairav    = [0, 1, 4, 5, 7, 8, 11]
scaleRagaTodi       = [0, 1, 3, 6, 7, 8, 11]
scaleRagaMarwa      = [0, 1, 4, 6, 7, 9, 11]
scaleRagaPurvi      = [0, 1, 4, 6, 7, 8, 11]
scaleRagaCharukeshi = [0, 2, 4, 5, 7, 8, 11]
scaleRagaAsavari    = [0, 2, 3, 5, 7, 8, 10]
scaleRagaBilawal    = [0, 2, 4, 5, 7, 9, 11]
scaleRagaKhamaj     = [0, 2, 4, 5, 7, 9, 10]
scaleRagaKalyan     = [0, 2, 4, 6, 7, 9, 11]
scaleRagaBhimpalasi = [0, 2, 3, 5, 7, 9, 10]
scaleRagaDarbari    = [0, 2, 3, 5, 7, 8, 9]

-----------------------------------------------------------
-- Microtonal (pure functions)
-----------------------------------------------------------

-- | Convert a cents interval to note and pitch bend
-- Returns (midiNote, bendCents)
centsToNote :: Pitch -> Int -> (Pitch, Int)
centsToNote root cents = (n, bendCents)
  where
    semitones = cents `div` 100
    bendCents = cents `mod` 100
    n = root + semitones

-----------------------------------------------------------
-- Microtonal scales (cents-based)
-----------------------------------------------------------

-- Arabic Maqamat with authentic quarter-tones
scaleMaqamBayatiCents, scaleMaqamRastCents, scaleMaqamSabaCents :: [Int]
scaleMaqamSikahCents, scaleMaqamHuzamCents, scaleMaqamIraqCents :: [Int]
scaleMaqamBastanikarCents :: [Int]

scaleMaqamBayatiCents     = [0, 150, 300, 500, 700, 800, 1000]
scaleMaqamRastCents       = [0, 200, 350, 500, 700, 900, 1050]
scaleMaqamSabaCents       = [0, 150, 300, 400, 700, 800, 1000]
scaleMaqamSikahCents      = [0, 150, 350, 500, 700, 850, 1050]
scaleMaqamHuzamCents      = [0, 150, 350, 500, 700, 900, 1050]
scaleMaqamIraqCents       = [0, 150, 350, 500, 650, 850, 1050]
scaleMaqamBastanikarCents = [0, 150, 300, 500, 700, 850, 1000]

-- Turkish Makamlar
scaleMakamUssakCents, scaleMakamHuseyniCents :: [Int]
scaleMakamUssakCents   = [0, 150, 300, 500, 700, 850, 1000]
scaleMakamHuseyniCents = [0, 150, 300, 500, 700, 900, 1000]

-- Indian 22-shruti scale
scaleShrutiCents :: [Int]
scaleShrutiCents = [0, 22, 70, 90, 112, 182, 204, 294, 316, 386, 408,
                    498, 520, 590, 610, 702, 792, 814, 884, 906, 996, 1018]

-----------------------------------------------------------
-- Music DSL - Pure representation
-----------------------------------------------------------

-- | A musical event (single point in time)
data Event
    = ENote Pitch Velocity Duration
    | ERest Duration
    deriving (Eq, Show)

-- | Music is a tree of sequential and parallel events
data Music
    = MEvent Event
    | MSeq [Music]    -- sequential (one after another)
    | MPar [Music]    -- parallel (simultaneous)
    deriving (Eq, Show)

------------------------------------------------------------
-- Constructors
------------------------------------------------------------

-- | Single note
note :: Pitch -> Velocity -> Duration -> Music
note p v d = MEvent (ENote p v d)

-- | Rest (silence)
rest :: Duration -> Music
rest d = MEvent (ERest d)

-- | Chord (notes played simultaneously)
chord :: [Pitch] -> Velocity -> Duration -> Music
chord ps v d = MPar [note p v d | p <- ps]

-- | Sequence (notes played in order)
line :: [Pitch] -> Velocity -> Duration -> Music
line ps v d = MSeq [note p v d | p <- ps]

------------------------------------------------------------
-- Combinators
------------------------------------------------------------

-- | Sequential composition
(+:+) :: Music -> Music -> Music
m1 +:+ m2 = MSeq [m1, m2]

-- | Parallel composition
(|||) :: Music -> Music -> Music
m1 ||| m2 = MPar [m1, m2]

-- | Repeat music n times
timesM :: Int -> Music -> Music
timesM n m = MSeq (replicate n m)

infixr 5 +:+
infixr 4 |||

------------------------------------------------------------
-- Transformations (pure)
------------------------------------------------------------

-- | Transpose all pitches by semitones
transpose :: Int -> Music -> Music
transpose n = mapEvents transposeEvent
  where
    transposeEvent (ENote p v d) = ENote (p + n) v d
    transposeEvent ev = ev

-- | Increase velocity by amount
louder :: Int -> Music -> Music
louder n = mapEvents louderEvent
  where
    louderEvent (ENote p v d) = ENote p (clamp (v + n)) d
    louderEvent ev = ev
    clamp x = max 0 (min 127 x)

-- | Decrease velocity by amount
softer :: Int -> Music -> Music
softer n = louder (-n)

-- | Stretch durations by factor (2 = twice as slow)
stretch :: Int -> Music -> Music
stretch factor = mapEvents stretchEvent
  where
    stretchEvent (ENote p v d) = ENote p v (d * factor)
    stretchEvent (ERest d) = ERest (d * factor)

-- | Compress durations by factor (2 = twice as fast)
compress :: Int -> Music -> Music
compress factor = mapEvents compressEvent
  where
    compressEvent (ENote p v d) = ENote p v (d `div` factor)
    compressEvent (ERest d) = ERest (d `div` factor)

-- | Map a function over all events
mapEvents :: (Event -> Event) -> Music -> Music
mapEvents f (MEvent ev) = MEvent (f ev)
mapEvents f (MSeq ms) = MSeq [mapEvents f m | m <- ms]
mapEvents f (MPar ms) = MPar [mapEvents f m | m <- ms]

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

-- | Collect all events from music (flattening structure)
collectEvents :: Music -> [Event]
collectEvents (MEvent ev) = [ev]
collectEvents (MSeq ms) = concatMap collectEvents ms
collectEvents (MPar ms) = concatMap collectEvents ms

-- | Calculate total duration of music
duration :: Music -> Duration
duration (MEvent (ENote _ _ d)) = d
duration (MEvent (ERest d)) = d
duration (MSeq ms) = sum [duration m | m <- ms]
duration (MPar ms) = maximum (0 : [duration m | m <- ms])
