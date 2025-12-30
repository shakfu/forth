-- | Unit tests for Music.hs
--
-- Run with: mhs-midi -C -i<path> TestMusic
--
module TestMusic(main) where

import Music

-- Simple test framework
data TestResult = Pass String | Fail String String

runTest :: String -> Bool -> TestResult
runTest name True  = Pass name
runTest name False = Fail name "assertion failed"

runTestEq :: (Eq a, Show a) => String -> a -> a -> TestResult
runTestEq name expected actual
    | expected == actual = Pass name
    | otherwise = Fail name ("expected " ++ show expected ++ " but got " ++ show actual)

printResult :: TestResult -> IO ()
printResult (Pass name) = putStrLn $ "  PASS: " ++ name
printResult (Fail name msg) = putStrLn $ "  FAIL: " ++ name ++ " - " ++ msg

isPassing :: TestResult -> Bool
isPassing (Pass _) = True
isPassing (Fail _ _) = False

-----------------------------------------------------------
-- Pitch Constants Tests
-----------------------------------------------------------

testPitchConstants :: [TestResult]
testPitchConstants =
    [ runTestEq "c4 = 60 (middle C)" 60 c4
    , runTestEq "d4 = 62" 62 d4
    , runTestEq "e4 = 64" 64 e4
    , runTestEq "f4 = 65" 65 f4
    , runTestEq "g4 = 67" 67 g4
    , runTestEq "a4 = 69" 69 a4
    , runTestEq "b4 = 71" 71 b4
    , runTestEq "c5 = 72" 72 c5
    , runTestEq "c0 = 12" 12 c0
    , runTestEq "c3 = 48" 48 c3
    , runTestEq "c7 = 96" 96 c7
    , runTestEq "cs4 = 61 (C#4)" 61 cs4
    , runTestEq "ds4 = 63 (D#4)" 63 ds4
    , runTestEq "fs4 = 66 (F#4)" 66 fs4
    , runTestEq "gs4 = 68 (G#4)" 68 gs4
    , runTestEq "as4 = 70 (A#4)" 70 as4
    -- Flat aliases
    , runTestEq "db = cs (enharmonic)" db cs
    , runTestEq "eb = ds (enharmonic)" eb ds
    , runTestEq "gb = fs (enharmonic)" gb fs
    , runTestEq "ab = gs (enharmonic)" ab gs
    , runTestEq "bb = as (enharmonic)" bb as
    -- Pitch class
    , runTestEq "c = 0 (pitch class)" 0 c
    , runTestEq "cs = 1 (pitch class)" 1 cs
    , runTestEq "d = 2 (pitch class)" 2 d
    , runTestEq "b = 11 (pitch class)" 11 b
    ]

-----------------------------------------------------------
-- Duration Constants Tests
-----------------------------------------------------------

testDurationConstants :: [TestResult]
testDurationConstants =
    [ runTestEq "whole = 2000ms" 2000 whole
    , runTestEq "half = 1000ms" 1000 half
    , runTestEq "quarter = 500ms" 500 quarter
    , runTestEq "eighth = 250ms" 250 eighth
    , runTestEq "sixteenth = 125ms" 125 sixteenth
    , runTestEq "dotted quarter = 750ms" 750 (dotted quarter)
    , runTestEq "dotted half = 1500ms" 1500 (dotted half)
    , runTestEq "bpm 120 = 500ms" 500 (bpm 120)
    , runTestEq "bpm 60 = 1000ms" 1000 (bpm 60)
    , runTestEq "bpm 240 = 250ms" 250 (bpm 240)
    ]

-----------------------------------------------------------
-- Velocity Constants Tests
-----------------------------------------------------------

testVelocityConstants :: [TestResult]
testVelocityConstants =
    [ runTestEq "ppp = 16" 16 ppp
    , runTestEq "pp = 33" 33 pp
    , runTestEq "p = 49" 49 p
    , runTestEq "mp = 64" 64 mp
    , runTestEq "mf = 80" 80 mf
    , runTestEq "ff = 96" 96 ff
    , runTestEq "fff = 112" 112 fff
    , runTest "ppp < pp" (ppp < pp)
    , runTest "pp < p" (pp < p)
    , runTest "p < mp" (p < mp)
    , runTest "mp < mf" (mp < mf)
    , runTest "mf < ff" (mf < ff)
    , runTest "ff < fff" (ff < fff)
    ]

-----------------------------------------------------------
-- Scale Building Tests
-----------------------------------------------------------

testScaleBuilding :: [TestResult]
testScaleBuilding =
    [ runTestEq "C major scale"
        [60, 62, 64, 65, 67, 69, 71]
        (buildScale c4 scaleMajor)
    , runTestEq "C minor scale"
        [60, 62, 63, 65, 67, 68, 70]
        (buildScale c4 scaleMinor)
    , runTestEq "C pentatonic"
        [60, 62, 64, 67, 69]
        (buildScale c4 scalePentatonic)
    , runTestEq "C blues"
        [60, 63, 65, 66, 67, 70]
        (buildScale c4 scaleBlues)
    , runTestEq "C chromatic length"
        12 (length (buildScale c4 scaleChromatic))
    , runTestEq "G major scale"
        [67, 69, 71, 72, 74, 76, 78]
        (buildScale g4 scaleMajor)
    -- Scale degree tests
    , runTestEq "scaleDegree 1 of C major = C"
        c4 (scaleDegree c4 scaleMajor 1)
    , runTestEq "scaleDegree 3 of C major = E"
        e4 (scaleDegree c4 scaleMajor 3)
    , runTestEq "scaleDegree 5 of C major = G"
        g4 (scaleDegree c4 scaleMajor 5)
    , runTestEq "scaleDegree 8 of C major = C5 (octave)"
        c5 (scaleDegree c4 scaleMajor 8)
    , runTestEq "scaleDegree 9 of C major = D5"
        d5 (scaleDegree c4 scaleMajor 9)
    -- inScale tests
    , runTest "C4 in C major" (inScale c4 c4 scaleMajor)
    , runTest "E4 in C major" (inScale e4 c4 scaleMajor)
    , runTest "G4 in C major" (inScale g4 c4 scaleMajor)
    , runTest "C#4 not in C major" (not (inScale cs4 c4 scaleMajor))
    , runTest "F#4 not in C major" (not (inScale fs4 c4 scaleMajor))
    -- quantize tests
    , runTestEq "quantize C4 to C major = C4"
        c4 (quantize c4 c4 scaleMajor)
    , runTest "quantize C#4 to C major (in scale)"
        (inScale (quantize cs4 c4 scaleMajor) c4 scaleMajor)  -- should snap to nearest scale tone
    , runTestEq "quantize F#4 to C major = G4"
        g4 (quantize fs4 c4 scaleMajor)  -- F#4 is closer to G4 (1 semitone) than F4 (1 semitone) - but test shows behavior
    ]

-----------------------------------------------------------
-- Music DSL Constructor Tests
-----------------------------------------------------------

testMusicConstructors :: [TestResult]
testMusicConstructors =
    [ runTestEq "note creates MEvent ENote"
        (MEvent (ENote c4 mf quarter))
        (note c4 mf quarter)
    , runTestEq "rest creates MEvent ERest"
        (MEvent (ERest quarter))
        (rest quarter)
    , runTestEq "chord creates MPar"
        (MPar [MEvent (ENote c4 mf quarter),
               MEvent (ENote e4 mf quarter),
               MEvent (ENote g4 mf quarter)])
        (chord [c4, e4, g4] mf quarter)
    , runTestEq "line creates MSeq"
        (MSeq [MEvent (ENote c4 mf quarter),
               MEvent (ENote e4 mf quarter),
               MEvent (ENote g4 mf quarter)])
        (line [c4, e4, g4] mf quarter)
    , runTestEq "empty chord"
        (MPar [])
        (chord [] mf quarter)
    , runTestEq "empty line"
        (MSeq [])
        (line [] mf quarter)
    ]

-----------------------------------------------------------
-- Music Combinator Tests
-----------------------------------------------------------

testMusicCombinators :: [TestResult]
testMusicCombinators =
    [ runTestEq "sequential composition (+:+)"
        (MSeq [note c4 mf quarter, note e4 mf quarter])
        (note c4 mf quarter +:+ note e4 mf quarter)
    , runTestEq "parallel composition (|||)"
        (MPar [note c4 mf quarter, note e4 mf quarter])
        (note c4 mf quarter ||| note e4 mf quarter)
    , runTestEq "timesM 3"
        (MSeq [note c4 mf quarter, note c4 mf quarter, note c4 mf quarter])
        (timesM 3 (note c4 mf quarter))
    , runTestEq "timesM 1"
        (MSeq [note c4 mf quarter])
        (timesM 1 (note c4 mf quarter))
    , runTestEq "timesM 0"
        (MSeq [])
        (timesM 0 (note c4 mf quarter))
    ]

-----------------------------------------------------------
-- Music Transformation Tests
-----------------------------------------------------------

testMusicTransformations :: [TestResult]
testMusicTransformations =
    [ -- transpose tests
      runTestEq "transpose +12 = octave up"
        [ENote c5 mf quarter]
        (collectEvents (transpose 12 (note c4 mf quarter)))
    , runTestEq "transpose -12 = octave down"
        [ENote c3 mf quarter]
        (collectEvents (transpose (-12) (note c4 mf quarter)))
    , runTestEq "transpose 0 = identity"
        (collectEvents (note c4 mf quarter))
        (collectEvents (transpose 0 (note c4 mf quarter)))
    , runTestEq "transpose preserves rest"
        [ERest quarter]
        (collectEvents (transpose 12 (rest quarter)))
    -- louder tests
    , runTestEq "louder +10"
        [ENote c4 90 quarter]
        (collectEvents (louder 10 (note c4 mf quarter)))
    , runTestEq "louder clamps at 127"
        [ENote c4 127 quarter]
        (collectEvents (louder 100 (note c4 mf quarter)))
    -- softer tests
    , runTestEq "softer -10"
        [ENote c4 70 quarter]
        (collectEvents (softer 10 (note c4 mf quarter)))
    , runTestEq "softer clamps at 0"
        [ENote c4 0 quarter]
        (collectEvents (softer 200 (note c4 mf quarter)))
    -- stretch tests
    , runTestEq "stretch 2 doubles duration"
        [ENote c4 mf 1000]
        (collectEvents (stretch 2 (note c4 mf quarter)))
    , runTestEq "stretch preserves rest"
        [ERest 1000]
        (collectEvents (stretch 2 (rest quarter)))
    -- compress tests
    , runTestEq "compress 2 halves duration"
        [ENote c4 mf 250]
        (collectEvents (compress 2 (note c4 mf quarter)))
    ]

-----------------------------------------------------------
-- Duration Calculation Tests
-----------------------------------------------------------

testDurationCalc :: [TestResult]
testDurationCalc =
    [ runTestEq "duration of single note"
        quarter (duration (note c4 mf quarter))
    , runTestEq "duration of rest"
        quarter (duration (rest quarter))
    , runTestEq "duration of sequence"
        1500 (duration (line [c4, e4, g4] mf quarter))
    , runTestEq "duration of chord"
        quarter (duration (chord [c4, e4, g4] mf quarter))
    , runTestEq "duration of nested seq"
        1000 (duration (note c4 mf quarter +:+ note e4 mf quarter))
    , runTestEq "duration of parallel = max"
        quarter (duration (note c4 mf quarter ||| note e4 mf eighth))
    ]

-----------------------------------------------------------
-- collectEvents Tests
-----------------------------------------------------------

testCollectEvents :: [TestResult]
testCollectEvents =
    [ runTestEq "collectEvents single note"
        [ENote c4 mf quarter]
        (collectEvents (note c4 mf quarter))
    , runTestEq "collectEvents sequence"
        [ENote c4 mf quarter, ENote e4 mf quarter]
        (collectEvents (line [c4, e4] mf quarter))
    , runTestEq "collectEvents chord"
        [ENote c4 mf quarter, ENote e4 mf quarter]
        (collectEvents (chord [c4, e4] mf quarter))
    , runTestEq "collectEvents count"
        3 (length (collectEvents (line [c4, e4, g4] mf quarter)))
    ]

-----------------------------------------------------------
-- PRNG Tests
-----------------------------------------------------------

testPRNG :: [TestResult]
testPRNG =
    [ runTest "nextRandom is deterministic"
        (fst (nextRandom 42) == fst (nextRandom 42))

    , runTestEq "nextRandom seed 0"
        0 (fst (nextRandom 0))

    , runTestEq "nextRandom seed 42"
        19081 (fst (nextRandom 42))

    , runTest "nextRandom produces different values for different seeds"
        (fst (nextRandom 1) /= fst (nextRandom 2))

    , runTestEq "randomRange lo=hi returns lo"
        5 (fst (randomRange 42 5 5))

    , runTest "randomRange stays in bounds"
        (let (r, _) = randomRange 42 0 10 in r >= 0 && r <= 10)

    , runTest "randomRange multiple calls stay in bounds"
        (all (\s -> let (r, _) = randomRange s 0 10 in r >= 0 && r <= 10) [1..100])

    , runTestEq "randomList length"
        5 (length (fst (randomList 42 5 0 10)))

    , runTest "randomList all in bounds"
        (all (\x -> x >= 0 && x <= 100) (fst (randomList 42 10 0 100)))
    ]

-----------------------------------------------------------
-- Euclidean Rhythm Tests
-----------------------------------------------------------

testEuclidean :: [TestResult]
testEuclidean =
    [ runTestEq "euclideanRhythm 0 8 (no hits)"
        [False, False, False, False, False, False, False, False]
        (euclideanRhythm 0 8)

    , runTestEq "euclideanRhythm 8 8 (all hits)"
        [True, True, True, True, True, True, True, True]
        (euclideanRhythm 8 8)

    , runTestEq "euclideanRhythm 1 4"
        [True, False, False, False]
        (euclideanRhythm 1 4)

    , runTestEq "euclideanRhythm 2 4"
        [True, False, True, False]
        (euclideanRhythm 2 4)

    , runTestEq "euclideanRhythm 3 8 (tresillo)"
        [True, False, False, True, False, False, True, False]
        (euclideanRhythm 3 8)

    , runTestEq "euclideanRhythm 5 8 (cinquillo)"
        [True, False, True, True, False, True, True, False]
        (euclideanRhythm 5 8)

    , runTestEq "euclideanRhythm hit count"
        3 (length (filter id (euclideanRhythm 3 8)))

    , runTestEq "euclideanRhythm total length"
        8 (length (euclideanRhythm 3 8))
    ]

-----------------------------------------------------------
-- Arpeggio Pattern Tests
-----------------------------------------------------------

testArpeggio :: [TestResult]
testArpeggio =
    [ runTestEq "arpUp identity"
        [1, 2, 3, 4] (arpUp [1, 2, 3, 4])

    , runTestEq "arpDown reverse"
        [4, 3, 2, 1] (arpDown [1, 2, 3, 4])

    , runTestEq "arpUpDown"
        [1, 2, 3, 4, 3, 2, 1] (arpUpDown [1, 2, 3, 4])

    , runTestEq "arpUpDown singleton"
        [1] (arpUpDown [1])

    , runTestEq "arpUpDown empty"
        [] (arpUpDown ([] :: [Int]))

    , runTestEq "arpUpDown two elements"
        [1, 2, 1] (arpUpDown [1, 2])
    ]

-----------------------------------------------------------
-- Retrograde Tests
-----------------------------------------------------------

testRetrograde :: [TestResult]
testRetrograde =
    [ runTest "retrograde reverses sequence"
        (collectEvents (retrograde (line [c4, e4, g4] mf quarter))
         == collectEvents (line [g4, e4, c4] mf quarter))

    , runTest "retrograde of retrograde is identity"
        (collectEvents (retrograde (retrograde (line [c4, e4, g4] mf quarter)))
         == collectEvents (line [c4, e4, g4] mf quarter))

    , runTest "retrograde preserves parallel"
        (duration (retrograde (chord [c4, e4, g4] mf quarter))
         == duration (chord [c4, e4, g4] mf quarter))
    ]

-----------------------------------------------------------
-- Invert Tests
-----------------------------------------------------------

testInvert :: [TestResult]
testInvert =
    [ runTestEq "invert c4 around c4 is c4"
        [ENote c4 mf quarter]
        (collectEvents (invert c4 (note c4 mf quarter)))

    , runTestEq "invert e4 around c4"
        [ENote 56 mf quarter]  -- c4 (60) - (e4 (64) - c4 (60)) = 56
        (collectEvents (invert c4 (note e4 mf quarter)))

    , runTestEq "invert g4 around c4"
        [ENote 53 mf quarter]  -- c4 (60) - (g4 (67) - c4 (60)) = 53
        (collectEvents (invert c4 (note g4 mf quarter)))

    , runTest "double inversion is identity"
        (collectEvents (invert c4 (invert c4 (note e4 mf quarter)))
         == collectEvents (note e4 mf quarter))
    ]

-----------------------------------------------------------
-- Shuffle Tests
-----------------------------------------------------------

testShuffle :: [TestResult]
testShuffle =
    [ runTestEq "shuffle preserves length"
        4 (length (shuffle 42 [1, 2, 3, 4]))

    , runTest "shuffle is deterministic"
        (shuffle 42 [1, 2, 3, 4] == shuffle 42 [1, 2, 3, 4])

    , runTest "different seeds produce different results"
        (shuffle 1 [1, 2, 3, 4, 5, 6, 7, 8] /= shuffle 2 [1, 2, 3, 4, 5, 6, 7, 8])

    , runTestEq "shuffle empty"
        [] (shuffle 42 ([] :: [Int]))

    , runTestEq "shuffle singleton"
        [1] (shuffle 42 [1])

    , runTest "shuffle contains all elements"
        (let xs = [1..10]; ys = shuffle 42 xs in all (`elem` ys) xs)
    ]

-----------------------------------------------------------
-- Pick Tests
-----------------------------------------------------------

testPick :: [TestResult]
testPick =
    [ runTest "pick returns element from list"
        (pick 42 [1, 2, 3, 4, 5] `elem` [1, 2, 3, 4, 5])

    , runTest "pick is deterministic"
        (pick 42 [1, 2, 3, 4, 5] == pick 42 [1, 2, 3, 4, 5])

    , runTestEq "pickN returns correct count"
        5 (length (pickN 42 5 [1, 2, 3]))

    , runTest "pickN all elements from list"
        (all (`elem` [1, 2, 3]) (pickN 42 10 [1, 2, 3]))
    ]

-----------------------------------------------------------
-- Random Walk Tests
-----------------------------------------------------------

testRandomWalk :: [TestResult]
testRandomWalk =
    [ runTestEq "randomWalk returns correct count"
        8 (length (randomWalk 42 c4 3 8))

    , runTest "randomWalk starts at start pitch"
        (head (randomWalk 42 c4 3 8) == c4)

    , runTest "randomWalk is deterministic"
        (randomWalk 42 c4 3 8 == randomWalk 42 c4 3 8)

    , runTest "randomWalk stays in MIDI range"
        (all (\p -> p >= 0 && p <= 127) (randomWalk 42 c4 12 100))

    , runTestEq "randomWalk with 0 count"
        [] (randomWalk 42 c4 3 0)
    ]

-----------------------------------------------------------
-- Drunk Walk Tests
-----------------------------------------------------------

testDrunkWalk :: [TestResult]
testDrunkWalk =
    [ let scale = buildScale c4 scaleMajor
      in runTestEq "drunkWalk returns correct count"
            8 (length (drunkWalk 42 c4 scale 2 8))

    , let scale = buildScale c4 scaleMajor
      in runTest "drunkWalk stays on scale"
            (all (`elem` scale) (drunkWalk 42 c4 scale 2 16))

    , let scale = buildScale c4 scaleMajor
      in runTest "drunkWalk is deterministic"
            (drunkWalk 42 c4 scale 2 8 == drunkWalk 42 c4 scale 2 8)

    , runTestEq "drunkWalk with empty scale"
        [] (drunkWalk 42 c4 [] 2 8)

    , runTestEq "drunkWalk with 0 count"
        [] (drunkWalk 42 c4 [c4, e4, g4] 2 0)
    ]

-----------------------------------------------------------
-- Main
-----------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== Music.hs Test Suite ==="
    putStrLn ""

    putStrLn "Pitch Constants:"
    mapM_ printResult testPitchConstants

    putStrLn ""
    putStrLn "Duration Constants:"
    mapM_ printResult testDurationConstants

    putStrLn ""
    putStrLn "Velocity Constants:"
    mapM_ printResult testVelocityConstants

    putStrLn ""
    putStrLn "Scale Building:"
    mapM_ printResult testScaleBuilding

    putStrLn ""
    putStrLn "Music Constructors:"
    mapM_ printResult testMusicConstructors

    putStrLn ""
    putStrLn "Music Combinators:"
    mapM_ printResult testMusicCombinators

    putStrLn ""
    putStrLn "Music Transformations:"
    mapM_ printResult testMusicTransformations

    putStrLn ""
    putStrLn "Duration Calculation:"
    mapM_ printResult testDurationCalc

    putStrLn ""
    putStrLn "collectEvents:"
    mapM_ printResult testCollectEvents

    putStrLn ""
    putStrLn "PRNG:"
    mapM_ printResult testPRNG

    putStrLn ""
    putStrLn "Euclidean Rhythm:"
    mapM_ printResult testEuclidean

    putStrLn ""
    putStrLn "Arpeggio Patterns:"
    mapM_ printResult testArpeggio

    putStrLn ""
    putStrLn "Retrograde:"
    mapM_ printResult testRetrograde

    putStrLn ""
    putStrLn "Invert:"
    mapM_ printResult testInvert

    putStrLn ""
    putStrLn "Shuffle:"
    mapM_ printResult testShuffle

    putStrLn ""
    putStrLn "Pick:"
    mapM_ printResult testPick

    putStrLn ""
    putStrLn "Random Walk:"
    mapM_ printResult testRandomWalk

    putStrLn ""
    putStrLn "Drunk Walk:"
    mapM_ printResult testDrunkWalk

    putStrLn ""
    let allTests = testPitchConstants ++ testDurationConstants ++ testVelocityConstants
                   ++ testScaleBuilding ++ testMusicConstructors ++ testMusicCombinators
                   ++ testMusicTransformations ++ testDurationCalc ++ testCollectEvents
                   ++ testPRNG ++ testEuclidean ++ testArpeggio ++ testRetrograde
                   ++ testInvert ++ testShuffle ++ testPick ++ testRandomWalk
                   ++ testDrunkWalk
        passed = length (filter isPassing allTests)
        total = length allTests
    putStrLn $ "=== Results: " ++ show passed ++ "/" ++ show total ++ " passed ==="

    if passed == total
        then putStrLn "ALL TESTS PASSED"
        else putStrLn "SOME TESTS FAILED"
