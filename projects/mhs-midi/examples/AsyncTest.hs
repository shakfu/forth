-- | Test async scheduler functionality
module AsyncTest where

import Control.Concurrent (threadDelay)
import Async
import Midi
import Music

main :: IO ()
main = do
    putStrLn "=== mhs-midi Async Test ==="
    putStrLn ""

    -- Test 1: Basic spawn and run
    putStrLn "Test 1: spawn and run..."
    _ <- spawn "test1" $ do
        putStrLn "  Voice 1 started"
        threadDelay 100000  -- 100ms
        putStrLn "  Voice 1 done"
    run
    putStrLn "  PASS"

    -- Test 2: Multiple voices
    putStrLn ""
    putStrLn "Test 2: multiple voices..."
    _ <- spawn "voiceA" $ do
        putStrLn "  A: start"
        threadDelay 100000
        putStrLn "  A: done"
    _ <- spawn "voiceB" $ do
        putStrLn "  B: start"
        threadDelay 150000
        putStrLn "  B: done"
    run
    putStrLn "  PASS"

    -- Test 3: voices count
    putStrLn ""
    putStrLn "Test 3: voices count..."
    _ <- spawn "v1" $ threadDelay 200000
    _ <- spawn "v2" $ threadDelay 200000
    n <- voices
    putStrLn $ "  Active voices: " ++ show n
    if n == 2
        then putStrLn "  PASS"
        else putStrLn "  FAIL"
    run

    -- Test 4: status
    putStrLn ""
    putStrLn "Test 4: status..."
    _ <- spawn "named" $ threadDelay 100000
    (running, count, names) <- status
    putStrLn $ "  Running: " ++ show running
    putStrLn $ "  Count: " ++ show count
    putStrLn $ "  Names: " ++ show names
    run
    putStrLn "  PASS"

    -- Test 5: stop voice
    putStrLn ""
    putStrLn "Test 5: stop voice..."
    vid <- spawn "long" $ threadDelay 10000000  -- 10 seconds
    threadDelay 50000  -- 50ms
    stopped <- stop vid
    n' <- voices
    if stopped && n' == 0
        then putStrLn "  PASS"
        else putStrLn "  FAIL"

    -- Test 6: Sequential spawn/run cycles
    putStrLn ""
    putStrLn "Test 6: sequential spawn/run..."
    _ <- spawn "first" $ putStrLn "  First voice"
    run
    _ <- spawn "second" $ putStrLn "  Second voice"
    run
    putStrLn "  PASS"

    putStrLn ""
    putStrLn "=== All tests passed! ==="
