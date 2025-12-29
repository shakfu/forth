-- | ListPorts.hs - List available MIDI ports
module ListPorts(main) where

import MusicPerform

printPort :: Int -> IO ()
printPort idx = do
    name <- midiPortName idx
    putStrLn $ "  [" ++ show idx ++ "] " ++ name

printPorts :: Int -> Int -> IO ()
printPorts idx count
    | idx >= count = return ()
    | otherwise = do
        printPort idx
        printPorts (idx + 1) count

main :: IO ()
main = do
    putStrLn "MHS-MIDI: List MIDI Ports"
    putStrLn ""

    count <- midiListPorts

    if count == 0
        then putStrLn "No MIDI output ports found."
        else do
            putStrLn $ "Found " ++ show count ++ " MIDI output port(s):"
            printPorts 0 count

    putStrLn ""
    putStrLn "To use a port, call: midiOpen <port-index>"
    putStrLn "To create a virtual port: midiOpenVirtual \"PortName\""
