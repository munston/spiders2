module Main where

import LlamaBatch
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

main :: IO ()
main = do
    let modelPath = "C:/Users/User/AppData/Local/llama.cpp/hugging-quants_Llama-3.2-1B-Instruct-Q4_K_M-GGUF_llama-3.2-1b-instruct-q4_k_m.gguf"
    
    -- 1. Initialize from our new module
    (generator, fileWriter, cleanupAll) <- initLlamaSystem modelPath
    
    -- 2. User provides the data
    putStrLn "--- Define Burst Sequence ---"
    sequenceList <- collectSequence
    
    if null sequenceList 
        then putStrLn "Empty sequence."
        else do
            putStr "Enter filename for output: " >> hFlush stdout
            fname <- getLine
            
            putStrLn "\n--- Starting Generation ---"
            putStr "AI > " >> hFlush stdout
            
            -- 3. Call the high-level file writer
            fileWriter fname sequenceList
            
            putStrLn "\n--- Done: Result saved to file ---"
    
    cleanupAll

-- | Input Collection UI
collectSequence :: IO Control
collectSequence = do
    putStr "\nWord (Enter to finish): " >> hFlush stdout
    w <- getLine
    if null w then return [] else do
        c <- getValidInt
        rest <- collectSequence
        return ((w, c) : rest)

-- | Validation UI
getValidInt :: IO Int
getValidInt = do
    putStr "Tokens to generate: " >> hFlush stdout
    input <- getLine
    case readMaybe input of
        Just n -> return n
        Nothing -> putStrLn "Invalid number." >> getValidInt