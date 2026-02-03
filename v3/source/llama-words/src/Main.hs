module Main where

import Words (getCorpus, useCorpus, randomWord, randomInt)
import LlamaWords (runEntropySession)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)

main :: IO ()
main = do
    let model = "C:/Users/User/AppData/Local/llama.cpp/hugging-quants_Llama-3.2-1B-Instruct-Q4_K_M-GGUF_llama-3.2-1b-instruct-q4_k_m.gguf"
    
    putStr "Entropy Modulus (Burst Range): " >> hFlush stdout
    mBase <- getValidInt
    
    putStr "Quantity of words: " >> hFlush stdout
    nWords <- getValidInt
    
    putStr "Session Filename: " >> hFlush stdout
    fname <- getLine

    putStrLn "\n[Step 1] Sourcing High-Score Triggers..."
    corpus <- getCorpus
    let cl = length corpus
    
    -- Select triggers and display with a readable delay
    triggers <- replicateM nWords $ do
        i <- randomInt
        w <- randomWord i >>= useCorpus cl corpus
        putStrLn $ "Found: " ++ w
        threadDelay 250000 -- 0.25 second delay
        return w

    putStrLn "\n[Step 2] Executing Llama Batch Sequence..."
    -- Pass parameters to the bridge module
    runEntropySession fname model mBase nWords

    putStrLn "\nTask Complete."

getValidInt :: IO Int
getValidInt = do
    input <- getLine
    case readMaybe input of
        Just n -> return n
        Nothing -> putStr "Please enter a valid number: " >> hFlush stdout >> getValidInt