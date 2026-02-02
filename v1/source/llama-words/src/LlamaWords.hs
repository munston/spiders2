module LlamaWords (runEntropySession, buildSequence) where

import Words (getCorpus, useCorpus, score, randomWord, randomInt)
import LlamaBatch (initLlamaSystem, FileWriterFunc)
import System.IO (hFlush, stdout)
import Control.Monad (replicateM)

-- | High-Entropy Logic: 
-- | The score of word[i] determines the token count of word[i+1]
buildSequence :: Int -> [String] -> [(String, Int)]
buildSequence _ [] = []
buildSequence modBase triggers = 
    -- Seed with 7 tokens for the first word
    zip triggers (7 : map (\w -> (score w `mod` modBase) + 1) (init triggers))

-- | This is what you will call from GHCi
runEntropySession :: String -> FilePath -> Int -> Int -> IO ()
runEntropySession filename modelPath modBase numWords = do
    putStrLn "--- Initializing AI Engine ---"
    (generator, fileWriter, cleanup) <- initLlamaSystem modelPath
    
    putStrLn "--- Sourcing Triggers from Corpus ---"
    corpus <- getCorpus
    let cl = length corpus
    
    -- Generate the raw word list
    rawTriggers <- replicateM numWords $ do
        i <- randomInt
        randomWord i >>= useCorpus cl corpus
        
    -- Apply the entropy logic to create the [(String, Int)] list
    let sequence = buildSequence modBase rawTriggers
    
    putStrLn $ "Processing " ++ show numWords ++ " words..."
    fileWriter filename sequence
    
    putStrLn "--- Session Complete. Cleaning up... ---"
    cleanup