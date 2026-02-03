{-# Language FlexibleContexts #-}
-- | Words - Text scoring using hex-digit values.
--
-- Scoring: Each hex digit (0-9, a-f) contributes its value.
-- Non-hex characters contribute 0.
--
-- Examples:
--   score "abc"    = 10 + 11 + 12 = 33
--   score "hello"  = 14 + 14 = 28  (only 'e' twice scores)
--   score "123"    = 1 + 2 + 3 = 6
--   score "xyz"    = 0            (no hex digits)
module Words
    ( -- * Scoring
      score
    , scoreText
    , scoreDouble
      -- * Corpus operations
    , getCorpus
    , findBetter
    , generateBatch
      -- * Random utilities
    , randomWord
    , randomLetter
    ) where

import System.Random
import Data.Char
import Control.Monad (replicateM)
import Control.Exception (catch, SomeException)

-- | Score a string by summing hex digit values.
-- a=10, b=11, c=12, d=13, e=14, f=15, 0-9 as-is.
-- Non-hex characters contribute 0.
score :: String -> Int
score = sum . map hexValue
  where
    hexValue c
        | isHexDigit c = digitToInt c
        | otherwise    = 0

-- | Score text and return as Double (for comparison interface)
scoreDouble :: String -> Double
scoreDouble = fromIntegral . score

-- | Score text, normalizing by length to avoid bias toward long text
scoreText :: String -> Double
scoreText s
    | null s    = 0.0
    | otherwise = fromIntegral (score s) / fromIntegral (length s)

-- | Load word corpus from words.txt
-- Looks in current directory first, then tries common locations
getCorpus :: IO [String]
getCorpus = do
    -- Try current dir first, then source/words/
    let paths = ["words.txt", "../words/words.txt", "../../source/words/words.txt"]
    content <- tryPaths paths
    return $ words content
  where
    tryPaths [] = return ""  -- fallback: empty corpus
    tryPaths (p:ps) = do
        exists <- doesFileExist p
        if exists then readFile p else tryPaths ps

    doesFileExist :: FilePath -> IO Bool
    doesFileExist path = do
        result <- tryIO (readFile path >> return True)
        return $ either (const False) id result

    tryIO :: IO a -> IO (Either SomeException a)
    tryIO action = (Right <$> action) `catch` (\e -> return $ Left (e :: SomeException))

-- | Find a word from corpus with higher score than input.
-- Tries random words until it finds a better one.
findBetter :: [String] -> String -> IO String
findBetter corpus w = go 100  -- max attempts
  where
    targetScore = score w
    corpusLen = length corpus

    go :: Int -> IO String
    go 0 = return w  -- give up, return original
    go n = do
        i <- randomRIO (0, corpusLen - 1)
        let candidate = corpus !! i
        if score candidate > targetScore
            then return candidate
            else go (n - 1)

-- | Generate a batch of random words, improved via corpus
generateBatch :: Int -> IO [String]
generateBatch n = do
    corpus <- getCorpus
    replicateM n $ do
        seed <- randomWord
        findBetter corpus seed

-- | Generate a random word of random length (1-10 chars)
randomWord :: IO String
randomWord = do
    len <- randomRIO (1, 10)
    replicateM len randomLetter

-- | Generate a random lowercase letter
randomLetter :: IO Char
randomLetter = do
    i <- randomRIO (0, 25)
    return $ "abcdefghijklmnopqrstuvwxyz" !! i
