-- | Postbox.Compare - Comparison of multiple message responses with scoring.
--
-- This module provides functionality to:
--   1. Send multiple messages (submessages) to be processed
--   2. Collect responses for each
--   3. Score each response using a scoring function
--   4. Return ranked results with (score, response) pairs
--
-- Two modes of operation:
--   1. LLM-scored: Responses come from LLM, scored by provided function
--   2. Local-scored: Responses scored locally (e.g., by length)
--
-- Response format (as text in postbox message):
--   "SCORE:123.45\nRESPONSE:The actual response text here"
module Postbox.Compare
    ( -- * Types
      CompareRequest(..)
    , CompareResult(..)
    , ScoreFunc
      -- * Scoring functions
    , scoreByLength
    , scoreByWordCount
    , scoreByHex
    , scoreByHexNormalized
      -- * Formatting
    , formatResult
    , parseResult
    , formatResults
    , parseResults
      -- * Comparison operations
    , compareMessages
    , rankResults
    ) where

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Text.Read (readMaybe)
import qualified Words

-- | Scoring function type: takes response text, returns numeric score
type ScoreFunc = String -> Double

-- | A request to compare multiple messages
data CompareRequest = CompareRequest
    { compareMessages' :: [String]   -- ^ List of messages to compare
    , compareModality  :: String     -- ^ Modality to use for processing
    } deriving (Show, Eq)

-- | Result of processing and scoring a single message
data CompareResult = CompareResult
    { resultScore    :: Double   -- ^ Numeric score
    , resultMessage  :: String   -- ^ Original message sent
    , resultResponse :: String   -- ^ Response received
    } deriving (Show, Eq)

-- | Score by response length (default scorer)
scoreByLength :: ScoreFunc
scoreByLength = fromIntegral . length

-- | Score by word count
scoreByWordCount :: ScoreFunc
scoreByWordCount = fromIntegral . length . words

-- | Score by hex digit sum (a=10, b=11, ..., f=15, 0-9 as-is)
-- Good for cheap local scoring without LLM credits
scoreByHex :: ScoreFunc
scoreByHex = Words.scoreDouble

-- | Score by hex digit sum, normalized by length
-- Avoids bias toward longer responses
scoreByHexNormalized :: ScoreFunc
scoreByHexNormalized = Words.scoreText

-- | Format a single result for transmission as postbox message
-- Format: "SCORE:123.45\nMESSAGE:original\nRESPONSE:response text"
formatResult :: CompareResult -> String
formatResult r = unlines
    [ "SCORE:" ++ show (resultScore r)
    , "MESSAGE:" ++ resultMessage r
    , "RESPONSE:" ++ resultResponse r
    ]

-- | Parse a formatted result back into CompareResult
parseResult :: String -> Maybe CompareResult
parseResult s = do
    let lns = lines s
    scoreLine <- findLine "SCORE:" lns
    msgLine   <- findLine "MESSAGE:" lns
    respLine  <- findLine "RESPONSE:" lns
    score     <- readMaybe scoreLine
    return CompareResult
        { resultScore    = score
        , resultMessage  = msgLine
        , resultResponse = respLine
        }
  where
    findLine prefix = lookup prefix . map splitLine
    splitLine ln = case break (== ':') ln of
        (key, ':':val) -> (key ++ ":", val)
        _              -> (ln, "")

-- | Format multiple results for transmission
-- Separated by "---" lines
formatResults :: [CompareResult] -> String
formatResults rs = unlines $ concatMap fmt rs
  where
    fmt r = [formatResult r, "---"]

-- | Parse multiple results
parseResults :: String -> [CompareResult]
parseResults s =
    let blocks = splitOn "---" s
    in  concatMap (maybe [] pure . parseResult) blocks
  where
    splitOn sep str = go str
      where
        go "" = []
        go ss = case breakOn sep ss of
            (before, "") -> [before]
            (before, after) -> before : go (drop (length sep) after)

    breakOn needle haystack = go "" haystack
      where
        needleLen = length needle
        go acc [] = (reverse acc, "")
        go acc s'@(c:cs)
            | take needleLen s' == needle = (reverse acc, s')
            | otherwise = go (c:acc) cs

-- | Process messages and score responses
-- Takes a processing function that handles each message
compareMessages :: (String -> IO String)  -- ^ Process function (e.g., LLM call)
                -> ScoreFunc              -- ^ Scoring function
                -> [String]               -- ^ Messages to compare
                -> IO [CompareResult]
compareMessages process scorer msgs = do
    results <- mapM processOne msgs
    return $ rankResults results
  where
    processOne msg = do
        response <- process msg
        let score = scorer response
        return CompareResult
            { resultScore    = score
            , resultMessage  = msg
            , resultResponse = response
            }

-- | Rank results by score (highest first)
rankResults :: [CompareResult] -> [CompareResult]
rankResults = sortBy (comparing (Down . resultScore))
