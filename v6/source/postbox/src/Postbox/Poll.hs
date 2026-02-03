-- | Poll - broadcast messages and score responses.
-- Sends a message to all active postboxes and collects/scores responses.
--
-- The scoring function is applied locally when responses arrive,
-- so it never needs to be transmitted in the message itself.
module Postbox.Poll
    ( -- * Types
      PollResult(..)
    , ScoreFunction
      -- * Polling
    , poll
    , pollWithTimeout
      -- * Default scoring
    , scoreByLength
    , scoreByWordCount
    ) where

import Control.Concurrent (threadDelay, forkIO, newMVar, readMVar, modifyMVar_)
import Control.Monad (forM_, when)
import System.Directory (listDirectory, doesFileExist, removeFile)
import System.FilePath ((</>), takeFileName)
import Data.List (sortBy, isPrefixOf)
import Data.Ord (comparing, Down(..))

-- | Scoring function: takes response content, returns a score
type ScoreFunction = String -> Double

-- | Result of polling a single agent
data PollResult = PollResult
    { pollAgent    :: String   -- ^ Name of the responding agent
    , pollResponse :: String   -- ^ The response content
    , pollScore    :: Double   -- ^ Score assigned by scoring function
    } deriving (Show, Eq)

-- | Score by length (longer = higher score)
scoreByLength :: ScoreFunction
scoreByLength = fromIntegral . length

-- | Score by word count
scoreByWordCount :: ScoreFunction
scoreByWordCount = fromIntegral . length . words

-- | Poll all active postboxes and score responses.
-- This is a simplified version that:
-- 1. Sends a request to all active postboxes via sendToAll
-- 2. Waits for responses in our inbox
-- 3. Scores each response
-- 4. Returns sorted results (highest score first)
--
-- Note: This requires the caller to have already sent the broadcast.
-- This function just collects and scores responses from inbox.
poll :: FilePath          -- ^ Our inbox path
     -> String            -- ^ Response filename prefix to look for
     -> ScoreFunction     -- ^ Scoring function
     -> IO [PollResult]
poll inboxDir responsePrefix scorer = do
    files <- listDirectory inboxDir
    let responseFiles = filter (isPrefixOf responsePrefix) files
    results <- mapM (scoreFile inboxDir scorer) responseFiles
    return $ sortBy (comparing (Down . pollScore)) results

-- | Poll with a timeout, collecting responses as they arrive.
-- Checks for responses every interval until timeout is reached.
pollWithTimeout :: FilePath        -- ^ Our inbox path
                -> String          -- ^ Response filename prefix
                -> ScoreFunction   -- ^ Scoring function
                -> Int             -- ^ Timeout in seconds
                -> Int             -- ^ Check interval in milliseconds
                -> IO [PollResult]
pollWithTimeout inboxDir responsePrefix scorer timeoutSecs intervalMs = do
    resultsVar <- newMVar []
    let iterations = (timeoutSecs * 1000) `div` intervalMs

    collectLoop resultsVar iterations
    readMVar resultsVar
  where
    collectLoop resultsVar remaining
        | remaining <= 0 = return ()
        | otherwise = do
            newResults <- poll inboxDir responsePrefix scorer
            modifyMVar_ resultsVar $ \existing -> do
                let existingAgents = map pollAgent existing
                let fresh = filter (\r -> pollAgent r `notElem` existingAgents) newResults
                return (existing ++ fresh)
            threadDelay (intervalMs * 1000)
            collectLoop resultsVar (remaining - 1)

-- | Score a single response file
scoreFile :: FilePath -> ScoreFunction -> FilePath -> IO PollResult
scoreFile inboxDir scorer filename = do
    let path = inboxDir </> filename
    content <- readFile path
    let agent = extractAgent filename
    let score = scorer content
    return PollResult
        { pollAgent    = agent
        , pollResponse = content
        , pollScore    = score
        }

-- | Extract agent name from response filename.
-- "response-to-me-from-agent-1.txt" -> "agent-1"
extractAgent :: FilePath -> String
extractAgent f =
    let base = takeWhileNot (== '.') f
        parts = splitOn "-from-" base
    in case parts of
        [_, agent] -> agent
        _          -> base
  where
    takeWhileNot p = takeWhile (not . p)

    splitOn :: String -> String -> [String]
    splitOn sep str = go str
      where
        sepLen = length sep
        go s | null s = []
             | otherwise = case findSubstring sep s of
                 Nothing -> [s]
                 Just i  -> take i s : go (drop (i + sepLen) s)

        findSubstring :: String -> String -> Maybe Int
        findSubstring needle haystack = go' 0 haystack
          where
            needleLen = length needle
            go' _ [] = Nothing
            go' i s@(_:rest)
                | take needleLen s == needle = Just i
                | otherwise = go' (i + 1) rest
