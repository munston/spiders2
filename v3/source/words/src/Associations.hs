-- | Word Association Corpus
--
-- Structure:
--   associations/
--     a/
--       apple.csv    -- word,weight pairs
--       art.csv
--     b/
--       blue.csv
--     ...
--
-- File format (CSV):
--   associated_word,weight
--   fruit,0.9
--   red,0.7
--
-- Query process:
--   1. Look up word file: associations/{first_letter}/{word}.csv
--   2. Load associations with weights
--   3. Shuffle against random seed
--   4. Use weights for weighted random selection
module Associations
    ( -- * Types
      Association(..)
    , AssociationCorpus
      -- * Loading
    , loadAssociations
    , loadCorpusWord
      -- * Querying
    , getAssociation
    , getWeightedAssociation
    , walkAssociations
      -- * Paths
    , associationPath
    , corpusRoot
    ) where

import System.Random
import System.Directory (doesFileExist)
import Data.Char (toLower)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Control.Exception (catch, SomeException)
import Text.Read (readMaybe)

-- | A single word association with weight
data Association = Association
    { assocWord   :: String
    , assocWeight :: Double
    } deriving (Show, Eq)

-- | Corpus is just a list of associations for a word
type AssociationCorpus = [Association]

-- | Root directory for associations
corpusRoot :: FilePath
corpusRoot = "associations"

-- | Path to association file for a word
-- e.g., "apple" -> "associations/a/apple.csv"
associationPath :: String -> FilePath
associationPath word
    | null word = corpusRoot ++ "/unknown.csv"
    | otherwise = corpusRoot ++ "/" ++ [toLower (head word)] ++ "/" ++ map toLower word ++ ".csv"

-- | Load associations from a CSV file
loadAssociations :: FilePath -> IO AssociationCorpus
loadAssociations path = do
    exists <- doesFileExist path
    if not exists
        then return []
        else do
            content <- readFileSafe path
            return $ parseCSV content

-- | Read file safely
readFileSafe :: FilePath -> IO String
readFileSafe path = readFile path `catch` handler
  where
    handler :: SomeException -> IO String
    handler _ = return ""

-- | Parse CSV content into associations
parseCSV :: String -> AssociationCorpus
parseCSV content = concatMap parseLine (lines content)
  where
    parseLine line = case break (== ',') line of
        (word, ',':rest) -> case readMaybe rest of
            Just weight -> [Association word weight]
            Nothing     -> []
        _ -> []

-- | Load associations for a specific word
loadCorpusWord :: String -> IO AssociationCorpus
loadCorpusWord word = loadAssociations (associationPath word)

-- | Get a random association (uniform)
getAssociation :: AssociationCorpus -> IO (Maybe String)
getAssociation [] = return Nothing
getAssociation assocs = do
    i <- randomRIO (0, length assocs - 1)
    return $ Just $ assocWord (assocs !! i)

-- | Get a weighted random association
-- Higher weights = more likely to be selected
getWeightedAssociation :: AssociationCorpus -> IO (Maybe String)
getWeightedAssociation [] = return Nothing
getWeightedAssociation assocs = do
    -- Normalize weights and create cumulative distribution
    let totalWeight = sum (map assocWeight assocs)
    if totalWeight <= 0
        then getAssociation assocs  -- fallback to uniform
        else do
            r <- randomRIO (0.0, totalWeight)
            return $ Just $ selectByWeight r assocs
  where
    selectByWeight _ [] = ""
    selectByWeight r (a:as)
        | r <= assocWeight a = assocWord a
        | otherwise = selectByWeight (r - assocWeight a) as

-- | Walk associations: start from a word, follow n associations
-- Returns the path taken
walkAssociations :: String -> Int -> IO [String]
walkAssociations startWord 0 = return [startWord]
walkAssociations startWord n = do
    assocs <- loadCorpusWord startWord
    next <- getWeightedAssociation assocs
    case next of
        Nothing -> return [startWord]  -- dead end
        Just nextWord -> do
            rest <- walkAssociations nextWord (n - 1)
            return (startWord : rest)
