module Spider.Pipeline
    ( Provider(..)
    , textToUrl
    , runPipeline
    , runRawQuery
    , processFile
    ) where

import LLM.Types (Message(..))
import qualified Groq.Client as Groq
import qualified HuggingFace.Client as HF
import qualified Scraper.Client as Scraper
import Postbox (Postbox, readMessage, writeMessage, deleteMessage)

-- | Which LLM provider to use.
data Provider = Groq | HuggingFace
    deriving (Show, Eq)

-- | Stage 1: Convert a text query to a Wikipedia URL.
textToUrl :: Provider -> String -> IO (Either String String)
textToUrl provider input = do
    let msgs = [ Message "system" "Return ONLY the most relevant English Wikipedia URL for the given topic. No explanation, just the URL."
               , Message "user" input
               ]
    queryLLM provider msgs

-- | Stage 3: Summarize scraped content into bullet points.
summarize :: Provider -> String -> String -> IO (Either String String)
summarize provider topic content = do
    let msgs = [ Message "system" "Summarize the following content into 3 concise factual bullet points."
               , Message "user" ("Topic: " ++ topic ++ "\n\nContent:\n" ++ take 3000 content)
               ]
    queryLLM provider msgs

-- | Internal: Route to the correct LLM provider.
queryLLM :: Provider -> [Message] -> IO (Either String String)
queryLLM Groq        = Groq.query
queryLLM HuggingFace = HF.query

-- | Raw LLM query - send user input directly to LLM without pipeline.
runRawQuery :: Provider -> String -> IO (Either String String)
runRawQuery provider input = do
    let msgs = [ Message "user" input ]
    queryLLM provider msgs

-- | Full 3-stage pipeline: Keyword → URL → Scrape → Summary
runPipeline :: Provider -> String -> IO (Either String String)
runPipeline provider keyword = do
    putStrLn $ "  [1/3] Getting URL for: " ++ keyword

    -- Stage 1: Get URL
    urlResult <- textToUrl provider keyword
    case urlResult of
        Left err -> return $ Left $ "URL stage failed: " ++ err
        Right url -> do
            let cleanUrl = strip url
            putStrLn $ "  [2/3] Scraping: " ++ cleanUrl

            -- Stage 2: Scrape
            scrapeResult <- Scraper.scrapeUrl cleanUrl
            case scrapeResult of
                Left err -> return $ Left $ "Scrape stage failed: " ++ err
                Right content -> do
                    putStrLn $ "  [3/3] Summarizing " ++ show (length content) ++ " chars..."

                    -- Stage 3: Summarize
                    summarize provider keyword content

-- | Process a file through the full pipeline.
processFile :: Postbox -> Provider -> FilePath -> FilePath -> IO ()
processFile pb provider inPath outPath = do
    putStrLn $ "Processing: " ++ inPath

    input <- readMessage inPath
    result <- runPipeline provider input

    case result of
        Right summary -> do
            writeMessage outPath summary
            deleteMessage pb inPath
            putStrLn $ "Done: " ++ outPath ++ " (archived input)"
        Left err -> do
            putStrLn $ "Error: " ++ err

-- | Strip leading/trailing whitespace (for URL cleanup).
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  where
    isSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'
