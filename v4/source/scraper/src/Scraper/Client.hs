{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scraper.Client
    ( scrapeUrl
    , scrapeUrlParagraphs
    ) where

import Text.HTML.Scalpel (texts, scrapeStringLike)
import Network.HTTP.Simple
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Control.Exception (try, SomeException)

-- | Scrape a URL and extract all paragraph text.
-- Returns either an error message or the extracted text.
scrapeUrl :: String -> IO (Either String String)
scrapeUrl url = scrapeUrlParagraphs url 5000

-- | Scrape a URL with a character limit on the result.
scrapeUrlParagraphs :: String -> Int -> IO (Either String String)
scrapeUrlParagraphs url charLimit = do
    result <- try $ do
        request <- parseRequest url
        let requestWithUA = setRequestHeader "User-Agent" [userAgent] request
        response <- httpLBS requestWithUA
        let htmlBytes = BL.toStrict (getResponseBody response)
        let htmlText = TE.decodeUtf8 htmlBytes
        return $ extractParagraphs htmlText charLimit

    case result of
        Left (e :: SomeException) -> return $ Left ("Scrape error: " ++ show e)
        Right text -> return $ Right text

-- | Internal: Extract paragraph text from HTML.
extractParagraphs :: T.Text -> Int -> String
extractParagraphs html charLimit =
    case scrapeStringLike html (texts "p") of
        Just paragraphs ->
            let combined = T.unwords paragraphs
                trimmed = T.take charLimit combined
            in T.unpack trimmed
        Nothing -> "Error: No content found"

-- | User agent string for requests.
userAgent :: B8.ByteString
userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
