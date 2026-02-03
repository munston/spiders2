-- | Request - handling request/response patterns in messages.
-- A request is a message that expects a response sent back to the sender.
--
-- Message format:
--   "?modality:content"  - Request expecting response (use specified modality)
--   "?:content"          - Request expecting response (use default modality)
--   "modality:content"   - One-way message (no response expected)
--   "content"            - One-way message (use default modality)
--
-- Sender identification comes from the filename (e.g., "from-agent-1.txt")
module Postbox.Request
    ( -- * Types
      Request(..)
    , RequestType(..)
      -- * Parsing
    , parseRequest
    , parseFilename
      -- * Response
    , responseFilename
    , formatResponse
    ) where

import Postbox.Modality (parseModality, defaultModality)

-- | Type of message: request (expects response) or one-way
data RequestType = RequestExpectsResponse | OneWay
    deriving (Show, Eq)

-- | A parsed request
data Request = Request
    { requestType     :: RequestType  -- ^ Whether response is expected
    , requestModality :: String       -- ^ Which modality to use
    , requestContent  :: String       -- ^ The actual message content
    , requestSender   :: Maybe String -- ^ Sender name (from filename)
    } deriving (Show, Eq)

-- | Parse a message into a Request.
-- Handles the "?" prefix for requests expecting responses.
parseRequest :: String -> Request
parseRequest s = case s of
    ('?':rest) -> case parseModality rest of
        Just (modality, content) -> Request RequestExpectsResponse modality content Nothing
        Nothing -> Request RequestExpectsResponse defaultModality rest Nothing
    _ -> case parseModality s of
        Just (modality, content) -> Request OneWay modality content Nothing
        Nothing -> Request OneWay defaultModality s Nothing

-- | Parse sender name from filename.
-- "from-agent-1.txt" -> Just "agent-1"
-- "query-12345.txt"  -> Nothing
parseFilename :: FilePath -> Maybe String
parseFilename f =
    let base = takeWhileNot (== '.') f
    in if take 5 base == "from-"
       then Just (drop 5 base)
       else Nothing
  where
    takeWhileNot p = takeWhile (not . p)

-- | Generate response filename for a sender.
-- "agent-1" -> "response-from-me.txt" (where me is the responder)
responseFilename :: String -> String -> FilePath
responseFilename responderName senderName =
    "response-to-" ++ senderName ++ "-from-" ++ responderName ++ ".txt"

-- | Format a response message (just the content, no special encoding needed)
formatResponse :: String -> String
formatResponse = id
