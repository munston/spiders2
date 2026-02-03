{-# LANGUAGE OverloadedStrings #-}

module LLM.JSON
    ( mkRequest
    , parseResponse
    ) where

import LLM.Types (Message(..))

import Data.Aeson (Value(..), object, (.=), encode, decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Vector ((!?))

-- | Build a JSON request body from model name and messages.
-- Returns a JSON string ready for HTTP POST.
mkRequest :: String -> [Message] -> String
mkRequest model msgs = LBS.unpack $ encode $ object
    [ K.fromText "model"    .= T.pack model
    , K.fromText "messages" .= map messageToValue msgs
    , K.fromText "stream"   .= False
    ]
  where
    messageToValue :: Message -> Value
    messageToValue m = object
        [ K.fromText "role"    .= T.pack (role m)
        , K.fromText "content" .= T.pack (content m)
        ]

-- | Parse a JSON response and extract the assistant's message content.
-- Returns either an error description or the content string.
parseResponse :: String -> Either String String
parseResponse jsonStr = case decode (LBS.pack jsonStr) of
    Nothing -> Left "Invalid JSON"
    Just val -> extractContent val

-- | Internal: Navigate the JSON structure to find choices[0].message.content
extractContent :: Value -> Either String String
extractContent (Object obj) =
    case KM.lookup (K.fromText "choices") obj of
        Just (Array choices) ->
            case choices !? 0 of
                Just (Object choice) ->
                    case KM.lookup (K.fromText "message") choice of
                        Just (Object msg) ->
                            case KM.lookup (K.fromText "content") msg of
                                Just (String txt) -> Right (T.unpack txt)
                                _ -> Left "Missing content field"
                        _ -> Left "Missing message field"
                _ -> Left "Empty choices array"
        Just _ -> Left "choices is not an array"
        Nothing -> extractError obj

extractContent _ = Left "Response is not a JSON object"

-- | Internal: Try to extract an error message from the response
extractError :: KM.KeyMap Value -> Either String String
extractError obj =
    case KM.lookup (K.fromText "error") obj of
        Just (Object errObj) ->
            case KM.lookup (K.fromText "message") errObj of
                Just (String msg) -> Left ("API Error: " ++ T.unpack msg)
                _ -> Left "Unknown API error"
        _ -> Left "Unexpected response format"
