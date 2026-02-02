module Groq.Client
    ( query
    , queryWithKey
    ) where

import LLM.Types (Message)
import LLM.JSON (mkRequest, parseResponse)
import LLM.HTTP (postJSON)

-- | Default API key (can be overridden with queryWithKey)
apiKey :: String
apiKey = "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"

-- | Query Groq's Llama 3.3 70B model with the default API key.
query :: [Message] -> IO (Either String String)
query = queryWithKey apiKey

-- | Query Groq's Llama 3.3 70B model with a custom API key.
queryWithKey :: String -> [Message] -> IO (Either String String)
queryWithKey key msgs = do
    let body = mkRequest "llama-3.3-70b-versatile" msgs
    resp <- postJSON "api.groq.com" "/openai/v1/chat/completions"
                     [ ("Authorization", "Bearer " ++ key)
                     , ("Content-Type", "application/json")
                     ] body
    return (resp >>= parseResponse)
