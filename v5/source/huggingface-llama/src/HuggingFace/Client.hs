module HuggingFace.Client
    ( query
    , queryWithKey
    ) where

import LLM.Types (Message)
import LLM.JSON (mkRequest, parseResponse)
import LLM.HTTP (postJSON)

-- | Default API token (can be overridden with queryWithKey)
apiToken :: String
apiToken = "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"

-- | Query HuggingFace's Llama 3.2 3B model with the default token.
query :: [Message] -> IO (Either String String)
query = queryWithKey apiToken

-- | Query HuggingFace's Llama 3.2 3B model with a custom token.
queryWithKey :: String -> [Message] -> IO (Either String String)
queryWithKey token msgs = do
    let body = mkRequest "meta-llama/Llama-3.2-3B-Instruct" msgs
    resp <- postJSON "router.huggingface.co" "/v1/chat/completions"
                     [ ("Authorization", "Bearer " ++ token)
                     , ("Content-Type", "application/json")
                     , ("User-Agent", "HaskellSpider/1.0")
                     ] body
    return (resp >>= parseResponse)
