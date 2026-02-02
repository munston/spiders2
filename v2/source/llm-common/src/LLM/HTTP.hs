{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLM.HTTP
    ( postJSON
    ) where

import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Client as Client
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI
import Control.Exception (try, SomeException)

-- | POST a JSON body to an HTTPS endpoint.
-- All parameters and results are String - no ByteString or Text exposed.
--
-- Example:
--   postJSON "api.groq.com" "/openai/v1/chat/completions"
--            [("Authorization", "Bearer sk-...")] bodyJson
--
postJSON :: String              -- ^ Host (e.g., "api.groq.com")
         -> String              -- ^ Path (e.g., "/openai/v1/chat/completions")
         -> [(String, String)]  -- ^ Headers as (name, value) pairs
         -> String              -- ^ JSON body
         -> IO (Either String String)
postJSON host path headers body = do
    result <- try $ do
        let request = buildRequest host path headers body
        response <- HTTP.httpLBS request
        return (HTTP.getResponseStatusCode response, LBS.unpack (HTTP.getResponseBody response))

    case result of
        Left (e :: SomeException) ->
            return $ Left ("Network error: " ++ show e)
        Right (status, responseBody) ->
            if status >= 200 && status < 300
                then return $ Right responseBody
                else return $ Left ("HTTP " ++ show status ++ ": " ++ take 200 responseBody)

-- | Internal: Build the HTTP request from String parameters
buildRequest :: String -> String -> [(String, String)] -> String -> HTTP.Request
buildRequest host path headers body =
    HTTP.setRequestMethod (B.pack "POST")
    $ HTTP.setRequestSecure True
    $ HTTP.setRequestPort 443
    $ HTTP.setRequestHost (B.pack host)
    $ HTTP.setRequestPath (B.pack path)
    $ HTTP.setRequestBodyLBS (LBS.pack body)
    $ HTTP.setRequestResponseTimeout (Client.responseTimeoutMicro (90 * 1000000))
    $ foldr addHeader baseRequest headers
  where
    baseRequest = HTTP.defaultRequest

    addHeader :: (String, String) -> HTTP.Request -> HTTP.Request
    addHeader (name, value) =
        HTTP.addRequestHeader (CI.mk (B.pack name)) (B.pack value)
