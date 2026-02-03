-- | Modality - different modes of message processing.
-- A postbox can register multiple modalities, and messages can indicate
-- which modality to use via prefix syntax: "modality:message"
--
-- Standard modalities:
--   echo     - Simple echo (for testing)
--   llm-raw  - Raw LLM query (send directly to LLM)
--   llm-wiki - Wiki pipeline (keyword -> URL -> scrape -> summarize)
--   fly      - Execute fly commands
--   default  - Alias for the postbox's default mode
module Postbox.Modality
    ( -- * Types
      Modality(..)
    , ModalityHandler
    , ModalityRegistry
      -- * Registry operations
    , emptyRegistry
    , registerModality
    , registerModalities
    , lookupModality
    , listModalities
    , dispatchMessage
      -- * Standard modality names
    , modalityEcho
    , modalityLLMRaw
    , modalityLLMWiki
    , modalityFly
    , defaultModality
      -- * Standard modality builders
    , mkEchoModality
      -- * Message parsing
    , parseModality
    ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- | Handler function for a modality.
-- Takes: postbox name, input content, returns output content
type ModalityHandler = String -> String -> IO String

-- | A modality defines how messages are processed.
data Modality = Modality
    { modalityName        :: String           -- ^ Unique identifier (e.g., "echo", "llm", "fly")
    , modalityDescription :: String           -- ^ Human-readable description
    , modalityHandler     :: ModalityHandler  -- ^ The handler function
    }

-- | Registry of available modalities.
newtype ModalityRegistry = ModalityRegistry (Map String Modality)

-- Standard modality names
modalityEcho, modalityLLMRaw, modalityLLMWiki, modalityFly, defaultModality :: String
modalityEcho    = "echo"
modalityLLMRaw  = "llm-raw"
modalityLLMWiki = "llm-wiki"
modalityFly     = "fly"
defaultModality = "default"

-- | Create an empty registry.
emptyRegistry :: ModalityRegistry
emptyRegistry = ModalityRegistry Map.empty

-- | Register a modality.
registerModality :: Modality -> ModalityRegistry -> ModalityRegistry
registerModality m (ModalityRegistry reg) =
    ModalityRegistry (Map.insert (modalityName m) m reg)

-- | Register multiple modalities at once.
registerModalities :: [Modality] -> ModalityRegistry -> ModalityRegistry
registerModalities ms reg = foldr registerModality reg ms

-- | Look up a modality by name.
lookupModality :: String -> ModalityRegistry -> Maybe Modality
lookupModality name (ModalityRegistry reg) = Map.lookup name reg

-- | List all registered modality names.
listModalities :: ModalityRegistry -> [String]
listModalities (ModalityRegistry reg) = Map.keys reg

-- | Dispatch a message to the appropriate handler.
-- Returns Nothing if the modality is not found.
dispatchMessage :: ModalityRegistry -> String -> String -> String -> IO (Maybe String)
dispatchMessage reg modalityName' postboxName content =
    case lookupModality modalityName' reg of
        Nothing -> return Nothing
        Just m  -> Just <$> modalityHandler m postboxName content

-- | Parse modality prefix from message.
-- "echo:hello" -> Just ("echo", "hello")
-- "hello"      -> Nothing (use default)
parseModality :: String -> Maybe (String, String)
parseModality s = case break (== ':') s of
    (prefix, ':':rest) | not (null prefix) && not (any (== ' ') prefix) ->
        Just (prefix, rest)
    _ -> Nothing

-- | Create a simple echo modality.
mkEchoModality :: Modality
mkEchoModality = Modality
    { modalityName        = modalityEcho
    , modalityDescription = "Simple echo - returns input with prefix"
    , modalityHandler     = \pbName content -> return $ "[" ++ pbName ++ " echo] " ++ content
    }
