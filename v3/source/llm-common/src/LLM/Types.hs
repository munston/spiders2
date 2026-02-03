module LLM.Types
    ( Message(..)
    ) where

-- | A chat message with role and content.
-- Both fields are String - no Text or other types exposed.
data Message = Message
    { role    :: String   -- ^ "system", "user", or "assistant"
    , content :: String   -- ^ The message content
    } deriving (Show, Eq)
