{-# LANGUAGE ForeignFunctionInterface #-}
module LlamaText where

import Llama (LlamaVocab)
import Foreign.C.String (CString, peekCStringLen)
import Foreign.C.Types (CBool(..))
import Foreign.Marshal.Array (allocaArray)
import Data.Int (Int32)

foreign import ccall unsafe "llama.h llama_token_to_piece"
    c_llama_token_to_piece :: LlamaVocab -> Int32 -> CString -> Int32 -> Int32 -> CBool -> IO Int32

detokenize :: LlamaVocab -> Int32 -> IO String
detokenize vocab token = do
    let bufferSize = 64
    allocaArray bufferSize $ \ptr -> do
        len <- c_llama_token_to_piece vocab token ptr (fromIntegral bufferSize) 0 (CBool 0)
        if len < 0 then return "" else peekCStringLen (ptr, fromIntegral len)

detokenizeBatch :: LlamaVocab -> [Int32] -> IO String
detokenizeBatch vocab tokens = concat <$> mapM (detokenize vocab) tokens