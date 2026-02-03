{-# LANGUAGE ForeignFunctionInterface #-}
module Llama where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes, alloca)
import Foreign.Marshal.Array (allocaArray, peekArray, pokeArray)
import Foreign.Storable
import Data.Int (Int32, Int8)
import Control.Monad (foldM)

type LlamaModel = Ptr ()
type LlamaContext = Ptr ()
type LlamaVocab = Ptr ()
type LlamaSampler = Ptr ()

data LlamaBatch = LlamaBatch
  { batch_n_tokens :: Int32
  , batch_token :: Ptr Int32
  , batch_embd :: Ptr CFloat
  , batch_pos :: Ptr Int32
  , batch_n_seq_id :: Ptr Int32
  , batch_seq_id :: Ptr (Ptr Int32)
  , batch_logits :: Ptr Int8
  } deriving (Show)

instance Storable LlamaBatch where
  sizeOf _ = 56
  alignment _ = 8
  peek ptr = do
    n <- peekByteOff ptr 0
    tok <- peekByteOff ptr 8
    emb <- peekByteOff ptr 16
    p <- peekByteOff ptr 24
    nseq <- peekByteOff ptr 32
    seqid <- peekByteOff ptr 40
    log <- peekByteOff ptr 48
    return $ LlamaBatch n tok emb p nseq seqid log
  poke ptr (LlamaBatch n tok emb p nseq seqid log) = do
    pokeByteOff ptr 0 n
    pokeByteOff ptr 8 tok
    pokeByteOff ptr 16 emb
    pokeByteOff ptr 24 p
    pokeByteOff ptr 32 nseq
    pokeByteOff ptr 40 seqid
    pokeByteOff ptr 48 log

foreign import ccall unsafe "llama_backend_init" 
    c_llama_backend_init :: IO ()

foreign import ccall unsafe "llama_model_default_params" 
    c_llama_model_default_params :: Ptr () -> IO ()

foreign import ccall unsafe "llama_context_default_params" 
    c_llama_context_default_params :: Ptr () -> IO ()

foreign import ccall unsafe "llama_load_model_from_file" 
    c_llama_load_model_from_file :: CString -> Ptr () -> IO LlamaModel

foreign import ccall unsafe "llama_new_context_with_model" 
    c_llama_new_context_with_model :: LlamaModel -> Ptr () -> IO LlamaContext

foreign import ccall unsafe "llama_model_get_vocab" 
    c_llama_model_get_vocab :: LlamaModel -> IO LlamaVocab

foreign import ccall unsafe "llama_tokenize" 
    c_llama_tokenize :: LlamaVocab -> CString -> Int32 -> Ptr Int32 -> Int32 -> Int32 -> Int32 -> IO Int32

foreign import ccall unsafe "llama_decode" 
    c_llama_decode :: LlamaContext -> Ptr LlamaBatch -> IO Int32

foreign import ccall unsafe "llama_sampler_init_greedy" 
    c_llama_sampler_init_greedy :: IO LlamaSampler

foreign import ccall unsafe "llama_sampler_sample" 
    c_llama_sampler_sample :: LlamaSampler -> LlamaContext -> Int32 -> IO Int32

foreign import ccall unsafe "llama_vocab_is_eog"
    c_llama_vocab_is_eog :: LlamaVocab -> Int32 -> IO Int32

foreign import ccall unsafe "llama_sampler_free"
    c_llama_sampler_free :: LlamaSampler -> IO ()

foreign import ccall unsafe "llama_free"
    c_llama_free :: LlamaContext -> IO ()

foreign import ccall unsafe "llama_model_free"
    c_llama_model_free :: LlamaModel -> IO ()

setupLlama :: FilePath -> IO (LlamaModel, LlamaContext, LlamaVocab)
setupLlama path = do
    c_llama_backend_init
    allocaBytes 1024 $ \mparams -> allocaBytes 1024 $ \cparams -> do
        c_llama_model_default_params mparams
        c_llama_context_default_params cparams
        withCString path $ \cPath -> do
            model <- c_llama_load_model_from_file cPath mparams
            if model == nullPtr then error "Model load failed" else do
                ctx <- c_llama_new_context_with_model model cparams
                vocab <- c_llama_model_get_vocab model
                return (model, ctx, vocab)

tokenize :: LlamaVocab -> String -> IO [Int32]
tokenize vocab text = withCString text $ \cStr -> do
    let n = fromIntegral (length text) :: Int32
    allocaArray (fromIntegral n + 16) $ \ptr -> do
        res <- c_llama_tokenize vocab cStr n ptr (n + 16) 1 0
        if res <= 0 
            then error $ "Tokenization failed: " ++ show res
            else peekArray (fromIntegral res) ptr

initSampler :: IO LlamaSampler
initSampler = c_llama_sampler_init_greedy

-- Manually construct batch for single token
decodeOne :: LlamaContext -> Int32 -> Int32 -> IO ()
decodeOne ctx tok pos =
  allocaArray 1 $ \tokPtr ->
    allocaArray 1 $ \posPtr ->
      allocaArray 1 $ \nseqPtr ->
        alloca $ \batchPtr -> do
          pokeArray tokPtr [tok]
          pokeArray posPtr [pos]
          pokeArray nseqPtr [1]
          let batch = LlamaBatch
                { batch_n_tokens = 1
                , batch_token = tokPtr
                , batch_embd = nullPtr
                , batch_pos = posPtr
                , batch_n_seq_id = nseqPtr
                , batch_seq_id = nullPtr
                , batch_logits = nullPtr
                }
          poke batchPtr batch
          ret <- c_llama_decode ctx batchPtr
          if ret /= 0 then error $ "Decode failed: " ++ show ret else return ()

decodePrompt :: LlamaContext -> [Int32] -> Int32 -> IO Int32
decodePrompt ctx toks pos0 =
  foldM (\p t -> decodeOne ctx t p >> return (p + 1)) pos0 toks

sampleToken :: LlamaSampler -> LlamaContext -> IO Int32
sampleToken sampler ctx = c_llama_sampler_sample sampler ctx (-1)

tokenizeEx :: LlamaVocab -> String -> Int32 -> IO [Int32]
tokenizeEx vocab text addBos = withCString text $ \cStr -> do
    let n = fromIntegral (length text) :: Int32
    allocaArray (fromIntegral n + 16) $ \ptr -> do
        -- Use the passed addBos instead of hardcoded 1
        res <- c_llama_tokenize vocab cStr n ptr (n + 16) addBos 0
        if res <= 0 
            then error $ "Tokenization failed"
            else peekArray (fromIntegral res) ptr

isEOG :: LlamaVocab -> Int32 -> IO Bool
isEOG vocab token = (0 /=) <$> c_llama_vocab_is_eog vocab token

cleanup :: LlamaSampler -> LlamaContext -> LlamaModel -> IO ()
cleanup sampler ctx model = do
    c_llama_sampler_free sampler
    c_llama_free ctx
    c_llama_model_free model