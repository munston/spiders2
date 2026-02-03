module LlamaBatch (
    Control,
    initLlamaSystem,
    GeneratorFunc,
    FileWriterFunc
) where

import Llama
import LlamaText
import System.IO (hFlush, stdout, appendFile)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import Data.Int (Int32)
import Control.Monad (foldM)

type Control = [(String, Int)]
type GeneratorFunc = Control -> IO String
type FileWriterFunc = String -> Control -> IO ()

initLlamaSystem :: FilePath -> IO (GeneratorFunc, FileWriterFunc, IO ())
initLlamaSystem path = do
    (model, ctx, vocab) <- setupLlama path
    sampler <- initSampler
    createDirectoryIfMissing True "llama-chats"

    let generator :: GeneratorFunc
        generator control = do
            (fullText, _) <- foldM (processStep ctx vocab sampler) ("", 0) control
            return fullText

    let fileWriter :: FileWriterFunc
        fileWriter fileName control = do
            let path' = "llama-chats" </> fileName <.> "txt"
            result <- generator control
            appendFile path' $ result ++ "\n"

    let cleanupAction = Llama.cleanup sampler ctx model
    return (generator, fileWriter, cleanupAction)

processStep :: LlamaContext -> LlamaVocab -> LlamaSampler -> (String, Int32) -> (String, Int) -> IO (String, Int32)
processStep ctx vocab sampler (acc, pos) (trigger, count) = do
    -- Strictly formatted screen output
    putStr $ "TRIGGER: " ++ trigger ++ " | RESULT: "
    hFlush stdout

    let addBos = if pos == 0 then 1 else 0
    tokens <- tokenizeEx vocab trigger addBos
    newPos <- decodePrompt ctx tokens pos
    
    (burstText, finalPos) <- generateExact ctx vocab sampler newPos count
    
    putStrLn "" -- Finish the line
    let segment = "TRIGGER: " ++ trigger ++ " | RESULT: " ++ burstText ++ "\n"
    return (acc ++ segment, finalPos)

generateExact :: LlamaContext -> LlamaVocab -> LlamaSampler -> Int32 -> Int -> IO (String, Int32)
generateExact ctx vocab sampler pos count
    | count <= 0 = return ("", pos)
    | otherwise = do
        t <- sampleToken sampler ctx
        decodeOne ctx t pos
        rawChunk <- detokenize vocab t
        
        -- SANITIZE: Remove newlines from tokens to keep output on one line
        let chunk = map (\c -> if c == '\n' || c == '\r' then ' ' else c) rawChunk
        
        putStr chunk
        hFlush stdout
        
        (rest, nextPos) <- generateExact ctx vocab sampler (pos + 1) (count - 1)
        return (chunk ++ rest, nextPos)