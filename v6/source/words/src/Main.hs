module Main where

import Words
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Control.Concurrent

go :: Int -> IO ()
go n = do
    wordsList <- generateBatch n
    mapM_ (\w -> print w >> threadDelay 250000) wordsList

main :: IO ()
main = do
    putStr "How many words to generate? (Enter to quit): "
    hFlush stdout
    input <- getLine
    
    case readMaybe input of
        Just n -> do
            go n
            putStrLn "" -- Add a spacer after the list
            main        -- Loop back
        Nothing -> 
            if null input 
                then putStrLn "Goodbye!" 
                else putStrLn "Please enter a valid number." >> main