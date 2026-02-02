-- | Fly - programmatic interface to batch script operations.
-- All system functionality is accessed through batch scripts.
-- Fly provides the mechanism for postbox agents to invoke these scripts.
module Postbox.Fly
    ( -- * Fly operations (invoke batch scripts)
      flyStatus
    , flyKillAll
    , flySendMessage
    , flySendAll
    , flyCompare
    , flyWordsGenerate
    , flyList
    , flyStartAgent
    , flyPing
      -- * Low-level
    , runBat
    ) where

import System.Process (readCreateProcess, shell, CreateProcess(..))
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)

-- | Run a batch file in the neighborhood (parent) directory
runBat :: String -> [String] -> IO String
runBat batName args = do
    cwd <- getCurrentDirectory
    let neighborhood = cwd </> ".."
    let batPath = neighborhood </> batName
    let cmd = unwords (batPath : args)
    let proc = (shell cmd) { cwd = Just neighborhood }
    readCreateProcess proc ""

-- | Run a batch file without capturing output
runBat_ :: String -> [String] -> IO ()
runBat_ batName args = do
    _ <- runBat batName args
    return ()

-- | fly:status - Get status of all postboxes
flyStatus :: IO String
flyStatus = runBat "status.bat" []

-- | fly:list - List all discovered postboxes
flyList :: IO String
flyList = runBat "list.bat" []

-- | fly:kill-all - Send pkill to all active postboxes
flyKillAll :: IO ()
flyKillAll = runBat_ "kill-all.bat" []

-- | fly:send - Send a message to an agent
flySendMessage :: String -> String -> IO ()
flySendMessage agentName message =
    runBat_ "send-message.bat" [agentName, message]

-- | fly:send-all - Broadcast a message to all active agents
flySendAll :: String -> IO String
flySendAll message = runBat "send-all.bat" [message]

-- | fly:start - Start an agent with given name and mode
flyStartAgent :: String -> String -> IO ()
flyStartAgent agentName mode =
    runBat_ "start-agent.bat" [agentName, mode]

-- | fly:ping - Ping an agent
flyPing :: String -> IO ()
flyPing agentName =
    runBat_ "ping.bat" [agentName]

-- | fly:compare - Send compare request with multiple messages
-- Messages are passed as separate arguments to compare.bat
flyCompare :: String -> [String] -> IO String
flyCompare agentName messages =
    runBat "compare.bat" (agentName : map quote messages)
  where
    quote s = "\"" ++ s ++ "\""

-- | fly:words - Generate words using a words-mode agent
flyWordsGenerate :: String -> Int -> IO String
flyWordsGenerate agentName count =
    runBat "words-generate.bat" [agentName, show count]
