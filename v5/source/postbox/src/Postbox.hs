module Postbox
    ( -- * Configuration
      Postbox(..)
    , mkPostbox
      -- * Lifecycle
    , startup
    , run
    , runFast
    , shutdown
      -- * Watcher
    , watch
    , watchOnce
      -- * File Operations
    , getInboxFiles
    , getWorkFiles
    , readMessage
    , writeMessage
    , deleteMessage
    , archiveMessage
      -- * Paths
    , inboxPath
    , outboxPath
    , savedPath
    , presenceFile
      -- * Presence
    , markOnline
    , markOffline
    , isOnline
    , cleanStalePresence
      -- * Neighborhood
    , discoverPostboxes
    , getActivePostboxes
    , killAll
    , getNeighborhood
      -- * Inter-Postbox Messaging
    , sendMessageTo
    , sendMessageToName
    , sendToAll
    , sendToAllActive
      -- * Commands
    , sendPkill
    , sendPkillTo
    , isPkill
    , pkillFile
    ) where

import Control.Concurrent (threadDelay, newEmptyMVar, takeMVar, tryPutMVar, tryTakeMVar, MVar)
import Control.Exception (bracket_, catch, finally, SomeException)
import Control.Monad (forM_, forM, when, unless, forever, void)
import System.Directory (listDirectory, createDirectoryIfMissing, removeFile, doesFileExist, doesDirectoryExist, copyFile)
import System.FilePath ((</>), takeExtension, takeBaseName, takeFileName, takeDirectory)
import System.FSNotify (withManager, watchDir, Event(..))
import Data.Char (toLower, isDigit)
import Data.List (isPrefixOf, sortBy)
import Data.Ord (comparing, Down(..))
import Text.Read (readMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

-- | Postbox configuration.
data Postbox = Postbox
    { name     :: String      -- ^ Name of this postbox (e.g., "spider-app")
    , root     :: FilePath    -- ^ Root postbox directory (contains inbox, outbox, saved)
    , contacts :: [String]    -- ^ Names of connected postboxes
    } deriving (Show, Eq)

-- | Get the inbox path for a postbox.
inboxPath :: Postbox -> FilePath
inboxPath pb = root pb </> "inbox"

-- | Get the outbox path for a postbox.
outboxPath :: Postbox -> FilePath
outboxPath pb = root pb </> "outbox"

-- | Get the saved path for a postbox.
savedPath :: Postbox -> FilePath
savedPath pb = root pb </> "saved"

-- | Get the presence file path (indicates postbox is online).
presenceFile :: Postbox -> FilePath
presenceFile pb = root pb </> "currently-on.txt"

-- | Create a postbox with default root path (current directory).
mkPostbox :: String -> [String] -> Postbox
mkPostbox n cs = Postbox
    { name     = n
    , root     = "."
    , contacts = cs
    }

-- | Initialize postbox: create dirs, clean stale presence/pkill.
startup :: Postbox -> IO ()
startup pb = do
    createDirectoryIfMissing True (inboxPath pb)
    createDirectoryIfMissing True (outboxPath pb)
    createDirectoryIfMissing True (savedPath pb)
    cleanStalePresence pb
    cleanStalePkills pb

-- | Clean any pkill messages that arrived while we were offline.
cleanStalePkills :: Postbox -> IO ()
cleanStalePkills pb = do
    files <- listDirectory (inboxPath pb)
    let pkills = filter (isPrefixOf "pkill-") files
    forM_ pkills $ \f -> do
        let path = inboxPath pb </> f
        removeFile path
        putStrLn $ "[" ++ name pb ++ "] Cleaned stale pkill: " ++ f

-- | Clean stale presence file from previous crash.
cleanStalePresence :: Postbox -> IO ()
cleanStalePresence pb = do
    exists <- doesFileExist (presenceFile pb)
    when exists $ do
        putStrLn $ "[" ++ name pb ++ "] Cleaned stale presence (previous crash?)"
        removeFile (presenceFile pb)

-- | Mark postbox as online (write presence file with timestamp).
markOnline :: Postbox -> IO ()
markOnline pb = do
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
    writeFile (presenceFile pb) $ name pb ++ " online since " ++ timestamp ++ "\n"
    putStrLn $ "[" ++ name pb ++ "] Marked online"

-- | Mark postbox as offline (delete presence file).
markOffline :: Postbox -> IO ()
markOffline pb = do
    exists <- doesFileExist (presenceFile pb)
    when exists $ do
        removeFile (presenceFile pb)
        putStrLn $ "[" ++ name pb ++ "] Marked offline"

-- | Check if a postbox is online (presence file exists).
isOnline :: Postbox -> IO Bool
isOnline pb = doesFileExist (presenceFile pb)

-- | Discover all postbox directories in a neighborhood.
-- A postbox directory is identified by having an "inbox" subdirectory.
discoverPostboxes :: FilePath -> IO [Postbox]
discoverPostboxes neighborhood = do
    exists <- doesDirectoryExist neighborhood
    if not exists
        then return []
        else do
            entries <- listDirectory neighborhood
            postboxes <- forM entries $ \entry -> do
                let dir = neighborhood </> entry
                isDir <- doesDirectoryExist dir
                hasInbox <- doesDirectoryExist (dir </> "inbox")
                if isDir && hasInbox
                    then return $ Just $ Postbox entry dir []
                    else return Nothing
            return $ catMaybes postboxes
  where
    catMaybes = foldr (\x acc -> case x of Just v -> v : acc; Nothing -> acc) []

-- | Get all currently active (online) postboxes in a neighborhood.
getActivePostboxes :: FilePath -> IO [Postbox]
getActivePostboxes neighborhood = do
    all_pb <- discoverPostboxes neighborhood
    filterM isOnline all_pb
  where
    filterM _ [] = return []
    filterM p (x:xs) = do
        keep <- p x
        rest <- filterM p xs
        return $ if keep then x : rest else rest

-- | Send pkill to all active postboxes in a neighborhood.
killAll :: FilePath -> String -> IO ()
killAll neighborhood sender = do
    active <- getActivePostboxes neighborhood
    if null active
        then putStrLn "No active postboxes found."
        else do
            putStrLn $ "Killing " ++ show (length active) ++ " postbox(es)..."
            forM_ active $ \pb -> do
                sendPkillTo pb sender
                putStrLn $ "  Sent pkill to: " ++ name pb

-- | Send a pkill message directly to a postbox's inbox.
sendPkillTo :: Postbox -> String -> IO ()
sendPkillTo target sender = do
    let filename = pkillFile sender
    let path = inboxPath target </> filename
    writeFile path ""

-- | Get the neighborhood path (parent directory containing sibling postboxes).
getNeighborhood :: Postbox -> FilePath
getNeighborhood pb = takeDirectory (root pb)

-- | Send a message to another postbox's inbox.
sendMessageTo :: Postbox -> String -> String -> IO ()
sendMessageTo target filename content = do
    let path = inboxPath target </> filename
    writeFile path content
    putStrLn $ "Sent message to " ++ name target ++ ": " ++ filename

-- | Send a message to a postbox by name (looks up in neighborhood).
sendMessageToName :: Postbox -> String -> String -> String -> IO Bool
sendMessageToName sender targetName filename content = do
    let neighborhood = getNeighborhood sender
    postboxes <- discoverPostboxes neighborhood
    case filter (\p -> name p == targetName) postboxes of
        []     -> do
            putStrLn $ "Postbox not found: " ++ targetName
            return False
        (target:_) -> do
            sendMessageTo target filename content
            return True

-- | Send a message to all postboxes in the neighborhood (excluding self).
-- Returns list of postbox names that received the message.
sendToAll :: Postbox -> String -> String -> IO [String]
sendToAll sender filename content = do
    let neighborhood = getNeighborhood sender
    postboxes <- discoverPostboxes neighborhood
    let others = filter (\p -> name p /= name sender) postboxes
    forM others $ \target -> do
        sendMessageTo target filename content
        return (name target)

-- | Send a message to all ACTIVE postboxes in the neighborhood (excluding self).
-- Returns list of postbox names that received the message.
sendToAllActive :: Postbox -> String -> String -> IO [String]
sendToAllActive sender filename content = do
    let neighborhood = getNeighborhood sender
    active <- getActivePostboxes neighborhood
    let others = filter (\p -> name p /= name sender) active
    forM others $ \target -> do
        sendMessageTo target filename content
        return (name target)

-- | Run the postbox loop until pkill received (polling mode).
-- Automatically marks online/offline and cleans up on crash.
run :: Postbox
    -> (FilePath -> FilePath -> IO ())  -- ^ Handler for work messages
    -> Int                               -- ^ Poll interval in seconds
    -> IO ()
run pb handler delaySecs = do
    markOnline pb
    loop `finally` markOffline pb
  where
    loop = do
        -- Check for pkill first
        pkillReceived <- checkPkill pb
        if pkillReceived
            then shutdown pb
            else do
                -- Process work files
                watchOnce pb handler
                threadDelay (delaySecs * 1000000)
                loop

-- | Run the postbox with file system watching (event-driven, instant response).
-- This is more efficient than polling - uses zero CPU when idle.
-- Automatically marks online/offline and cleans up on crash.
runFast :: Postbox
        -> (FilePath -> FilePath -> IO ())  -- ^ Handler for work messages
        -> IO ()
runFast pb handler = do
    markOnline pb
    (withManager $ \mgr -> do
        -- Process any existing files first
        watchOnce pb handler

        -- Set up file system watcher - MVar acts as a signal
        trigger <- newEmptyMVar :: IO (MVar ())

        -- Watch inbox for new files
        void $ watchDir mgr (inboxPath pb) (const True) $ \_event -> do
            -- Signal that something happened (non-blocking put)
            void $ tryPutMVar trigger ()

        -- Main loop: wait for events, process, check pkill
        let loop = do
                -- Wait for a file event (blocks until something happens)
                takeMVar trigger
                -- Small delay to let file finish writing
                threadDelay 50000  -- 50ms
                -- Drain any queued events
                drainMVar trigger
                -- Check for pkill
                pkillReceived <- checkPkill pb
                if pkillReceived
                    then shutdown pb
                    else do
                        watchOnce pb handler
                        loop
        loop) `finally` markOffline pb
  where
    drainMVar mv = do
        result <- tryTakeMVar mv
        case result of
            Nothing -> return ()
            Just _  -> drainMVar mv

-- | Check if a pkill message has arrived.
checkPkill :: Postbox -> IO Bool
checkPkill pb = do
    files <- listDirectory (inboxPath pb)
    let pkills = filter (isPrefixOf "pkill-") files
    if null pkills
        then return False
        else do
            -- Clean up the pkill message(s)
            forM_ pkills $ \f -> do
                let path = inboxPath pb </> f
                putStrLn $ "[" ++ name pb ++ "] Received pkill from: " ++ extractSender f
                removeFile path
            return True

-- | Extract sender name from pkill filename (e.g., "pkill-spider-app.txt" -> "spider-app")
extractSender :: FilePath -> String
extractSender f = drop 6 (takeBaseName f)  -- drop "pkill-"

-- | Graceful shutdown: notify all contacts, then exit.
shutdown :: Postbox -> IO ()
shutdown pb = do
    putStrLn $ "[" ++ name pb ++ "] Shutting down..."
    -- Notify all contacts
    forM_ (contacts pb) $ \contact -> do
        sendPkill pb contact
    putStrLn $ "[" ++ name pb ++ "] Goodbye."

-- | Send a pkill message to another postbox.
-- Writes to their inbox (assumes same parent directory structure).
sendPkill :: Postbox -> String -> IO ()
sendPkill pb target = do
    -- Target inbox is at ../target/inbox/ relative to our location
    -- For simplicity, assume flat structure: target's inbox is "inbox" in their dir
    -- This writes to OUR outbox with the pkill filename for the target
    let filename = pkillFile (name pb)
    let path = outboxPath pb </> filename
    writeFile path ""
    putStrLn $ "[" ++ name pb ++ "] Sent pkill to: " ++ target

-- | Generate pkill filename for a sender.
pkillFile :: String -> FilePath
pkillFile sender = "pkill-" ++ sender ++ ".txt"

-- | Check if a filename is a pkill message.
isPkill :: FilePath -> Bool
isPkill f = "pkill-" `isPrefixOf` f

-- | Legacy watch function (runs forever, no pkill support).
-- Note: Assumes inbox/outbox are subdirectories of current directory.
watch :: FilePath -> FilePath -> (FilePath -> FilePath -> IO ()) -> Int -> IO ()
watch _inb _outb handler delaySecs = do
    let pb = Postbox "legacy" "." []
    startup pb
    run pb handler delaySecs

-- | Process all current inbox work files once (no loop).
watchOnce :: Postbox -> (FilePath -> FilePath -> IO ()) -> IO ()
watchOnce pb handler = do
    files <- getWorkFiles pb
    forM_ files $ \fileName -> do
        let inPath  = inboxPath pb </> fileName
        let outPath = outboxPath pb </> fileName
        handler inPath outPath

-- | Get all .txt files from a directory.
getInboxFiles :: FilePath -> IO [FilePath]
getInboxFiles dir = do
    entries <- listDirectory dir
    return $ filter isTxtFile entries
  where
    isTxtFile f = map toLower (takeExtension f) == ".txt"

-- | Get only work files (excludes pkill messages).
getWorkFiles :: Postbox -> IO [FilePath]
getWorkFiles pb = do
    files <- getInboxFiles (inboxPath pb)
    return $ filter (not . isPkill) files

-- | Read a message file, stripping whitespace.
readMessage :: FilePath -> IO String
readMessage path = do
    content <- readFile path
    return (strip content)

-- | Write a message to a path.
writeMessage :: FilePath -> String -> IO ()
writeMessage = writeFile

-- | Archive a message to the saved directory with incrementing prefix.
archiveMessage :: Postbox -> FilePath -> IO ()
archiveMessage pb srcPath = do
    nextNum <- getNextSavedNumber pb
    let fileName = takeFileName srcPath
    let numberedName = padNumber nextNum ++ "-" ++ fileName
    let destPath = savedPath pb </> numberedName
    copyFile srcPath destPath

-- | Delete a message file (archives it first).
deleteMessage :: Postbox -> FilePath -> IO ()
deleteMessage pb path = do
    archiveMessage pb path
    removeFile path

-- | Get the next available number for saved files.
getNextSavedNumber :: Postbox -> IO Int
getNextSavedNumber pb = do
    files <- listDirectory (savedPath pb)
    let numbers = map extractNumber files
    let maxNum = maximum (0 : numbers)
    return (maxNum + 1)

-- | Extract the leading number from a filename (e.g., "0001-foo.txt" -> 1).
extractNumber :: FilePath -> Int
extractNumber f =
    let prefix = takeWhile isDigit f
    in  case readMaybe prefix of
            Just n  -> n
            Nothing -> 0

-- | Pad a number to 4 digits (e.g., 1 -> "0001").
padNumber :: Int -> String
padNumber n =
    let s = show n
    in  replicate (4 - length s) '0' ++ s

-- | Strip leading/trailing whitespace.
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  where
    isSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'
