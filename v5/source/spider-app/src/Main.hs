module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, takeFileName)
import System.Directory (getCurrentDirectory)
import Postbox (Postbox(..), startup, runFast, inboxPath, outboxPath, savedPath,
                discoverPostboxes, getActivePostboxes, readMessage, writeMessage, deleteMessage,
                sendMessageToName, sendToAllActive)
import Spider.Pipeline (Provider(..), processFile, runPipeline, runRawQuery)
import qualified Postbox.Fly as Fly
import Postbox.Modality (Modality(..), ModalityRegistry, emptyRegistry, registerModality,
                         lookupModality, parseModality, defaultModality,
                         modalityEcho, modalityLLMRaw, modalityLLMWiki, modalityFly)
import Postbox.Request (Request(..), RequestType(..), parseRequest, parseFilename, responseFilename)
import Postbox.Compare (CompareResult(..), compareMessages, scoreByLength, scoreByHex, formatResults)
import qualified Words

-- | Available modes (default processing mode for this agent)
data Mode = LLMMode Provider | EchoMode | WordsMode
    deriving (Show, Eq)

-- | Parse mode from string
parseMode :: String -> Maybe Mode
parseMode "groq"        = Just (LLMMode Groq)
parseMode "huggingface" = Just (LLMMode HuggingFace)
parseMode "hf"          = Just (LLMMode HuggingFace)
parseMode "echo"        = Just EchoMode
parseMode "test"        = Just EchoMode
parseMode "words"       = Just WordsMode
parseMode _             = Nothing

-- | Show usage
usage :: IO ()
usage = do
    putStrLn "Usage: spider <name> <mode>"
    putStrLn ""
    putStrLn "  name  - Name for this postbox (e.g., agent-1, groq-bot)"
    putStrLn "  mode  - Default processing mode:"
    putStrLn "          groq | huggingface | hf  - LLM modes available"
    putStrLn "          echo | test              - Fast echo for testing"
    putStrLn "          words                    - Word generation mode"
    putStrLn ""
    putStrLn "Message Syntax:"
    putStrLn "  content              - Use default modality"
    putStrLn "  modality:content     - Use specific modality"
    putStrLn "  ?modality:content    - Request expecting response"
    putStrLn "  @agent message       - Route to another agent"
    putStrLn "  fly:command args     - Execute fly command"
    putStrLn ""
    putStrLn "Modalities (LLM mode):"
    putStrLn "  echo      - Simple echo"
    putStrLn "  llm-raw   - Raw LLM query (direct to LLM)"
    putStrLn "  llm-wiki  - Wiki pipeline (keyword -> URL -> scrape -> summarize)"
    putStrLn "  compare   - Compare multiple messages (separated by |||) and score"
    putStrLn "  fly       - Execute fly commands"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn "  spider agent-groq groq       # Start LLM agent"
    putStrLn "  spider agent-test echo       # Start echo agent"
    putStrLn ""
    putStrLn "  Message: \"quantum computing\"       -> uses default (llm-wiki)"
    putStrLn "  Message: \"llm-raw:tell me a joke\"  -> raw LLM query"
    putStrLn "  Message: \"?echo:ping\"              -> echo, send response back"
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        [pbName, modeStr] -> do
            case parseMode modeStr of
                Nothing -> do
                    putStrLn $ "Unknown mode: " ++ modeStr
                    usage
                Just mode -> runAgent pbName mode
        _ -> usage

-- | Run an agent with the given name and mode
runAgent :: String -> Mode -> IO ()
runAgent pbName mode = do
    cwd <- getCurrentDirectory
    let postbox = Postbox
            { name     = pbName
            , root     = cwd
            , contacts = []
            }

    startup postbox

    putStrLn $ "[" ++ name postbox ++ "] Online. Mode: " ++ showMode mode
    putStrLn $ "[" ++ name postbox ++ "] Watching: " ++ inboxPath postbox ++ " -> " ++ outboxPath postbox
    putStrLn $ "[" ++ name postbox ++ "] Saved to: " ++ savedPath postbox
    putStrLn $ "[" ++ name postbox ++ "] Neighborhood: " ++ takeDirectory (root postbox)
    putStrLn $ "[" ++ name postbox ++ "] Send 'pkill-<sender>.txt' to inbox to stop."
    putStrLn ""

    -- Show who else is online at startup
    showNeighbors postbox

    -- Run with appropriate handler (using modality system)
    case mode of
        LLMMode provider -> runFast postbox (handleMessage postbox (llmRegistry postbox provider))
        EchoMode         -> runFast postbox (handleMessage postbox (echoRegistry postbox))
        WordsMode        -> runFast postbox (handleMessage postbox (wordsRegistry postbox))

-- | Show mode as string
showMode :: Mode -> String
showMode (LLMMode p) = "LLM (" ++ show p ++ ")"
showMode EchoMode    = "Echo (fast test)"
showMode WordsMode   = "Words (generative)"

-- | Build modality registry for echo mode
echoRegistry :: Postbox -> ModalityRegistry
echoRegistry pb = foldr registerModality emptyRegistry
    [ Modality modalityEcho "Simple echo"
        (\_ content -> return $ "[" ++ name pb ++ " echo] " ++ content)
    , Modality modalityFly "Execute fly commands"
        (\_ content -> case words content of
            (cmd:args) -> executeFly cmd args
            []         -> return "[fly] ERROR: no command")
    , Modality "compare" "Compare multiple messages and score responses"
        (\pbName content -> do
            let (scorer, msgs) = parseCompare content
            let processor msg = return $ "[" ++ pbName ++ " echo] " ++ msg
            results <- compareMessages processor scorer msgs
            return $ formatResults results)
    , Modality "default" "Default echo mode"
        (\_ content -> return $ "[" ++ name pb ++ " echo] " ++ content)
    ]

-- | Build modality registry for words mode (generative)
wordsRegistry :: Postbox -> ModalityRegistry
wordsRegistry pb = foldr registerModality emptyRegistry
    [ Modality modalityEcho "Simple echo"
        (\_ content -> return $ "[" ++ name pb ++ " echo] " ++ content)
    , Modality modalityFly "Execute fly commands"
        (\_ content -> case words content of
            (cmd:args) -> executeFly cmd args
            []         -> return "[fly] ERROR: no command")
    , Modality "words" "Generate words from input"
        (\_ content -> do
            -- Use input as seed, generate words based on hex score
            generated <- Words.generateBatch 5
            return $ "[" ++ name pb ++ " words] " ++ unwords generated)
    , Modality "compare" "Compare messages using words generation"
        (\pbName content -> do
            let (scorer, msgs) = parseCompare content
            let processor msg = do
                    -- Generate words for each message
                    generated <- Words.generateBatch 3
                    return $ "[" ++ pbName ++ " words] " ++ unwords generated
            results <- compareMessages processor scorer msgs
            return $ formatResults results)
    , Modality "default" "Default: generate words"
        (\_ content -> do
            generated <- Words.generateBatch 5
            return $ "[" ++ name pb ++ " words] " ++ unwords generated)
    ]

-- | Build modality registry for LLM mode
llmRegistry :: Postbox -> Provider -> ModalityRegistry
llmRegistry pb provider = foldr registerModality emptyRegistry
    [ Modality modalityEcho "Simple echo"
        (\_ content -> return $ "[" ++ name pb ++ " echo] " ++ content)
    , Modality modalityFly "Execute fly commands"
        (\_ content -> case words content of
            (cmd:args) -> executeFly cmd args
            []         -> return "[fly] ERROR: no command")
    , Modality modalityLLMRaw "Raw LLM query"
        (\_ content -> do
            result <- runRawQuery provider content
            case result of
                Right response -> return response
                Left err -> return $ "[llm-raw ERROR] " ++ err)
    , Modality modalityLLMWiki "Wiki pipeline (keyword -> URL -> scrape -> summarize)"
        (\_ content -> do
            result <- runPipeline provider content
            case result of
                Right summary -> return summary
                Left err -> return $ "[llm-wiki ERROR] " ++ err)
    , Modality "compare" "Compare multiple messages via LLM and score responses"
        (\_ content -> do
            let (scorer, msgs) = parseCompare content
            let processor msg = do
                    result <- runRawQuery provider msg
                    case result of
                        Right response -> return response
                        Left err -> return $ "[ERROR] " ++ err
            results <- compareMessages processor scorer msgs
            return $ formatResults results)
    , Modality "words-compare" "Generate words for each message, then LLM compares them"
        (\_ content -> do
            -- Split messages
            let msgs = splitOn ";;;" content
            -- Generate words for each message
            generatedList <- mapM (\_ -> Words.generateBatch 5) msgs
            let wordSets = map unwords generatedList
            -- Build LLM comparison prompt
            let numbered = zipWith (\i ws -> show i ++ ". " ++ ws) [1..] wordSets
            let prompt = "Compare these word sets and rank them from best to worst. " ++
                        "Explain briefly why each ranks where it does:\n\n" ++
                        unlines numbered
            -- Ask LLM to compare
            result <- runRawQuery provider prompt
            case result of
                Right response -> return $ "[words-compare]\n" ++
                    "Generated:\n" ++ unlines numbered ++ "\nLLM Comparison:\n" ++ response
                Left err -> return $ "[words-compare ERROR] " ++ err)
    , Modality "default" "Default: wiki pipeline"
        (\_ content -> do
            result <- runPipeline provider content
            case result of
                Right summary -> return summary
                Left err -> return $ "[ERROR] " ++ err)
    ]

-- | Universal message handler using modality system
-- Handles:
--   "?modality:content"  - Request expecting response
--   "modality:content"   - One-way with modality
--   "@agent message"     - Route to another agent
--   "content"            - Use default modality
handleMessage :: Postbox -> ModalityRegistry -> FilePath -> FilePath -> IO ()
handleMessage pb registry inPath outPath = do
    putStrLn $ "Processing: " ++ inPath
    input <- readMessage inPath
    let sender = parseFilename (takeFileName inPath)
    let request = parseRequest input

    -- Check for routing syntax first (takes precedence)
    case parseRoute input of
        Just (targetName, msg) -> do
            putStrLn $ "  Routing to: " ++ targetName
            let filename = "from-" ++ name pb ++ ".txt"
            sent <- sendMessageToName pb targetName filename msg
            if sent
                then do
                    writeMessage outPath $ "[routed to " ++ targetName ++ "] " ++ msg
                    deleteMessage pb inPath
                    putStrLn $ "Done: routed to " ++ targetName
                else do
                    writeMessage outPath $ "[ERROR: agent not found: " ++ targetName ++ "] " ++ msg
                    deleteMessage pb inPath
                    putStrLn $ "Error: agent not found"
        Nothing -> do
            -- Dispatch to modality
            let modName = requestModality request
            let content = requestContent request
            putStrLn $ "  Modality: " ++ modName

            case lookupModality modName registry of
                Nothing -> do
                    let err = "[ERROR: unknown modality: " ++ modName ++ "]"
                    writeMessage outPath err
                    deleteMessage pb inPath
                    putStrLn $ "Error: " ++ err
                Just modality -> do
                    result <- modalityHandler modality (name pb) content
                    writeMessage outPath result
                    deleteMessage pb inPath

                    -- If request expects response, send back to sender
                    case (requestType request, sender) of
                        (RequestExpectsResponse, Just senderName) -> do
                            let respFile = responseFilename (name pb) senderName
                            sent <- sendMessageToName pb senderName respFile result
                            if sent
                                then putStrLn $ "  Response sent to: " ++ senderName
                                else putStrLn $ "  Warning: could not send response to " ++ senderName
                        _ -> return ()

                    putStrLn $ "Done: " ++ modName

-- | Fast echo handler (legacy compatibility)
echoFile :: Postbox -> FilePath -> FilePath -> IO ()
echoFile pb = handleMessage pb (echoRegistry pb)

-- | Parse fly command: "fly:command arg1 arg2" -> Just (command, [args])
parseFly :: String -> Maybe (String, [String])
parseFly s = case stripPrefix "fly:" s of
    Just rest -> case words rest of
        (cmd:args) -> Just (cmd, args)
        []         -> Nothing
    Nothing -> Nothing
  where
    stripPrefix prefix str
        | take (length prefix) str == prefix = Just (drop (length prefix) str)
        | otherwise = Nothing

-- | Execute a fly command and return the result
executeFly :: String -> [String] -> IO String
executeFly "status" _     = Fly.flyStatus
executeFly "list"   _     = Fly.flyList
executeFly "kill-all" _   = Fly.flyKillAll >> return "[fly] kill-all executed"
executeFly "send" (agent:msgParts) = do
    let msg = unwords msgParts
    Fly.flySendMessage agent msg
    return $ "[fly] sent to " ++ agent ++ ": " ++ msg
executeFly "send" [] = return "[fly] ERROR: send requires agent and message"
executeFly "send-all" msgParts = do
    let msg = unwords msgParts
    result <- Fly.flySendAll msg
    return $ "[fly] broadcast: " ++ result
executeFly "start" [agent, mode] = do
    Fly.flyStartAgent agent mode
    return $ "[fly] started " ++ agent ++ " in " ++ mode ++ " mode"
executeFly "start" _ = return "[fly] ERROR: start requires agent and mode"
executeFly "ping" [agent] = do
    Fly.flyPing agent
    return $ "[fly] pinged " ++ agent
executeFly "ping" _ = return "[fly] ERROR: ping requires agent name"
executeFly cmd _ = return $ "[fly] ERROR: unknown command: " ++ cmd

-- | Parse routing syntax: "@agent-name message" -> Just (agent, message)
parseRoute :: String -> Maybe (String, String)
parseRoute s = case s of
    ('@':rest) -> case break (== ' ') rest of
        (agent, ' ':msg) | not (null agent) -> Just (agent, msg)
        _ -> Nothing
    _ -> Nothing

-- | Display active neighbors
showNeighbors :: Postbox -> IO ()
showNeighbors pb = do
    -- Neighborhood is parent directory (where sibling postboxes live)
    let neighborhood = takeDirectory (root pb)
    active <- getActivePostboxes neighborhood
    let others = filter (\p -> name p /= name pb) active
    if null others
        then putStrLn $ "[" ++ name pb ++ "] No other postboxes online."
        else do
            putStrLn $ "[" ++ name pb ++ "] Active neighbors:"
            mapM_ (\p -> putStrLn $ "  - " ++ name p) others

-- | Split a string on a delimiter
splitOn :: String -> String -> [String]
splitOn sep str = go str
  where
    sepLen = length sep
    go "" = []
    go s = case breakOn sep s of
        (before, "") -> [before]
        (before, after) -> before : go (drop sepLen after)

    breakOn needle haystack = go' "" haystack
      where
        needleLen = length needle
        go' acc [] = (reverse acc, "")
        go' acc s'@(c:cs)
            | take needleLen s' == needle = (reverse acc, s')
            | otherwise = go' (c:acc) cs

-- | Parse compare content: "scorer:msg1;;;msg2" or "msg1;;;msg2"
-- Supported scorers: "hex" (default), "length"
-- Format: compare:hex:msg1;;;msg2 or compare:msg1;;;msg2
parseCompare :: String -> (String -> Double, [String])
parseCompare content =
    case break (== ':') content of
        ("hex", ':':rest)    -> (scoreByHex, splitOn ";;;" rest)
        ("length", ':':rest) -> (scoreByLength, splitOn ";;;" rest)
        _                    -> (scoreByHex, splitOn ";;;" content)  -- default to hex
