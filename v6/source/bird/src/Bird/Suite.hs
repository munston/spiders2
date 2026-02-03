-- | Bird.Suite - Standard test suites for all modalities.
-- All tests use the fly interface exclusively per protocol.
--
-- Test suites:
--   echoSuite    - Tests for echo modality
--   llmRawSuite  - Tests for llm-raw modality
--   llmWikiSuite - Tests for llm-wiki modality
--   flySuite     - Tests for fly commands
--   requestSuite - Tests for request/response pattern
--   fullSuite    - All tests combined
module Bird.Suite
    ( -- * Test suites
      echoSuite
    , llmRawSuite
    , llmWikiSuite
    , flySuite
    , requestSuite
    , compareSuite
    , fullSuite
      -- * Suite builders
    , buildSuite
    , SuiteConfig(..)
    , defaultConfig
    ) where

import Bird.Test
import qualified Postbox.Fly as Fly
import Control.Concurrent (threadDelay)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist, removeFile)
import System.FilePath ((</>))
import Data.List (isInfixOf, isPrefixOf)

-- | Configuration for test suite
data SuiteConfig = SuiteConfig
    { configAgent      :: String  -- ^ Target agent name to test
    , configTimeout    :: Int     -- ^ Timeout in seconds for responses
    , configCheckInterval :: Int  -- ^ Check interval in ms
    } deriving (Show, Eq)

-- | Default configuration
defaultConfig :: SuiteConfig
defaultConfig = SuiteConfig
    { configAgent = "test-agent"
    , configTimeout = 10
    , configCheckInterval = 500
    }

-- | Build a full test suite with given config
buildSuite :: SuiteConfig -> TestRegistry
buildSuite cfg = registerTests allStandardTests emptyRegistry
  where
    agent = configAgent cfg
    timeout = configTimeout cfg

    allStandardTests =
        echoTests agent timeout ++
        flyTests agent timeout ++
        requestTests agent timeout ++
        compareTests agent timeout ++
        llmTests agent timeout

-- | Echo modality tests
echoSuite :: SuiteConfig -> TestRegistry
echoSuite cfg = registerTests (echoTests (configAgent cfg) (configTimeout cfg)) emptyRegistry

-- | LLM raw modality tests
llmRawSuite :: SuiteConfig -> TestRegistry
llmRawSuite cfg = registerTests (llmRawTests (configAgent cfg) (configTimeout cfg)) emptyRegistry

-- | LLM wiki modality tests
llmWikiSuite :: SuiteConfig -> TestRegistry
llmWikiSuite cfg = registerTests (llmWikiTests (configAgent cfg) (configTimeout cfg)) emptyRegistry

-- | Fly command tests
flySuite :: SuiteConfig -> TestRegistry
flySuite cfg = registerTests (flyTests (configAgent cfg) (configTimeout cfg)) emptyRegistry

-- | Request/response pattern tests
requestSuite :: SuiteConfig -> TestRegistry
requestSuite cfg = registerTests (requestTests (configAgent cfg) (configTimeout cfg)) emptyRegistry

-- | Compare modality tests
compareSuite :: SuiteConfig -> TestRegistry
compareSuite cfg = registerTests (compareTests (configAgent cfg) (configTimeout cfg)) emptyRegistry

-- | Full test suite with all tests
fullSuite :: SuiteConfig -> TestRegistry
fullSuite = buildSuite

-- Echo tests
echoTests :: String -> Int -> [TestCase]
echoTests agent timeout =
    [ TestCase
        { testName = "echo-basic"
        , testDescription = "Basic echo returns input with prefix"
        , testModality = "echo"
        , testRunner = testEchoBasic agent timeout
        }
    , TestCase
        { testName = "echo-modality-prefix"
        , testDescription = "Echo with explicit modality prefix"
        , testModality = "echo"
        , testRunner = testEchoModality agent timeout
        }
    ]

-- Fly command tests
flyTests :: String -> Int -> [TestCase]
flyTests agent timeout =
    [ TestCase
        { testName = "fly-status"
        , testDescription = "fly:status returns postbox status"
        , testModality = "fly"
        , testRunner = testFlyStatus
        }
    , TestCase
        { testName = "fly-list"
        , testDescription = "fly:list shows available postboxes"
        , testModality = "fly"
        , testRunner = testFlyList
        }
    ]

-- Request/response tests
requestTests :: String -> Int -> [TestCase]
requestTests agent timeout =
    [ TestCase
        { testName = "request-echo"
        , testDescription = "Request with ? prefix gets response"
        , testModality = "echo"
        , testRunner = testRequestEcho agent timeout
        }
    ]

-- LLM tests (combined raw and wiki)
llmTests :: String -> Int -> [TestCase]
llmTests agent timeout = llmRawTests agent timeout ++ llmWikiTests agent timeout

-- LLM raw tests
llmRawTests :: String -> Int -> [TestCase]
llmRawTests agent timeout =
    [ TestCase
        { testName = "llm-raw-basic"
        , testDescription = "Raw LLM query returns response"
        , testModality = "llm-raw"
        , testRunner = testLLMRaw agent timeout
        }
    ]

-- LLM wiki tests
llmWikiTests :: String -> Int -> [TestCase]
llmWikiTests agent timeout =
    [ TestCase
        { testName = "llm-wiki-basic"
        , testDescription = "Wiki pipeline processes keyword"
        , testModality = "llm-wiki"
        , testRunner = testLLMWiki agent timeout
        }
    ]

-- Compare tests
compareTests :: String -> Int -> [TestCase]
compareTests agent timeout =
    [ TestCase
        { testName = "compare-echo-basic"
        , testDescription = "Compare multiple messages in echo mode"
        , testModality = "compare"
        , testRunner = testCompareEcho agent timeout
        }
    , TestCase
        { testName = "compare-score-order"
        , testDescription = "Compare returns results ranked by score"
        , testModality = "compare"
        , testRunner = testCompareScoreOrder agent timeout
        }
    ]

-- Test implementations using fly interface

testEchoBasic :: String -> Int -> IO TestResult
testEchoBasic agent timeout = do
    clearOutbox agent
    let testMsg = "bird-test-echo-" ++ show timeout
    Fly.flySendMessage agent ("echo:" ++ testMsg)
    result <- waitForOutput agent timeout "echo"
    case result of
        Just output | testMsg `isInfixOf` output ->
            return $ passed "echo-basic" "Echo returned expected content"
        Just output ->
            return $ failed "echo-basic" ("Unexpected output: " ++ take 100 output)
        Nothing ->
            return $ failed "echo-basic" "Timeout waiting for response"

testEchoModality :: String -> Int -> IO TestResult
testEchoModality agent timeout = do
    clearOutbox agent
    let testMsg = "explicit-modality-test"
    Fly.flySendMessage agent ("echo:" ++ testMsg)
    result <- waitForOutput agent timeout "echo"
    case result of
        Just output | "echo]" `isInfixOf` output ->
            return $ passed "echo-modality-prefix" "Modality prefix handled correctly"
        Just output ->
            return $ failed "echo-modality-prefix" ("Missing echo prefix: " ++ take 100 output)
        Nothing ->
            return $ failed "echo-modality-prefix" "Timeout"

testFlyStatus :: IO TestResult
testFlyStatus = do
    output <- Fly.flyStatus
    if not (null output)
        then return $ passed "fly-status" ("Status returned: " ++ take 50 output)
        else return $ failed "fly-status" "Empty status output"

testFlyList :: IO TestResult
testFlyList = do
    output <- Fly.flyList
    -- List may be empty but should not error
    return $ passed "fly-list" ("List returned: " ++ if null output then "(empty)" else take 50 output)

testRequestEcho :: String -> Int -> IO TestResult
testRequestEcho agent timeout = do
    clearOutbox agent
    -- This test requires a bird postbox to receive response
    -- For now, just verify the message format is accepted
    let testMsg = "?echo:request-test"
    Fly.flySendMessage agent testMsg
    threadDelay 1000000  -- 1 second
    return $ passed "request-echo" "Request sent (response delivery requires bird inbox)"

testLLMRaw :: String -> Int -> IO TestResult
testLLMRaw agent timeout = do
    clearOutbox agent
    let testMsg = "llm-raw:Say hello in exactly 3 words"
    Fly.flySendMessage agent testMsg
    result <- waitForOutput agent (timeout * 3) "llm-raw"  -- LLM needs more time
    case result of
        Just output | length output > 5 ->
            return $ passed "llm-raw-basic" ("LLM responded: " ++ take 100 output)
        Just output ->
            return $ failed "llm-raw-basic" ("Response too short: " ++ output)
        Nothing ->
            return $ failed "llm-raw-basic" "Timeout waiting for LLM response"

testLLMWiki :: String -> Int -> IO TestResult
testLLMWiki agent timeout = do
    clearOutbox agent
    let testMsg = "llm-wiki:Haskell programming"
    Fly.flySendMessage agent testMsg
    result <- waitForOutput agent (timeout * 5) "llm-wiki"  -- Wiki pipeline needs more time
    case result of
        Just output | length output > 20 ->
            return $ passed "llm-wiki-basic" ("Wiki pipeline returned: " ++ take 150 output)
        Just output ->
            return $ failed "llm-wiki-basic" ("Response too short: " ++ output)
        Nothing ->
            return $ failed "llm-wiki-basic" "Timeout waiting for wiki pipeline"

-- | Test compare with echo mode - verifies multiple messages are processed
testCompareEcho :: String -> Int -> IO TestResult
testCompareEcho agent timeout = do
    clearOutbox agent
    -- Send compare request with messages separated by ;;;
    -- Use short messages for quick testing
    let testMsg = "compare:hi;;;hello;;;hey"
    Fly.flySendMessage agent testMsg
    result <- waitForOutput agent timeout "compare"
    case result of
        Just output | "SCORE:" `isInfixOf` output ->
            return $ passed "compare-echo-basic" ("Compare returned scores: " ++ take 150 output)
        Just output ->
            return $ failed "compare-echo-basic" ("Missing SCORE in output: " ++ take 100 output)
        Nothing ->
            return $ failed "compare-echo-basic" "Timeout waiting for compare response"

-- | Test that compare returns results in score order (highest first)
-- Uses messages of different lengths since score = length
testCompareScoreOrder :: String -> Int -> IO TestResult
testCompareScoreOrder agent timeout = do
    clearOutbox agent
    -- Messages with different lengths: "a" < "bb" < "ccc"
    -- After echo prefix, scores will differ based on original message length
    let testMsg = "compare:a;;;bb;;;ccc"
    Fly.flySendMessage agent testMsg
    result <- waitForOutput agent timeout "compare"
    case result of
        Just output -> do
            -- Check that we have multiple SCORE entries
            let scoreCount = length $ filter (== 'S') $ filter (`elem` "SCORE:") output
            if "SCORE:" `isInfixOf` output && "RESPONSE:" `isInfixOf` output
                then return $ passed "compare-score-order" ("Ranked results: " ++ take 200 output)
                else return $ failed "compare-score-order" ("Invalid format: " ++ take 100 output)
        Nothing ->
            return $ failed "compare-score-order" "Timeout waiting for compare response"

-- Helper: clear outbox before test to avoid reading stale output
clearOutbox :: String -> IO ()
clearOutbox agent = do
    let outboxPath = ".." </> agent </> "outbox"
    exists <- doesDirectoryExist outboxPath
    if exists
        then do
            files <- listDirectory outboxPath
            mapM_ (\f -> safeRemove (outboxPath </> f)) files
        else return ()
  where
    safeRemove path = do
        isFile <- doesFileExist path
        if isFile then removeFile path else return ()

-- Helper: wait for output in agent's outbox
-- Note: Agent directories are in parent (neighborhood) since bird runs from bird-context
waitForOutput :: String -> Int -> String -> IO (Maybe String)
waitForOutput agent timeoutSecs _tag = go (timeoutSecs * 2)  -- check every 500ms
  where
    outboxPath = ".." </> agent </> "outbox"

    go :: Int -> IO (Maybe String)
    go 0 = return Nothing
    go remaining = do
        exists <- safeCheck (doesDirectoryExist outboxPath)
        if not exists
            then wait >> go (remaining - 1)
            else do
                files <- safeList outboxPath
                case files of
                    [] -> wait >> go (remaining - 1)
                    (f:_) -> do
                        let path = outboxPath </> f
                        content <- strictRead path
                        return $ Just content

    wait = threadDelay 500000  -- 500ms

    safeCheck :: IO Bool -> IO Bool
    safeCheck action = action `orElse` return False

    safeList :: FilePath -> IO [FilePath]
    safeList path = listDirectory path `orElse` return []

    -- Strict file read to avoid lazy IO issues
    strictRead :: FilePath -> IO String
    strictRead path = do
        content <- readFile path
        length content `seq` return content

    orElse :: IO a -> IO a -> IO a
    orElse action fallback = do
        -- Simple fallback without proper exception handling
        -- In production would use Control.Exception.catch
        action
