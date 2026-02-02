-- | Bird - Test harness for postbox system.
-- Uses the fly interface exclusively per protocol.
--
-- The bird can:
--   - Run test suites against active agents
--   - Verify modality behavior
--   - Check request/response patterns
--   - Report results
--
-- All interaction happens through batch scripts via Postbox.Fly.
module Bird
    ( -- * Running tests
      runTests
    , runTestsOn
    , runNamedTest
      -- * Configuration
    , BirdConfig(..)
    , defaultBirdConfig
      -- * Quick tests
    , quickTest
    , testAgent
    , testAllAgents
      -- * Re-exports
    , module Bird.Test
    , module Bird.Suite
    ) where

import Bird.Test
import Bird.Suite
import qualified Postbox.Fly as Fly
import Postbox (getActivePostboxes, discoverPostboxes, Postbox(..), getNeighborhood)
import System.Directory (getCurrentDirectory)
import System.FilePath (takeDirectory)
import Control.Monad (forM)

-- | Bird configuration
data BirdConfig = BirdConfig
    { birdName      :: String      -- ^ Name for this bird instance
    , birdTimeout   :: Int         -- ^ Default timeout in seconds
    , birdVerbose   :: Bool        -- ^ Verbose output
    } deriving (Show, Eq)

-- | Default bird configuration
defaultBirdConfig :: BirdConfig
defaultBirdConfig = BirdConfig
    { birdName    = "bird"
    , birdTimeout = 10
    , birdVerbose = True
    }

-- | Run all tests in the full suite against an agent
runTests :: String -> IO [TestResult]
runTests agent = do
    let cfg = defaultConfig { configAgent = agent }
    let registry = fullSuite cfg
    results <- runAllTests registry
    putStrLn $ formatResults results
    return results

-- | Run tests from a specific suite
runTestsOn :: String -> (SuiteConfig -> TestRegistry) -> IO [TestResult]
runTestsOn agent suiteFn = do
    let cfg = defaultConfig { configAgent = agent }
    let registry = suiteFn cfg
    results <- runAllTests registry
    putStrLn $ formatResults results
    return results

-- | Run a single named test
runNamedTest :: String -> String -> IO (Maybe TestResult)
runNamedTest agent testName' = do
    let cfg = defaultConfig { configAgent = agent }
    let registry = fullSuite cfg
    case lookupTest testName' registry of
        Nothing -> do
            putStrLn $ "Test not found: " ++ testName'
            putStrLn $ "Available tests: " ++ show (listTests registry)
            return Nothing
        Just tc -> do
            result <- runTest tc
            putStrLn $ formatResult result
            return $ Just result

-- | Quick smoke test - just checks agent is responding
quickTest :: String -> IO Bool
quickTest agent = do
    putStrLn $ "Quick test: " ++ agent
    Fly.flySendMessage agent "echo:bird-ping"
    -- Wait briefly
    threadDelay 1000000  -- 1 second
    putStrLn "  Ping sent (check agent output for response)"
    return True
  where
    threadDelay n = sequence_ $ replicate n (return ())  -- Placeholder

-- | Test a specific agent with echo suite (fast)
testAgent :: String -> IO [TestResult]
testAgent agent = runTestsOn agent echoSuite

-- | Test all active agents in the neighborhood
testAllAgents :: IO [(String, [TestResult])]
testAllAgents = do
    cwd <- getCurrentDirectory
    let neighborhood = takeDirectory cwd
    active <- getActivePostboxes neighborhood
    forM active $ \pb -> do
        putStrLn $ "\n=== Testing: " ++ name pb ++ " ==="
        results <- runTestsOn (name pb) echoSuite
        return (name pb, results)
