-- | Bird.Test - Test case definitions and registry.
-- Mirrors the Modality registry pattern for easy extension.
--
-- To add a new test:
--   1. Define a TestCase with name, description, and runner
--   2. Add it to the appropriate test suite
--   3. The bird will automatically pick it up
module Bird.Test
    ( -- * Types
      TestCase(..)
    , TestResult(..)
    , TestStatus(..)
    , TestRegistry
      -- * Registry operations
    , emptyRegistry
    , registerTest
    , registerTests
    , lookupTest
    , listTests
    , allTests
      -- * Running tests
    , runTest
    , runAllTests
      -- * Result helpers
    , passed
    , failed
    , skipped
    , formatResult
    , formatResults
    ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Exception (try, SomeException)

-- | Status of a test run
data TestStatus = Pass | Fail | Skip
    deriving (Show, Eq)

-- | Result of running a single test
data TestResult = TestResult
    { resultName    :: String      -- ^ Test name
    , resultStatus  :: TestStatus  -- ^ Pass/Fail/Skip
    , resultMessage :: String      -- ^ Details or error message
    , resultOutput  :: Maybe String -- ^ Captured output if any
    } deriving (Show, Eq)

-- | A test case definition
data TestCase = TestCase
    { testName        :: String           -- ^ Unique identifier
    , testDescription :: String           -- ^ Human-readable description
    , testModality    :: String           -- ^ Which modality this tests
    , testRunner      :: IO TestResult    -- ^ The test execution
    }

-- | Registry of available tests
newtype TestRegistry = TestRegistry (Map String TestCase)

-- | Create an empty registry
emptyRegistry :: TestRegistry
emptyRegistry = TestRegistry Map.empty

-- | Register a test
registerTest :: TestCase -> TestRegistry -> TestRegistry
registerTest t (TestRegistry reg) =
    TestRegistry (Map.insert (testName t) t reg)

-- | Register multiple tests
registerTests :: [TestCase] -> TestRegistry -> TestRegistry
registerTests ts reg = foldr registerTest reg ts

-- | Look up a test by name
lookupTest :: String -> TestRegistry -> Maybe TestCase
lookupTest name (TestRegistry reg) = Map.lookup name reg

-- | List all test names
listTests :: TestRegistry -> [String]
listTests (TestRegistry reg) = Map.keys reg

-- | Get all test cases
allTests :: TestRegistry -> [TestCase]
allTests (TestRegistry reg) = Map.elems reg

-- | Run a single test with exception handling
runTest :: TestCase -> IO TestResult
runTest tc = do
    result <- try (testRunner tc)
    case result of
        Right r  -> return r
        Left err -> return $ failed (testName tc)
            ("Exception: " ++ show (err :: SomeException))

-- | Run all tests in a registry
runAllTests :: TestRegistry -> IO [TestResult]
runAllTests reg = mapM runTest (allTests reg)

-- | Create a passing result
passed :: String -> String -> TestResult
passed name msg = TestResult name Pass msg Nothing

-- | Create a failing result
failed :: String -> String -> TestResult
failed name msg = TestResult name Fail msg Nothing

-- | Create a skipped result
skipped :: String -> String -> TestResult
skipped name msg = TestResult name Skip msg Nothing

-- | Format a single result for display
formatResult :: TestResult -> String
formatResult r = statusIcon ++ " " ++ resultName r ++ ": " ++ resultMessage r
  where
    statusIcon = case resultStatus r of
        Pass -> "[PASS]"
        Fail -> "[FAIL]"
        Skip -> "[SKIP]"

-- | Format multiple results with summary
formatResults :: [TestResult] -> String
formatResults rs = unlines $
    ["", "Test Results:", replicate 40 '-'] ++
    map formatResult rs ++
    [replicate 40 '-', summary]
  where
    passes = length $ filter ((== Pass) . resultStatus) rs
    fails  = length $ filter ((== Fail) . resultStatus) rs
    skips  = length $ filter ((== Skip) . resultStatus) rs
    total  = length rs
    summary = "Total: " ++ show total ++
              " | Passed: " ++ show passes ++
              " | Failed: " ++ show fails ++
              " | Skipped: " ++ show skips
