module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Bird
import Bird.Test (TestStatus(..), resultStatus)
import qualified Postbox.Fly as Fly

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["test", agent] -> do
            results <- runTests agent
            if any ((== Fail) . resultStatus) results
                then exitFailure
                else exitSuccess

        ["test", agent, suite] -> do
            results <- case suite of
                "echo"     -> runTestsOn agent echoSuite
                "fly"      -> runTestsOn agent flySuite
                "llm-raw"  -> runTestsOn agent llmRawSuite
                "llm-wiki" -> runTestsOn agent llmWikiSuite
                "request"  -> runTestsOn agent requestSuite
                "compare"  -> runTestsOn agent compareSuite
                "full"     -> runTestsOn agent fullSuite
                _          -> do
                    putStrLn $ "Unknown suite: " ++ suite
                    usage
                    return []
            if any ((== Fail) . resultStatus) results
                then exitFailure
                else exitSuccess

        ["run", testName', agent] -> do
            result <- runNamedTest agent testName'
            case result of
                Just r | resultStatus r == Fail -> exitFailure
                _ -> exitSuccess

        ["ping", agent] -> do
            _ <- quickTest agent
            return ()

        ["list"] -> do
            putStrLn "Available test suites:"
            putStrLn "  echo     - Echo modality tests (fast)"
            putStrLn "  fly      - Fly command tests"
            putStrLn "  llm-raw  - Raw LLM query tests"
            putStrLn "  llm-wiki - Wiki pipeline tests"
            putStrLn "  request  - Request/response pattern tests"
            putStrLn "  compare  - Compare/score multiple messages"
            putStrLn "  full     - All tests"
            putStrLn ""
            putStrLn "Available tests in full suite:"
            let registry = fullSuite defaultConfig
            mapM_ (\t -> putStrLn $ "  " ++ t) (listTests registry)

        ["status"] -> do
            output <- Fly.flyStatus
            putStrLn output

        ["agents"] -> do
            output <- Fly.flyList
            putStrLn output

        ["send", agent, msg] -> do
            Fly.flySendMessage agent msg
            putStrLn $ "Sent to " ++ agent ++ ": " ++ msg

        ["send-all", msg] -> do
            result <- Fly.flySendAll msg
            putStrLn result

        _ -> usage

usage :: IO ()
usage = do
    putStrLn "bird - Test harness for postbox system"
    putStrLn ""
    putStrLn "Usage:"
    putStrLn "  bird test <agent>              Run full test suite on agent"
    putStrLn "  bird test <agent> <suite>      Run specific suite on agent"
    putStrLn "  bird run <test-name> <agent>   Run single named test"
    putStrLn "  bird ping <agent>              Quick echo test"
    putStrLn "  bird list                      List available tests/suites"
    putStrLn ""
    putStrLn "Fly commands (via batch scripts):"
    putStrLn "  bird status                    Show all postbox status"
    putStrLn "  bird agents                    List available postboxes"
    putStrLn "  bird send <agent> <message>    Send message to agent"
    putStrLn "  bird send-all <message>        Broadcast to all agents"
    putStrLn ""
    putStrLn "Test suites:"
    putStrLn "  echo, fly, llm-raw, llm-wiki, request, compare, full"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn "  bird test agent-test echo      # Run echo tests on agent-test"
    putStrLn "  bird ping agent-groq           # Quick ping test"
    putStrLn "  bird send agent-1 'echo:hello' # Send echo message"
    putStrLn "  bird send agent-1 '?echo:ping' # Send request (expects response)"
    exitFailure
