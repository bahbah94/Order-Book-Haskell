-- Main.hs
module Main where

import CLI.Parser
import CLI.Execute
import CLI.State
import CLI.Commands
import Control.Monad (forever)
import System.Exit (exitSuccess)
import System.Environment (getArgs)
import Options.Applicative

data LoopCommand = 
    ExecuteCmd Command
  | Exit

-- Main REPL loop
main :: IO ()
main = do
    putStrLn "Order Matching Engine - Interactive Mode"
    putStrLn "Type 'exit' to quit"
    
    -- Initialize state once
    state <- initState
    
    -- Start REPL loop
    repl state

repl :: CliState -> IO ()
repl state = do
    putStr "> "
    args <- words <$> getLine
    
    case args of
        ["exit"] -> do
            putStrLn "Exiting..."
            exitSuccess
        _ -> do
            -- Parse args into command
            let result = execParserPure defaultPrefs (info commandParser fullDesc) args
            case result of
                Success cmd -> do
                    newState <- executeCommand state cmd
                    repl newState
                Failure err -> do
                    putStrLn $ "Error: " ++ show err
                    repl state
                CompletionInvoked _ -> do
                    putStrLn "Completion not supported"
                    repl state