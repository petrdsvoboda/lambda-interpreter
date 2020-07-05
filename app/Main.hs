module Main where

import           System.Environment
import           System.Console.ANSI
import           System.Exit
import           System.Posix.Signals
import           Control.Concurrent
import qualified Control.Exception             as E
import           CLI
import           Macro
import           Types

-- | Run program with defined macros
-- listen for Ctrl+C
-- enable quiet mode with -q arg
main :: IO ()
main = do
    args <- getArgs
    tid  <- myThreadId
    installHandler keyboardSignal (Catch (E.throwTo tid ExitSuccess)) Nothing
    run (ProgramFlags { quiet = "-q" `elem` args }) macroHeap
