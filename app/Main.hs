module Main where

import           System.Console.ANSI
import           System.Exit
import           System.Posix.Signals
import           Control.Concurrent
import qualified Control.Exception             as E
import           CLI
import           Macro

main :: IO ()
main = do
    tid <- myThreadId
    installHandler keyboardSignal (Catch (E.throwTo tid ExitSuccess)) Nothing
    run idToVal
