module CLI where

import           System.IO
import           Evaluator
import           Parser
import           Lexer
import           Control.Monad

import           Types

-- | Prompts text and allows answer on same line
prompt :: IO String
prompt = do
    putStr "> "
    hFlush stdout
    getLine

log :: Term -> IO Char
log term = do
    putStr $ show term
    hFlush stdout
    getChar

answer :: Term -> IO ()
answer term = putStrLn $ "< " ++ toString term

compute :: Term -> IO Term
compute term = do
    let evaluated = eval term
    if (evaluated == term)
        then return evaluated
        else do
            CLI.log evaluated
            compute evaluated

run :: IO ()
run = do
    line <- prompt
    res  <- compute . tmap expandMacros $ fromString line
    answer res
    run
