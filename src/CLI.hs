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
    CLI.log term
    let evaluated = eval term
    if (evaluated == term) then return evaluated else compute evaluated

run :: [SavedMacro] -> IO ()
run macros = do
    line <- prompt
    let (term, assignTo) = exprFromString line
    let macros' = case assignTo of
            Just x  -> macros ++ [(x, line)]
            Nothing -> macros
    res <- compute $ tmap (expandMacros macros') term
    answer res
    run macros'
