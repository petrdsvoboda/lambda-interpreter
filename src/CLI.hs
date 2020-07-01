module CLI where

import           System.IO
import           Evaluator
import           Parser
import           Lexer
import           Control.Monad

import           Types
import           Data.Tuple                     ( swap )

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

answer :: [SavedMacro] -> EvalRes -> IO ()
answer macros term = putStrLn $ "< " ++ res
  where
    res = case term of
        Right t   -> toString (map swap macros) t
        Left  err -> err


compute :: [SavedMacro] -> Term -> IO EvalRes
compute macros term = do
    let evaluated = eval macros term
    case evaluated of
        Right t -> if (t == term)
            then return evaluated
            else do
                CLI.log t
                compute macros t
        _ -> return evaluated

run :: [SavedMacro] -> IO ()
run macros = do
    line <- prompt
    let (term, assignTo) = exprFromString line
    let macros' = case assignTo of
            Just x  -> macros ++ [(x, show term)]
            Nothing -> macros
    res <- compute macros' term
    answer macros' res
    run macros'
