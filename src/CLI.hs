module CLI where

import           System.IO
import           Evaluator
import           Parser
import           Lexer
import           Control.Monad

import           Types
import           Data.Tuple                     ( swap )
import qualified Data.List                     as List

-- | Prompts text and allows answer on same line
prompt :: IO String
prompt = do
    putStr "> "
    hFlush stdout
    getLine

log :: Term -> IO ()
log term = do
    putStr $ show term
    hFlush stdout
    getChar
    return ()

answer :: MacroHeap -> Either String String -> IO ()
answer macros res = case res of
    Right x   -> putStrLn $ "< " ++ x
    Left  err -> putStrLn err


compute :: ProgramFlags -> MacroHeap -> Term -> IO (Either String String)
compute flags macros term = do
    let evaluated = eval macros term
    case evaluated of
        Right t -> if (t == term)
            then return (Right $ toString macros t)
            else do
                unless (quiet flags) (CLI.log t)
                compute flags macros t
        Left err -> return (Left err)

run :: ProgramFlags -> MacroHeap -> IO ()
run flags macros = do
    line <- prompt
    let (term, assignTo) = exprFromString line
    let macros' = case assignTo of
            Just x  -> macros ++ [(x, show term, term)]
            Nothing -> macros
    res <- compute flags macros' term
    answer macros' res
    run flags macros'
