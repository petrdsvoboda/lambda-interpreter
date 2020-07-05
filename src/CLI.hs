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

-- | Prints current term and waits for any input
log :: Term -> IO ()
log term = do
    putStr $ show term
    hFlush stdout
    getChar
    return ()

-- | Prints term or errors
answer :: MacroHeap -> ProgramStep -> IO ()
answer macros res = case res of
    Right x   -> putStrLn $ "< " ++ x
    Left  err -> putStrLn err


compute :: ProgramFlags -> MacroHeap -> Term -> IO ProgramStep
compute flags macros term = do
    let evaluated = eval macros term
    case evaluated of
        Right t -> if (t == term)
            then return (Right $ toString macros t)
            else do
                unless (quiet flags) (CLI.log t)
                compute flags macros t
        Left err -> return (Left err)

-- | Runs program loop, saving macros
-- Uses flags for output
-- quiet - no intermediate answers
run :: ProgramFlags -> MacroHeap -> IO ()
run flags macros = do
    line <- prompt
    let input = fromStringChecked line
    -- | Evaluate user input and provide response
    case input of
        Left err -> do
            putStrLn ("Error: " ++ err)
            run flags macros
        Right expr -> do
            let (term, assignTo) = expr
            let macros' = case assignTo of -- Save new macro
                    Just x  -> macros ++ [(x, show term, term)]
                    Nothing -> macros
            res <- compute flags macros' term
            answer macros' res
            run flags macros'
