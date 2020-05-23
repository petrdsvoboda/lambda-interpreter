module CLI where

import           System.IO
import           Evaluator
import           LambdaParser
import           TextParser

-- | Prompts text and allows answer on same line
prompt :: IO String
prompt = do
    putStr "> "
    hFlush stdout
    getLine


run :: IO ()
run = do
    line <- prompt
    print . lambdaEval . buildExpr $ getBlocks line
    run
