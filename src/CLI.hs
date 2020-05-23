module CLI where

import           System.IO
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
    print . buildExpr $ getBlocks line
    run
