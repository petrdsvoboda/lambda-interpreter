module Lib
  ( getData
  , getExpr
  , expr
  )
where

import           CLI
import           LambdaParser
import           Lexer
import           Types

expr :: String
expr = "((\\x.(\\y.(x+y+(\\z.(z))(1))))(2))(3)"

printBlocks :: [Block] -> IO ()
printBlocks = mapM_ print

getData = print $ getBlocks expr
getExpr = print . buildExpr $ getBlocks expr
