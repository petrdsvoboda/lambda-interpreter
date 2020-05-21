module Lib
  ( getData
  )
where

import           TextParser
import           Types

expr :: String
expr = "(((\\x.(\\y.(x+y+(\\z.(z))(1))))(2))(3))"

printBlocks :: [Block] -> IO ()
printBlocks = mapM_ print

getData = print $ getBlocks expr
