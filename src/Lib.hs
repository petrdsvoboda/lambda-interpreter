module Lib
  ( getData
  )
where

import           TextParser
import           Types

expr :: String
expr = "(\\x.x+x)(1)(2)(\\x.(\\y.x+y))(\\x.(\\y.(\\z.x+y+z)))"

printBlocks :: [Block] -> IO ()
printBlocks b = mapM_ printBlock b
 where
  printBlock :: Block -> IO ()
  printBlock b = case b of
    BlockText t  -> putStrLn t
    SubBlocks bs -> printBlocks bs

getData = printBlocks $ getBlocks expr
