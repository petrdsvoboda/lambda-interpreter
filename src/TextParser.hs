module TextParser
    ( getBlocks
    )
where

import qualified Data.List                     as List
import           Chars
import           Types

getBlock :: (Int, String, [String]) -> Char -> (Int, String, [String])
getBlock (d, expr, bs) c
    | c == bgnBlockChar && d == 0 && expr == "" = (d + 1, expr, bs)
    | c == bgnBlockChar && d == 0 = (d + 1, "", bs ++ [expr])
    | c == bgnBlockChar           = (d + 1, expr ++ [c], bs)
    | c == endBlockChar && d == 1 = (d - 1, "", bs ++ [expr])
    | c == endBlockChar           = (d - 1, expr ++ [c], bs)
    | otherwise                   = (d, expr ++ [c], bs)

getBlocks :: String -> [Block]
getBlocks text = map getSubBlocks bs
  where
    getSubBlocks :: String -> Block
    getSubBlocks b = case List.find (== bgnBlockChar) b of
        Just _  -> SubBlocks (getBlocks b)
        Nothing -> BlockText b
    (_, _, bs) = foldl getBlock (0, "", []) text
