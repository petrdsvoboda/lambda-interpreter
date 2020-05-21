module TextParser
    ( getBlocks
    )
where

import qualified Data.List                     as List
import           Chars
import           Types

getBlock :: (Int, String, [String]) -> Char -> (Int, String, [String])
getBlock (d, expr, bs) c = case c of
    '(' -> case d of
        0 -> case expr of
            "" -> (d + 1, expr, bs)
            _  -> (d + 1, "", bs ++ [expr])
        _ -> (d + 1, expr ++ [c], bs)
    ')' -> case d of
        1 -> (d - 1, "", bs ++ [expr])
        _ -> (d - 1, expr ++ [c], bs)
    _ -> (d, expr ++ [c], bs)

getBlocks :: String -> [Block]
getBlocks text = map getSubBlocks bs
  where
    getSubBlocks :: String -> Block
    getSubBlocks b = case List.find isCBegin b of
        Just _  -> SubBlocks (getBlocks b)
        Nothing -> BlockText b
    (_, _, bs) = foldl getBlock (0, "", []) text
