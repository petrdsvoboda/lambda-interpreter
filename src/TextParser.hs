module TextParser
    ( getBlocks
    )
where

import qualified Data.List                     as List
import           Chars
import           Types

parseText :: (Int, String, [String]) -> Char -> (Int, String, [String])
parseText (d, expr, bs) c = case c of
    '(' -> case d of
        0 -> case expr of
            "" -> (d + 1, expr, bs)
            _  -> (d + 1, "", bs ++ [expr])
        _ -> (d + 1, expr ++ [c], bs)
    ')' -> case d of
        1 -> (d - 1, "", bs ++ [expr])
        _ -> (d - 1, expr ++ [c], bs)
    _ -> (d, expr ++ [c], bs)

getBlocks :: String -> Block
getBlocks text = case List.find isCBegin text of
    Just _  -> SubBlocks $ map getBlocks bs
    Nothing -> BlockText text
    where (_, _, bs) = foldl parseText (0, "", []) text
