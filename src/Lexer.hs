module Lexer
    ( getBlocks
    , tokenize
    )
where

import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           Chars
import           Types

tokenMap :: Map.Map Char Token
tokenMap = Map.fromList
    [ ('(' , Separator Begin)
    , (')' , Separator End)
    , ('\\', Keyword Fn)
    , ('.' , Keyword EndFn)
    ]

charsSplit = ['(', ')', '.', '\\']
charsWhitespace = [' ', '\n', '\t']

tokenize :: String -> [Token]
tokenize text = case id of
    Nothing -> tokens
    Just x  -> tokens ++ [Identifier x]
  where
    getToken :: (Maybe String, [Token]) -> Char -> (Maybe String, [Token])
    getToken (prev, tokens) curr
        | curr `elem` charsSplit = case prev of
            Nothing -> (Nothing, tokens ++ [tokenMap Map.! curr])
            Just x  -> (Nothing, tokens ++ [Identifier x, tokenMap Map.! curr])
        | curr `elem` charsWhitespace = case prev of
            Nothing -> (Nothing, tokens)
            Just x  -> (Nothing, tokens ++ [Identifier x])
        | otherwise = case prev of
            Nothing -> (Just [curr], tokens)
            Just x  -> (Just (x ++ [curr]), tokens)
    (id, tokens) = foldl getToken (Nothing, []) text

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
