module Lexer
    ( tokenize
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
    , ('=' , Keyword Assign)
    , ('\\', Keyword Fn)
    , ('.' , Keyword EndFn)
    ]

charsSplit = ['(', ')', '.', '\\', '=']
charsWhitespace = [' ', '\n', '\t']

tokenize :: String -> [Token]
tokenize text = case id of
    Nothing -> tokens
    Just x  -> tokens ++ [Identifier x]
  where
    addToken :: (Maybe String, [Token]) -> Char -> (Maybe String, [Token])
    addToken (prev, tokens) curr
        | curr `elem` charsSplit = case prev of
            Nothing -> (Nothing, tokens ++ [tokenMap Map.! curr])
            Just x  -> (Nothing, tokens ++ [Identifier x, tokenMap Map.! curr])
        | curr `elem` charsWhitespace = case prev of
            Nothing -> (Nothing, tokens)
            Just x  -> (Nothing, tokens ++ [Identifier x])
        | otherwise = case prev of
            Nothing -> (Just [curr], tokens)
            Just x  -> (Just (x ++ [curr]), tokens)
    (id, tokens) = foldl addToken (Nothing, []) text

