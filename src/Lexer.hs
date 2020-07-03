module Lexer
    ( tokenize
    , validate
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

validate :: [Token] -> Maybe String
validate tokens = case (depthCheck, assignmentCheck) of
    (False, _    ) -> Just "Mismatching brackets"
    (_    , False) -> Just "Incorrect assignment"
    (True , True ) -> Nothing
  where
    step :: Int -> Token -> Int
    step depth token = case token of
        Separator Begin -> depth + 1
        Separator End   -> depth - 1
        _               -> depth
    depthCheck         = (foldl step 0 tokens) == 0
    keywordAssignCount = List.length (List.filter (== Keyword Assign) tokens)
    assignmentCheck =
        if List.length tokens > 1 && ((tokens !! 1) == Keyword Assign)
            then keywordAssignCount == 1
            else keywordAssignCount == 0
