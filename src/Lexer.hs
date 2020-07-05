{-|
Module      : Lexer
Description : Conversion from sting to tokens
Copyright   : (c) Petr Svoboda, 2020
License     : GPL-3
Maintainer  : svobop51@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Manages conversion and validation from string to tokens with clear structure.
-}
module Lexer
    ( tokenize
    , validate
    )
where

import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           Chars
import           Types

-- | Keyword map (special characters used to identify structure)
tokenMap :: Map.Map Char Token
tokenMap = Map.fromList
    [ ('(' , Separator Begin)
    , (')' , Separator End)
    , ('=' , Keyword Assign)
    , ('\\', Keyword Fn)
    , ('.' , Keyword EndFn)
    ]

-- | Converts string to list of tokens
-- Separates keywords and identifiers
-- Creates tree structure based on separators
tokenize :: String -> [Token]
tokenize text = case id of
    Nothing -> tokens
    Just x  -> tokens ++ [Identifier x] -- ^ Don't forget to add the last identifier
  where
    -- | Keywords
    charsSplit      = map fst (Map.toList tokenMap)
    -- | Chars to ignore
    charsWhitespace = [' ', '\n', '\t']
    -- | Parses token to previous structure
    addToken :: (Maybe String, [Token]) -> Char -> (Maybe String, [Token])
    addToken (prev, tokens) curr
        |
     -- | Identify keyword, create identifier
          curr `elem` charsSplit = case prev of
            Nothing -> (Nothing, tokens ++ [tokenMap Map.! curr])
            Just x  -> (Nothing, tokens ++ [Identifier x, tokenMap Map.! curr])
        |
     -- | Identify whitespace, create identifier
          curr `elem` charsWhitespace = case prev of
            Nothing -> (Nothing, tokens)
            Just x  -> (Nothing, tokens ++ [Identifier x])
        |
     -- | Identify other char, add it to future identifier
          otherwise = case prev of
            Nothing -> (Just [curr], tokens)
            Just x  -> (Just (x ++ [curr]), tokens)
     -- | Classify all characters
    (id, tokens) = foldl addToken (Nothing, []) text

-- | Validate found token and return errors
-- Identifies mismatching brackets and incorrect assignment
validate :: [Token] -> Maybe String
validate tokens = case (depthCheck, assignmentCheck) of
    (False, _    ) -> Just "Mismatching brackets"
    (_    , False) -> Just "Incorrect assignment"
    (True , True ) -> Nothing
  where
    -- Checks depth based on separators
    step :: Int -> Token -> Int
    step depth token = case token of
        Separator Begin -> depth + 1
        Separator End   -> depth - 1
        _               -> depth
    depthCheck         = (foldl step 0 tokens) == 0
    -- Assignment keyword should be only second token if it is in tokens
    keywordAssignCount = List.length (List.filter (== Keyword Assign) tokens)
    assignmentCheck =
        if List.length tokens > 1 && ((tokens !! 1) == Keyword Assign)
            then keywordAssignCount == 1
            else keywordAssignCount == 0
