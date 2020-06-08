module LambdaParser
    ( buildExpr
    )
where

import qualified Data.List                     as List
import qualified Data.Char                     as Char
import qualified Data.Stack                    as Stack
import           Chars
import           Types

parseAbstraction :: String -> (Char, Term Char)
parseAbstraction text = (head text, parseBlockText $ drop 1 text)

parseBlockText :: String -> Term Char
parseBlockText text = Macro text

buildExpr :: Block -> Term Char
buildExpr b = case b of
    (BlockText text      ) -> parseBlockText text
    (SubBlocks (sb : sbs)) -> case sb of
        (BlockText text) -> case head text of
            '\\' -> Abstraction (text !! 1, buildExpr (SubBlocks sbs))
            _    -> Application (parseBlockText text, buildExpr (SubBlocks sbs))
        _ -> Application (buildExpr sb, buildExpr (SubBlocks sbs))
    (SubBlocks []) -> Empty

parseVar :: String -> Term Char
parseVar text =
    if Char.isLower $ head text then Variable $ head text else Macro text

type ParserStack = (Int, Bool, Bool, [Term Char], Term Char)

parseTree :: [Token] -> Term Char
parseTree tokens = term
  where
    parseToken :: ParserStack -> Token -> ParserStack
    parseToken stack token = case token of
        Identifier x -> case inFn of
            True  -> (depth, isFn, inFn, fnVar ++ [var], term)
            False -> (depth, isFn, inFn, fnVar, Application (term, var))
            where var = parseVar x
        Separator sep -> case sep of
            Begin -> (depth + 1, False, False, [], Empty)
            End   -> case isFn of
                True -> (depth, True, True, fnVar, term)
                False -> (depth, True, True, fnVar, term)
        Keyword key -> case key of
            Fn    -> (depth, True, True, fnVar, term)
            EndFn -> (depth, isFn, False, fnVar, term)
        where
            (depth, isFn, inFn, fnVar, term) = List.last stack
            stackItem = 
    (_, _, _, _, term) = foldl parseToken [(0, False, False, [], Empty)] tokens
