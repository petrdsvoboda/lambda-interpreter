module LambdaParser
    ( buildExpr
    )
where

import qualified Data.List                     as List
import qualified Data.Char                     as Char
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

parseTree :: [Token] -> Term Char
parseTree tokens = term
  where
    parseToken :: (Int, Term Char) -> Token -> (Int, Term Char)
    parseToken (d, term) token = case token of
        Identifier x   -> (d, Application (term, var)) where var = parseVar x
        Separator  sep -> (d, Empty)
        _              -> (d, Empty)
    (_, term) = foldl parseToken (0, Empty) tokens
