module LambdaParser
    ( buildExpr
    )
where

import qualified Data.List                     as List
import qualified Data.Char                     as Char
import           Chars
import           Types

parseVariable :: String -> VarData
parseVariable text = case Char.isLower (head text) of
    True  -> Simple 'x'
    False -> Macro text

parseAbstraction :: String -> (Char, Term)
parseAbstraction text = (head text, parseBlockText $ drop 1 text)

parseBlockText :: String -> Term
parseBlockText text = Variable (Macro text)

buildExpr :: Block -> Term
buildExpr b = case b of
    (BlockText text      ) -> parseBlockText text
    (SubBlocks [sb1, sb2]) -> case sb1 of
        (BlockText text) -> case head text of
            '\\' -> Abstraction (text !! 1, buildExpr sb2)
            _    -> Application (parseBlockText text, buildExpr sb2)
        _ -> Application (buildExpr sb1, buildExpr sb2)
    (SubBlocks (sb : sbs)) ->
        Application (buildExpr sb, buildExpr (SubBlocks sbs))
