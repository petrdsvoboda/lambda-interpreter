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
    (BlockText text                ) -> parseBlockText text
    (SubBlocks ((BlockText b) : bs)) -> case head b of
        '\\' -> Abstraction (b !! 1, buildExpr (SubBlocks bs))
        _    -> Application (parseBlockText b, buildExpr (SubBlocks bs))
