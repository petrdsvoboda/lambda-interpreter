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
parseBlockText text = case head text of
    lambdaChar -> Abstraction (parseAbstraction $ drop 1 text)
    _          -> Variable (Simple 'x')

buildExpr :: Block -> Term
buildExpr b = case b of
    (BlockText text) -> parseBlockText text
    (SubBlocks bs  ) -> Variable (Simple 'x')
