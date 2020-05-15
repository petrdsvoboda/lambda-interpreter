module LambdaParser
    ( buildExpr
    )
where

import qualified Data.List                     as List
import qualified Data.Char                     as Char
import           Chars
import           Types

getBlock :: (Int, String, [String]) -> Char -> (Int, String, [String])
getBlock (d, expr, bs) c
    | c == bgnBlockChar && d == 0 && expr == "" = (d + 1, expr, bs)
    | c == bgnBlockChar && d == 0 = (d + 1, "", bs ++ [expr])
    | c == bgnBlockChar           = (d + 1, expr ++ [c], bs)
    | c == endBlockChar && d == 1 = (d - 1, "", bs ++ [expr])
    | c == endBlockChar           = (d - 1, expr ++ [c], bs)
    | otherwise                   = (d, expr ++ [c], bs)

parseVariable :: String -> VarData
parseVariable text = case Char.isLower (head text) of
    True  -> Simple text
    False -> Macro text

parseAbstraction :: String -> (Char, Term)
parseAbstraction text = (head text, parseBlockText $ drop 1 text)

parseBlockText :: String -> Term
parseBlockText text = case head text of
    lambdaChar -> Abstraction (parseAbstraction $ drop 1 text)
    _          -> foldl

buildExpr :: Block -> Term
buildExpr b = case b of
    (BlockText text) -> parseBlockText text
    (SubBlocks bs  ) -> Variable (Simple "ok")
