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
