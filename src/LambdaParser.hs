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

data StackItem = StackItem { isFn::Bool, inFn::Bool, fnVar::[Term Char], term::Term Char}

emptyItem :: StackItem
emptyItem = StackItem { isFn = False, inFn = False, fnVar = [], term = Empty }

parseTree :: [Token] -> Term Char
parseTree tokens = term
  where
    expandStack :: Stack.Stack StackItem -> Token -> Stack.Stack StackItem
    expandStack stack token
        | token == Separator Begin = Stack.push emptyItem stack
        | otherwise                = stack
    consolidateStack :: Stack.Stack StackItem -> Token -> Stack.Stack StackItem
    consolidateStack stack token
        | token == Separator End = Stack.push emptyItem stack
            where
                curr = Stack.top stack
                prev = Stack.top (Stack.pop stack)
        | otherwise              = stack
    parseToken :: Stack.Stack StackItem -> Token -> Stack.Stack StackItem
    parseToken stack token = Stack.push stackItem (Stack.pop stack)
      where
        curr@StackItem { isFn = isFn, inFn = inFn, fnVar = fnVar, term = term }
            = Stack.top stack
        stackItem = case token of
            Identifier x -> case inFn of
                True  -> curr { fnVar = fnVar ++ [var] }
                False -> curr { term = Application (term, var) }
                where var = parseVar x
            Keyword key -> case key of
                Fn    -> curr { isFn = True, inFn = True }
                EndFn -> curr { inFn = False }
            _ -> curr
    stack = foldl
        (\acc curr ->
            expandStack (parseToken (consolidateStack acc curr) curr) curr
        )
        (Stack.fromList [emptyItem])
        tokens
    StackItem { term = term } = Stack.top stack
