module LambdaParser
    ( buildExpr
    , parseTree
    )
where

import qualified Data.List                     as List
import qualified Data.Char                     as Char
import qualified Data.Stack                    as Stack
import           Chars
import           Types

parseAbstraction :: String -> (Char, Term)
parseAbstraction text = (head text, parseBlockText $ drop 1 text)

parseBlockText :: String -> Term
parseBlockText text = Macro text

buildExpr :: Block -> Term
buildExpr b = case b of
    (BlockText text      ) -> parseBlockText text
    (SubBlocks (sb : sbs)) -> case sb of
        (BlockText text) -> case head text of
            '\\' -> Abstraction (text, buildExpr (SubBlocks sbs))
            _    -> Application (parseBlockText text, buildExpr (SubBlocks sbs))
        _ -> Application (buildExpr sb, buildExpr (SubBlocks sbs))
    (SubBlocks []) -> Empty

parseVar :: String -> Term
parseVar text = if Char.isLower $ head text then Variable text else Macro text

data StackItem = StackItem { isFn::Bool, inFn::Bool, fnVar::[String], term::Term}

emptyItem :: StackItem
emptyItem = StackItem { isFn = False, inFn = False, fnVar = [], term = Empty }

parseTree :: [Token] -> Term
parseTree tokens = term
  where
    expandStack :: Stack.Stack StackItem -> Token -> Stack.Stack StackItem
    expandStack stack token
        | token == Separator Begin = Stack.push emptyItem stack
        | otherwise                = stack
    consolidateStack :: Stack.Stack StackItem -> Token -> Stack.Stack StackItem
    consolidateStack stack token
        | token == Separator End = Stack.push next (Stack.pop (Stack.pop stack))
        | otherwise              = stack
      where
        StackItem { fnVar = fnVar, term = term } = Stack.top stack
        prev@StackItem { term = termPrev }       = Stack.top (Stack.pop stack)
        next = case term == Empty of
            True  -> prev
            False -> case List.null fnVar of
                True  -> prev { term = apply termPrev term }
                False -> prev
                    { term = apply termPrev $ Abstraction (head fnVar, term)
                    }
    parseToken :: Stack.Stack StackItem -> Token -> Stack.Stack StackItem
    parseToken stack token = Stack.push stackItem (Stack.pop stack)
      where
        curr@StackItem { isFn = isFn, inFn = inFn, fnVar = fnVar, term = term }
            = Stack.top stack
        stackItem = case token of
            Identifier x -> case inFn of
                True  -> curr { fnVar = fnVar ++ [x] }
                False -> curr { term = apply term var }
                where var = parseVar x
            Keyword key -> case key of
                Fn    -> curr { isFn = True, inFn = True }
                EndFn -> curr { inFn = False }
            _ -> curr

    apply :: Term -> Term -> Term
    apply a b = if a == Empty then b else Application (a, b)
    parse :: Stack.Stack StackItem -> Token -> Stack.Stack StackItem
    parse stack token =
        expandStack (parseToken (consolidateStack stack token) token) token
    stack                     = foldl parse (Stack.fromList [emptyItem]) tokens
    StackItem { term = term } = Stack.top stack
