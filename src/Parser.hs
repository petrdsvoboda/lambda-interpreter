module Parser
    ( parseTree
    , fromString
    , toString
    )
where

import qualified Data.List                     as List
import qualified Data.Char                     as Char
import qualified Data.Stack                    as Stack
import           Chars
import           Types
import           Lexer
import           Macro


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
                True -> prev { term = termPrev <> term }
                False ->
                    prev { term = termPrev <> Abstraction (head fnVar, term) }
    parseToken :: Stack.Stack StackItem -> Token -> Stack.Stack StackItem
    parseToken stack token = Stack.push stackItem (Stack.pop stack)
      where
        curr@StackItem { isFn = isFn, inFn = inFn, fnVar = fnVar, term = term }
            = Stack.top stack
        stackItem = case token of
            Identifier x -> case inFn of
                True  -> curr { fnVar = fnVar ++ [x] }
                False -> curr { term = term <> var }
                where var = parseVar x
            Keyword key -> case key of
                Fn    -> curr { isFn = True, inFn = True }
                EndFn -> curr { inFn = False }
            _ -> curr

    parse :: Stack.Stack StackItem -> Token -> Stack.Stack StackItem
    parse stack token =
        expandStack (parseToken (consolidateStack stack token) token) token
    stack                     = foldl parse (Stack.fromList [emptyItem]) tokens
    StackItem { term = term } = Stack.top stack

fromString :: String -> Term
fromString = parseTree . tokenize

toString :: Term -> String
toString term = case lookupId $ show term of
    Just x  -> x
    Nothing -> show term