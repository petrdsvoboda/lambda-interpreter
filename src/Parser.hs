module Parser
    ( parse
    , parseStatement
    , exprFromString
    , termFromString
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


-- FIXME: parse (x) to Application [Variable "x"] not Variable "x"

parseVar :: String -> Term
parseVar text = if Char.isLower $ head text then Variable text else Macro text

data StackItem = StackItem { isFn::Bool, inFn::Bool, fnVar::[String], term::Term}

emptyItem :: StackItem
emptyItem = StackItem { isFn = False, inFn = False, fnVar = [], term = Empty }

parseToken :: Stack.Stack StackItem -> Token -> Stack.Stack StackItem
parseToken stack token = expand . analyze $ consolidate stack
  where
    expand :: Stack.Stack StackItem -> Stack.Stack StackItem
    expand stack | token == Separator Begin = Stack.push emptyItem stack
                 | otherwise                = stack
    consolidate :: Stack.Stack StackItem -> Stack.Stack StackItem
    consolidate stack
        | token == Separator End = Stack.push next (Stack.pop (Stack.pop stack))
        | otherwise              = stack
      where
        StackItem { fnVar = fnVar, term = term } = Stack.top stack
        prev@StackItem { term = termPrev }       = Stack.top (Stack.pop stack)
        next = case term == Empty of
            True  -> prev
            False -> case List.null fnVar of
                True  -> prev { term = termPrev <> Application [term] }
                False -> prev { term = termPrev <> Abstraction (fnVar, term) }
    analyze :: Stack.Stack StackItem -> Stack.Stack StackItem
    analyze stack = Stack.push stackItem (Stack.pop stack)
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

parseStatement :: [Token] -> Term
parseStatement tokens = term
  where
    stack = foldl parseToken (Stack.fromList [emptyItem]) tokens
    StackItem { term = term } = Stack.top stack

parse :: [Token] -> Expr
parse tokens = (term, assignTo)
  where
    isAssignment = length tokens > 1 && (tokens !! 1 == (Keyword Assign))
    assignTo     = case head tokens of
        Identifier x -> Just x
        _            -> Nothing
    tokensStatement = if isAssignment then drop 2 tokens else tokens
    term            = parseStatement tokensStatement

exprFromString :: String -> Expr
exprFromString = parse . tokenize

termFromString :: String -> Term
termFromString = parseStatement . tokenize

toString :: Term -> String
toString term = case lookupId $ show term of
    Just x  -> x
    Nothing -> show term
