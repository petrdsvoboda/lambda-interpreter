module Evaluator
    ( lambdaEval
    )
where

import           Chars
import           Types

macros :: String -> Term
macros m = case m of
    "+" -> Variable (Simple 'k')

lambdaEval :: Term -> Term
lambdaEval term = case term of
    (Variable var) -> case var of
        (Simple v) -> Variable (Simple v)
        (Macro  m) -> macros m
    (Abstraction (v, t)) -> Variable (Simple 'v')
    (Application (a, b)) -> Variable (Simple 'v')
