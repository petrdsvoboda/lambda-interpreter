module Evaluator
    ( lambdaEval
    )
where

import           Chars
import           Types
import           LambdaParser
import           TextParser

macros :: String -> String
macros m = case m of
    "+" -> "(\\x.(\\y.(\\s.(\\z.(x s (y s z))))))"
    "*" -> "(\\x.(\\y.(\\s.(x (y s)))))"
    "0" -> "(\\s.(\\z.z))"
    "1" -> "(\\s.(\\z.(s z)))"
    "2" -> "(\\s.(\\z.(s (s z))))"
    "3" -> "(\\s.(\\z.(s (s (s z)))))"

lambdaEval :: Term -> Term
lambdaEval term = case term of
    (Variable var) -> case var of
        (Simple v) -> Variable (Simple v)
        (Macro  m) -> buildExpr . getBlocks $ macros m
    (Abstraction (v, t)) -> Variable (Simple 'v')
    (Application (a, b)) -> Variable (Simple 'v')
    (Normal      t     ) -> lambdaEval t
