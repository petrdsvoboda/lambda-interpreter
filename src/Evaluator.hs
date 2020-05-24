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
    "SUCC"  -> "(\\x.(\\s.(\\z.(s (x s z)))))"
    "PRED"  -> "(\\x.(\\s.(\\z.(x (\\f.(\\g.(g (f s)) (\\g.(z)) (\\m.(m))))))))"
    "TRUE"  -> "(\\t.(\\f.(t)))"
    "FALSE" -> "(\\t.(\\f.(f)))"
    "AND"   -> "(\\x.(\\y.(x y x)))"
    "OR"    -> "(\\x.(\\y.(x x y)))"
    "NOT"   -> "(\\p.(p TRUE FALSE))"
    "+"     -> "(\\x.(\\y.(\\s.(\\z.(x s (y s z))))))"
    "-"     -> "(\\m.(\\n.((n PRED) m)))"
    "*"     -> "(\\x.(\\y.(\\s.(x (y s)))))"
    "0"     -> "(\\s.(\\z.z))"
    "1"     -> "(\\s.(\\z.(s z)))"
    "2"     -> "(\\s.(\\z.(s (s z))))"
    "3"     -> "(\\s.(\\z.(s (s (s z)))))"
    "Y"     -> "(\\f.((\\x.(f (x x))) (\\x.(f (x x)))))"

replaceVar :: Char -> Term -> Term -> Term
replaceVar c a b = 

lambdaEval :: Term -> Term
lambdaEval term = case term of
    (Variable var) -> case var of
        (Simple v) -> Variable (Simple v)
        (Macro  m) -> buildExpr . getBlocks $ macros m
    (Abstraction (v, t)) -> Variable (Simple 'v')
    (Application (a, b)) -> case a of
        (Abstraction (v, t)) -> replaceVar v t b
    (Normal      t     ) -> lambdaEval t
    (Empty) -> Empty
