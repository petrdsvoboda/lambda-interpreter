module Evaluator
    ( lambdaEval
    , expandMacros
    )
where

import           Chars
import           Types
import           LambdaParser
import           Lexer

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

betaReduction :: String -> Term -> Term -> Term
betaReduction name term arg = case term of
    Variable x -> if x == name then arg else term
    Application (a, b) ->
        Application (betaReduction name a arg, betaReduction name b arg)
    Abstraction (v, t) ->
        if v == name then term else Abstraction (v, betaReduction name t arg)
    _ -> term

expandMacros :: Term -> Term
expandMacros term = case term of
    Macro       m      -> parseTree . tokenize $ macros m
    Abstraction (v, t) -> Abstraction (v, expandMacros t)
    Application (a, b) -> Application (expandMacros a, expandMacros b)
    _                  -> term


lambdaEval :: Term -> Term
lambdaEval term = case expandMacros term of
    Application (a, b) -> case a of
        Abstraction (v, t) -> betaReduction v t b
        _                  -> Application (lambdaEval a, lambdaEval b)
    _ -> term
