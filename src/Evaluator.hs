module Evaluator
    ( eval
    , expandMacros
    , tmap
    )
where

import           Chars
import           Types
import           Parser
import           Lexer
import           Macro


tmap :: (Term -> Term) -> Term -> Term
tmap f t = case t of
    Empty              -> Empty
    Application ts     -> Application (map (tmap f) ts)
    Abstraction (v, b) -> Abstraction (v, tmap f t)
    _                  -> f t

-- tfold :: (Term -> Term) -> String -> Term -> Term
-- tfold f v t = case t of
--     Empty              -> Empty
--     Application (a, b) -> tfold f v a <> tfold f v b
--     Abstraction (inner_v, inner_t) ->
--         if inner_v == v then t else Abstraction (inner_v, tfold f v inner_t)
--     _ -> f t

-- betaReduction :: String -> Term -> Term -> Term
-- betaReduction v arg t = case t of
--     Variable var -> if v == var then arg else t
--     _            -> t

betaReduction :: String -> Term -> Term -> Term
betaReduction name arg term = case term of
    Variable    x       -> if x == name then arg else term
    Application ts      -> Application (map (betaReduction name arg) ts)
    Abstraction (vs, t) -> if name `elem` vs
        then term
        else Abstraction (vs, betaReduction name arg t)
    _ -> term

expandMacros :: Term -> Term
expandMacros t = case t of
    Macro m -> case lookupVal m of
        Just val -> fromString val
        Nothing  -> Empty
    _ -> t

eval :: Term -> Term
eval term = case term of
    Abstraction (v, t)         -> Abstraction (v, eval t)
    Application (a : b : rest) -> case a of
        Abstraction ((v1 : v2 : vs), t) ->
            Abstraction ((v2 : vs), betaReduction v1 b t) <> Application rest
        Abstraction ([v], t) -> betaReduction v b t <> Application rest
        _                    -> a <> eval (b <> Application rest)
    Application [t] -> eval t
    _               -> term
