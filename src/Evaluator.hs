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
    Empty                   -> Empty
    Application ([]       ) -> Empty
    Application (term : ts) -> tmap f term <> tmap f (Application ts)
    Abstraction (v, b)      -> Abstraction (v, tmap f t)
    _                       -> f t

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
    Abstraction (vs, t) -> if name `elem` vs
        then term
        else Abstraction (vs, betaReduction name arg t)
    Application (t : ts) -> case t of
        Application _ ->
            Application [betaReduction name arg t]
                <> betaReduction name arg (Application ts)
        _ ->
            betaReduction name arg t <> betaReduction name arg (Application ts)
    Application [] -> Application []
    _              -> term

expandMacros :: [SavedMacro] -> Term -> Term
expandMacros macros t = case t of
    Macro m -> case (Macro.lookup macros m) of
        Just val -> fst $ fromString val
        Nothing  -> Empty
    _ -> t

eval :: Term -> Term
eval term = case term of
    Abstraction (v, t)         -> Abstraction (v, eval t)
    Application (a : b : rest) -> case a of
        Abstraction ((v : vs), t) -> case b of
            Application _ ->
                Abstraction (vs, betaReduction v (Application [b]) t)
                    <> Application rest
            _ -> Abstraction (vs, betaReduction v b t) <> Application rest
        _ -> a <> eval (Application (b : rest))
    Application [t] -> Application [eval t]
    _               -> term
