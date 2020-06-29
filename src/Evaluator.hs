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
import           Debug.Trace

tmap :: (Term -> Term) -> Term -> Term
tmap f term = case term of
    Empty                -> Empty
    Application ([]    ) -> Application []
    Application (t : ts) -> applied <> tmap f (Application ts)
      where
        applied = case t of
            Application _ -> Application [tmap f t]
            _             -> tmap f t
    Abstraction (v, t) -> Abstraction (v, tmap f t)
    _                  -> f term

-- tfold :: (Term -> Term) -> String -> Term -> Term
-- tfold f v t = case t of
--     Empty              -> Empty
--     Application (a, b) -> tfold f v a <> tfold f v b
--     Abstraction (inner_v, inner_t) ->
--         if inner_v == v then t else Abstraction (inner_v, tfold f v inner_t)
--     _ -> f t

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
        Just val -> termFromString val
        Nothing  -> Empty
    _ -> t

consolidateAbstractions :: Term -> Term
consolidateAbstractions term = case term of
    Abstraction (vs, t) -> case t of
        Application [Abstraction (inner_vs, inner_t)] ->
            Abstraction (vs ++ inner_vs, consolidateAbstractions inner_t)
        _ -> Abstraction (vs, consolidateAbstractions t)
    Application ([]    ) -> Application []
    Application (t : ts) -> applied <> consolidateAbstractions (Application ts)
      where
        applied = case t of
            Application _ -> Application [consolidateAbstractions t]
            _             -> consolidateAbstractions t
    _ -> term

eval :: Term -> Term
eval term = consolidateAbstractions res
  where
    res = case term of
        Abstraction (v, t)         -> Abstraction (v, eval t)
        Application (a : b : rest) -> case a of
            Abstraction ((v : vs), t) -> Abstraction (vs, reduced)
                <> Application rest
              where
                reduced = case b of
                    Application _ -> betaReduction v (Application [b]) t
                    _             -> betaReduction v b t
            _ -> a <> eval (Application (b : rest))
        Application [t] -> Application [eval t]
        _               -> term
