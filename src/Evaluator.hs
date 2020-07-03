module Evaluator
    ( eval
    , tmap
    , macroExpansion
    , replace
    , betaReduction
    , consolidateApplication
    , consolidateAbstractions
    )
where

import           Chars
import           Types
import           Parser
import           Lexer
import           Debug.Trace
import qualified Data.Map                      as Map

-- FIXME: (\s z.s (z)) != (\s z.s z) but should


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

replace :: String -> Term -> Term -> Term
replace var with term = case term of
    Variable x -> if x == var then with else term
    Abstraction (vs, t) ->
        if var `elem` vs then term else Abstraction (vs, replace var with t)
    Application (t : ts) -> case t of
        Application _ -> Application [replace var with t]
            <> replace var with (Application ts)
        _ -> replace var with t <> replace var with (Application ts)
    Application [] -> Application []
    _              -> term

macroExpansion :: [SavedMacro] -> Term -> Maybe Term
macroExpansion macros term = case term of
    Abstraction (v, t) -> case inner of
        Just t -> Just $ Abstraction (v, t)
        _      -> Nothing
      where
        inner = case t of
            Macro m -> expand m
            _       -> macroExpansion macros t
    Application (a : b : rest) -> case a of
        Abstraction _ -> Just (Application (a : b : rest))
        Macro m -> expand m <> Just (Application (b : rest))
        _ -> Just a <> macroExpansion macros (Application (b : rest))
    Application [t] -> case macroExpansion macros t of
        Just inner -> Just (Application [inner])
        Nothing    -> Nothing
    _ -> Just term
  where
    expand :: String -> Maybe Term
    expand m = case (lookup macros m) of
        Just val -> Just $ fromString val
        Nothing  -> Nothing
    lookup :: Ord a => [(a, String)] -> a -> Maybe String
    lookup tuples x = Map.fromList tuples Map.!? x

consolidateAbstractions :: Term -> Term
consolidateAbstractions term = case term of
    Abstraction (vs, t) -> case t of
        Abstraction (inner_vs, inner_t) ->
            Abstraction (vs ++ inner_vs, consolidateAbstractions inner_t)
        Application [Abstraction (inner_vs, inner_t)] ->
            Abstraction (vs ++ inner_vs, consolidateAbstractions inner_t)
        Application [t] -> Abstraction (vs, t)
        _               -> Abstraction (vs, consolidateAbstractions t)
    Application ([]    ) -> Application []
    Application (t : ts) -> applied <> consolidateAbstractions (Application ts)
      where
        applied = case t of
            Application _ -> Application [consolidateAbstractions t]
            _             -> consolidateAbstractions t
    _ -> term

consolidateApplication :: Term -> Term
consolidateApplication term = case term of
    Application ([t]) -> consolidateApplication t
    _                 -> term

consolidate :: Term -> Term
consolidate = consolidateApplication . consolidateAbstractions

betaReduction :: Term -> Term
betaReduction term = case term of
    Abstraction (v, t)         -> Abstraction (v, betaReduction t)
    Application (a : b : rest) -> case a of
        Abstraction ((v : vs), t) -> Abstraction (vs, reduced)
            <> Application rest
          where
            reduced = case b of
                Application _ -> replace v (Application [b]) t
                _             -> replace v b t
        _ -> a <> betaReduction (Application (b : rest))
    Application [t] -> Application [betaReduction t]
    _               -> term

eval :: [SavedMacro] -> Term -> EvalRes
eval macros term = case res of
    Left  _ -> res
    Right t -> Right $ consolidate t
  where
    expanded = case macroExpansion macros term of
        Just t  -> Right t
        Nothing -> Left "Error: Macro expansion"
    betaReduced = Right $ betaReduction term
    pick :: (Bool, EvalRes) -> EvalRes -> (Bool, EvalRes)
    pick acc@(found, prev) curr = if found
        then acc
        else case curr of
            Right t -> if curr == prev then (False, curr) else (True, curr)
            Left  _ -> (True, curr)
    (_, res) = foldl pick (False, Right term) [expanded, betaReduced]
