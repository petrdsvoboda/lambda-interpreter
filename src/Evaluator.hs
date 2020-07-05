{-|
Module      : Evaluator
Description : Evaluating terms
Copyright   : (c) Petr Svoboda, 2020
License     : GPL-3
Maintainer  : svobop51@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Logic for all term reductions and conversions
-}
module Evaluator
    ( eval
    , macroExpansion
    , alphaConversion
    , betaReduction
    , consolidateApplication
    , consolidateAbstractions
    )
where

import           Types
import           Parser
import           Lexer
import           EvaluatorHelpers
import qualified Data.Map                      as Map
import qualified Data.List                     as List

-- | Expand macro that is target of next application
-- Fails if macro is not defined
macroExpansion :: MacroHeap -> Term -> Either String Term
macroExpansion macros term = case term of
    Abstraction (v, t) -> case macroExpansion macros t of
        Right inner -> Right $ Abstraction (v, inner)
        Left  err   -> Left err
    Application (a : b : rest) -> case a of
        Abstraction _ -> Right (Application (a : b : rest))
        Macro m -> expand m <> Right (Application (b : rest))
        _ -> Right a <> macroExpansion macros (Application (b : rest))
    Application [t] -> case macroExpansion macros t of
        Right inner -> Right (Application [inner])
        Left  err   -> Left err
    Macro m -> expand m
    _       -> Right term
  where
    -- | Try to find tagreted macro, fail if macro definition doesn't exist
    lookup :: String -> Maybe Term
    lookup x =
        Map.fromList (map (\(fst, _, trd) -> (fst, trd)) macros) Map.!? x
    expand :: String -> Either String Term
    expand m = case (lookup m) of
        Just val -> Right val
        Nothing  -> Left ("Error: Can't find macro - " ++ m)

-- | Converts abstarction that is target of next application (if required)
-- Converts arguments so they won't collide with reduced term after beta reduction
alphaConversion :: Term -> Term
alphaConversion term = case term of
    Abstraction (vs, t)           -> Abstraction (vs, alphaConversion t)
    Application ts@(a : b : rest) -> case a of
        Abstraction (vs, t) -> Application (transformed : b : rest)
          where
            ids         = toIds b
            transformed = transform ids a
        _ -> Application (map (alphaConversion) ts)
    Application [t] -> Application [alphaConversion t]
    _               -> term
  where
    -- | Finds all free variables in expression that is gonna be used in reduction
    toIds :: Term -> [String]
    toIds term = case term of
        Application ts -> concat $ map toIds ts
        Variable    t  -> [t]
        _              -> []
    -- | Converts expression so it won't collide
    transform :: [String] -> Term -> Term
    transform vars term = case term of
        Abstraction (vs, t) -> if null shared
            then term
            else (Abstraction (notShared ++ map snd mapping, replaced))
          where
            -- | shared argument of abstraction and expression 
            shared    = List.intersect vars (List.tail vs)
            notShared = vs List.\\ shared
            -- | map shared arguments to new values
            mapping   = map ((\x -> (x, x ++ "_"))) shared
            -- | find element in mapping
            getEl x = Variable $ (Map.fromList mapping) Map.! x
            -- | replace argument with target defined from mapping
            replaced = foldl ((\acc x -> replace x (getEl x) acc)) t shared
        Application ts -> Application (map (transform vars) ts)
        _              -> term

-- | Performs application (beta reduction)
-- Finds if there is suitable abstraction that can be used for application
betaReduction :: Term -> Term
betaReduction term = case term of
    Abstraction (v, t)            -> Abstraction (v, betaReduction t)
    Application ts@(a : b : rest) -> case a of
        Abstraction ((v : vs), t) -> Application (replaced : rest)
            where replaced = Abstraction (vs, replace v b t)
        _ -> Application (map (betaReduction) ts)
    Application [t] -> Application [betaReduction t]
    _               -> term

-- | Consolidate empty abstractions
-- Removes all remnants of reduced terms
consolidateAbstractions :: Term -> Term
consolidateAbstractions term = case term of
    Abstraction (vs, t) -> case t of
        Abstraction (inner_vs, inner_t) -> -- Nested abstraction
            Abstraction (vs ++ inner_vs, consolidateAbstractions inner_t)
        Application [Abstraction (inner_vs, inner_t)] ->  -- Nested application with single abstraction
            Abstraction (vs ++ inner_vs, consolidateAbstractions inner_t)
        Application [t] -> Abstraction (vs, t) -- Nested application
        _               -> Abstraction (vs, consolidateAbstractions t)
    Application ([]    ) -> Application []
    Application (t : ts) -> applied <> consolidateAbstractions (Application ts)
      where
        applied = case t of
            Application _ -> Application [consolidateAbstractions t]
            _             -> consolidateAbstractions t
    _ -> term

-- | Consolidate nested applications
consolidateApplication :: Term -> Term
consolidateApplication term = case term of
    Abstraction (vs, t)  -> Abstraction (vs, consolidateApplication t)
    Application []       -> Application []
    Application (t : ts) -> Application
        (map consolidateApplication (newT ++ ts))
      where
        newT = case t of
            Application inner -> inner
            _                 -> [t]
    _ -> term

-- | Consolidate abstarctions and applications
consolidate :: Term -> Term
consolidate = consolidateApplication . consolidateAbstractions

-- | Finds evaluation step that can be used on term and performs it
-- If there is no step to be done, does nothing, steps:
-- 1) macro expansion
-- 2) alpha conversion
-- 3) beta reduction
eval :: MacroHeap -> Term -> EvalRes
eval macros term = case res of
    Left  _ -> res
    Right t -> Right $ consolidate t
  where
    expanded    = macroExpansion macros term
    converted   = Right $ alphaConversion term
    betaReduced = Right $ betaReduction term
    -- | Performs steps until one of them provides different term than was supplied
    pick :: (Bool, EvalRes) -> EvalRes -> (Bool, EvalRes)
    pick acc@(found, prev) curr = if found
        then acc
        else case curr of
            Right t -> if curr == prev then (False, curr) else (True, curr)
            Left  _ -> (True, curr)
    (_, res) =
        foldl pick (False, Right term) [expanded, converted, betaReduced]
