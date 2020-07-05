module Evaluator
    ( eval
    , macroExpansion
    , alphaConversion
    , betaReduction
    , consolidateApplication
    , consolidateAbstractions
    )
where

import           Chars
import           Types
import           Parser
import           Lexer
import           EvaluatorHelpers
import qualified Data.Map                      as Map
import qualified Data.List                     as List

apply :: (Term -> Term) -> Term -> Term
apply f term = case term of
    Abstraction (v, t)     -> Abstraction (v, apply f t)
    Application []         -> Application []
    Application (a : rest) -> Application (f a : rest)
    _                      -> term

macroExpansion :: MacroHeap -> Term -> Either String Term
macroExpansion macros term = case term of
    Abstraction (v, t) -> case inner of
        Right t -> Right $ Abstraction (v, t)
        _       -> inner
      where
        inner = case t of
            Macro m -> expand m
            _       -> macroExpansion macros t
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
    lookup :: String -> Maybe Term
    lookup x =
        Map.fromList (map (\(fst, _, trd) -> (fst, trd)) macros) Map.!? x
    expand :: String -> Either String Term
    expand m = case (lookup m) of
        Just val -> Right val
        Nothing  -> Left ("Error: Can't find macro - " ++ m)

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
    Abstraction (vs, t)  -> Abstraction (vs, consolidateApplication t)
    Application []       -> Application []
    Application (t : ts) -> Application
        (map consolidateApplication (newT ++ ts))
      where
        newT = case t of
            Application inner -> inner
            _                 -> [t]
    _ -> term

consolidate :: Term -> Term
consolidate = consolidateApplication . consolidateAbstractions

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
    toIds :: Term -> [String]
    toIds term = case term of
        Application ts -> concat $ map toIds ts
        Variable    t  -> [t]
        _              -> []
    transform :: [String] -> Term -> Term
    transform vars term = case term of
        Abstraction (vs, t) -> if null shared
            then term
            else (Abstraction (notShared ++ map snd mapping, replaced))
          where
            shared    = List.intersect vars (List.tail vs)
            notShared = vs List.\\ shared
            mapping   = map ((\x -> (x, x ++ "_"))) shared
            getEl x = Variable $ (Map.fromList mapping) Map.! x
            replaced = foldl ((\acc x -> replace x (getEl x) acc)) t shared
        Application ts -> Application (map (transform vars) ts)
        _              -> term

betaReduction :: Term -> Term
betaReduction term = case term of
    Abstraction (v, t)            -> Abstraction (v, betaReduction t)
    Application ts@(a : b : rest) -> case a of
        Abstraction ((v : vs), t) -> Application (replaced : rest)
            where replaced = Abstraction (vs, replace v b t)
        _ -> Application (map (betaReduction) ts)
    Application [t] -> Application [betaReduction t]
    _               -> term

eval :: MacroHeap -> Term -> EvalRes
eval macros term = case res of
    Left  _ -> res
    Right t -> Right $ consolidate t
  where
    expanded    = macroExpansion macros term
    converted   = Right $ alphaConversion term
    betaReduced = Right $ betaReduction term
    pick :: (Bool, EvalRes) -> EvalRes -> (Bool, EvalRes)
    pick acc@(found, prev) curr = if found
        then acc
        else case curr of
            Right t -> if curr == prev then (False, curr) else (True, curr)
            Left  _ -> (True, curr)
    (_, res) =
        foldl pick (False, Right term) [expanded, converted, betaReduced]
