{-|
Module      : EvaluatorHelpers
Description : Helpers for evaluator
Copyright   : (c) Petr Svoboda, 2020
License     : GPL-3
Maintainer  : svobop51@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Helpers for evaluator
-}
module EvaluatorHelpers
    ( replace
    )
where

import           Types

-- | Replaces variable with term in provided term
replace :: String -> Term -> Term -> Term
replace var with term = case term of
    Variable x -> if x == var then with else term
    Abstraction (vs, t) ->
        if var `elem` vs then term else Abstraction (vs, replace var with t)
    Application ts -> Application (map (replace var with) ts)
    _              -> term
