module EvaluatorHelpers
    ( replace
    )
where

import           Types

replace :: String -> Term -> Term -> Term
replace var with term = case term of
    Variable x -> if x == var then with else term
    Abstraction (vs, t) ->
        if var `elem` vs then term else Abstraction (vs, replace var with t)
    Application ts -> Application (map (replace var with) ts)
    _              -> term
