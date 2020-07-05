module Macro
  ( macroHeap
  )
where

import qualified Data.Map                      as Map
import           Data.Tuple                     ( swap )
import           Types
import           Parser
import           Evaluator

idToValBase :: [(String, String)]
idToValBase =
  [ ("SUCC" , "(\\x s z.s (x s z))")
  , ("PRED", "(\\x s z.x (\\f g.g (f s)) (\\g.z) (\\u.u))")
  , ("T"    , "(\\t f.t)")
  , ("TRUE" , "(\\t f.t)")
  , ("F"    , "(\\t f.f)")
  , ("FALSE", "(\\t f.f)")
  , ("AND"  , "(\\x y.x y x)")
  , ("OR"   , "(\\x y.x x y)")
  , ("NOT"  , "(\\p.p T F)")
  , ("IF"   , "(\\p t e.p t e)")
  , ("+"    , "(\\x y s z.x s (y s z))")
  , ("-"    , "(\\m n.(n PRED) m)")
  , ("*"    , "(\\x y s.x (y s))")
  , ("^"    , "(\\x y.y x)")
  , ("0"    , "(\\s z.z)")
  , ("ZERO" , "(\\n.n (\\x.F)T)")
  , ("Y"    , "(\\f.(\\x.f (x x)) (\\x.f (x x)))")
  , ("FAC"  , "(\\f n.ZERO n 1 (* n (f (- n 1))))")
  ]

idToVal :: [(String, String)]
idToVal = idToValBase ++ numbers
 where
  numbers :: [(String, String)]
  (_, numbers) = ($!) foldl genNext ("(\\s z.z)", []) [1 .. 100]
  genNext :: (String, [(String, String)]) -> Int -> (String, [(String, String)])
  genNext (prev, acc) curr = (incByOne, acc ++ [(show curr, incByOne)])
   where
    term     = ($!) fromString $! "((\\x s z.s (x s z)) " ++ prev ++ ")"
    reduced  = ($!) betaReduction $! betaReduction $! betaReduction term
    incByOne = ($!) show reduced

macroHeap :: MacroHeap
macroHeap = ($!) map (\(fst, snd) -> (fst, snd, ($!) fromString snd)) idToVal
