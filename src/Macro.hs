module Macro where

import qualified Data.Map                      as Map
import           Data.Tuple                     ( swap )
import           Types

idToVal :: [SavedMacro]
idToVal =
  [ ("SUCC" , "(\\x s z.s (x s z))")
  , ("PRED", "(\\x s z.x (\\f g.g (f s)) (\\g.z) (\\u.u))")
  , ("T"    , "(\\t f.t)")
  , ("TRUE" , "(\\t f.t)")
  , ("F"    , "(\\t f.f)")
  , ("FALSE", "(\\t f.f)")
  , ("AND"  , "(\\x y.x y x)")
  , ("OR"   , "(\\x y.x x y)")
  , ("NOT"  , "(\\p.p T F)")
  , ("+"    , "(\\x y s z.x s (y s z))")
  , ("-"    , "(\\m n.(n PRED) m)")
  , ("*"    , "(\\x y s.x (y s))")
  , ("^"    , "(\\x y.y x)")
  , ("0"    , "(\\s z.z)")
  , ("1"    , "(\\s z.s z)")
  , ("2"    , "(\\s z.s (s z))")
  , ("3"    , "(\\s z.s (s (s z)))")
  , ("4"    , "(\\s z.s (s (s (s z))))")
  , ("5"    , "(\\s z.s (s (s (s (s z)))))")
  , ("6"    , "(\\s z.s (s (s (s (s (s z))))))")
  , ("7"    , "(\\s z.s (s (s (s (s (s (s z)))))))")
  , ("8"    , "(\\s z.s (s (s (s (s (s (s (s z))))))))")
  , ("9", "(\\s z.s (s (s (s (s (s (s (s (s z)))))))))")
  , ("ZERO" , "(\\n.n (\\x.F)T)")
  , ("Y"    , "(\\f.(\\x.f x x) (\\x.f x x))")
  ]
valToId = map swap idToVal

ids = map fst idToVal
vals = map snd idToVal

lookup :: [SavedMacro] -> String -> Maybe String
lookup tuples x = Map.fromList tuples Map.!? x

lookupVal :: String -> Maybe String
lookupVal = Macro.lookup idToVal
lookupId :: String -> Maybe String
lookupId = Macro.lookup valToId
