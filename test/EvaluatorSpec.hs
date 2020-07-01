module EvaluatorSpec
    ( spec
    )
where

import qualified Data.Map                      as Map
import           Data.Tuple                     ( swap )
import           Test.Hspec

import           Evaluator
import           Parser
import           Types
import           Macro

expr :: [(String, String)]
expr =
    [ ("(\\x.x) y"          , "y")
    , ("(\\x.x x) y"        , "y y")
    , ("(\\x.x (x)) y"      , "y (y)")
    , ("(\\x.x (x)) (\\y.y)", "(\\y.y) ((\\y.y))")
    ]
token = map (show . swap) expr

toTerm :: String -> Term
toTerm x = termFromString (Map.fromList expr Map.! x)

macros = Macro.idToVal

spec :: Spec
spec = describe "macroExpansion" $ do
    it "expands" $ do
        macroExpansion macros (termFromString "1 1")
            `shouldBe` Just (termFromString "(\\s z.s z) 1")