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

spec :: Spec
spec = describe "eval" $ do
    it "evaluates" $ do
        eval (termFromString "(\\x.x) y") `shouldBe` toTerm "(\\x.x) y"
        eval (termFromString "(\\x.x x) y") `shouldBe` toTerm "(\\x.x x) y"
        eval (termFromString "(\\x.x (x)) y") `shouldBe` toTerm "(\\x.x (x)) y"
        eval (termFromString "(\\x.x (x)) (\\y.y)")
            `shouldBe` toTerm "(\\x.x (x)) (\\y.y)"
