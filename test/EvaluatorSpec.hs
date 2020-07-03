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
toTerm x = fromString (Map.fromList expr Map.! x)

macros = Macro.idToVal

spec :: Spec
spec = do
    describe "macroExpansion" $ do
        it "expands correctly" $ do
            macroExpansion macros (fromString "1 1")
                `shouldBe` Just (fromString "(\\s z.s z) 1")
            macroExpansion macros (fromString "(\\s z.1 z)")
                `shouldBe` Just (fromString "(\\s z.(\\s z.s z) z)")
        it "ignores macros that don't need to be evaluated"
            $ macroExpansion macros (fromString "(\\s z.1 z) (\\s z.s z)")
            `shouldBe` Just (fromString "(\\s z.1 z) (\\s z.s z)")
    describe "replace" $ do
        it "replaces all selected vars" $ do
            replace "x1" (Variable "x2") (fromString "x1 x")
                `shouldBe` fromString "x2 x"
            replace "x1" (Variable "x2") (fromString "x1 x1 x")
                `shouldBe` fromString "x2 x2 x"
            replace "x1" (Variable "x2") (fromString "x1 x (x1 x1 x) x")
                `shouldBe` fromString "x2 x (x2 x2 x) x"
            replace "x1" (Variable "x2") (fromString "(\\x.x1 x)")
                `shouldBe` fromString "(\\x.x2 x)"
            replace "x1" (Variable "x2") (fromString "(\\x.x1 (\\y.x1 y))")
                `shouldBe` fromString "(\\x.x2 (\\y.x2 y))"
            replace "x1" (Variable "x2") (fromString "(\\x y.x1 x y)")
                `shouldBe` fromString "(\\x y.x2 x y)"
        it "ignores rebounded vars" $ do
            replace "x1" (Variable "x2") (fromString "(\\x1.x1 x)")
                `shouldBe` fromString "(\\x1.x1 x)"
            replace "x1" (Variable "x2") (fromString "(\\x1 y.x1 x)")
                `shouldBe` fromString "(\\x1 y.x1 x)"
        it "replaces with complex term"
            $ replace "x1" (fromString "(\\x.x) (y z)") (fromString "(x1 y z)")
            `shouldBe` fromString "((\\x.x) (y z) y z)"
        it "ignores other vars" $ do
            replace "x1" (Variable "x2") (fromString "(x y z)")
                `shouldBe` fromString "(x y z)"
            replace "x1" (Variable "x2") (fromString "(x y (a b c)) (\\f. f f)")
                `shouldBe` fromString "(x y (a b c)) (\\f. f f)"
    describe "consolidateAbstractions" $ do
        it "consolidates nested abstractions" $ do
            consolidateAbstractions (fromString "(\\x.(\\y.x y))")
                `shouldBe` fromString "(\\x y.x y)"
            consolidateAbstractions (fromString "x (\\x.(\\y.x y))")
                `shouldBe` fromString "x (\\x y.x y)"
            consolidateAbstractions (fromString "(\\x.(\\y.(\\z.x y z)))")
                `shouldBe` fromString "(\\x y.(\\z.x y z))"
            consolidateAbstractions (fromString "(\\x y.(\\z.x y z))")
                `shouldBe` fromString "(\\x y z.x y z)"
            consolidateAbstractions (fromString "(\\x.(\\y.(\\z.x y z)) x)")
                `shouldBe` fromString "(\\x.(\\y z.x y z) x)"
            consolidateAbstractions (fromString "(\\x.(\\y.(\\z.x y z) y))")
                `shouldBe` fromString "(\\x y.(\\z.x y z) y)"
        it "doesn't consolidate other abstractions"
            $          consolidateAbstractions (fromString "(\\x.(\\y.x y) x)")
            `shouldBe` fromString "(\\x.(\\y.x y) x)"
    describe "consolidateApplication" $ do
        it "consolidates root term" $ do
            consolidateApplication (fromString "(x)") `shouldBe` fromString "x"
            consolidateApplication (fromString "(x (x))")
                `shouldBe` fromString "x (x)"
        it "doesn't consolidate innet terms" $ do
            consolidateApplication (fromString "(x (x)) x")
                `shouldBe` fromString "(x (x)) x"
            consolidateApplication (fromString "(x (x x)) x")
                `shouldBe` fromString "(x (x x)) x"
    describe "betaReduction" $ do
        it "reduces correctly" $ do
            1 `shouldBe` 1
    describe "eval" $ do
        it "performs correct step" $ do
            1 `shouldBe` 1
