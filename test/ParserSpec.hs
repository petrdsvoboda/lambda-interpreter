module ParserSpec
    ( spec
    )
where

import qualified Data.Map                      as Map
import           Data.Tuple                     ( swap )
import           Test.Hspec

import           Parser
import           Types
import           LexerSpec                      ( toToken )

expr :: [(String, Term)]
expr =
    [ ("x"    , Variable "x")
    , ("1"    , Macro "1")
    , ("x y z", Application [Variable "x", Variable "y", Variable "z"])
    , ("TEST" , Macro "TEST")
    , ("(x)"  , Application [Variable "x"])
    , ("x (x)", Application [Variable "x", Application [Variable "x"]])
    , ( "x x (x x (x))"
      , Application
          [ Variable "x"
          , Variable "x"
          , Application [Variable "x", Variable "x", Application [Variable "x"]]
          ]
      )
    , ("(\\x.x)"  , Abstraction (["x"], Variable "x"))
    , ("(\\x y.x)", Abstraction (["x", "y"], Variable "x"))
    , ( "(\\x y.x x y)(\\y.y 1)(2)"
      , Application
          [ Abstraction
              ( ["x", "y"]
              , Application [Variable "x", Variable "x", Variable "y"]
              )
          , Abstraction (["x"], Application [Variable "y", Macro "1"])
          , Application [Macro "2"]
          ]
      )
    ]
token = map (show . swap) expr

toTerm :: String -> Term
toTerm x = Map.fromList expr Map.! x

spec :: Spec
spec = describe "parseStatement" $ do
    it "parses identifier" $ do
        parseStatement (toToken "x") `shouldBe` toTerm "x"
        parseStatement (toToken "1") `shouldBe` toTerm "1"
        parseStatement (toToken "x y z") `shouldBe` toTerm "x y z"
        parseStatement (toToken "TEST") `shouldBe` toTerm "TEST"
    it "parses with parentheses" $ do
        parseStatement (toToken "(x)") `shouldBe` toTerm "(x)"
        parseStatement (toToken "x (x)") `shouldBe` toTerm "x (x)"
        parseStatement (toToken "x x (x x (x))")
            `shouldBe` toTerm "x x (x x (x))"
    it "parses function" $ do
        parseStatement (toToken "(\\x.x)") `shouldBe` toTerm "(\\x.x)"
        parseStatement (toToken "(\\x y.x)") `shouldBe` toTerm "(\\x y.x)"
    it "parses complex exxpression" $ do
        parseStatement (toToken "(\\x y.x x y)(\\y.y 1)(2)")
            `shouldBe` toTerm "(\\x y.x x y)(\\y.y 1)(2)"
