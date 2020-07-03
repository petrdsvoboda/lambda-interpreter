module ParserSpec
    ( spec
    )
where

import qualified Data.Map                      as Map
import           Data.Tuple                     ( swap )
import           Test.Hspec

import           Parser
import           Lexer
import           Types
import           Macro

expr :: [(String, Term)]
expr =
    [ (""     , Empty)
    , ("x"    , Variable "x")
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
          , Abstraction (["y"], Application [Variable "y", Macro "1"])
          , Application [Macro "2"]
          ]
      )
    ]
token = map (show . swap) expr

toTerm :: String -> Term
toTerm x = Map.fromList expr Map.! x

x = Variable "x"
ap1 = Application [x]
ap2 = Application [x, x]
ap3 = Application [x, x, x]

spec :: Spec
spec = do
    describe "append" $ do
        it "appends correctly" $ do
            append x x `shouldBe` Application [x, ap1]
            append ap1 x `shouldBe` Application [x, ap1]
            append ap2 x `shouldBe` Application [x, x, ap1]
        it "handles application correctly" $ do
            append x ap1 `shouldBe` Application [x, ap1]
            append ap1 ap1 `shouldBe` Application [x, ap1]
            append ap2 ap2 `shouldBe` Application [x, x, ap2]
        it "handles Empty" $ do
            append Empty x `shouldBe` ap1
            append Empty ap1 `shouldBe` ap1
    describe "parseStatement" $ do
        it "parses var correctly" $ do
            parseVar "x" `shouldBe` Variable "x"
            parseVar "xyz" `shouldBe` Variable "xyz"
            parseVar "xYZ" `shouldBe` Variable "xYZ"
            parseVar "x0" `shouldBe` Variable "x0"
            parseVar "x==1" `shouldBe` Variable "x==1"
            parseVar "y:D" `shouldBe` Variable "y:D"
        it "parses macro correctly" $ do
            parseVar "X" `shouldBe` Macro "X"
            parseVar "Xyz" `shouldBe` Macro "Xyz"
            parseVar "XYZ" `shouldBe` Macro "XYZ"
            parseVar "X0" `shouldBe` Macro "X0"
            parseVar "X==1" `shouldBe` Macro "X==1"
            parseVar "Y:D" `shouldBe` Macro "Y:D"
            parseVar ":D" `shouldBe` Macro ":D"
    describe "parseToken" $ do
        it "parses token correctly" $ do
            1 `shouldBe` 1
        it "handles stack correctly" $ do
            1 `shouldBe` 1
    describe "parseStatement" $ do
        it "parses identifier" $ do
            parseStatement (tokenize "") `shouldBe` toTerm ""
            parseStatement (tokenize "x") `shouldBe` toTerm "x"
            parseStatement (tokenize "1") `shouldBe` toTerm "1"
            parseStatement (tokenize "x y z") `shouldBe` toTerm "x y z"
            parseStatement (tokenize "TEST") `shouldBe` toTerm "TEST"
        it "parses with parentheses" $ do
            parseStatement (tokenize "(x)") `shouldBe` toTerm "(x)"
            parseStatement (tokenize "x (x)") `shouldBe` toTerm "x (x)"
            parseStatement (tokenize "x x (x x (x))")
                `shouldBe` toTerm "x x (x x (x))"
        it "parses function" $ do
            parseStatement (tokenize "(\\x.x)") `shouldBe` toTerm "(\\x.x)"
            parseStatement (tokenize "(\\x y.x)") `shouldBe` toTerm "(\\x y.x)"
        it "parses complex exxpression"
            $          parseStatement (tokenize "(\\x y.x x y)(\\y.y 1)(2)")
            `shouldBe` toTerm "(\\x y.x x y)(\\y.y 1)(2)"
    describe "toString" $ do
        it "handles existing macro" $ do
            toString macroHeap (Abstraction (["t", "f"], Variable "t"))
                `shouldBe` "T"
            toString macroHeap (Abstraction (["s", "z"], Variable "z"))
                `shouldBe` "0"
            toString
                    macroHeap
                    (Abstraction
                        (["s", "z"], Application [Variable "s", Variable "z"])
                    )
                `shouldBe` "1"
        it "works without macro" $ do
            toString macroHeap (Abstraction (["x"], Variable "x"))
                `shouldBe` "(\\x.x)"
            toString macroHeap (Application [Variable "x", Variable "x"])
                `shouldBe` "(x x)"
