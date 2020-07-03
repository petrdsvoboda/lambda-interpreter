module LexerSpec
  ( spec
  , toToken
  )
where

import qualified Data.Map                      as Map
import           Data.Tuple                     ( swap )
import           Test.Hspec

import           Lexer
import           Types

expr :: [(String, [Token])]
expr =
  [ ("x"    , [Identifier "x"])
  , ("1"    , [Identifier "1"])
  , ("x y z", [Identifier "x", Identifier "y", Identifier "z"])
  , ("TEST" , [Identifier "TEST"])
  , ("(x)", [Separator Begin, Identifier "x", Separator End])
  , ("x (x)", [Identifier "x", Separator Begin, Identifier "x", Separator End])
  , ( "x x (x x (x))"
    , [ Identifier "x"
      , Identifier "x"
      , Separator Begin
      , Identifier "x"
      , Identifier "x"
      , Separator Begin
      , Identifier "x"
      , Separator End
      , Separator End
      ]
    )
  , ( "(\\x.x)"
    , [ Separator Begin
      , Keyword Fn
      , Identifier "x"
      , Keyword EndFn
      , Identifier "x"
      , Separator End
      ]
    )
  , ( "(\\x y.x)"
    , [ Separator Begin
      , Keyword Fn
      , Identifier "x"
      , Identifier "y"
      , Keyword EndFn
      , Identifier "x"
      , Separator End
      ]
    )
  , ( "FN = (\\x.x)"
    , [ Identifier "FN"
      , Keyword Assign
      , Separator Begin
      , Keyword Fn
      , Identifier "x"
      , Keyword EndFn
      , Identifier "x"
      , Separator End
      ]
    )
  , ( "(\\x y.x x y)(\\y.y 1)(2)"
    , [ Separator Begin
      , Keyword Fn
      , Identifier "x"
      , Identifier "y"
      , Keyword EndFn
      , Identifier "x"
      , Identifier "x"
      , Identifier "y"
      , Separator End
      , Separator Begin
      , Keyword Fn
      , Identifier "y"
      , Keyword EndFn
      , Identifier "y"
      , Identifier "1"
      , Separator End
      , Separator Begin
      , Identifier "2"
      , Separator End
      ]
    )
  , ( "FN2 = (\\x y.x x y)(\\y.y 1)(2)"
    , [ Identifier "FN2"
      , Keyword Assign
      , Separator Begin
      , Keyword Fn
      , Identifier "x"
      , Identifier "y"
      , Keyword EndFn
      , Identifier "x"
      , Identifier "x"
      , Identifier "y"
      , Separator End
      , Separator Begin
      , Keyword Fn
      , Identifier "y"
      , Keyword EndFn
      , Identifier "y"
      , Identifier "1"
      , Separator End
      , Separator Begin
      , Identifier "2"
      , Separator End
      ]
    )
  , ("  x  "        , [Identifier "x"])
  , ("\t\tx\t\t"    , [Identifier "x"])
  , ("\t \t x \t \t", [Identifier "x"])
  , (":D"           , [Identifier ":D"])
  , ("0123"         , [Identifier "0123"])
  , ("--1"          , [Identifier "--1"])
  , ("--1 --2"      , [Identifier "--1", Identifier "--2"])
  , ( "(--1 --2) :D"
    , [ Separator Begin
      , Identifier "--1"
      , Identifier "--2"
      , Separator End
      , Identifier ":D"
      ]
    )
  ]
token = map (show . swap) expr

toToken :: String -> [Token]
toToken x = Map.fromList expr Map.! x

spec :: Spec
spec = do
  describe "tokenize" $ do
    it "tokenizes identifier" $ do
      tokenize "x" `shouldBe` toToken "x"
      tokenize "1" `shouldBe` toToken "1"
      tokenize "x y z" `shouldBe` toToken "x y z"
      tokenize "TEST" `shouldBe` toToken "TEST"
    it "tokenizes with parentheses" $ do
      tokenize "(x)" `shouldBe` toToken "(x)"
      tokenize "x (x)" `shouldBe` toToken "x (x)"
      tokenize "x x (x x (x))" `shouldBe` toToken "x x (x x (x))"
    it "tokenizes function" $ do
      tokenize "(\\x.x)" `shouldBe` toToken "(\\x.x)"
      tokenize "(\\x y.x)" `shouldBe` toToken "(\\x y.x)"
    it "tokenizes assignment" $ tokenize "FN = (\\x.x)" `shouldBe` toToken
      "FN = (\\x.x)"
    it "tokenizes complex exxpression"
      $          tokenize "(\\x y.x x y)(\\y.y 1)(2)"
      `shouldBe` toToken "(\\x y.x x y)(\\y.y 1)(2)"
    it "works with whitespace" $ do
      tokenize "  x  " `shouldBe` toToken "  x  "
      tokenize "\t\tx\t\t" `shouldBe` toToken "\t\tx\t\t"
      tokenize "\t \t x \t \t" `shouldBe` toToken "\t \t x \t \t"
    it "handles weird characters" $ do
      tokenize ":D" `shouldBe` toToken ":D"
      tokenize "0123" `shouldBe` toToken "0123"
      tokenize "--1" `shouldBe` toToken "--1"
      tokenize "--1 --2" `shouldBe` toToken "--1 --2"
      tokenize "(--1 --2) :D" `shouldBe` toToken "(--1 --2) :D"
  describe "validate" $ do
    it "validates correct tokens" $ do
      validate (toToken "x") `shouldBe` Nothing
      validate (toToken "TEST") `shouldBe` Nothing
      validate (toToken "x (x)") `shouldBe` Nothing
      validate (toToken "(\\x.x)") `shouldBe` Nothing
      validate (toToken "(\\x y.x)") `shouldBe` Nothing
      validate (toToken "FN = (\\x.x)") `shouldBe` Nothing
      validate (toToken "(\\x y.x x y)(\\y.y 1)(2)") `shouldBe` Nothing
      validate (toToken "FN2 = (\\x y.x x y)(\\y.y 1)(2)") `shouldBe` Nothing
    it "finds mismatching brackets" $ do
      validate (tokenize "(x") `shouldBe` Just "Mismatching brackets"
      validate (tokenize "x)") `shouldBe` Just "Mismatching brackets"
      validate (tokenize "(x x") `shouldBe` Just "Mismatching brackets"
      validate (tokenize "(x x (x)") `shouldBe` Just "Mismatching brackets"
      validate (tokenize "(x x (x)))") `shouldBe` Just "Mismatching brackets"
      validate (tokenize "(\\x.(x)") `shouldBe` Just "Mismatching brackets"
      validate (tokenize "(\\x.(x)))") `shouldBe` Just "Mismatching brackets"
      validate (tokenize "(\\x y.x x y)(\\y.y 1)(2")
        `shouldBe` Just "Mismatching brackets"
      validate (tokenize "(\\x y.x x y)\\y.y 1)(2)")
        `shouldBe` Just "Mismatching brackets"
    it "finds incorrect assignment" $ do
      validate (tokenize "X = X=") `shouldBe` Just "Incorrect assignment"
      validate (tokenize "X == x") `shouldBe` Just "Incorrect assignment"
      validate (tokenize "X = x =") `shouldBe` Just "Incorrect assignment"
      validate (tokenize "= x") `shouldBe` Just "Incorrect assignment"
      validate (tokenize "X = x (\\x.x=) x")
        `shouldBe` Just "Incorrect assignment"
    it "finds mismatching brackets first"
      $          validate (tokenize "X = X=(")
      `shouldBe` Just "Mismatching brackets"
