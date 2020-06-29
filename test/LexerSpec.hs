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
    , ( "x (x)"
      , [Identifier "x", Separator Begin, Identifier "x", Separator End]
      )
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
    , ("  x  "        , [Identifier "x"])
    , ("\t\tx\t\t"    , [Identifier "x"])
    , ("\t \t x \t \t", [Identifier "x"])
    ]
token = map (show . swap) expr

toToken :: String -> [Token]
toToken x = Map.fromList expr Map.! x

spec :: Spec
spec = describe "tokenize" $ do
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
    it "tokenizes assignment" $ do
        tokenize "FN = (\\x.x)" `shouldBe` toToken "FN = (\\x.x)"
    it "tokenizes complex exxpression" $ do
        tokenize "(\\x y.x x y)(\\y.y 1)(2)"
            `shouldBe` toToken "(\\x y.x x y)(\\y.y 1)(2)"
    it "works with whitespace" $ do
        tokenize "  x  " `shouldBe` toToken "  x  "
        tokenize "\t\tx\t\t" `shouldBe` toToken "\t\tx\t\t"
        tokenize "\t \t x \t \t" `shouldBe` toToken "\t \t x \t \t"
