module ParserSpec
    ( spec
    )
where

import           Test.Hspec

import           Parser
import           Types
import           LexerSpec                      ( toToken )

spec :: Spec
spec = describe "parseStatement" $ do
    it "parses identifier" $ do
        parseStatement (toToken "x") `shouldBe` Variable "x"
        parseStatement (toToken "1") `shouldBe` Macro "1"
        parseStatement (toToken "x y z") `shouldBe` Application
            [Variable "x", Variable "y", Variable "z"]
        parseStatement (toToken "TEST") `shouldBe` Macro "TEST"
