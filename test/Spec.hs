{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import           Test.Hspec

import qualified LexerSpec
import qualified ParserSpec
import qualified EvaluatorSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Lexer"     LexerSpec.spec
    describe "Parser"    ParserSpec.spec
    describe "Evaluator" EvaluatorSpec.spec
