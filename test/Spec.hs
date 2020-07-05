{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import           Test.Hspec

import qualified LexerSpec
import qualified ParserSpec
import qualified EvaluatorSpec
import qualified TypesSpec
import qualified CLISpec
import qualified StackSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Lexer"      LexerSpec.spec
    describe "Parser"     ParserSpec.spec
    describe "Evaluator"  EvaluatorSpec.spec
    describe "Types"      TypesSpec.spec
    describe "CLI"        CLISpec.spec
    describe "Data.Stack" StackSpec.spec
