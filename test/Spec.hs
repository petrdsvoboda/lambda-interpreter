import           Test.Hspec

import qualified LexerSpec
import qualified ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Lexer"  LexerSpec.spec
    describe "Parser" ParserSpec.spec
