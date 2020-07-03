module CLISpec
    ( spec
    )
where

import qualified Data.Map                      as Map
import           Data.Tuple                     ( swap )
import           Test.Hspec

import           CLI
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

macros = Macro.macroHeap

spec :: Spec
spec = do
    describe "compute" $ do
        it "computes basic arithmetic" $ do
            let input = fromString "+ 1 1"
            res <- compute (ProgramFlags { quiet = True }) macros input
            res `shouldBe` Right "2"
            let input = fromString "+ 12 23"
            res <- compute (ProgramFlags { quiet = True }) macros input
            res `shouldBe` Right "35"
            let input = fromString "- 17 3"
            res <- compute (ProgramFlags { quiet = True }) macros input
            res `shouldBe` Right "14"
            let input = fromString "* 4 6"
            res <- compute (ProgramFlags { quiet = True }) macros input
            res `shouldBe` Right "24"
            let input = fromString "^ 3 3"
            res <- compute (ProgramFlags { quiet = True }) macros input
            res `shouldBe` Right "27"
        it "handles Y Combinator" $ do
            let input = fromString "Y FAC 0"
            res <- compute (ProgramFlags { quiet = True }) macros input
            res `shouldBe` Right "1"
            let input = fromString "Y FAC 1"
            res <- compute (ProgramFlags { quiet = True }) macros input
            res `shouldBe` Right "1"
            let input = fromString "Y FAC 2"
            res <- compute (ProgramFlags { quiet = True }) macros input
            res `shouldBe` Right "2"
            let input = fromString "Y FAC 5"
            res <- compute (ProgramFlags { quiet = True }) macros input
            res `shouldBe` Right "120"
