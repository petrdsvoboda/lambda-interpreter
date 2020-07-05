module TypesSpec
    ( spec
    )
where

import qualified Data.Map                      as Map
import           Data.Tuple                     ( swap )
import           Test.Hspec
import           Test.QuickCheck
import           Control.Monad

import           Types
import           Parser

instance Arbitrary Term where
    arbitrary = do
        depth <- choose (0, 3)
        arbitrary' depth
      where
        arbitrary' :: Integer -> Gen Term
        arbitrary' depth = if depth > 0
            then oneof [empty, var, abs, ap]
            else return Empty
          where
            empty = return Empty
            var   = do
                len <- (choose (1, 3)) :: Gen Integer
                var <- sequence [ elements ['a' .. 'z'] | _ <- [0 .. len] ]
                return $ Variable var
            abs = do
                len <- (choose (1, 3)) :: Gen Integer
                let genVar =
                        sequence [ elements ['a' .. 'z'] | _ <- [0 .. len] ]
                varLen <- (choose (1, 3)) :: Gen Integer
                vars   <- sequence [ genVar | _ <- [0 .. varLen] ]
                t      <- arbitrary' (depth - 1)
                return $ Abstraction (vars, t)
            ap = do
                len <- (choose (1, 5)) :: Gen Integer
                ts  <- sequence [ arbitrary' (depth - 1) | _ <- [0 .. len] ]
                return $ Application ts

prop_SemigroupAssociative :: Term -> Term -> Term -> Bool
prop_SemigroupAssociative x y z = (x <> y) <> z == x <> (y <> z)

prop_MonoidLeftId :: Term -> Bool
prop_MonoidLeftId x = (mappend mempty x) == x

prop_MonoidRightId :: Term -> Bool
prop_MonoidRightId x = (mappend x mempty) == x

prop_EqReflexive :: Term -> Bool
prop_EqReflexive x = x == x

prop_EqSymmetry :: Term -> Term -> Bool
prop_EqSymmetry x y = (x == y) == (y == x)

prop_EqTransitivity :: Term -> Term -> Term -> Bool
prop_EqTransitivity x y z = case x == y of
    True -> case y == z of
        True  -> x == z
        False -> x /= z
    False -> case y == z of
        True  -> x /= z
        False -> True

x = Variable "x"
m = Macro "M"
ap1 = Application [x]
ap2 = Application [x, x]
ap3 = Application [x, x, x]
apNested = Application [x, Application [x]]
abs1 = Abstraction (["x"], x)
abs2 = Abstraction (["x"], ap2)
abs22 = Abstraction (["x", "x"], ap2)

eqAb1 = Abstraction (["x"], ap2)
eqAb2 = Abstraction (["x"], apNested)

spec :: Spec
spec = describe "Term" $ do
    it "correctly implements Show" $ do
        show Empty `shouldBe` ""
        show x `shouldBe` "x"
        show m `shouldBe` "M"
        show ap1 `shouldBe` "(x)"
        show ap2 `shouldBe` "(x x)"
        show ap3 `shouldBe` "(x x x)"
        show apNested `shouldBe` "(x (x))"
        show abs1 `shouldBe` "(\\x.x)"
        show abs2 `shouldBe` "(\\x.x x)"
        show abs22 `shouldBe` "(\\x x.x x)"
    it "Show is associative" $ property prop_SemigroupAssociative
    it "Show is monoid (left id)" $ property prop_MonoidLeftId
    it "Show is monoid (right id)" $ property prop_MonoidRightId
    context "Eq" $ do
        it "passes basic tests" $ do
            x `shouldBe` x
            m `shouldBe` m
            ap1 `shouldBe` ap1
            ap1 `shouldBe` x
            x `shouldBe` ap1
            ap2 `shouldBe` apNested
            eqAb1 `shouldBe` eqAb2
        it "passes complex tests" $ do
            fromString "(x y z)" `shouldBe` fromString "(x (y) ((z)))"
            fromString "(x (y z))" `shouldBe` fromString "(x ((y) ((z))))"
            fromString "(\\x.(x y) z)(x (y z))"
                `shouldBe` fromString "(\\x.((x (y)) z))(x ((y) ((z))))"
    it "Eq is Reflexive" $ property prop_EqReflexive
    it "Eq is symmetrical" $ property prop_EqSymmetry
    it "Eq is transitivite" $ property prop_EqTransitivity
    it "correctly implements Semigroup" $ do
        Empty <> Empty `shouldBe` Empty
        x <> Empty `shouldBe` x
        Empty <> x `shouldBe` x
        x <> x `shouldBe` ap2
        ap1 <> x `shouldBe` ap2
        x <> ap1 `shouldBe` ap2
        ap1 <> ap1 `shouldBe` ap2
