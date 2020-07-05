module TypesSpec
    ( spec
    )
where

import qualified Data.Map                      as Map
import           Data.Tuple                     ( swap )
import           Test.Hspec
import           Test.QuickCheck

import           Types

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
    it "correctly implements Eq" $ do
        x == x `shouldBe` True
        m == m `shouldBe` True
        ap1 == ap1 `shouldBe` True
        ap1 == x `shouldBe` True
        x == ap1 `shouldBe` True
        ap2 == apNested `shouldBe` True
        eqAb1 == eqAb2 `shouldBe` True
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
