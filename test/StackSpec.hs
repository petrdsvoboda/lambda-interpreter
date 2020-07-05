module StackSpec
    ( spec
    )
where

import           Test.Hspec
import           Control.Exception

import qualified Data.Stack                    as S

stack1 :: S.Stack Integer
stack1 = S.NonEmpty 1 (S.NonEmpty 2 (S.NonEmpty 3 S.Empty))
stack2 :: S.Stack String
stack2 = S.NonEmpty
    "Hello"
    (S.NonEmpty "how" (S.NonEmpty "are" (S.NonEmpty "you" S.Empty)))
stack3 :: S.Stack Int
stack3 = S.NonEmpty 1782 (S.NonEmpty (-245) S.Empty)
stackEmpty :: S.Stack Double
stackEmpty = S.Empty

stack1Popped :: S.Stack Integer
stack1Popped = S.NonEmpty 2 (S.NonEmpty 3 S.Empty)
stack2Popped :: S.Stack String
stack2Popped = S.NonEmpty "how" (S.NonEmpty "are" (S.NonEmpty "you" S.Empty))
stack3Popped :: S.Stack Int
stack3Popped = S.NonEmpty (-245) S.Empty

spec :: Spec
spec = do
    describe "top" $ do
        it "returns top element" $ do
            S.top stack1 `shouldBe` 1
            S.top stack2 `shouldBe` "Hello"
            S.top stack3 `shouldBe` 1782
        it "raises error if empty"
            $             evaluate (S.top stackEmpty)
            `shouldThrow` errorCall "Empty stack"

    describe "topSafe" $ do
        it "returns top element as Just" $ do
            S.topSafe stack1 `shouldBe` Just 1
            S.topSafe stack2 `shouldBe` Just "Hello"
            S.topSafe stack3 `shouldBe` Just 1782
        it "returns Nothing if empty stack"
            $          S.topSafe stackEmpty
            `shouldBe` Nothing

    describe "pop" $ do
        it "returns stack without top element" $ do
            S.pop stack1 `shouldBe` stack1Popped
            S.pop stack2 `shouldBe` stack2Popped
            S.pop stack3 `shouldBe` stack3Popped
        it "raises error if empty"
            $             evaluate (S.pop stackEmpty)
            `shouldThrow` errorCall "Empty stack"

    describe "popSafe" $ do
        it "returns stack without top element as Just" $ do
            S.popSafe stack1 `shouldBe` Just stack1Popped
            S.popSafe stack2 `shouldBe` Just stack2Popped
            S.popSafe stack3 `shouldBe` Just stack3Popped
        it "returns Nothing if empty stack"
            $          S.popSafe stackEmpty
            `shouldBe` Nothing

    describe "push" $ it "adds new top element" $ do
        S.push 7 stack1 `shouldBe` S.NonEmpty 7 stack1
        S.push "Hi" stack2 `shouldBe` S.NonEmpty "Hi" stack2
        S.push (-4) stack3 `shouldBe` S.NonEmpty (-4) stack3
        S.push 'a' S.Empty `shouldBe` S.NonEmpty 'a' S.Empty

    describe "size" $ do
        it "returns 0 for empty stack" $ S.size S.Empty `shouldBe` 0
        it "returns number of elements" $ do
            S.size stack1 `shouldBe` 3
            S.size stack2 `shouldBe` 4
            S.size stack3 `shouldBe` 2

    describe "null" $ do
        it "returns true for empty stack" $ S.null S.Empty `shouldBe` True
        it "returns false for nonempty stack" $ do
            S.null stack1 `shouldBe` False
            S.null stack2 `shouldBe` False
            S.null stack3 `shouldBe` False

    describe "fromList" $ it "creates stack from list" $ do
        S.fromList ([] :: String) `shouldBe` S.Empty
        S.fromList ["Hello", "how", "are", "you"] `shouldBe` stack2
