module EvaluatorSpec
    ( spec
    )
where

import qualified Data.Map                      as Map
import           Data.Tuple                     ( swap )
import           Test.Hspec

import           Evaluator
import           EvaluatorHelpers
import           Parser
import           Types
import           Macro

macros = Macro.macroHeap

spec :: Spec
spec = do
    describe "replace" $ do
        it "replaces all selected vars" $ do
            replace "x1" (Variable "x2") (fromString "x1 x")
                `shouldBe` fromString "x2 x"
            replace "x1" (Variable "x2") (fromString "x1 x1 x")
                `shouldBe` fromString "x2 x2 x"
            replace "x1" (Variable "x2") (fromString "x1 x (x1 x1 x) x")
                `shouldBe` fromString "x2 x (x2 x2 x) x"
            replace "x1" (Variable "x2") (fromString "(\\x.x1 x)")
                `shouldBe` fromString "(\\x.x2 x)"
            replace "x1" (Variable "x2") (fromString "(\\x.x1 (\\y.x1 y))")
                `shouldBe` fromString "(\\x.x2 (\\y.x2 y))"
            replace "x1" (Variable "x2") (fromString "(\\x y.x1 x y)")
                `shouldBe` fromString "(\\x y.x2 x y)"
            replace "x1" (Variable "x2") (fromString "(\\x y.x1 x y)")
                `shouldBe` fromString "(\\x y.x2 x y)"
        it "ignores rebounded vars" $ do
            replace "x1" (Variable "x2") (fromString "(\\x1.x1 x)")
                `shouldBe` fromString "(\\x1.x1 x)"
            replace "x1" (Variable "x2") (fromString "(\\x1 y.x1 x)")
                `shouldBe` fromString "(\\x1 y.x1 x)"
        it "replaces with complex term"
            $          replace
                           "x1"
                           (Application [Abstraction (["x"], Variable "x")])
                           (Application [Variable "x1", Variable "y", Variable "z"])
            `shouldBe` (Application
                           [ (Application [Abstraction (["x"], Variable "x")])
                           , Variable "y"
                           , Variable "z"
                           ]
                       )
        it "ignores other vars" $ do
            replace "x1" (Variable "x2") (fromString "(x y z)")
                `shouldBe` fromString "(x y z)"
            replace "x1" (Variable "x2") (fromString "(x y (a b c)) (\\f. f f)")
                `shouldBe` fromString "(x y (a b c)) (\\f. f f)"
    describe "macroExpansion" $ do
        it "expands correctly" $ do
            macroExpansion macros (fromString "1 1")
                `shouldBe` Right (fromString "(\\s z.s z) 1")
            macroExpansion macros (fromString "(\\s z.1 z)")
                `shouldBe` Right (fromString "(\\s z.(\\s z.s z) z)")
        it "ignores macros that don't need to be evaluated"
            $ macroExpansion macros (fromString "(\\s z.1 z) (\\s z.s z)")
            `shouldBe` Right (fromString "(\\s z.1 z) (\\s z.s z)")
        it "returns nothing if macro not found" $ do
            macroExpansion macros (fromString "NOTHING")
                `shouldBe` Left "Error: Can't find macro - NOTHING"
            macroExpansion macros (fromString "(\\x.x (y NIL))")
                `shouldBe` Left "Error: Can't find macro - NIL"
            macroExpansion macros (fromString "A1 A2")
                `shouldBe` Left "Error: Can't find macro - A1"
    describe "consolidateAbstractions" $ do
        it "consolidates nested abstractions" $ do
            consolidateAbstractions (fromString "(\\x.(\\y.x y))")
                `shouldBe` fromString "(\\x y.x y)"
            consolidateAbstractions (fromString "x (\\x.(\\y.x y))")
                `shouldBe` fromString "x (\\x y.x y)"
            consolidateAbstractions (fromString "(\\x.(\\y.(\\z.x y z)))")
                `shouldBe` fromString "(\\x y.(\\z.x y z))"
            consolidateAbstractions (fromString "(\\x y.(\\z.x y z))")
                `shouldBe` fromString "(\\x y z.x y z)"
            consolidateAbstractions (fromString "(\\x.(\\y.(\\z.x y z)) x)")
                `shouldBe` fromString "(\\x.(\\y z.x y z) x)"
            consolidateAbstractions (fromString "(\\x.(\\y.(\\z.x y z) y))")
                `shouldBe` fromString "(\\x y.(\\z.x y z) y)"
        it "doesn't consolidate other abstractions"
            $          consolidateAbstractions (fromString "(\\x.(\\y.x y) x)")
            `shouldBe` fromString "(\\x.(\\y.x y) x)"
    describe "consolidateApplication" $ do
        it "correctly consolidates" $ do
            consolidateApplication (Application [Variable "x"])
                `shouldBe` Variable "x"
            consolidateApplication
                    (Application [Variable "x", Application [Variable "x"]])
                `shouldBe` Application [Variable "x", Variable "x"]
            consolidateApplication
                    (Application
                        [ Application
                            [ Variable "x"
                            , Application [Variable "x", Variable "x"]
                            ]
                        , Variable "x"
                        ]
                    )
                `shouldBe` (Application
                               [ Variable "x"
                               , Application [Variable "x", Variable "x"]
                               , Variable "x"
                               ]
                           )
        it "doesn't consolidate inner terms" $ do
            consolidateApplication
                    (Application
                        [Variable "x", Application [Variable "x", Variable "x"]]
                    )
                `shouldBe` (Application
                               [ Variable "x"
                               , Application [Variable "x", Variable "x"]
                               ]
                           )
            consolidateApplication
                    (Application
                        [ Application
                            [ Variable "x"
                            , Application [Variable "x", Variable "x"]
                            ]
                        , Application [Variable "x", Variable "x"]
                        ]
                    )
                `shouldBe` (Application
                               [ Variable "x"
                               , Application [Variable "x", Variable "x"]
                               , Application [Variable "x", Variable "x"]
                               ]
                           )
    describe "alphaConversion" $ do
        it "converts correctly" $ do
            alphaConversion
                    (Application
                        [ Abstraction
                            ( ["x", "y"]
                            , Application [Variable "x", Variable "y"]
                            )
                        , Variable "y"
                        ]
                    )
                `shouldBe` (Application
                               [ Abstraction
                                   ( ["x", "y_"]
                                   , Application [Variable "x", Variable "y_"]
                                   )
                               , Variable "y"
                               ]
                           )
            alphaConversion
                    (Application
                        [ Abstraction
                            ( ["x", "y"]
                            , Application [Variable "x", Variable "y"]
                            )
                        , Application [Variable "x", Variable "y"]
                        ]
                    )
                `shouldBe` (Application
                               [ Abstraction
                                   ( ["x", "y_"]
                                   , Application [Variable "x", Variable "y_"]
                                   )
                               , Application [Variable "x", Variable "y"]
                               ]
                           )

        it "handles not shared vars" $ do
            alphaConversion
                    (Application
                        [ Abstraction
                            ( ["x", "y"]
                            , Abstraction
                                ( ["a", "b"]
                                , Application [Variable "x", Variable "b"]
                                )
                            )
                        , Variable "b"
                        ]
                    )
                `shouldBe` (Application
                               [ Abstraction
                                   ( ["x", "y"]
                                   , Abstraction
                                       ( ["a", "b"]
                                       , Application
                                           [Variable "x", Variable "b"]
                                       )
                                   )
                               , Variable "b"
                               ]
                           )
            alphaConversion
                    (Application
                        [ Abstraction
                            ( ["x", "y"]
                            , Abstraction
                                ( ["y", "z"]
                                , Application
                                    [ Variable "x"
                                    , Variable "y"
                                    , Variable "z"
                                    , Variable "z"
                                    ]
                                )
                            )
                        , Variable "y"
                        ]
                    )
                `shouldBe` (Application
                               [ Abstraction
                                   ( ["x", "y_"]
                                   , Abstraction
                                       ( ["y", "z"]
                                       , Application
                                           [ Variable "x"
                                           , Variable "y"
                                           , Variable "z"
                                           , Variable "z"
                                           ]
                                       )
                                   )
                               , Variable "y"
                               ]
                           )
        it "doesn't convert single applications"
            $          alphaConversion
                           (Application
                               [ Abstraction
                                     ( ["x", "y"]
                                     , Application [Variable "x", Variable "y"]
                                     )
                               ]
                           )
            `shouldBe` (Application
                           [ Abstraction
                                 ( ["x", "y"]
                                 , Application [Variable "x", Variable "y"]
                                 )
                           ]
                       )
    describe "betaReduction" $ do
        it "reduces correctly" $ do
            betaReduction
                    (Application
                        [Abstraction (["x"], Variable "x"), Variable "y"]
                    )
                `shouldBe` (Application [Abstraction ([], Variable "y")])
            betaReduction
                    (Application
                        [ Abstraction
                            (["x"], Application [Variable "x", Variable "x"])
                        , Variable "y"
                        ]
                    )
                `shouldBe` (Application
                               [ Abstraction
                                     ( []
                                     , Application [Variable "y", Variable "y"]
                                     )
                               ]
                           )
            betaReduction
                    (Application
                        [ Abstraction
                            ( ["x"]
                            , Application
                                [ Variable "x"
                                , Application [Variable "x", Variable "x"]
                                ]
                            )
                        , Variable "y"
                        ]
                    )
                `shouldBe` (Application
                               [ Abstraction
                                     ( []
                                     , Application
                                         [ Variable "y"
                                         , Application
                                             [Variable "y", Variable "y"]
                                         ]
                                     )
                               ]
                           )
            betaReduction
                    (Application
                        [ Abstraction
                            ( ["x"]
                            , Application
                                [ Variable "x"
                                , Application [Variable "x", Variable "x"]
                                ]
                            )
                        , Application [Variable "y", Variable "z"]
                        ]
                    )
                `shouldBe` (Application
                               [ Abstraction
                                     ( []
                                     , Application
                                         [ Application
                                             [Variable "y", Variable "z"]
                                         , Application
                                             [ Application
                                                 [Variable "y", Variable "z"]
                                             , Application
                                                 [Variable "y", Variable "z"]
                                             ]
                                         ]
                                     )
                               ]
                           )
        it "ignores nested same var abstractions" $ do
            betaReduction
                    (Application
                        [ Abstraction (["x"], Abstraction (["x"], Variable "x"))
                        , Variable "y"
                        ]
                    )
                `shouldBe` (Application
                               [ Abstraction
                                     ([], Abstraction (["x"], Variable "x"))
                               ]
                           )
            betaReduction
                    (Application
                        [ Abstraction
                            ( ["x"]
                            , Application
                                [ Abstraction (["x"], Variable "x")
                                , Variable "x"
                                ]
                            )
                        , Variable "y"
                        ]
                    )
                `shouldBe` (Application
                               [ Abstraction
                                     ( []
                                     , Application
                                         [ Abstraction (["x"], Variable "x")
                                         , Variable "y"
                                         ]
                                     )
                               ]
                           )
    describe "eval" $ do
        it "performs correct step" $ do
            eval macros (Application [Macro "+", Macro "1", Macro "1"])
                `shouldBe` Right
                               (Application
                                   [ Abstraction
                                       ( ["x", "y", "s", "z"]
                                       , Application
                                           [ Variable "x"
                                           , Variable "s"
                                           , Application
                                               [ Variable "y"
                                               , Variable "s"
                                               , Variable "z"
                                               ]
                                           ]
                                       )
                                   , Macro "1"
                                   , Macro "1"
                                   ]
                               )
            eval
                    macros
                    (Application
                        [ Abstraction
                            ( ["x", "y", "s", "z"]
                            , Application
                                [ Variable "x"
                                , Variable "s"
                                , Application
                                    [Variable "y", Variable "s", Variable "z"]
                                ]
                            )
                        , Macro "1"
                        , Macro "1"
                        ]
                    )
                `shouldBe` Right
                               (Application
                                   [ Abstraction
                                       ( ["y", "s", "z"]
                                       , Application
                                           [ Macro "1"
                                           , Variable "s"
                                           , Application
                                               [ Variable "y"
                                               , Variable "s"
                                               , Variable "z"
                                               ]
                                           ]
                                       )
                                   , Macro "1"
                                   ]
                               )
            eval macros (Application [Macro "1", Macro "1"]) `shouldBe` Right
                (Application
                    [ Abstraction
                        (["s", "z"], Application [Variable "s", Variable "z"])
                    , Macro "1"
                    ]
                )
            eval
                    macros
                    (Application
                        [ Abstraction
                            ( ["s", "z"]
                            , Application [Variable "s", Variable "z"]
                            )
                        , Macro "1"
                        ]
                    )
                `shouldBe` Right
                               (Application
                                   [ Abstraction
                                         ( ["z"]
                                         , Application [Macro "1", Variable "z"]
                                         )
                                   ]
                               )
            eval
                    macros
                    (Application
                        [ Abstraction
                              (["z"], Application [Macro "1", Variable "z"])
                        ]
                    )
                `shouldBe` Right
                               (Application
                                   [ Abstraction
                                         ( ["z"]
                                         , Application
                                             [ Abstraction
                                                 ( ["s", "z"]
                                                 , Application
                                                     [ Variable "s"
                                                     , Variable "z"
                                                     ]
                                                 )
                                             , Variable "z"
                                             ]
                                         )
                                   ]
                               )
            eval
                    macros
                    (Application
                        [ Abstraction
                              ( ["z"]
                              , Application
                                  [ Abstraction
                                      ( ["s", "z"]
                                      , Application [Variable "s", Variable "z"]
                                      )
                                  , Variable "z"
                                  ]
                              )
                        ]
                    )
                `shouldBe` Right
                               (Application
                                   [ Abstraction
                                         ( ["z"]
                                         , Application
                                             [ Abstraction
                                                 ( ["s", "z_"]
                                                 , Application
                                                     [ Variable "s"
                                                     , Variable "z_"
                                                     ]
                                                 )
                                             , Variable "z"
                                             ]
                                         )
                                   ]
                               )
        it "does nothing if no step available"
            $          eval
                           macros
                           (Abstraction
                               (["s", "z"], Application [Variable "s", Variable "z"])
                           )
            `shouldBe` Right
                           (Abstraction
                               ( ["s", "z"]
                               , Application [Variable "s", Variable "z"]
                               )
                           )
        it "returns error on unidentified macro"
            $          eval macros (Macro "X")
            `shouldBe` Left "Error: Can't find macro - X"

