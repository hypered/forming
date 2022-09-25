module Main where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, (@?=))

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

import Forming
import Forming.Syntax
import Forming.Type


--------------------------------------------------------------------------------
main :: IO ()
main = do
  Tasty.defaultMain (Tasty.testGroup "All tests" [baseTree, assortedTree])


--------------------------------------------------------------------------------
-- | These are evaluation tests on abstract syntax trees (no lexing or parsing
-- involved).
baseTree :: TestTree
baseTree = Tasty.testGroup "Base tests"
  [ HUnit.testCase "Error reporting (no such rule - 1)" $
      eval "a" []
      @?= Error ["a"] (NoSuchRule "a")

  , HUnit.testCase "Error reporting (no such rule - 2)" $
      eval "a" [Binding "b" (Int 5)]
      @?= Error ["a"] (NoSuchRule "a")

  , HUnit.testCase "Error reporting (multiple rules)" $
      eval "a" [Binding "a" (Int 5), Binding "a" (Int 6)]
      @?= Error ["a"] (MultipleRules "a")

  , HUnit.testCase "Error reporting (unset variable)" $
      eval "a" [Unset "a" Nothing]
      @?= UnsetVariables ["a"]


  , HUnit.testCase "a = True" $
      eval "a" [Binding "a" (Bool True)]
      @?= Result (Bool True)

  , HUnit.testCase "a = False" $
      eval "a" [Binding "a" (Bool False)]
      @?= Result (Bool False)

  , HUnit.testCase "a = 5" $
      eval "a" [Binding "a" (Int 5)]
      @?= Result (Int 5)

  , HUnit.testCase "a = \"x\"" $
      eval "a" [Binding "a" (Decimal 3500)]
      @?= Result (Decimal (read "3500.00"))

  , HUnit.testCase "a = \"x\"" $
      eval "a" [Binding "a" (String "x")]
      @?= Result (String "x")


  , HUnit.testCase "Annotation Bool - Bool" $
      eval "a" [Binding "a" (Annotation (Bool True) TBool)]
      @?= Result (Bool True)

  , HUnit.testCase "Annotation Int - Bool" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Binding "a" (Annotation (Int 4) TBool)]

  , HUnit.testCase "Annotation String - Bool" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Binding "a" (Annotation (String "x") TBool)]


  , HUnit.testCase "Annotation Bool - Int" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Binding "a" (Annotation (Bool True) TInt)]

  , HUnit.testCase "Annotation Int - Int" $
      eval "a" [Binding "a" (Annotation (Int 4) TInt)]
      @?= Result (Int 4)

  , HUnit.testCase "Annotation String - Int" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Binding "a" (Annotation (String "x") TInt)]


  , HUnit.testCase "Annotation Bool - String" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Binding "a" (Annotation (Bool True) TString)]

  , HUnit.testCase "Annotation Int - String" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Binding "a" (Annotation (Int 4) TString)]

  , HUnit.testCase "Annotation String - String" $
      eval "a" [Binding "a" (Annotation (String "x") TString)]
      @?= Result (String "x")


  , HUnit.testCase "Annotation Bool - Enum" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Binding "a" (Annotation (Bool True) (TEnum ["x", "y"]))]

  , HUnit.testCase "Annotation Int - Enum" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Binding "a" (Annotation (Int 4) (TEnum ["x", "y"]))]

  , HUnit.testCase "Annotation String - Enum" $
      eval "a" [Binding "a" (Annotation (String "x") (TEnum ["x", "y"]))]
      @?= Result (String "x")

  , HUnit.testCase "Annotation String - Enum (mismatch)" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Binding "a" (Annotation (String "x") (TEnum ["y", "z"]))]


  , HUnit.testCase "Annotated unset variable" $
      evaluate [] "a" [Unset "a" Nothing] [Input "a" (Bool True)]
      @?= Result (Bool True)

  , HUnit.testCase "Annotated unset variable" $
      evaluate [] "a" [Unset "a" (Just TBool)] [Input "a" (Bool True)]
      @?= Result (Bool True)

  , HUnit.testCase "Annotated unset variable" $ assertBool "expected type-mismatch" $
      isTypeMismatch $
        evaluate [] "a" [Unset "a" (Just TInt)] [Input "a" (Bool True)]


  , HUnit.testCase "Union, empty and 1 field" $
      eval "v" [Binding "v" (Union (Object []) (Object [("a", Int 1)]))]
      @?= Result (Object [("a", Int 1)])

  , HUnit.testCase "Union, same field (right biased)" $
      eval "v" [Binding "v" (Union (Object [("a", Int 1)]) (Object [("a", Int 2)]))]
      @?= Result (Object [("a", Int 2)])

  , HUnit.testCase "Union, 1 field and 1 field" $
      eval "v" [Binding "v" (Union (Object [("a", Int 1)]) (Object [("b", Int 2)]))]
      @?= Result (Object [("a", Int 1), ("b", Int 2)])

  , HUnit.testCase "Union, with \"Names\"" $
      eval "v"
        [ Binding "v" (Union (Object [("a", Int 1)]) (Names ["b"]))
        , Binding "b" (Int 2)
        ]
      @?= Result (Object [("a", Int 1), ("b", Int 2)])

  , HUnit.testCase "Integer equality" $
      eval "w" [Binding "w" (Equal (Int 1) (Int 1))]
      @?= Result (Bool True)

  , HUnit.testCase "Integer equality" $
      eval "w" [Binding "w" (Equal (Int 1) (Int 2))]
      @?= Result (Bool False)

  , HUnit.testCase "Boolean equality" $
      eval "w" [Binding "w" (Equal (Bool True) (Bool True))]
      @?= Result (Bool True)

  , HUnit.testCase "Boolean equality" $
      eval "w" [Binding "w" (Equal (Bool True) (Bool False))]
      @?= Result (Bool False)

  , HUnit.testCase "String equality" $
      eval "w" [Binding "w" (Equal (String "a") (String "a"))]
      @?= Result (Bool True)

  , HUnit.testCase "String equality" $
      eval "w" [Binding "w" (Equal (String "a") (String "b"))]
      @?= Result (Bool False)

  , HUnit.testCase "Equality, type mismatch" $ assertBool "expected type-mismatch" $
      isTypeMismatch $
        eval "w" [Binding "w" (Equal (Bool True) (Int 1))]
  ]

assortedTree :: TestTree
assortedTree = Tasty.testGroup "Assorted tests"
  [ HUnit.testCase "TODO" $
      eval "a" rules @?= Result (Int 5)

  , HUnit.testCase "TODO" $
      eval "b" rules @?= Result (Int 6)

  , HUnit.testCase "TODO" $
      eval "c" rules @?= Result (Int 6)

  , HUnit.testCase "TODO" $
      eval "d" rules @?= Result (Int 6)

  , HUnit.testCase "TODO" $
      eval "e" rules @?= UnsetVariables ["e"]

  , HUnit.testCase "TODO" $
      evaluate [] "e" rules [Input "e" (Int 4)] @?= Result (Int 4)

  , HUnit.testCase "TODO" $
      eval "f" rules @?= UnsetVariables ["e"]

  , HUnit.testCase "TODO" $
      evaluate [] "f" rules [Input "e" (Int 4)] @?= Result (Int 4)

  , HUnit.testCase "TODO" $
      eval "g-add" rules @?= Result (Int 11)

  , HUnit.testCase "TODO" $
      eval "g-sub" rules @?= Result (Int (- 1))

  , HUnit.testCase "TODO" $
      eval "g-mul" rules @?= Result (Int 30)

  , HUnit.testCase "TODO" $
      eval "g-div" rules @?= Result (Int 0)

  , HUnit.testCase "TODO" $
      eval "h" rules @?= Result (Int 17)

  , HUnit.testCase "TODO" $
      eval "i" [Binding "i" (Bool True)] @?= Result (Bool True)

  , HUnit.testCase "TODO" $ assertBool "expected type-mismatch" $
      isTypeMismatch $
        eval "j" [Binding "j" $ Add (Int 1) (Bool True)]

  , HUnit.testCase "TODO" $ assertBool "expected type-mismatch" $
      isTypeMismatch $
        eval "j" [Binding "j" $ Add (Bool True) (Int 1)]

  , HUnit.testCase "TODO" $
      eval "k" [Binding "k" $ LessThan (Int 1) (Int 2)]
      @?= Result (Bool True)

  , HUnit.testCase "TODO" $
      eval "k" [Binding "k" $ LessThan (Int 2) (Int 2)]
      @?= Result (Bool False)

  , HUnit.testCase "TODO" $
      eval "k" [Binding "k" $ Cond (Bool True) (Int 1) (Int 2)]
      @?= Result (Int 1)

  , HUnit.testCase "TODO" $ assertBool "expected type-mismatch" $
      isTypeMismatch $
        eval "j" [Binding "j" $ Cond (Int 0) (Int 1) (Int 2)]

  , HUnit.testCase "TODO" $
      eval "l" rules @?= UnsetVariables ["i"]

  , HUnit.testCase "TODO" $
      evaluate [] "l" rules [Input "i" (Bool True)] @?= Result (Int 5)

  , HUnit.testCase "TODO" $
      evaluate [] "l" rules [Input "i" (Bool False)] @?= UnsetVariables ["e"]

  , HUnit.testCase "TODO" $
      evaluate [] "l" rules [Input "i" (Bool False), Input "e" (Int 4)] @?= Result (Int 4)
  ]

eval name rules = evaluate [] name rules []


--------------------------------------------------------------------------------
-- Examples rules to play with the CLI.
rules =
  [ rule_1, rule_2, rule_3, rule_4, rule_5, rule_6
  , rule_7add, rule_7sub , rule_7mul, rule_7div
  , rule_8, rule_9
  , rule_10, rule_11, rule_12, rule_13, rule_14, rule_15, rule_16, rule_17
  , rule_18
  ]

rule_1 = Binding "a" (Int 5)

rule_2 = Binding "b" (Int 6)

rule_3 = Binding "c" (Name "b")

rule_4 = Binding "d" (Name "c")

rule_5 = Unset "e" Nothing

-- Reference an unset rule.
rule_6 = Binding "f" (Name "e")

rule_7add = Binding "g-add" (Add (Name "a") (Name "b"))
rule_7sub = Binding "g-sub" (Sub (Name "a") (Name "b"))
rule_7mul = Binding "g-mul" (Mul (Name "a") (Name "b"))
rule_7div = Binding "g-div" (Div (Name "a") (Name "b"))

rule_8 = Binding "h" (Sum [Name "a", Name "b", Name "c"])

rule_9 = Unset "i" Nothing

rule_10 = Binding "k" (Cond (Name "i") (Int 1) (Int 2))

rule_11 = Binding "l" (Cond (Name "i") (Name "a") (Name "e"))

rule_12 = Binding "m" (Object [("a", Int 1)])

rule_13 = Binding "n" (Object [("a", Int 1), ("b", Int 2)])

rule_14 = Binding "o" (Names ["a", "b"])

rule_15 = Binding "p" (List [Int 1, Bool True, Name "a"])

rule_16 = Binding "q" (String "a")

rule_17 = Binding "r" (AssertInt (Name "e") (GreaterThan 1))

rule_18 = Binding "cycle" (Name "cycle") -- TODO Find cycles.
