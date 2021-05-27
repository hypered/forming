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
      eval "a" [Rule "b" (Exp (Int 5))]
      @?= Error ["a"] (NoSuchRule "a")

  , HUnit.testCase "Error reporting (multiple rules)" $
      eval "a" [Rule "a" (Exp (Int 5)), Rule "a" (Exp (Int 6))]
      @?= Error ["a"] (MultipleRules "a")

  , HUnit.testCase "Error reporting (unset variable)" $
      eval "a" [Rule "a" Unset]
      @?= UnsetVariables ["a"]


  , HUnit.testCase "a = True" $
      eval "a" [Rule "a" (Exp (Bool True))]
      @?= Result (Bool True)

  , HUnit.testCase "a = False" $
      eval "a" [Rule "a" (Exp (Bool False))]
      @?= Result (Bool False)

  , HUnit.testCase "a = 5" $
      eval "a" [Rule "a" (Exp (Int 5))]
      @?= Result (Int 5)

  , HUnit.testCase "a = \"x\"" $
      eval "a" [Rule "a" (Exp (Decimal 3500))]
      @?= Result (Decimal (read "3500.00"))

  , HUnit.testCase "a = \"x\"" $
      eval "a" [Rule "a" (Exp (String "x"))]
      @?= Result (String "x")


  , HUnit.testCase "Annotation Bool - Bool" $
      eval "a" [Rule "a" (Exp (Annotation (Bool True) TBool))]
      @?= Result (Bool True)

  , HUnit.testCase "Annotation Int - Bool" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Rule "a" (Exp (Annotation (Int 4) TBool))]

  , HUnit.testCase "Annotation String - Bool" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Rule "a" (Exp (Annotation (String "x") TBool))]


  , HUnit.testCase "Annotation Bool - Int" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Rule "a" (Exp (Annotation (Bool True) TInt))]

  , HUnit.testCase "Annotation Int - Int" $
      eval "a" [Rule "a" (Exp (Annotation (Int 4) TInt))]
      @?= Result (Int 4)

  , HUnit.testCase "Annotation String - Int" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Rule "a" (Exp (Annotation (String "x") TInt))]


  , HUnit.testCase "Annotation Bool - String" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Rule "a" (Exp (Annotation (Bool True) TString))]

  , HUnit.testCase "Annotation Int - String" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Rule "a" (Exp (Annotation (Int 4) TString))]

  , HUnit.testCase "Annotation String - String" $
      eval "a" [Rule "a" (Exp (Annotation (String "x") TString))]
      @?= Result (String "x")


  , HUnit.testCase "Annotation Bool - Enum" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Rule "a" (Exp (Annotation (Bool True) (TEnum ["x", "y"])))]

  , HUnit.testCase "Annotation Int - Enum" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Rule "a" (Exp (Annotation (Int 4) (TEnum ["x", "y"])))]

  , HUnit.testCase "Annotation String - Enum" $
      eval "a" [Rule "a" (Exp (Annotation (String "x") (TEnum ["x", "y"])))]
      @?= Result (String "x")

  , HUnit.testCase "Annotation String - Enum (mismatch)" $
      assertBool "expected type-mismatch" $ isTypeMismatch $
        eval "a" [Rule "a" (Exp (Annotation (String "x") (TEnum ["y", "z"])))]


  , HUnit.testCase "TODO" $
      eval "v" [Rule "v" (Exp (Union (Object []) (Object [("a", Int 1)])))]
      @?= Result (Object [("a", Int 1)])

  , HUnit.testCase "TODO" $
      eval "v" [Rule "v" (Exp (Union (Object [("a", Int 1)]) (Object [("a", Int 2)])))]
      @?= Result (Object [("a", Int 2)])

  , HUnit.testCase "TODO" $
      eval "v" [Rule "v" (Exp (Union (Object [("a", Int 1)]) (Object [("b", Int 2)])))]
      @?= Result (Object [("a", Int 1), ("b", Int 2)])

  , HUnit.testCase "TODO" $
      eval "v"
        [ Rule "v" (Exp (Union (Object [("a", Int 1)]) (Names ["b"])))
        , Rule "b" (Exp (Int 2))
        ]
      @?= Result (Object [("a", Int 1), ("b", Int 2)])

  , HUnit.testCase "TODO" $
      eval "w" [Rule "w" (Exp (Equal (Int 1) (Int 1)))]
      @?= Result (Bool True)

  , HUnit.testCase "TODO" $
      eval "w" [Rule "w" (Exp (Equal (Int 1) (Int 2)))]
      @?= Result (Bool False)

  , HUnit.testCase "TODO" $
      eval "w" [Rule "w" (Exp (Equal (Bool True) (Bool True)))]
      @?= Result (Bool True)

  , HUnit.testCase "TODO" $
      eval "w" [Rule "w" (Exp (Equal (Bool True) (Bool False)))]
      @?= Result (Bool False)

  , HUnit.testCase "TODO" $
      eval "w" [Rule "w" (Exp (Equal (String "a") (String "a")))]
      @?= Result (Bool True)

  , HUnit.testCase "TODO" $
      eval "w" [Rule "w" (Exp (Equal (String "a") (String "b")))]
      @?= Result (Bool False)

  , HUnit.testCase "TODO" $ assertBool "expected type-mismatch" $
      isTypeMismatch $
        eval "w" [Rule "w" (Exp (Equal (Bool True) (Int 1)))]
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
      eval "i" [Rule "i" (Exp $ Bool True)] @?= Result (Bool True)

  , HUnit.testCase "TODO" $ assertBool "expected type-mismatch" $
      isTypeMismatch $
        eval "j" [Rule "j" (Exp $ Add (Int 1) (Bool True))]

  , HUnit.testCase "TODO" $ assertBool "expected type-mismatch" $
      isTypeMismatch $
        eval "j" [Rule "j" (Exp $ Add (Bool True) (Int 1))]

  , HUnit.testCase "TODO" $
      eval "k" [Rule "k" (Exp $ LessThan (Int 1) (Int 2))]
      @?= Result (Bool True)

  , HUnit.testCase "TODO" $
      eval "k" [Rule "k" (Exp $ LessThan (Int 2) (Int 2))]
      @?= Result (Bool False)

  , HUnit.testCase "TODO" $
      eval "k" [Rule "k" (Exp $ Cond (Bool True) (Int 1) (Int 2))]
      @?= Result (Int 1)

  , HUnit.testCase "TODO" $ assertBool "expected type-mismatch" $
      isTypeMismatch $
        eval "j" [Rule "j" (Exp $ Cond (Int 0) (Int 1) (Int 2))]

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

rule_1 = Rule "a" (Exp (Int 5))

rule_2 = Rule "b" (Exp (Int 6))

rule_3 = Rule "c" (Exp (Name "b"))

rule_4 = Rule "d" (Exp (Name "c"))

rule_5 = Rule "e" Unset

-- Reference an unset rule.
rule_6 = Rule "f" (Exp (Name "e"))

rule_7add = Rule "g-add" (Exp (Add (Name "a") (Name "b")))
rule_7sub = Rule "g-sub" (Exp (Sub (Name "a") (Name "b")))
rule_7mul = Rule "g-mul" (Exp (Mul (Name "a") (Name "b")))
rule_7div = Rule "g-div" (Exp (Div (Name "a") (Name "b")))

rule_8 = Rule "h" (Exp (Sum [Name "a", Name "b", Name "c"]))

rule_9 = Rule "i" Unset

rule_10 = Rule "k" (Exp (Cond (Name "i") (Int 1) (Int 2)))

rule_11 = Rule "l" (Exp (Cond (Name "i") (Name "a") (Name "e")))

rule_12 = Rule "m" (Exp (Object [("a", Int 1)]))

rule_13 = Rule "n" (Exp (Object [("a", Int 1), ("b", Int 2)]))

rule_14 = Rule "o" (Exp (Names ["a", "b"]))

rule_15 = Rule "p" (Exp (List [Int 1, Bool True, Name "a"]))

rule_16 = Rule "q" (Exp (String "a"))

rule_17 = Rule "r" (Exp (AssertInt (Name "e") (GreaterThan 1)))

rule_18 = Rule "cycle" (Exp (Name "cycle")) -- TODO Find cycles.
