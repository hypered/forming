-- This is the main development script: it can be loaded in GHCi or run from the
-- command-line to play with the concepts of this library.

module Main where

import System.Environment (getArgs)

import Forming


--------------------------------------------------------------------------------
main = print (all id tests, tests)


--------------------------------------------------------------------------------
tests =
  [ evaluate [] "a" [] [] == Error ["a"] (NoSuchRule "a")
  , evaluate [] "a" [Rule "a" (Exp $ Int 5), Rule "a" (Exp $ Int 6)] [] ==
    Error ["a"] (MultipleRules "a")
  , evaluate [] "a" [Rule "a" (Exp $ Int 5)] [] == Result (Int 5)
  , evaluate [] "a" rules [] == Result (Int 5)
  , evaluate [] "b" rules [] == Result (Int 6)
  , evaluate [] "c" rules [] == Result (Int 6)
  , evaluate [] "d" rules [] == Result (Int 6)
  , evaluate [] "e" rules [] == UnsetVariables ["e"]
  , evaluate [] "e" rules [Input "e" (Int 4)] == Result (Int 4)
  , evaluate [] "f" rules [] == UnsetVariables ["e"]
  , evaluate [] "f" rules [Input "e" (Int 4)] == Result (Int 4)
  , evaluate [] "g-add" rules [] == Result (Int 11)
  , evaluate [] "g-sub" rules [] == Result (Int (- 1))
  , evaluate [] "g-mul" rules [] == Result (Int 30)
  , evaluate [] "g-div" rules [] == Result (Int 0)
  , evaluate [] "h" rules [] == Result (Int 17)
  , evaluate [] "i" [Rule "i" (Exp $ Bool True)] [] == Result (Bool True)
  , isTypeMismatch $ evaluate [] "j" [Rule "j" (Exp $ Add (Int 1) (Bool True))] []
  , isTypeMismatch $ evaluate [] "j" [Rule "j" (Exp $ Add (Bool True) (Int 1))] []
  , evaluate [] "k" [Rule "k" (Exp $ Cond (Bool True) (Int 1) (Int 2))] []
      == Result (Int 1)
  , isTypeMismatch $ evaluate [] "j" [Rule "j" (Exp $ Cond (Int 0) (Int 1) (Int 2))] []
  , evaluate [] "l" rules [] == UnsetVariables ["i"]
  , evaluate [] "l" rules [Input "i" (Bool True)] == Result (Int 5)
  , evaluate [] "l" rules [Input "i" (Bool False)] == UnsetVariables ["e"]
  , evaluate [] "l" rules [Input "i" (Bool False), Input "e" (Int 4)] == Result (Int 4)
  , evaluate [] "q" [Rule "q" (Exp $ String "a")] [] == Result (String "a")
  , isTypeMismatch $
    evaluate [] "r" [Rule "r" (Exp (Annotation (String "a") TInt))] []
  , evaluate [] "r" [Rule "r" (Exp (Annotation (Int 4) TInt))] [] == Result (Int 4)
  ] 

--------------------------------------------------------------------------------
-- Examples rules to play with the CLI.
rules =
  [ rule_1, rule_2, rule_3, rule_4, rule_5, rule_6
  , rule_7add, rule_7sub , rule_7mul, rule_7div
  , rule_8, rule_9
  , rule_10, rule_11, rule_12, rule_13, rule_14, rule_15, rule_16, rule_17
  , rule_cycle
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

rule_cycle = Rule "cycle" (Exp (Name "cycle")) -- TODO Find cycles.
