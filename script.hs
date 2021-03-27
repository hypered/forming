module Main where

import System.Environment (getArgs)


--------------------------------------------------------------------------------
main = do
  args <- getArgs
  case args of

    -- Run the tests
    ["--run-tests"] -> print tests

    -- List all rules
    ["--list"] -> print rules

    -- Evaluate one rule
    [name] -> print (evaluate name rules)

    _ -> error "TODO Usage."


--------------------------------------------------------------------------------
evaluate name rs = case filter ((== name) . rName) rs of
  [r] -> reduce r rs
  [] -> Left NoSuchRule
  _ -> Left MultipleRules

reduce r rs = case rFormula r of
  Int x -> Right (Int x)
  Name name -> evaluate name rs


--------------------------------------------------------------------------------
-- TODO E.g. rule names cannot be the empty string, or multiple rules with the
-- same names cannot be present (although this one is also detected at
-- evaluation time).
validate = undefined

--------------------------------------------------------------------------------
-- A rule is a binding of a name to a formula, which can be reduced (evaluated)
-- to a value. Names can be multiple words, e.g. "meal unit price".
data Rule = Rule
  { rName :: String
  , rFormula :: Value
  }
  deriving Show

-- A value can be a literal, or the use of a rule.
data Value = Int Int | Name String
  deriving (Eq, Show)

data EvaluationError = NoSuchRule | MultipleRules | Cycles
  deriving (Eq, Show)


--------------------------------------------------------------------------------
tests =
  [ evaluate "a" [] == Left NoSuchRule
  , evaluate "a" [Rule "a" (Int 5), Rule "a" (Int 6)] == Left MultipleRules
  , evaluate "a" [Rule "a" (Int 5)] == Right (Int 5)
  , evaluate "a" rules == Right (Int 5)
  , evaluate "b" rules == Right (Int 6)
  , evaluate "c" rules == Right (Int 6)
  , evaluate "d" rules == Right (Int 6)
  ] 

--------------------------------------------------------------------------------
-- Examples rules to play with the CLI.
rules = [rule_1, rule_2, rule_3, rule_4, rule_cycle]

rule_1 = Rule "a" (Int 5)

rule_2 = Rule "b" (Int 6)

rule_3 = Rule "c" (Name "b")

rule_4 = Rule "d" (Name "c")

rule_cycle = Rule "cycle" (Name "cycle") -- TODO Find cycles.
