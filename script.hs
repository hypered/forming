module Main where

import Data.Either (partitionEithers)
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
    "--set" : var : val : [name] -> do
      case replace var (Int $ read val) rules of
        Right rules' -> print $ evaluate name rules'
        Left err -> print err

    _ -> error "TODO Usage."


--------------------------------------------------------------------------------
evaluate name rs = case filter ((== name) . rName) rs of
  [r] -> reduce r rs
  [] -> Error NoSuchRule
  _ -> Error MultipleRules

reduce r rs = case rFormula r of
  Unset -> UnsetVariables [rName r]
  Int x -> Result (Int x)
  Name name -> evaluate name rs

-- TODO This doesn't make an error it the rule to be replaced doesn't exist.
replace var val rs = case partitionEithers (map f rs) of
  ([], rs') -> Right rs'
  (errors, _) -> Left errors
  where
  f r = if rName r == var
        then
          if rFormula r == Unset
          then Right (Rule var val)
          else Left (RuleCannotBeSet (rName r))
        else Right r


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
  deriving (Eq, Show)

-- A value can be unset, a literal, or the use of a rule.
data Value = Unset | Int Int | Name String
  deriving (Eq, Show)

data EvaluationError = NoSuchRule | MultipleRules | Cycles
  deriving (Eq, Show)

data Result = Result Value | UnsetVariables [String] | Error EvaluationError
  deriving (Eq, Show)

data RuleError = RuleCannotBeSet String -- ^ Only Unset rules can be Set.
  deriving (Eq, Show)

--------------------------------------------------------------------------------
tests =
  [ evaluate "a" [] == Error NoSuchRule
  , evaluate "a" [Rule "a" (Int 5), Rule "a" (Int 6)] == Error MultipleRules
  , evaluate "a" [Rule "a" (Int 5)] == Result (Int 5)
  , evaluate "a" rules == Result (Int 5)
  , evaluate "b" rules == Result (Int 6)
  , evaluate "c" rules == Result (Int 6)
  , evaluate "d" rules == Result (Int 6)
  , evaluate "e" rules == UnsetVariables ["e"]
  , replace "a" (Int 4) [rule_1] == Left [RuleCannotBeSet "a"]
  , replace "e" (Int 4) [rule_5] == Right [Rule "e" (Int 4)]
  ] 

--------------------------------------------------------------------------------
-- Examples rules to play with the CLI.
rules = [rule_1, rule_2, rule_3, rule_4, rule_5, rule_cycle]

rule_1 = Rule "a" (Int 5)

rule_2 = Rule "b" (Int 6)

rule_3 = Rule "c" (Name "b")

rule_4 = Rule "d" (Name "c")

rule_5 = Rule "e" Unset

rule_cycle = Rule "cycle" (Name "cycle") -- TODO Find cycles.
