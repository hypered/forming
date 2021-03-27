module Main where

import Data.List (nub)
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
    [name] ->
      print (evaluate name rules [])

    -- Evaluate one rule, but also provide an input
    "--set" : var : val : [name] -> do
      print $ evaluate name rules [Input var (Int $ read val)]

    _ -> error "TODO Usage."


--------------------------------------------------------------------------------
evaluate name rs is = case filter ((== name) . rName) rs of
  [r] -> case rFormula r of
    Unset -> case lookupInput (rName r) is of
      Just (Int x) -> Result (Int x)
      Just _ -> error "Inputs cannot contain Unset or Name."
      Nothing -> UnsetVariables [rName r]
    Exp e -> reduce e rs is
  [] -> Error NoSuchRule
  _ -> Error MultipleRules

reduce e rs is = case e of
  Int x -> Result (Int x)
  Name name -> evaluate name rs is
  Add e1 e2 -> case (reduce e1 rs is, reduce e2 rs is) of
    (Result (Int a), Result (Int b)) -> Result (Int (a + b))
    (Error err, _) -> Error err -- TODO Combine multiple possible errors.
    (_, Error err) -> Error err -- TODO Combine multiple possible errors.
    (UnsetVariables xs, UnsetVariables ys) -> UnsetVariables (nub $ xs ++ ys)
    (UnsetVariables xs, _) -> UnsetVariables xs
    (_, UnsetVariables ys) -> UnsetVariables ys

lookupInput name is = lookup name is'
  where is' = map (\(Input name val) -> (name, val)) is


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
  , rFormula :: Formula
  }
  deriving (Eq, Show)

data Formula = Unset | Exp Exp
  deriving (Eq, Show)

-- An expression can be a literal, or the use of a rule, or an addition.
data Exp = Int Int | Name String | Add Exp Exp
  deriving (Eq, Show)

data EvaluationError = NoSuchRule | MultipleRules | Cycles
  deriving (Eq, Show)

data Result = Result Exp | UnsetVariables [String] | Error EvaluationError
  deriving (Eq, Show)

data RuleError = RuleCannotBeSet String -- ^ Only Unset rules can be Set.
  deriving (Eq, Show)

-- | An input is like the simplest rule: it binds a value to a name.
-- That value is used to replace an unset variable with the same name.
-- TODO Raise an error if an input is provided for a variable that cannot be set.
-- TODO Raise an error if an input is provided for non-existing variable.
data Input = Input String Exp

--------------------------------------------------------------------------------
tests =
  [ evaluate "a" [] [] == Error NoSuchRule
  , evaluate "a" [Rule "a" (Exp $ Int 5), Rule "a" (Exp $ Int 6)] [] == Error MultipleRules
  , evaluate "a" [Rule "a" (Exp $ Int 5)] [] == Result (Int 5)
  , evaluate "a" rules [] == Result (Int 5)
  , evaluate "b" rules [] == Result (Int 6)
  , evaluate "c" rules [] == Result (Int 6)
  , evaluate "d" rules [] == Result (Int 6)
  , evaluate "e" rules [] == UnsetVariables ["e"]
  , evaluate "e" rules [Input "e" (Int 4)] == Result (Int 4)
  , evaluate "f" rules [] == UnsetVariables ["e"]
  , evaluate "f" rules [Input "e" (Int 4)] == Result (Int 4)
  , evaluate "g" rules [] == Result (Int 11)
  ] 

--------------------------------------------------------------------------------
-- Examples rules to play with the CLI.
rules = [rule_1, rule_2, rule_3, rule_4, rule_5, rule_6, rule_7, rule_cycle]

rule_1 = Rule "a" (Exp (Int 5))

rule_2 = Rule "b" (Exp (Int 6))

rule_3 = Rule "c" (Exp (Name "b"))

rule_4 = Rule "d" (Exp (Name "c"))

rule_5 = Rule "e" Unset

-- Reference an unset rule.
rule_6 = Rule "f" (Exp (Name "e"))

rule_7 = Rule "g" (Exp (Add (Name "a") (Name "b")))

rule_cycle = Rule "cycle" (Exp (Name "cycle")) -- TODO Find cycles.
