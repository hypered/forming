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
    [name] ->
      print (evaluate name rules [])

    -- Evaluate one rule, but also provide an input
    "--set" : var : val : [name] -> do
      print $ evaluate name rules [Input var (Int $ read val)]

    _ -> error "TODO Usage."


--------------------------------------------------------------------------------
evaluate name rs is = case filter ((== name) . rName) rs of
  [r] -> reduce r rs is
  [] -> Error NoSuchRule
  _ -> Error MultipleRules

reduce r rs is = case rFormula r of
  Unset -> case lookupInput (rName r) is of
    Just (Int x) -> Result (Int x)
    Just _ -> error "Inputs cannot contain Unset or Name."
    Nothing -> UnsetVariables [rName r]
  Int x -> Result (Int x)
  Name name -> evaluate name rs is

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
  , rFormula :: Exp
  }
  deriving (Eq, Show)

-- An expression can be unset, a literal, or the use of a rule.
data Exp = Unset | Int Int | Name String
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
  , evaluate "a" [Rule "a" (Int 5), Rule "a" (Int 6)] [] == Error MultipleRules
  , evaluate "a" [Rule "a" (Int 5)] [] == Result (Int 5)
  , evaluate "a" rules [] == Result (Int 5)
  , evaluate "b" rules [] == Result (Int 6)
  , evaluate "c" rules [] == Result (Int 6)
  , evaluate "d" rules [] == Result (Int 6)
  , evaluate "e" rules [] == UnsetVariables ["e"]
  , evaluate "e" rules [Input "e" (Int 4)] == Result (Int 4)
  , evaluate "f" rules [] == UnsetVariables ["e"]
  , evaluate "f" rules [Input "e" (Int 4)] == Result (Int 4)
  ] 

--------------------------------------------------------------------------------
-- Examples rules to play with the CLI.
rules = [rule_1, rule_2, rule_3, rule_4, rule_5, rule_6, rule_cycle]

rule_1 = Rule "a" (Int 5)

rule_2 = Rule "b" (Int 6)

rule_3 = Rule "c" (Name "b")

rule_4 = Rule "d" (Name "c")

rule_5 = Rule "e" Unset

-- Reference an unset rule.
rule_6 = Rule "f" (Name "e")

rule_cycle = Rule "cycle" (Name "cycle") -- TODO Find cycles.
