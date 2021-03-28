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
    ["--list"] -> mapM_ print rules

    -- List unset names
    ["--unset"] -> mapM_ print (filter isUnset rules)

    [] -> error "TODO Usage."

    -- Parse inputs of the form `--set a 1` and evaluate one rule
    rest -> do
      let (name, is) = makeInputs rest []
      mapM_ print is
      print (evaluate name rules is)

makeInputs [name] is = (name, is)
makeInputs ("--set" : var : val : rest) is = case val of
  _ | all (`elem` "0123456789") val ->
    makeInputs rest (is ++ [Input var (Int $ read val)])
  "True" ->
    makeInputs rest (is ++ [Input var (Bool True)])
  "False" ->
    makeInputs rest (is ++ [Input var (Bool False)])
  _ -> error "TODO Usage."

isUnset (Rule _ Unset) = True
isUnset _ = False


--------------------------------------------------------------------------------
evaluate name rs is = case filter ((== name) . rName) rs of
  [r] -> case rFormula r of
    Unset -> case lookupInput (rName r) is of
      Just (Bool x) -> Result (Bool x)
      Just (Int x) -> Result (Int x)
      Just _ -> error "Inputs cannot contain Unset or Name."
      Nothing -> UnsetVariables [rName r]
    Exp e -> reduce e rs is
  [] -> Error NoSuchRule
  _ -> Error MultipleRules

reduce e rs is = case e of
  Bool x -> Result (Bool x)
  Int x -> Result (Int x)
  List [] -> Result (List [])
  List (e : es) -> case reduce e rs is of
    (Result x) -> case reduce (List es) rs is of
      (Result (List xs)) -> Result (List (x : xs))
      (Result _) -> error "Can't happen; the Result is necessarily a List."
      Error err -> Error err
      UnsetVariables xs -> UnsetVariables xs
    Error err -> Error err
    UnsetVariables xs -> UnsetVariables xs
  Object kvs -> case reduce (List (map snd kvs)) rs is of
    (Result (List xs)) -> Result (Object (zip (map fst kvs) xs))
    (Result _) -> error "Can't happen; the Result is necessarily a List."
    Error err -> Error err
    UnsetVariables xs -> UnsetVariables xs
  Name name -> evaluate name rs is
  Names names -> case reduce (List (map Name names)) rs is of
    (Result (List xs)) -> Result (Object (zip names xs))
    (Result _) -> error "Can't happen; the Result is necessarily a List."
    Error err -> Error err
    UnsetVariables xs -> UnsetVariables xs
  Cond e1 e2 e3 -> case reduce e1 rs is of
    (Result (Bool True)) -> reduce e2 rs is
    (Result (Bool False)) -> reduce e3 rs is
    (Result t) -> Error (TypeMismatch $ "Expected a Bool, got " ++ show t)
    Error err -> Error err
    UnsetVariables xs -> UnsetVariables xs
  Add e1 e2 -> case (reduce e1 rs is, reduce e2 rs is) of
    (Result (Int a), Result (Int b)) -> Result (Int (a + b))
    (Result (Int _), Result t) -> Error (TypeMismatch $ "Expected an Int, got " ++ show t)
    (Result t, Result (Int _)) -> Error (TypeMismatch $ "Expected an Int, got " ++ show t)
    (Error err, _) -> Error err -- TODO Combine multiple possible errors.
    (_, Error err) -> Error err -- TODO Combine multiple possible errors.
    (UnsetVariables xs, UnsetVariables ys) -> UnsetVariables (nub $ xs ++ ys)
    (UnsetVariables xs, _) -> UnsetVariables xs
    (_, UnsetVariables ys) -> UnsetVariables ys
  Sum [] -> Result (Int 0)
  Sum (e : es) -> reduce (Add e (Sum es)) rs is

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

data Exp =
    Bool Bool
  | Int Int
  | List [Exp]
  | Object [(String, Exp)] -- TODO Use a Map.
  | Name String
  | Names [String] -- ^ I think this is similar to Nix's `inherit`.
  | Cond Exp Exp Exp -- if _ then _ else _
  | Add Exp Exp
  | Sum [Exp]
  deriving (Eq, Show)

data EvaluationError = NoSuchRule | MultipleRules | Cycles | TypeMismatch String
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
  deriving Show

isTypeMismatch (Error (TypeMismatch _)) = True
isTypeMismatch _ = False

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
  , evaluate "h" rules [] == Result (Int 17)
  , evaluate "i" [Rule "i" (Exp $ Bool True)] [] == Result (Bool True)
  , isTypeMismatch $ evaluate "j" [Rule "j" (Exp $ Add (Int 1) (Bool True))] []
  , isTypeMismatch $ evaluate "j" [Rule "j" (Exp $ Add (Bool True) (Int 1))] []
  , evaluate "k" [Rule "k" (Exp $ Cond (Bool True) (Int 1) (Int 2))] []
      == Result (Int 1)
  , isTypeMismatch $ evaluate "j" [Rule "j" (Exp $ Cond (Int 0) (Int 1) (Int 2))] []
  , evaluate "l" rules [] == UnsetVariables ["i"]
  , evaluate "l" rules [Input "i" (Bool True)] == Result (Int 5)
  , evaluate "l" rules [Input "i" (Bool False)] == UnsetVariables ["e"]
  , evaluate "l" rules [Input "i" (Bool False), Input "e" (Int 4)] == Result (Int 4)
  ] 

--------------------------------------------------------------------------------
-- Examples rules to play with the CLI.
rules =
  [ rule_1, rule_2, rule_3, rule_4, rule_5, rule_6, rule_7, rule_8, rule_9
  , rule_10, rule_11, rule_12, rule_13, rule_14, rule_15, rule_cycle
  ]
  ++ form_1

rule_1 = Rule "a" (Exp (Int 5))

rule_2 = Rule "b" (Exp (Int 6))

rule_3 = Rule "c" (Exp (Name "b"))

rule_4 = Rule "d" (Exp (Name "c"))

rule_5 = Rule "e" Unset

-- Reference an unset rule.
rule_6 = Rule "f" (Exp (Name "e"))

rule_7 = Rule "g" (Exp (Add (Name "a") (Name "b")))

rule_8 = Rule "h" (Exp (Sum [Name "a", Name "b", Name "c"]))

rule_9 = Rule "i" Unset

rule_10 = Rule "k" (Exp (Cond (Name "i") (Int 1) (Int 2)))

rule_11 = Rule "l" (Exp (Cond (Name "i") (Name "a") (Name "e")))

rule_12 = Rule "m" (Exp (Object [("a", Int 1)]))

rule_13 = Rule "n" (Exp (Object [("a", Int 1), ("b", Int 2)]))

rule_14 = Rule "o" (Exp (Names ["a", "b"]))

rule_15 = Rule "p" (Exp (List [Int 1, Bool True, Name "a"]))

rule_cycle = Rule "cycle" (Exp (Name "cycle")) -- TODO Find cycles.


--------------------------------------------------------------------------------

-- A example form.
-- The second value must be provided if the first one is set to True.
--   $ runghc script.hs --set "has a cat" True --set "a cat's name" 1 form
--   Result (Object [("has a cat",Bool True),("a cat's name",Int 1)])
--
form_1 =
  [ Rule "has a cat" Unset
  , Rule "a cat's name" Unset
  , Rule "form" (Exp (Cond
      (Name "has a cat")
      (Names ["has a cat", "a cat's name"])
      (Names ["has a cat"])))
  ]
