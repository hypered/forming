-- This file contains the main code. To interact with it during development,
-- use bin/play.hs.

{-# LANGUAGE RecordWildCards #-}

module Core where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Data.List (nub)


--------------------------------------------------------------------------------
run Computation{..} = do
  args <- getArgs

  case args of

    -- List all rules.
    ["--list"] -> mapM_ print cRules

    -- List unset names.
    ["--unset"] -> mapM_ print (filter isUnset cRules)

    -- List unset names involved in a specific rule.
    ["--unset", name] -> print (gatherUnsets name cRules)

    -- Show a help message.
    ["--help"] -> do
      putStrLn cName
      -- Evaluate without input to give a hint a possible user inputs.
      case evaluate cMain cRules [] of
        UnsetVariables names -> printResult (UnsetVariables names)
        Result _ -> putStrLn "This computation doesn't require any user input."

    -- Parse inputs of the form `--set a 1` and evaluate one rule.
    rest -> do
      case makeInputs rest [] of
        Right (mname, is) ->
          case evaluate (maybe cMain id mname) cRules is of
            UnsetVariables names -> do
              putStrLn "ERROR: missing user inputs."
              printResult (UnsetVariables names)
              exitFailure
            Result x -> printResult (Result x)
        Left err -> do
          putStrLn ("ERROR: " ++ err)
          exitFailure

printResult r = case r of
  UnsetVariables names -> do
    putStrLn "This computation expects the following user inputs:\n"
    mapM_ (putStrLn . ("  " ++)) names
    putStrLn "\nUse `--set a 1` to provide the value 1 to the input \"a\"."
  Result (Int x) -> print x


--------------------------------------------------------------------------------
-- | This assemble inputs but also return an optional name to be evaluated.
makeInputs ("--set" : var : val : rest) is = case val of
  _ | all (`elem` "0123456789") val ->
    makeInputs rest (is ++ [Input var (Int $ read val)])
  "True" ->
    makeInputs rest (is ++ [Input var (Bool True)])
  "False" ->
    makeInputs rest (is ++ [Input var (Bool False)])
  _ ->
    --TODO Add some type signature, or quotes araound strings.
    makeInputs rest (is ++ [Input var (String val)])
makeInputs ["--set"] _ =
  Left "--set expects two arguments (none given here)"
makeInputs ["--set", _] _ =
  Left "--set expects two arguments (only one given here)"
makeInputs [name] is = Right (Just name, is)
makeInputs [] is = Right (Nothing, is)

isUnset (Rule _ Unset) = True
isUnset _ = False


--------------------------------------------------------------------------------
evaluate name rs is = case filter ((== name) . rName) rs of
  [r] -> case rFormula r of
    Unset -> case lookupInput (rName r) is of
      Just (Bool x) -> Result (Bool x)
      Just (Int x) -> Result (Int x)
      Just (String x) -> Result (String x)
      Just _ -> error "Inputs cannot contain Unset or Name."
      Nothing -> UnsetVariables [rName r]
    Exp e -> reduce e rs is
  [] -> Error NoSuchRule
  _ -> Error MultipleRules

reduce e rs is = case e of
  Bool x -> Result (Bool x)
  Int x -> Result (Int x)
  AssertInt assertion e -> case reduce e rs is of
    Result x -> case check assertion x of
      Nothing -> Result x
      Just err -> Error err
    Error err -> Error err
    UnsetVariables xs -> UnsetVariables xs
  String x -> Result (String x)
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

-- TODO There is no sharing, so multiple occurence of same name is computed
-- multiple times.
gatherUnsets name rs = case filter ((== name) . rName) rs of
  [r] -> case rFormula r of
    Unset -> Right [r]
    Exp e -> gatherUnsets' rs e
  [] -> Left NoSuchRule
  _ -> Left MultipleRules

gatherUnsets' rs e = case e of
  Bool x -> Right []
  Int x -> Right []
  String x -> Right []
  List [] -> Right []
  List (e : es) -> case gatherUnsets' rs e of
    Right x -> case gatherUnsets' rs (List es) of
      Right xs -> Right (x ++ xs)
      Left err -> Left err
    Left err -> Left err
  Object kvs -> gatherUnsets' rs (List (map snd kvs))
  Name name -> gatherUnsets name rs
  Names names -> gatherUnsets' rs (List (map Name names))
  Cond e1 e2 e3 -> gatherUnsets' rs (List [e1, e2, e3])
  Add e1 e2 -> gatherUnsets' rs (List [e1, e2])
  Sum es -> gatherUnsets' rs (List es)

check :: AssertionInt -> Exp -> Maybe EvaluationError
check a@(GreaterThan y) _x = case _x of
  Int x | x > y -> Nothing
        | otherwise -> Just (AssertionIntError a)
  _ -> Just (TypeMismatch ("Expected an Int, got " ++ show _x))


--------------------------------------------------------------------------------
-- TODO E.g. rule names cannot be the empty string, or multiple rules with the
-- same names cannot be present (although this one is also detected at
-- evaluation time).
-- TODO Raise an error when a user input is given for fixed rule (i.e. not an
-- Unset).
validate = undefined

--------------------------------------------------------------------------------
-- A computation, could also be called a form, is a list of rules with a main
-- one to evaluate.
data Computation = Computation
  { cName :: String
  , cMain :: String -- Default rule to evaluate, must appear in the cRules.
  , cRules :: [Rule]
  }


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
    -- Is it really useful to have assertions on whole Exp, instead of Unset
    -- values ?
  | Int Int | AssertInt AssertionInt Exp
  | String String
  | List [Exp]
  | Object [(String, Exp)] -- TODO Use a Map.
  | Name String
  | Names [String] -- ^ I think this is similar to Nix's `inherit`.
  | Cond Exp Exp Exp -- if _ then _ else _
  | Add Exp Exp
  | Sum [Exp]
  deriving (Eq, Show)

data AssertionInt = GreaterThan Int
  deriving (Eq, Show)

data EvaluationError = NoSuchRule | MultipleRules | Cycles | TypeMismatch String
  | AssertionIntError AssertionInt
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
