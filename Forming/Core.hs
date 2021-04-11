-- This file contains the main code.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Forming.Core where

import Control.Arrow (first, right)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.HashMap.Strict as H (toList)
import Data.Aeson (decode, Value)
import qualified Data.Aeson as A (Value(Bool, Number, Object, String))
import Data.List (nub)
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import System.Exit (exitFailure)


--------------------------------------------------------------------------------
runWithInputs :: Computation -> Either String (Maybe String, [Input]) -> IO ()
runWithInputs Computation{..} mis = case mis of
  Right (mname, is) ->
    case evaluate [] (maybe cMain id mname) cRules is of
      UnsetVariables names -> do
        putStrLn "ERROR: missing user inputs."
        printUnsetVariables names
        exitFailure
      Result x -> printValue 0 x
      Error stack err -> do
        putStr "ERROR: "
        printError stack err
  Left err -> do
    putStrLn ("ERROR: " ++ err)
    exitFailure

printUnsetVariables names = do
  putStrLn "This computation expects the following user inputs:\n"
  mapM_ (putStrLn . ("  " ++)) names
  putStrLn "\nUse `--set a 1` to provide the value 1 to the input \"a\"."

printError stack err = case err of
  NoSuchRule name -> putStrLn $ "no such rule \"" ++ name ++"\"."
  MultipleRules name -> putStrLn $
    "multiple rules have the same name \"" ++ name ++ "\"."
  Cycles -> putStrLn "The rules form a cycle."
  TypeMismatch err -> do
    putStrLn $ "type mismatch: " ++ err
    putStrLn $ "while evaluating rules " ++ show stack
  AssertionIntError mname err -> do
    putStrLn $ "an assertion has failed: " ++
      maybe "" (\name -> "\"" ++ name ++ "\" must be ") mname
      ++ show err
    putStrLn $ "while evaluating rules " ++ show stack

printValue indent v = case v of
  Int x -> putStrLn (padding ++ show x)
  Bool x -> putStrLn (padding ++ show x)
  String x -> putStrLn (padding ++ show x)
  Object xs -> mapM_ (\(k, v) -> do
    putStrLn (padding ++ k ++ ": ") >> printValue (indent + 1) v) xs

  where padding = replicate (indent * 2) ' '


--------------------------------------------------------------------------------
-- | This assemble inputs but also return an optional name to be evaluated.
makeInputs :: [String] -> [Input] -> Either String (Maybe String, [Input])
makeInputs ("--set" : var : val : rest) is =
  makeInputs rest (is ++ [Input var $ parseInput val])
makeInputs ["--set"] _ =
  Left "--set expects two arguments (none given here)"
makeInputs ["--set", _] _ =
  Left "--set expects two arguments (only one given here)"
makeInputs [name] is = Right (Just name, is)
makeInputs [] is = Right (Nothing, is)

makeInputsFromJson :: String -> Either String (Maybe String, [Input])
makeInputsFromJson s = case decode (pack s) :: Maybe Value of
  Just (A.Object kvs_) ->
    let kvs = (H.toList kvs_)
    in Right (Nothing, map (\(k, v) -> Input (T.unpack k) (parseInput' v)) kvs)
  Just _ -> Left "input JSON is not an object."
  Nothing -> Left "malformed input JSON."

parseInput val = case val of
  _ | length val > 0 && all (`elem` ("0123456789" :: String)) val ->
    Int $ read val
  "True" -> Bool True
  "False" -> Bool False
  _ -> String val -- TODO Add some type signature, or quotes around strings.

parseInput' (A.Bool x) = Bool x
parseInput' (A.Number x) = case floatingOrInteger x of
  Right  v -> Int v
  Left _ -> error "TODO Support floats"
parseInput' (A.String x) = String (T.unpack x)

isUnset (Rule _ Unset) = True
isUnset _ = False


--------------------------------------------------------------------------------
evaluate stack name rs is = case filter ((== name) . rName) rs of
  [r] -> case rFormula r of
    Unset -> case lookupInput (rName r) is of
      Just (Bool x) -> Result (Bool x)
      Just (Int x) -> Result (Int x)
      Just (String x) -> Result (String x)
      Just _ -> error "Inputs cannot contain Unset or Name."
      Nothing -> UnsetVariables [rName r]
    Exp e -> reduce (name : stack) e rs is
  [] -> Error [name] (NoSuchRule name)
  _ -> Error [name] (MultipleRules name)

reduce stack e rs is = case e of
  Bool x -> Result (Bool x)
  Int x -> Result (Int x)
  AssertInt assertion e -> case reduce stack e rs is of
    Result x -> case check e assertion x of
      Nothing -> Result x
      Just err -> Error stack err
    Error stack' err -> Error stack' err
    UnsetVariables xs -> UnsetVariables xs
  String x -> Result (String x)
  List [] -> Result (List [])
  List (e : es) -> case reduce stack e rs is of
    (Result x) -> case reduce stack (List es) rs is of
      (Result (List xs)) -> Result (List (x : xs))
      (Result _) -> error "Can't happen; the Result is necessarily a List."
      Error stack' err -> Error stack' err
      UnsetVariables xs -> UnsetVariables xs
    Error stack' err -> Error stack' err
    UnsetVariables xs -> UnsetVariables xs
  Object kvs -> case reduce stack (List (map snd kvs)) rs is of
    (Result (List xs)) -> Result (Object (zip (map fst kvs) xs))
    (Result _) -> error "Can't happen; the Result is necessarily a List."
    Error stack' err -> Error stack' err
    UnsetVariables xs -> UnsetVariables xs
  Name name -> evaluate stack name rs is
  Names names -> case reduce stack (List (map Name names)) rs is of
    (Result (List xs)) -> Result (Object (zip names xs))
    (Result _) -> error "Can't happen; the Result is necessarily a List."
    Error stack' err -> Error stack' err
    UnsetVariables xs -> UnsetVariables xs
  Cond e1 e2 e3 -> case reduce stack e1 rs is of
    (Result (Bool True)) -> reduce stack e2 rs is
    (Result (Bool False)) -> reduce stack e3 rs is
    (Result t) -> Error stack (TypeMismatch $ "Expected a Bool, got " ++ show t)
    Error stack' err -> Error stack' err
    UnsetVariables xs -> UnsetVariables xs
  Add e1 e2 -> case (reduce stack e1 rs is, reduce stack e2 rs is) of
    (Result (Int a), Result (Int b)) -> Result (Int (a + b))
    (Result (Int _), Result t) -> Error stack (TypeMismatch $ "Expected an Int, got " ++ show t)
    (Result t, Result (Int _)) -> Error stack (TypeMismatch $ "Expected an Int, got " ++ show t)
    (Error stack' err, _) -> Error stack' err -- TODO Combine multiple possible errors.
    (_, Error stack' err) -> Error stack' err -- TODO Combine multiple possible errors.
    (UnsetVariables xs, UnsetVariables ys) -> UnsetVariables (nub $ xs ++ ys)
    (UnsetVariables xs, _) -> UnsetVariables xs
    (_, UnsetVariables ys) -> UnsetVariables ys
  Sum [] -> Result (Int 0)
  Sum (e : es) -> reduce stack (Add e (Sum es)) rs is

lookupInput name is = lookup name is'
  where is' = map (\(Input name val) -> (name, val)) is

-- TODO There is no sharing, so multiple occurence of same name is computed
-- multiple times.
gatherUnsets name rs = case filter ((== name) . rName) rs of
  [r] -> case rFormula r of
    Unset -> Right [r]
    Exp e -> gatherUnsets' rs e
  [] -> Left (NoSuchRule name)
  _ -> Left (MultipleRules name)

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

-- | Giving the unevaluated expression is used in the special case it is a
-- Name, to provide a better error message.
check :: Exp -> AssertionInt -> Exp -> Maybe EvaluationError
check e a@(GreaterThan y) _x = case _x of
  Int x | x > y -> Nothing
        | otherwise -> case e of
    Name name -> Just (AssertionIntError (Just name) a)
    _ -> Just (AssertionIntError Nothing a)
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

data EvaluationError =
    NoSuchRule String
  | MultipleRules String
  | Cycles
  | TypeMismatch String
  | AssertionIntError (Maybe String) AssertionInt
    -- ^ When the assertion involves directly a Name, it is give here.
  deriving (Eq, Show)

data Result = Result Exp | UnsetVariables [String] | Error [String] EvaluationError
  deriving (Eq, Show)

data RuleError = RuleCannotBeSet String -- ^ Only Unset rules can be Set.
  deriving (Eq, Show)

-- | An input is like the simplest rule: it binds a value to a name.
-- That value is used to replace an unset variable with the same name.
-- TODO Raise an error if an input is provided for a variable that cannot be set.
-- TODO Raise an error if an input is provided for non-existing variable.
data Input = Input String Exp
  deriving Show

isTypeMismatch (Error _ (TypeMismatch _)) = True
isTypeMismatch _ = False
