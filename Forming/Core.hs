-- This file contains the main code.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Forming.Core where

import Control.Arrow (first, right)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.HashMap.Strict as H (toList)
import Data.Aeson (decode, Value)
import qualified Data.Aeson as A (Value(Bool, Number, Object, String))
import Data.List (deleteFirstsBy, intersperse, nub, nubBy)
import Data.Maybe (isNothing)
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
  TypeMismatch mname err -> do
    putStrLn $ "type mismatch: " ++
      err ++
      maybe "" (\name -> " for the variable \"" ++ name ++ "\"") mname
    putStrLn $ "while evaluating rules " ++ show stack
  AssertionIntError mname err -> do
    putStrLn $ "an assertion has failed: " ++
      maybe "" (\name -> "\"" ++ name ++ "\" must be ") mname
      ++ show err
    putStrLn $ "while evaluating rules " ++ show stack
  AssertionError mname -> do
    putStrLn $ "an assertion has failed: " ++
      maybe "" (\name -> "\"" ++ name ++ "\" ") mname
      ++ "must be True"
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
  AssertInt e assertion -> case reduce stack e rs is of
    Result x -> case check e assertion x of
      Nothing -> Result x
      Just err -> Error stack err
    Error stack' err -> Error stack' err
    UnsetVariables xs -> UnsetVariables xs
  String x -> Result (String x)
  Annotation e t -> case reduce stack e rs is of
    Result x -> case checkType e t x of
      Nothing -> Result x
      Just err -> Error stack err
    Error stack' err -> Error stack' err
    UnsetVariables xs -> UnsetVariables xs
  Assert e assertion -> case reduce stack assertion rs is of
    Result x -> case bcheck assertion x of
      Nothing -> reduce stack e rs is
      Just err -> Error stack err
    Error stack' err -> Error stack' err
    UnsetVariables xs -> UnsetVariables xs
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
    (Result t) -> Error stack (TypeMismatch Nothing $ "Expected a Bool, got " ++ show t)
    Error stack' err -> Error stack' err
    UnsetVariables xs -> UnsetVariables xs
  Add e1 e2 -> ibinop (+) stack rs is e1 e2
  Sub e1 e2 -> ibinop (-) stack rs is e1 e2
  Mul e1 e2 -> ibinop (*) stack rs is e1 e2
  Div e1 e2 -> ibinop div stack rs is e1 e2
  Sum [] -> Result (Int 0)
  Sum (e : es) -> reduce stack (Add e (Sum es)) rs is

  LessThan e1 e2 -> ibbinop (<) stack rs is e1 e2

  Equal e1 e2 -> equal stack rs is e1 e2
  Union e1 e2 -> union unionRight stack rs is e1 e2

bbinop = binop TBool . op
  where op f a b = case (a, b) of
          (Bool a_, Bool b_) -> Bool (f a_ b_)
          (_, _) -> error "bbinop called with wrong types"
          -- It may mean that checkType has a bug.

ibinop = binop TInt . op
  where op f a b = case (a, b) of
          (Int a_, Int b_) -> Int (f a_ b_)
          (_, _) -> error "ibinop called with wrong types"
          -- It may mean that checkType has a bug.

ibbinop = binop TInt . op
  where op f a b = case (a, b) of
          (Int a_, Int b_) -> Bool (f a_ b_)
          (_, _) -> error "ibinop called with wrong types"
          -- It may mean that checkType has a bug.

union = binop TObject . op
  where op f a b = case (a, b) of
          (Object as, Object bs) -> Object (f as bs)
          (_, _) -> error "union called with wrong types"
          -- It may mean that checkType has a bug.

equal = binop' (checkingEqClass (\a b -> Bool (a == b)))

unionRight as bs = deleteFirstsBy (\a b -> fst a == fst b) as bs ++ bs

binop t f stack rs is e1 e2 = binop' (checkingType t f) stack rs is e1 e2

binop' f stack rs is e1 e2 = case (reduce stack e1 rs is, reduce stack e2 rs is) of
  (Result a, Result b) -> f stack e1 e2 a b
  (Error stack' err, _) ->
    Error stack' err -- TODO Combine multiple possible errors.
  (_, Error stack' err) ->
    Error stack' err -- TODO Combine multiple possible errors.
  (UnsetVariables xs, UnsetVariables ys) ->
    UnsetVariables (nub $ xs ++ ys)
  (UnsetVariables xs, _) ->
    UnsetVariables xs
  (_, UnsetVariables ys) ->
    UnsetVariables ys

checkingType t f stack e1 e2 a b =
  case (checkType e1 t a, checkType e2 t b) of
    (Nothing, Nothing) ->
      -- We know that a and b are both t, unless checkType is bugged.
      Result (f a b)
    (Just err, _) -> Error stack err
    (_, Just err) -> Error stack err

checkingEqClass f stack e1 e2 a b =
  case (a, b) of
    (Int _, Int _) -> Result (f a b)
    (Bool _, Bool _) -> Result (f a b)
    (String _, String _) -> Result (f a b)
    _ -> Error stack (TypeMismatch Nothing "TODO Better error message")

lookupInput name is = lookup name is'
  where is' = map (\(Input name val) -> (name, val)) is

-- TODO There is no sharing, so multiple occurence of same name is computed
-- multiple times.
-- In addition of reporting unset variables, this reports its type when it is
-- trivial to do (e.g. there is a direct annotation of that variable).
gatherUnsets :: Maybe Type -> String -> [Rule]
  -> Either EvaluationError [(Rule, Maybe Type)]
gatherUnsets mtype name rs = case filter ((== name) . rName) rs of
  [r] -> case rFormula r of
    Unset -> Right [(r, mtype)]
    Exp e -> gatherUnsets' mtype rs e
  [] -> Left (NoSuchRule name)
  _ -> Left (MultipleRules name)

gatherUnsets' :: Maybe Type -> [Rule] -> Exp -> Either EvaluationError [(Rule, Maybe Type)]
gatherUnsets' mtype rs e = case e of
  Bool x -> Right []
  Int x -> Right []
  AssertInt e1 _ -> gatherUnsets' (Just TInt) rs e1
  String x -> Right []
  Annotation e1 t -> gatherUnsets' (Just t) rs e1
  Assert e1 e2 -> gatherUnsets' mtype rs (List
    [ maybe e1 (Annotation e1) mtype
    , Annotation e2 TBool
    ])
  List [] -> Right []
  List (e : es) -> case gatherUnsets' Nothing rs e of
    Right x -> case gatherUnsets' Nothing rs (List es) of
      Right xs ->
        let typedFirsts = filter (not . isNothing . snd) (x ++ xs) ++ x ++ xs
        in Right (nubBy (\a b -> rName (fst a) == rName (fst b)) typedFirsts)
      Left err -> Left err
    Left err -> Left err
  Object kvs -> gatherUnsets' mtype rs (List (map snd kvs))
  Name name -> gatherUnsets mtype name rs
  Names names -> gatherUnsets' mtype rs (List (map Name names))
  Cond e1 e2 e3 -> gatherUnsets' mtype rs (List
    [ Annotation e1 TBool
    , maybe e2 (Annotation e2) mtype
    , maybe e3 (Annotation e3) mtype
    ])
  -- We can propage a TInt here, but this won't be true when we support, say,
  -- Double.
  Add e1 e2 -> gatherUnsets' (Just TInt) rs (List [e1, e2])
  Sub e1 e2 -> gatherUnsets' (Just TInt) rs (List [e1, e2])
  Mul e1 e2 -> gatherUnsets' (Just TInt) rs (List [e1, e2])
  Div e1 e2 -> gatherUnsets' (Just TInt) rs (List [e1, e2])
  Sum es -> gatherUnsets' (Just TInt) rs (List es)
  LessThan e1 e2 -> gatherUnsets' (Just TInt) rs (List [e1, e2])
  Equal e1 e2 -> gatherUnsets' Nothing rs (List [e1, e2])
  Union e1 e2 -> gatherUnsets' (Just TObject) rs (List [e1, e2])

-- | Giving the unevaluated expression is used in the special case it is a
-- Name, to provide a better error message.
check :: Exp -> AssertionInt -> Exp -> Maybe EvaluationError
check e a@(GreaterThan y) _x = case _x of
  Int x | x > y -> Nothing
        | otherwise -> case e of
    Name name -> Just (AssertionIntError (Just name) a)
    _ -> Just (AssertionIntError Nothing a)
  _ -> Just (TypeMismatch Nothing ("Expected an Int, got " ++ show _x))

-- | Giving the unevaluated expression is used in the special case it is a
-- Name, to provide a better error message.
bcheck :: Exp -> Exp -> Maybe EvaluationError
bcheck e _x = case _x of
  Bool True -> Nothing
  Bool False -> case e of
    Name name -> Just (AssertionError (Just name))
    _ -> Just (AssertionError Nothing)
  _ -> Just (TypeMismatch Nothing ("Expected a Bool, got " ++ show _x))

-- | `checkType` works similarly to `check`.
checkType :: Exp -> Type -> Exp -> Maybe EvaluationError
checkType e a _x = case (_x, a) of
  (Bool x, TBool) -> Nothing
  (Int x, TInt) -> Nothing
  (String x, TString) -> Nothing
  (String x, TEnum xs) | x `elem` xs -> Nothing
  (Object _, TObject) -> Nothing
  _ -> case e of
    Name name -> Just (TypeMismatch (Just name)
      ("Expected an " ++ t ++ ", got " ++ show _x))
    _ -> Just (TypeMismatch Nothing ("Expected an " ++ t ++ ", got " ++ show _x))
  where
  t = case a of
    TBool -> "Bool"
    TInt -> "Int"
    TString -> "String"
    TEnum xs -> concat $ intersperse "|" xs
    TObject -> "Object"


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
  { cSlug :: String -- ^ A short name, that can be used in URLs.
  , cName :: String -- ^ A display name, can contain spaces.
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
  | Int Int | AssertInt Exp AssertionInt
  | String String

  -- Type-checking is currently done during evaluation, instead as a real
  -- type-checking phase. I.e. this acts like a dynamically-typed language.
  | Annotation Exp Type
  | Assert Exp Exp -- ^ Returns the first exp, provided the second is True.

  | List [Exp]
  | Object [(String, Exp)] -- TODO Use a Map.

  | Name String
  | Names [String] -- ^ I think this is similar to Nix's `inherit`.

  | Cond Exp Exp Exp -- if _ then _ else _

  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Sum [Exp]

  | LessThan Exp Exp

  | Equal Exp Exp
  | Union Exp Exp
    -- ^ Creates the right-biased union of two objects (i.e. prefers the values
    -- from the right-hand object, when they exist in both).
  deriving (Eq, Show)

data AssertionInt = GreaterThan Int
  deriving (Eq, Show)

data Type = TBool | TInt | TString | TEnum [String] | TObject
  deriving (Eq, Show)

data EvaluationError =
    NoSuchRule String
  | MultipleRules String
  | Cycles
  | TypeMismatch (Maybe String) String
    -- ^ When the type mismatch is reported by a failed annotation involving
    -- directly a Name, it is given here.
  | AssertionIntError (Maybe String) AssertionInt
    -- ^ When the assertion involves directly a Name, it is given here.
  | AssertionError (Maybe String)
    -- ^ When the assertion involves directly a Name, it is given here.
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

isTypeMismatch (Error _ (TypeMismatch _ _)) = True
isTypeMismatch _ = False
