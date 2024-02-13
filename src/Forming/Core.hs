-- This file contains the main code.

{-# LANGUAGE OverloadedStrings #-}

module Forming.Core where

import Control.Monad (join)
import Data.List (deleteFirstsBy, intercalate, lookup, nub, nubBy)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Protolude hiding (check, evaluate, reduce, Sum, Type)
import Forming.Syntax
import Forming.Type


--------------------------------------------------------------------------------
evaluate :: [Text] -> Text -> [Rule] -> [Input] -> Result
evaluate stack name rs is = case filter ((== name) . rName) rs' of
  [r] -> case r of

    Unset _ mtype -> case lookupInput (rName r) is of
      -- TODO Ruleout Syntax that are not "Value"
      Just (Name _) -> panic "Inputs cannot be a Name."
      Just e -> case mtype of
        Just t -> case checkType e t e of
          Nothing -> Result e
          Just err -> Error stack err
        Nothing -> Result e
      Nothing -> UnsetVariables [rName r]

    Binding _ e -> reduce (name : stack) e rs' is

  [] -> Error [name] (NoSuchRule name)

  _ -> Error [name] (MultipleRules name)

  where
  rs' = filter isNamedRule rs

reduce :: [Text] -> Syntax -> [Rule] -> [Input] -> Result
reduce stack e rs is = case e of
  Bool x -> Result (Bool x)
  Int x -> Result (Int x)
  AssertInt e assertion -> case reduce stack e rs is of
    Result x -> case check e assertion x of
      Nothing -> Result x
      Just err -> Error stack err
    Error stack' err -> Error stack' err
    UnsetVariables xs -> UnsetVariables xs
  Decimal x -> Result (Decimal x)
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
      (Result _) -> panic "Can't happen; the Result is necessarily a List."
      Error stack' err -> Error stack' err
      UnsetVariables xs -> UnsetVariables xs
    Error stack' err -> Error stack' err
    UnsetVariables xs -> UnsetVariables xs
  Object kvs -> case reduce stack (List (map snd kvs)) rs is of
    (Result (List xs)) -> Result (Object (zip (map fst kvs) xs))
    (Result _) -> panic "Can't happen; the Result is necessarily a List."
    Error stack' err -> Error stack' err
    UnsetVariables xs -> UnsetVariables xs
  Name name -> evaluate stack name rs is
  Names names -> case reduce stack (List (map Name names)) rs is of
    (Result (List xs)) -> Result (Object (zip names xs))
    (Result _) -> panic "Can't happen; the Result is necessarily a List."
    Error stack' err -> Error stack' err
    UnsetVariables xs -> UnsetVariables xs
  Cond e1 e2 e3 -> case reduce stack e1 rs is of
    (Result (Bool True)) -> reduce stack e2 rs is
    (Result (Bool False)) -> reduce stack e3 rs is
    (Result t) -> Error stack (TypeMismatch Nothing $ "Expected a Bool, got " <> show t)
    Error stack' err -> Error stack' err
    UnsetVariables xs -> UnsetVariables xs
  Add e1 e2 -> add stack rs is e1 e2
  Sub e1 e2 -> sub stack rs is e1 e2
  Mul e1 e2 -> mul stack rs is e1 e2
  Div e1 e2 -> div' stack rs is e1 e2
  Sum [] -> Result (Int 0)
  Sum (e : es) -> reduce stack (Add e (Sum es)) rs is

  LessThan e1 e2 -> ibbinop (<) stack rs is e1 e2

  Equal e1 e2 -> equal stack rs is e1 e2
  Union e1 e2 -> union unionRight stack rs is e1 e2

bbinop = binop TBool . op
  where op f a b = case (a, b) of
          (Bool a_, Bool b_) -> Bool (f a_ b_)
          (_, _) -> panic "bbinop called with wrong types"
          -- It may mean that checkType has a bug.

ibinop = binop TInt . op
  where op f a b = case (a, b) of
          (Int a_, Int b_) -> Int (f a_ b_)
          (_, _) -> panic "ibinop called with wrong types"
          -- It may mean that checkType has a bug.

ibbinop = binop TInt . op
  where op f a b = case (a, b) of
          (Int a_, Int b_) -> Bool (f a_ b_)
          (_, _) -> panic "ibinop called with wrong types"
          -- It may mean that checkType has a bug.

union = binop TObject . op
  where op f a b = case (a, b) of
          (Object as, Object bs) -> Object (f as bs)
          (_, _) -> panic "union called with wrong types"
          -- It may mean that checkType has a bug.

equal = binop' (checkingEqClass (\a b -> Bool (a == b)))

add = binop' (checkingNumClass f)
  where f (Int a) (Int b) = Int (a + b)
        f (Decimal a) (Decimal b) = Decimal (a + b)
sub = binop' (checkingNumClass f)
  where f (Int a) (Int b) = Int (a - b)
        f (Decimal a) (Decimal b) = Decimal (a - b)
mul = binop' (checkingNumClass f)
  where f (Int a) (Int b) = Int (a * b)
        f (Decimal a) (Decimal b) = Decimal (a * b)
div' = binop' (checkingNumClass f)
  where f (Int a) (Int b) = Int (div a b)
        f (Decimal a) (Decimal b) = Decimal (a / b)

unionRight as bs = deleteFirstsBy (\a b -> fst a == fst b) as bs ++ bs

binop t f = binop' (checkingType t f)

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
    _ -> Error stack (TypeMismatch Nothing "Can't be compared.")

checkingNumClass f stack e1 e2 a b =
  case (a, b) of
    (Int _, Int _) -> Result (f a b)
    (Decimal _, Decimal _) -> Result (f a b)
    _ -> Error stack (TypeMismatch Nothing "Can't do arithmetic.")

lookupInput name is = lookup name is'
  where is' = map (\(Input name val) -> (name, val)) is

-- TODO There is no sharing, so multiple occurence of same name is computed
-- multiple times.
-- In addition of reporting unset variables, this reports its type when it is
-- trivial to do (e.g. there is a direct annotation of that variable).
gatherUnsets :: Maybe Type -> Text -> [Rule]
  -> Either EvaluationError [(Rule, Maybe Type)]
gatherUnsets mtype name rs = case filter ((== name) . rName) rs' of
  [r] -> case r of
    Unset _ Nothing -> Right [(r, mtype)]
    Unset _ mtype' -> Right [(r, mtype')]
    Binding _ e -> gatherUnsets' mtype rs' e
  [] -> Left (NoSuchRule name)
  _ -> Left (MultipleRules name)

  where
  rs' = filter isNamedRule rs

gatherUnsets' :: Maybe Type -> [Rule] -> Syntax -> Either EvaluationError [(Rule, Maybe Type)]
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
        let typedFirsts = filter (isJust . snd) (x ++ xs) ++ x ++ xs
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
check :: Syntax -> AssertionInt -> Syntax -> Maybe EvaluationError
check e a@(GreaterThan y) _x = case _x of
  Int x | x > y -> Nothing
        | otherwise -> case e of
    Name name -> Just (AssertionIntError (Just name) a)
    _ -> Just (AssertionIntError Nothing a)
  _ -> Just (TypeMismatch Nothing ("Expected an Int, got " <> show _x))

-- | Giving the unevaluated expression is used in the special case it is a
-- Name, to provide a better error message.
bcheck :: Syntax -> Syntax -> Maybe EvaluationError
bcheck e _x = case _x of
  Bool True -> Nothing
  Bool False -> case e of
    Name name -> Just (AssertionError (Just name))
    _ -> Just (AssertionError Nothing)
  _ -> Just (TypeMismatch Nothing ("Expected a Bool, got " <> show _x))

-- | `checkType` works similarly to `check`.
checkType :: Syntax -> Type -> Syntax -> Maybe EvaluationError
checkType e a _x = case (_x, a) of
  (Bool x, TBool) -> Nothing
  (Int x, TInt) -> Nothing
  (Decimal x, TDecimal) -> Nothing
  (String x, TString) -> Nothing
  (String x, TEnum xs) | x `elem` xs -> Nothing
  (Object _, TObject) -> Nothing
  _ -> case e of
    Name name -> Just (TypeMismatch (Just name)
      ("Expected " <> t <> ", got " <> show _x))
    _ -> Just (TypeMismatch Nothing ("Expected " <> t <> ", got " <> show _x))
  where
  t :: Text
  t = case a of
    TBool -> "a Bool"
    TInt -> "an Int"
    TDecimal -> "a Decimal"
    TString -> "a String"
    TEnum xs -> T.intercalate "|" xs
    TObject -> "an Object"


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
  { cSlug :: Text -- ^ A short name, that can be used in URLs.
  , cName :: Text -- ^ A display name, can contain spaces.
  , cMain :: Text -- Default rule to evaluate, must appear in the cRules.
  , cRules :: [Rule]
  }

-- A rule is a binding of a name to a formula, which can be reduced (evaluated)
-- to a value. Names can be multiple words, e.g. "meal unit price".
-- It can be an unnamed (naked) expression, to allow Forming to be used as a
-- simple calculator.
data Rule =
    Unset { rName :: Text, rType :: Maybe Type }
  | Binding { rName :: Text , rExpression :: Syntax }
  | Naked { rExpression :: Syntax }
  deriving (Eq, Show)

data EvaluationError =
    NoSuchRule Text
  | MultipleRules Text
  | Cycles
  | TypeMismatch (Maybe Text) Text
    -- ^ When the type mismatch is reported by a failed annotation involving
    -- directly a Name, it is given here.
  | AssertionIntError (Maybe Text) AssertionInt
    -- ^ When the assertion involves directly a Name, it is given here.
  | AssertionError (Maybe Text)
    -- ^ When the assertion involves directly a Name, it is given here.
  deriving (Eq, Show)

data Result = Result Syntax | UnsetVariables [Text] | Error [Text] EvaluationError
  deriving (Eq, Show)

-- | An input is like the simplest rule: it binds a value to a name.
-- That value is used to replace an unset variable with the same name.
-- TODO Raise an error if an input is provided for a variable that cannot be set.
-- TODO Raise an error if an input is provided for non-existing variable.
data Input = Input Text Syntax
  deriving Show

isNamedRule (Unset _ _) = True
isNamedRule (Binding _ _) = True
isNamedRule _ = False

isTypeMismatch :: Result -> Bool
isTypeMismatch (Error _ (TypeMismatch _ _)) = True
isTypeMismatch _ = False


--------------------------------------------------------------------------------
lookupType name rules = join $ lookup name rules'
  where rules' = map (\(rule, mtype) -> (rName rule, mtype)) rules
