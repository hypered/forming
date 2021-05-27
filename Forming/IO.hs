-- | This module defines functions to interact with the outside world, as used
-- by the Forming CLI.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Forming.IO where

import Data.Aeson (decode, encode, object, toJSON, Value, (.=))
import qualified Data.Aeson as A (Value(Bool, Number, Object, String))
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as LB (putStr)
import qualified Data.HashMap.Strict as H (toList)
import Data.Maybe (fromMaybe)
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import System.Exit (exitFailure)

import Forming.Core
import Forming.Syntax
import Forming.Type


--------------------------------------------------------------------------------
compute :: Computation -> Either String (Maybe String, [Input]) -> Either String Result
compute Computation{..} mis = case mis of
  Right (mname, is) -> Right $ evaluate [] (fromMaybe cMain mname) cRules is
  Left err -> Left err


--------------------------------------------------------------------------------
printOutput :: Either String Result -> IO ()
printOutput (Left err) = do
  putStrLn ("ERROR: " ++ err)
  exitFailure
printOutput (Right result) = case result of
  Result x -> printValue 0 x
  UnsetVariables names -> do
    putStrLn "ERROR: missing user inputs."
    printUnsetVariables names
    exitFailure
  Error stack err -> do
    putStr "ERROR: "
    printError stack err
    exitFailure

printUnsetVariables :: [String] -> IO ()
printUnsetVariables names = do
  putStrLn "This computation expects the following user inputs:\n"
  mapM_ (putStrLn . ("  " ++)) names
  putStrLn "\nUse `--set a 1` to provide the value 1 to the input \"a\"."

printError :: [String] -> EvaluationError -> IO ()
printError stack err = case err of
  NoSuchRule _ -> putStrLn $ stringError err
  MultipleRules _ -> putStrLn $ stringError err
  Cycles -> putStrLn $ stringError err
  TypeMismatch _ _ -> do
    putStrLn $ stringError err
    putStrLn $ "while evaluating rules " ++ show stack
  AssertionIntError _ _ -> do
    putStrLn $ stringError err
    putStrLn $ "while evaluating rules " ++ show stack
  AssertionError _ -> do
    putStrLn $ stringError err
    putStrLn $ "while evaluating rules " ++ show stack

printValue :: Int -> Syntax -> IO ()
printValue indent v = case v of
  Bool x -> putStrLn (padding ++ show x)
  Int x -> putStrLn (padding ++ show x)
  Decimal x -> putStrLn (padding ++ show x)
  String x -> putStrLn (padding ++ show x)
  Object xs -> do
    putStrLn (padding ++ "{")
    mapM_ (\(k, v) -> do
      putStrLn (padding ++ "  " ++ k ++ ": ") >> printValue (indent + 2) v)
      xs
    putStrLn (padding ++ "}")

  where padding = replicate (indent * 2) ' '


--------------------------------------------------------------------------------
-- | Similar to printOutput but format its output as JSON.
-- TODO Use an error code or at least a symbolic error name, with additional
-- details and possibly additional structures (e.g. input names).
printOutputAsJson :: Either String Result -> IO ()
printOutputAsJson (Left err) = do
  LB.putStr $ encode (object [T.pack "error" .= err])
  exitFailure
printOutputAsJson (Right result) = case result of
  Result x ->
    LB.putStr $ encode (object ["ouput" .= jsonValue x])
  UnsetVariables names -> do
    LB.putStr $ encode (object ["error" .= ("missing user inputs" :: String)])
    exitFailure
  Error stack err -> do
    LB.putStr $ encode (object ["error" .= stringError err, "stack" .= stack])
    exitFailure

jsonValue :: Syntax -> Value
jsonValue v = case v of
  Int x -> toJSON x
  Bool x -> toJSON x
  String x -> toJSON x
  Object xs -> object (map (\(k, v) -> (T.pack k, jsonValue v)) xs)

stringError :: EvaluationError -> String
stringError err = case err of
  NoSuchRule name ->
    "no such rule \"" ++ name ++"\"."
  MultipleRules name ->
    "multiple rules have the same name \"" ++ name ++ "\"."
  Cycles ->
    "the rules form a cycle."
  TypeMismatch mname err ->
    "type mismatch: " ++
      err ++
      maybe "" (\name -> " for the variable \"" ++ name ++ "\"") mname
  AssertionIntError mname err -> do
    "an assertion has failed: " ++
      maybe "" (\name -> "\"" ++ name ++ "\" must be ") mname
      ++ show err
  AssertionError mname -> do
    "an assertion has failed: " ++
      maybe "" (\name -> "\"" ++ name ++ "\" ") mname
      ++ "must be True"

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
makeInputs rest _ =
  Left $ "unexpected arguments: " ++ unwords rest

makeInputsFromJson :: String -> Either String (Maybe String, [Input])
makeInputsFromJson s = case decode (pack s) :: Maybe Value of
  Just (A.Object kvs_) ->
    let kvs = H.toList kvs_
    in Right (Nothing, map (\(k, v) -> Input (T.unpack k) (parseInput' v)) kvs)
  Just _ -> Left "input JSON is not an object."
  Nothing -> Left "malformed input JSON."

-- TODO Either allow the user to give a type when giving an input, or allow
-- only inputs whose types are known after calling gatherUnsets.
parseInput :: String -> Syntax
parseInput val = case val of
  _ | not (null val) && all (`elem` ("0123456789" :: String)) val ->
    Int $ read val
  _ | not (null val) && all (`elem` ("0123456789." :: String)) val ->
    Decimal $ read val
  "True" -> Bool True
  "False" -> Bool False
  _ -> String val

parseInput' (A.Bool x) = Bool x
parseInput' (A.Number x) = case floatingOrInteger x of
  Right  v -> Int v
  Left _ -> error "TODO Support floats"
parseInput' (A.String x) = String (T.unpack x)
parseInput' v = error ("TODO Support " ++ show v)
