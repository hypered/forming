-- | This module defines functions to interact with the outside world, as used
-- by the Forming CLI.

{-# LANGUAGE RecordWildCards #-}

module Forming.IO where

import Data.Aeson (decode, encode, object, Value, (.=))
import qualified Data.Aeson as A (Value(Bool, Number, Object, String))
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as LB (putStr)
import qualified Data.HashMap.Strict as H (toList)
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import System.Exit (exitFailure)

import Forming.Core
import Forming.Syntax
import Forming.Type


--------------------------------------------------------------------------------
compute :: Computation -> Either String (Maybe String, [Input]) -> Either String Result
compute Computation{..} mis = case mis of
  Right (mname, is) -> Right $ evaluate [] (maybe cMain id mname) cRules is
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

printValue :: Int -> Syntax -> IO ()
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

parseInput :: String -> Syntax
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
