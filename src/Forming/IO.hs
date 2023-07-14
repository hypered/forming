-- | This module defines functions to interact with the outside world, as used
-- by the Forming CLI.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Forming.IO where

import Data.Aeson (decode, encode, object, toJSON, Value, (.=))
import qualified Data.Aeson as A (Value(Bool, Number, Object, String))
import qualified Data.Aeson.Key as A (fromString, toString)
import qualified Data.Aeson.KeyMap as A (toList)
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
compute :: Computation -> Maybe String -> Either String [Input] -> Either String Result
compute Computation{..} mname mis = case mis of
  Right is ->
    -- Special case for the CLI: if there is only one naked expression,
    -- evaluate it (instead of using named rules).
    case cRules of
      [Naked e] -> return $ reduce [] e cRules is
      _ -> return $ evaluate [] (fromMaybe cMain mname) cRules is
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
  LB.putStr $ encode (object ["error" .= err])
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
  Object xs -> object (map (\(k, v) -> (A.fromString k, jsonValue v)) xs)

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
-- | This assemble inputs.  The first argument is a list of input rules (i.e.
-- Unsets). Those are used to possibly give a type for ambiguous inputs (e.g.
-- 10 can be both an int and a decimal).
makeInputs :: [(Rule, Maybe Type)] -> [(String, String)] -> [Input] -> Either String [Input]
makeInputs unsets ((var, val) : rest) is =
  let mtype = lookupType var unsets
  in makeInputs unsets rest (is ++ [Input var $ parseInput mtype val])
makeInputs _ [] is = Right is

makeInputsFromJson :: String -> Either String [Input]
makeInputsFromJson s = case decode (pack s) :: Maybe Value of
  Just (A.Object kvs_) ->
    let kvs = A.toList kvs_
    in Right $ map (\(k, v) -> Input (A.toString k) (parseInput' v)) kvs
  Just _ -> Left "input JSON is not an object."
  Nothing -> Left "malformed input JSON."

-- TODO Either allow the user to give a type when giving an input, or allow
-- only inputs whose types are known after calling gatherUnsets.
parseInput :: Maybe Type -> String -> Syntax
parseInput (Just TBool) "True" = Bool True
parseInput (Just TBool) "False" = Bool False
parseInput (Just TInt) val = Int $ read val
parseInput (Just TDecimal) val = Decimal $ read val
parseInput (Just TString) val = String val
-- TODO I think I have some hack that understand a String when a Enum is
-- expected. So lat's return a String for now, but parseInput should be allowed
-- to fail (i.e. return an Either).
parseInput (Just (TEnum _)) val = String val
parseInput Nothing val = case val of
  _ | not (null val) && all (`elem` ("0123456789" :: String)) val ->
    Int $ read val
  _ | not (null val) && all (`elem` ("0123456789." :: String)) val ->
    Decimal $ read val
  "True" -> Bool True
  "False" -> Bool False
  _ -> String val
parseInput x _ = error (show x)

parseInput' (A.Bool x) = Bool x
parseInput' (A.Number x) = case floatingOrInteger x of
  Right  v -> Int v
  Left _ -> error "TODO Support floats"
parseInput' (A.String x) = String (T.unpack x)
parseInput' v = error ("TODO Support " ++ show v)
