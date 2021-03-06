-- This file contains the main code. To interact with it during development,
-- use bin/play.hs.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Forming
  ( module Forming
  , module Forming.Core
  ) where

import Control.Arrow (first, right)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.HashMap.Strict as H (toList)
import Data.Aeson (decode, Value)
import qualified Data.Aeson as A (Value(Bool, Number, Object, String))
import Data.List (nub)
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Hypered.Html
  ( cAddWrapper, cFont, cStaticPath, Font(IbmPlex)
  , defaultConfig
  , generate')

import Forming.Core
import Forming.IO
import Forming.Html (pageComputation)
import Forming.Server (runServer)


--------------------------------------------------------------------------------
defaultMain :: [Computation] -> IO ()
defaultMain cs = do
  args <- getArgs

  case args of

    ["--help"] -> do
      putStrLn "Available computations:"
      mapM_ (putStrLn . (\c -> "  " ++ cSlug c ++ "  " ++ cName c)) cs

    ["--serve"] -> runServer cs

    slug : rest -> case lookup slug (map (\c -> (cSlug c, c)) cs) of
      Just c -> run c rest
      Nothing -> do
        putStrLn "ERROR: the given computation slug doesn't exist."
        exitFailure

    [] -> do
      putStrLn "ERROR: a computation slug is expected."
      putStrLn "Run with --help to see the available computations."
      exitFailure

defaultMainOne :: Computation -> IO ()
defaultMainOne c = do
  args <- getArgs
  run c args


--------------------------------------------------------------------------------
run :: Computation -> [String] -> IO ()
run c@Computation{..} args = do

  case args of

    -- Show a help message.
    ["--help"] -> do
      putStrLn cName
      -- Evaluate without input to give a hint of possible user inputs.
      case evaluate [] cMain cRules [] of
        UnsetVariables names -> printUnsetVariables names
        Error _ (NoSuchRule _) -> putStrLn ("The rule \"" ++ cMain ++ "\" doesn't exist.")
        Result _ -> putStrLn "This computation doesn't require any user input."

    -- Generate an HTML page with technical comments.
    -- This uses Hypered's design-system Haskell code.
    ["--html"] -> do
      let conf' = defaultConfig
            { cAddWrapper = False, cFont = IbmPlex, cStaticPath = "../../static" }
      generate' "form.html" "Forming"
        conf' (const (pageComputation c))

    -- List all rules.
    ["--list"] -> mapM_ print cRules

    -- List unset names, and their types if possible.
    ["--unset"] -> print (gatherUnsets Nothing cMain cRules)

    -- List unset names involved in a specific rule, and their types if possible.
    ["--unset", name] -> print (gatherUnsets Nothing name cRules)

    -- Parse inputs given as JSON and evaluate one rule.
    ["--json", s] -> printOutputAsJson $ compute c $ makeInputsFromJson s
    [name, "--json", s] -> printOutputAsJson $ compute c $
      right (first (const (Just name))) $ makeInputsFromJson s

    -- Parse inputs of the form `--set a 1` (with makeInputs) and evaluate one
    -- rule. Before creating the inputs, gatherUnsets is called to compute
    -- types, which are necessary to correctly parse inputs.
    -- TODO Note that cMain and the name given by makeInputs can be different.
    -- So it is possible that `gatherUnsets` will infer a type (or Nothing)
    -- differently that what `compute` will use.
    -- Maybe I should have a check to make sure that all the rules together
    -- are consistent, and maybe I should force that every inputs are typed.
    -- This is not the case in e.g. the "trivial-a" example.
    rest -> case gatherUnsets Nothing cMain cRules of
      Right unsets -> printOutput $ compute c $ makeInputs unsets rest []
      Left err -> print err
