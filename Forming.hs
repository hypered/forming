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
import Forming.Html (pageComputation)


--------------------------------------------------------------------------------
run c@Computation{..} = do
  args <- getArgs

  case args of

    -- Show a help message.
    ["--help"] -> do
      putStrLn cName
      -- Evaluate without input to give a hint a possible user inputs.
      case evaluate [] cMain cRules [] of
        UnsetVariables names -> printUnsetVariables names
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

    -- List unset names.
    ["--unset"] -> mapM_ print (filter isUnset cRules)

    -- List unset names involved in a specific rule.
    ["--unset", name] -> print (gatherUnsets name cRules)

    -- Parse inputs given as JSON and evaluate one rule.
    ["--json", s] -> runWithInputs c $ makeInputsFromJson s
    ["--json", s, name] -> runWithInputs c $
      right (first (const (Just name))) $ makeInputsFromJson s

    -- Parse inputs of the form `--set a 1` and evaluate one rule.
    rest -> runWithInputs c $ makeInputs rest []
