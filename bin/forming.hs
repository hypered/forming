-- This is the main Forming program, designed to parse, run and serve external
-- expressions written in the Forming concrete syntax.
{-# Language OverloadedStrings #-}
module Main where

import qualified Options.Applicative as A

import qualified Forming.Command as Command
import qualified Forming.Run as Run


--------------------------------------------------------------------------------
main :: IO ()
main = A.execParser Command.parserInfo >>= Run.run
