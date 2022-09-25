-- This is the main Forming program, designed to parse, run and serve external
-- expressions written in the Forming concrete syntax.
{-# Language OverloadedStrings #-}
module Main where

import qualified Forming.Run as Run


--------------------------------------------------------------------------------
main :: IO ()
main = Run.run
