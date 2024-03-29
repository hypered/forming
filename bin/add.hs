-- This single-computation script is kept as an example. Other exemples are in
-- the multiple-computations `forming-examples.hs` script.
module Main where

import Forming


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMainOne $
  Computation
    "add"
    "Compute the addition of two integers a and b."
    "value"
    [ Binding "value" (Add (Name "a") (Name "b"))
    , Unset "a" Nothing
    , Unset "b" Nothing
    ]
