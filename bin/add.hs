-- This single-computation script is kept as an example. Other exemples are in
-- the multiple-computations `forming-examples.hs` script.
module Main where

import Forming
import Forming.Syntax


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMainOne $
  Computation
    "add"
    "Compute the addition of two integers a and b."
    "value"
    [ Rule "value" (Exp (Add (Name "a") (Name "b")))
    , Rule "a" (Unset Nothing)
    , Rule "b" (Unset Nothing)
    ]
