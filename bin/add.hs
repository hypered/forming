-- This single-computation script is kept as an example. Other exemples are in
-- the mulitple-computations script forming-examples.hs.
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
    , Rule "a" Unset
    , Rule "b" Unset
    ]
