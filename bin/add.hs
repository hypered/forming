module Main where

import Forming


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
