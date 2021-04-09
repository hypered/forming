module Main where

import Core


--------------------------------------------------------------------------------
main :: IO ()
main = run $
  Computation
    "Compute the addition of two integers a and b."
    "value"
    [ Rule "value" (Exp (Add (Name "a") (Name "b")))
    , Rule "a" Unset
    , Rule "b" Unset
    ]
