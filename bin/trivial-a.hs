module Main where

import Forming


--------------------------------------------------------------------------------
main :: IO ()
main = run $
  Computation
    "Compute a user input."
    "value"
    [ Rule "value" (Exp (Name "a"))
    , Rule "a" Unset
    ]
