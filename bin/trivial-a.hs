module Main where

import Forming


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMainOne $
  Computation
    "trivial-a"
    "Compute a user input."
    "value"
    [ Rule "value" (Exp (Name "a"))
    , Rule "a" Unset
    ]
