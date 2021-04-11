module Main where

import Forming


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMainOne $
  Computation
    "trivial-1"
    "Compute a literal integer."
    "value"
    [ Rule "value" (Exp (Name "a"))
    , Rule "a" (Exp (Int 1))
    ]
