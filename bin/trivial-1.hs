module Main where

import Forming


--------------------------------------------------------------------------------
main :: IO ()
main = run $
  Computation
    "Compute a literal integer."
    "value"
    [ Rule "value" (Exp (Name "a"))
    , Rule "a" (Exp (Int 1))
    ]
