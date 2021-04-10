module Main where

import Forming


--------------------------------------------------------------------------------
main :: IO ()
main = run $
  Computation
    "Compute the addition of two integers a and b. The result must be greater\n\
    \than 10."
    "value"
    [ Rule "value" (Exp (AssertInt (GreaterThan 10) (Add (Name "a") (Name "b"))))
    , Rule "a" Unset
    , Rule "b" Unset
    ]
