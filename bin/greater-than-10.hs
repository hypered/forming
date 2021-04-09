module Main where

import Core


--------------------------------------------------------------------------------
main :: IO ()
main = run $
  Computation
    "A form to obtain an integer greater than 10."
    "value"
    [ Rule "value" (Exp (AssertInt (GreaterThan 10) (Name "a")))
    , Rule "a" Unset
    ]
