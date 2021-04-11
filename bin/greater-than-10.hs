module Main where

import Forming


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMainOne $
  Computation
    "greater-than-10"
    "A form to obtain an integer greater than 10."
    "value"
    [ Rule "value" (Exp (AssertInt (GreaterThan 10) (Name "a")))
    , Rule "a" Unset
    ]
