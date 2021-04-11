module Main where

import Forming


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
  [ Computation
      "add"
      "Compute the addition of two integers a and b."
      "value"
      [ Rule "value" (Exp (Add (Name "a") (Name "b")))
      , Rule "a" Unset
      , Rule "b" Unset
      ]
  , Computation
      "greater-than-10"
      "A form to obtain an integer greater than 10."
      "value"
      [ Rule "value" (Exp (AssertInt (GreaterThan 10) (Name "a")))
      , Rule "a" Unset
      ]
  ]
