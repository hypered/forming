module Main where

import Forming


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
  [ Computation
      "trivial-1"
      "Compute a literal integer."
      "value"
      [ Rule "value" (Exp (Name "a"))
      , Rule "a" (Exp (Int 1))
      ]
  , Computation
      "trivial-a"
      "Compute a user input."
      "value"
      [ Rule "value" (Exp (Name "a"))
      , Rule "a" Unset
      ]
  , Computation
      "annotate-a"
      "Compute a int user input."
      "value"
      [ Rule "value" (Exp (Annotation (Name "a") TInt))
      , Rule "a" Unset
      ]
  , Computation
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
  , Computation
      "add-greater-than-10"
      "Compute the addition of two integers a and b. The result must be greater\n\
      \than 10."
      "value"
      [ Rule "value" (Exp (AssertInt (GreaterThan 10) (Add (Name "a") (Name "b"))))
      , Rule "a" Unset
      , Rule "b" Unset
      ]
  , Computation
      "has-a-cat"
      "A form to obtain a cat's name, if there is a cat."
      "form"
      [ Rule "has a cat" Unset
      , Rule "a cat's name" Unset
      , Rule "form" (Exp (Cond
          (Name "has a cat")
          (Names ["has a cat", "a cat's name"])
          (Names ["has a cat"])))
      ]
  ]
