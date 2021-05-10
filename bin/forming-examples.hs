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
      "annotate-enum"
      "Compute a user input in hello or bye."
      "value"
      [ Rule "value" (Exp (Annotation (Name "a") (TEnum ["hello", "bye"])))
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
      [ Rule "value" (Exp (AssertInt (Name "a") (GreaterThan 10)))
      , Rule "a" Unset
      ]
  , Computation
      "add-greater-than-10"
      "Compute the addition of two integers a and b. The result must be greater\n\
      \than 10."
      "value"
      [ Rule "value" (Exp (AssertInt (Add (Name "a") (Name "b")) (GreaterThan 10)))
      , Rule "a" Unset
      , Rule "b" Unset
      ]
  -- Similar to add-greater-than-10, but using a more generic Assert mechanism,
  -- instead of the more specific AssertInt (which can be removed, although it
  -- provides better error messages for now).
  , Computation
      "assert-greater-than-10"
      "Compute the addition of two integers a and b. The result must be greater\n\
      \than 10."
      "value"
      [ Rule "value" (Exp (Assert (Name "c") (LessThan (Int 10) (Name "c"))))
      , Rule "a" Unset
      , Rule "b" Unset
      , Rule "c" (Exp (Add (Name "a") (Name "b")))
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
