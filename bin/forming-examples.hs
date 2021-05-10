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
      , Rule "form" (Exp
        (Union
          (Names ["has a cat"])
          (Cond (Name "has a cat") (Names ["a cat's name"]) (Names []))
        ))
      ]
  , Computation
      "compensation"
      "A form to compute a net salary given a gross salary."
      "form"
      [ Rule "status" Unset
      , Rule "regime" Unset
      , Rule "working hours" Unset
      , Rule "reference hours" Unset
      , Rule "marital status" Unset
      , Rule "disability" Unset
      , Rule "dependant children" Unset
      , Rule "disabled children" Unset
      , Rule "gross salary" Unset
      , Rule "form" (Exp (Union
        (Object
          [ ("status", Annotation (Name "status")
              (TEnum ["blue-collar", "white-collar"]))
          , ("regime", Annotation (Name "regime")
              (TEnum ["full-time", "incomplete-full-time", "part-time"]))
          , ("marital status", Annotation (Name "marital status")
              (TEnum ["single", "married-1-income", "married-2-income"]))
          , ("disability", Annotation (Name "disability") TBool)
          , ("dependant children", Annotation (Name "dependant children") TInt)
          , ("disabled children", Annotation (Name "disabled children") TInt)
          , ("gross salary", Annotation (Name "gross salary") TInt)
          ])
        (Cond
           -- TODO Add Or "incomplete-full-time"
           (Equal (Name "regime") (String "part-time"))
           (Names ["working hours", "reference hours"])
           (Names []))
        ))
      ]
  ]
