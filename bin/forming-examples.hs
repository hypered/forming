-- This multiple-computations script demonstrates multiple hard-coded
-- computations, run with `defaultMain`. See `add.hs` for a single-computation
-- script using `defaultMainOne` instead.
module Main where

import Forming
import Forming.Syntax
import Forming.Type


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
      , Rule "a" (Unset Nothing)
      ]
  , Computation
      "annotate-int"
      "Compute an int user input."
      "value"
      [ Rule "value" (Exp (Annotation (Name "a") TInt))
      , Rule "a" (Unset Nothing)
      ]
  , Computation
      "annotate-decimal"
      "Compute a decimal user input."
      "value"
      [ Rule "value" (Exp (Annotation (Name "a") TDecimal))
      , Rule "a" (Unset Nothing)
      ]
  , Computation
      "annotate-enum"
      "Compute a user input in hello or bye."
      "value"
      [ Rule "value" (Exp (Annotation (Name "a") (TEnum ["hello", "bye"])))
      , Rule "a" (Unset Nothing)
      ]
  , Computation
      "add"
      "Compute the addition of two integers a and b."
      "value"
      [ Rule "value" (Exp (Add (Name "a") (Name "b")))
      , Rule "a" (Unset Nothing)
      , Rule "b" (Unset Nothing)
      ]
  , Computation
      "greater-than-10"
      "A form to obtain an integer greater than 10."
      "value"
      [ Rule "value" (Exp (AssertInt (Name "a") (GreaterThan 10)))
      , Rule "a" (Unset Nothing)
      ]
  , Computation
      "add-greater-than-10"
      "Compute the addition of two integers a and b. The result must be greater\n\
      \than 10."
      "value"
      [ Rule "value" (Exp (AssertInt (Add (Name "a") (Name "b")) (GreaterThan 10)))
      , Rule "a" (Unset Nothing)
      , Rule "b" (Unset Nothing)
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
      , Rule "a" (Unset Nothing)
      , Rule "b" (Unset Nothing)
      , Rule "c" (Exp (Add (Name "a") (Name "b")))
      ]
  , Computation
      "has-a-cat"
      "A form to obtain a cat's name, if there is a cat."
      "form"
      [ Rule "has a cat" (Unset Nothing)
      , Rule "a cat's name" (Unset Nothing)
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
      [ Rule "status" (Unset Nothing)
      , Rule "regime" (Unset Nothing)
      , Rule "working hours" (Unset Nothing)
      , Rule "reference hours" (Unset Nothing)
      , Rule "marital status" (Unset Nothing)
      , Rule "disability" (Unset Nothing)
      , Rule "dependant children" (Unset Nothing)
      , Rule "disabled children" (Unset Nothing)
      , Rule "gross salary" (Unset Nothing)

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
          , ("gross salary", Annotation (Name "gross salary") TDecimal)
          ])
        (Cond
           -- TODO Add Or "incomplete-full-time"
           (Equal (Name "regime") (String "part-time"))
           (Names ["working hours", "reference hours"])
           (Names []))
        ))

      , Rule "result" (Exp (Name "personnal social contribution"))

      , Rule "personnal social contribution" (Exp
         (Mul
           (Name "personnal social contribution . normalized")
           (Decimal (read "0.1307")))
        )
      , Rule "personnal social contribution . normalized" (Exp
        (Cond
           (Equal (Name "status") (String "white-collar"))
           (Name "gross salary")
           (Mul (Name "gross salary") (Decimal (read "1.08"))))
        )
      ]
  ]
