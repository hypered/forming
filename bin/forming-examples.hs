-- This multiple-computations script demonstrates multiple hard-coded
-- computations, run with `defaultMain`. See `add.hs` for a single-computation
-- script using `defaultMainOne` instead.
module Main where

import Forming
import Forming.Lexer (readDecimal)
import Forming.Type
import Protolude


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
  [ Computation
      "trivial-1"
      "Compute a literal integer."
      "value"
      [ Binding "value" (Name "a")
      , Binding "a" (Int 1)
      ]
  , Computation
      "trivial-a"
      "Compute a user input."
      "value"
      [ Binding "value" (Name "a")
      , Unset "a" Nothing
      ]
  , Computation
      "annotate-int"
      "Compute an int user input."
      "value"
      [ Binding "value" (Annotation (Name "a") TInt)
      , Unset "a" Nothing
      ]
  , Computation
      "annotate-decimal"
      "Compute a decimal user input."
      "value"
      [ Binding "value" (Annotation (Name "a") TDecimal)
      , Unset "a" Nothing
      ]
  , Computation
      "annotate-enum"
      "Compute a user input in hello or bye."
      "value"
      [ Binding "value" (Annotation (Name "a") (TEnum ["hello", "bye"]))
      , Unset "a" Nothing
      ]
  , Computation
      "add"
      "Compute the addition of two integers a and b."
      "value"
      [ Binding "value" (Add (Name "a") (Name "b"))
      , Unset "a" Nothing
      , Unset "b" Nothing
      ]
  , Computation
      "greater-than-10"
      "A form to obtain an integer greater than 10."
      "value"
      [ Binding "value" (AssertInt (Name "a") (GreaterThan 10))
      , Unset "a" Nothing
      ]
  , Computation
      "add-greater-than-10"
      "Compute the addition of two integers a and b. The result must be greater\n\
      \than 10."
      "value"
      [ Binding "value" (AssertInt (Add (Name "a") (Name "b")) (GreaterThan 10))
      , Unset "a" Nothing
      , Unset "b" Nothing
      ]
  -- Similar to add-greater-than-10, but using a more generic Assert mechanism,
  -- instead of the more specific AssertInt (which can be removed, although it
  -- provides better error messages for now).
  , Computation
      "assert-greater-than-10"
      "Compute the addition of two integers a and b. The result must be greater\n\
      \than 10."
      "value"
      [ Binding "value" (Assert (Name "c") (LessThan (Int 10) (Name "c")))
      , Unset "a" Nothing
      , Unset "b" Nothing
      , Binding "c" (Add (Name "a") (Name "b"))
      ]
  , Computation
      "has-a-cat"
      "A form to obtain a cat's name, if there is a cat."
      "form"
      [ Unset "has a cat" Nothing
      , Unset "a cat's name" Nothing
      , Binding "form"
          (Union
            (Names ["has a cat"])
            (Cond (Name "has a cat") (Names ["a cat's name"]) (Names []))
          )
      ]
  , Computation
      "compensation"
      "A form to compute a net salary given a gross salary."
      "form"
      [ Unset "status" Nothing
      , Unset "regime" Nothing
      , Unset "working hours" Nothing
      , Unset "reference hours" Nothing
      , Unset "marital status" Nothing
      , Unset "disability" Nothing
      , Unset "dependant children" Nothing
      , Unset "disabled children" Nothing
      , Unset "gross salary" Nothing

      , Binding "form" (Union
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
        )

      , Binding "result" (Name "personnal social contribution")

      , Binding "personnal social contribution"
         (Mul
           (Name "personnal social contribution . normalized")
           (Decimal (readDecimal "0.1307")))
      , Binding "personnal social contribution . normalized"
         (Cond
           (Equal (Name "status") (String "white-collar"))
           (Name "gross salary")
           (Mul (Name "gross salary") (Decimal (readDecimal "1.08"))))
      ]
  , Computation
      "signup"
      "A sign up form."
      "form"
      [ Unset "username" Nothing
      , Unset "email address" Nothing
      , Unset "password" Nothing

      , Binding "form"
          (Object
            [ ("username", Annotation (Name "username") TString)
            , ("email address", Annotation (Name "email address") TString)
            , ("password", Annotation (Name "password") TString) --TODO TPassword
            ])
      ]
  , Computation
      "login"
      "A login form."
      "form"
      [ Unset "username" Nothing
      , Unset "password" Nothing

      , Binding "form"
          (Object
            [ ("username", Annotation (Name "username") TString)
            , ("password", Annotation (Name "password") TString) --TODO TPassword
            ])
      ]
  ]
