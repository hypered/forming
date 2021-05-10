module Forming.Syntax where

import Forming.Type

data Syntax =
    Bool Bool
    -- Is it really useful to have assertions on whole Syntax, instead of Unset
    -- values ?
  | Int Int | AssertInt Syntax AssertionInt
  | String String

  -- Type-checking is currently done during evaluation, instead as a real
  -- type-checking phase. I.e. this acts like a dynamically-typed language.
  | Annotation Syntax Type
  | Assert Syntax Syntax -- ^ Returns the first exp, provided the second is True.

  | List [Syntax]
  | Object [(String, Syntax)] -- TODO Use a Map.

  | Name String
  | Names [String] -- ^ I think this is similar to Nix's `inherit`.

  | Cond Syntax Syntax Syntax -- if _ then _ else _

  | Add Syntax Syntax
  | Sub Syntax Syntax
  | Mul Syntax Syntax
  | Div Syntax Syntax
  | Sum [Syntax]

  | LessThan Syntax Syntax

  | Equal Syntax Syntax
  | Union Syntax Syntax
    -- ^ Creates the right-biased union of two objects (i.e. prefers the values
    -- from the right-hand object, when they exist in both).
  deriving (Eq, Show)

data AssertionInt = GreaterThan Int
  deriving (Eq, Show)
