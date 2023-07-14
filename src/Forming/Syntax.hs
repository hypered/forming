module Forming.Syntax where

import Data.Decimal (Decimal)

import Forming.Type


--------------------------------------------------------------------------------
data Syntax =
    Bool Bool
  | Int Int
  | Decimal Decimal
  | String String

  -- Type-checking is currently done during evaluation, instead as a real
  -- type-checking phase. I.e. this acts like a dynamically-typed language.
  | Annotation Syntax Type
    -- Is it really useful to have assertions on whole Syntax, instead of Unset
    -- values, or just Name ?
  | Assert Syntax Syntax -- ^ Returns the first exp, provided the second is True.
  | AssertInt Syntax AssertionInt

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
