module Forming.Type where

data Type = TBool | TInt | TDecimal | TString | TEnum [String] | TObject
  deriving (Eq, Show)
