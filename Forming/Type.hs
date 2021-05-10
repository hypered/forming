module Forming.Type where

data Type = TBool | TInt | TString | TEnum [String] | TObject
  deriving (Eq, Show)
