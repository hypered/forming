module Forming.Type where

import Protolude

data Type = TBool | TInt | TDecimal | TString | TEnum [Text] | TObject
  deriving (Eq, Show)
