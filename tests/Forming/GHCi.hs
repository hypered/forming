{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | This module is loaded only in the GHCi session.
-- It define example data and import modules (available qualified in GHCi too).
-- If GHCi and this module are setup correctly, this should look like this:
--
--   $ scripts/ghci.sh
--   ...
--   Loaded GHCi configuration from scripts/ghci.conf
--   ghci>
--
-- In particular, everything is loaded, then Main doesn't add new modules.
-- Main gives us the `:main` function.
-- We should be able to use `:main`, example data from this module, and
-- exported symbols from the (qualified) modules, e.g. `Syntax.Int 4`.
-- We can also run tests with e.g. `runTests`.
module Forming.GHCi (
  ) where

import Forming.Core qualified as Core
import Forming.Run qualified as Run
import Forming.Syntax qualified as Syntax

-- This import is present to set -interactive-print in ghci.conf.
import Text.Pretty.Simple qualified

--------------------------------------------------------------------------------
-- Example input data from Forming.Runner are also available, e.g. `rules`.
