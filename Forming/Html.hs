{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Forming.Html where

import Data.Text (Text)
import qualified Data.Text.Lazy.IO as T
import System.FilePath (joinPath, splitPath, takeDirectory, FilePath, (</>))
import System.Directory (createDirectoryIfMissing)
import System.IO (hPutStr, withFile, IOMode(WriteMode))
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)

import Hypered.Html (navigationReesd, loginForm)

import Forming.Core (Computation(..))


------------------------------------------------------------------------------
exampleLoginForm Computation{..} = do
  H.header $
    navigationReesd
  H.p $ H.toHtml cName
  loginForm
