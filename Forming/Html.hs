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

import Hypered.Html (navigationReesd)

import Forming.Core (gatherUnsets, Computation(..), Rule(..))


------------------------------------------------------------------------------
pageComputation c@Computation{..} = do
  H.header $
    navigationReesd
  H.p $ H.toHtml cName
  htmlComputation c


------------------------------------------------------------------------------
htmlComputation Computation{..} = do
  H.form ! A.class_ "bg-white mw7"
         ! A.method "POST"
         ! A.action "/submit/xxxxxxxx"
         $ do
    H.div ! A.class_ "pa4 bt br bl b--black bw1" $ do
      H.h2 $ H.toHtml cName
      case gatherUnsets cMain cRules of
        Left err -> error (show err)
        Right rules -> mapM_ htmlRule rules
      H.a ! A.class_ "black no-underline hy-hover-blue"
          ! A.href "/reset"
          $ "Reset password"
    H.div ! A.class_ "flex justify-between" $ do
      H.a ! A.class_ "bg-white b--black black ph3 pb4 pt3 tl w-100 dib no-underline ba bw1"
          ! A.href "/register"
          $ "Register"
      H.button ! A.class_ "bg-black b--black white ph3 pb4 pt3 tl w-100 button-reset ba bw1" $ "Submit —>"

htmlRule Rule{..} = do
  H.div ! A.class_ "mv3" $
    H.div ! A.class_ "mb3" $ do
      H.label ! A.class_ "db fw6 mv1" $ H.toHtml rName
              ! A.for "username"
      H.input ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv2 ph2 w-100 outline-0 border-box"
              ! A.label "username"
              ! A.name "username"
              ! A.id "username"
              ! A.type_ "text"
              ! A.placeholder ""
