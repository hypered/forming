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
  H.div ! A.class_ "center mw7" $ do
    H.p $ H.toHtml cName
    htmlComputation c
    H.div ! A.class_ "tc moon-gray pt4" $ "Powered by Reesd"

pageComputationDoc namespace c@Computation{..} = do
  H.header $
    navigationReesd
  H.span $ do
    H.code (H.toHtml namespace)
    " / "
    H.code (H.toHtml cSlug)
  H.p $ do
    H.toHtml cName
    " "
    H.a ! A.href (H.toValue $ "/noteed/" ++ cSlug) $ "View live form."
  H.code . H.toHtml $ "Main rule: " ++ cMain
  mapM_ (H.code . H.toHtml . show) cRules


------------------------------------------------------------------------------
htmlComputation Computation{..} = do
  -- Note that `center` seems to require the containing element doesn't use the
  -- flex stuff.
  H.form ! A.class_ "bg-white mw7"
         ! A.method "POST"
         ! A.action (H.toValue ("/noteed/" ++ cSlug ++ "/+submit"))
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
  let iden = H.toValue rName -- TODO rules can have spaces or quotes
  H.div ! A.class_ "mv3" $
    H.div ! A.class_ "mb3" $ do
      H.label ! A.class_ "db fw6 mv1" $ H.toHtml rName
              ! A.for iden
      H.input ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv2 ph2 w-100 outline-0 border-box"
              ! A.label iden
              ! A.name iden
              ! A.id iden
              ! A.type_ "text"
              ! A.placeholder ""
