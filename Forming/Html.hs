{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Forming.Html where

import Data.List (intercalate)
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
import Forming.Type (Type(..))


------------------------------------------------------------------------------
pageComputation c@Computation{..} = do
  H.div ! A.class_ "center mw7" $ do
    H.p $ H.toHtml cName
    htmlComputation c
    H.div ! A.class_ "tc moon-gray pt4" $ "Powered by Reesd"

pageComputationDoc namespace c@Computation{..} = do
  H.header navigationReesd
  H.span $ do
    H.a ! A.href (H.toValue $ "/" ++ namespace)
        ! A.class_ "black" $
      H.code (H.toHtml namespace)
    " / "
    H.code (H.toHtml cSlug)
  H.p $ do
    H.toHtml cName
    " "
    H.a ! A.href (H.toValue $ "/noteed/" ++ cSlug) $ "View live form."
  H.div $ do
    "Main rule: "
    H.code . H.toHtml $ cMain
  H.div $ do
    "Inputs: "
  H.code . H.pre $
    case gatherUnsets Nothing cMain cRules of
      Left err -> error (show err)
      Right unsets -> mapM_ (H.toHtml . (++ "\n") . show) unsets
  H.div $ do
    "Rules:"
  H.code . H.pre $
    mapM_ (H.toHtml . (++ "\n") . show) cRules


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
      case gatherUnsets Nothing cMain cRules of
        Left err -> error (show err)
        Right unsets -> mapM_ htmlInput unsets
    H.div ! A.class_ "flex justify-between" $ do
      H.div ! A.class_ "bg-white b--black black ph3 pb4 pt3 tl w-100 dib no-underline ba bw1"
          $ ""
      H.button ! A.class_ "bg-black b--black white ph3 pb4 pt3 tl w-100 button-reset ba bw1" $ "Submit →"

htmlInput :: (Rule, Maybe Type) -> Html
htmlInput (Rule{..}, mtype) = do
  let iden = H.toValue rName -- TODO rules can have spaces or quotes
  H.div ! A.class_ "mv3" $
    H.div ! A.class_ "mb3" $ do
      H.label ! A.class_ "db fw6 mv1"
              ! A.for iden $
        H.span $ do
          H.toHtml rName
          case mtype of
            Nothing -> return ()
            Just t -> do
              " "
              htmlType t
      H.input ! A.class_ "input-reset bl-0 bt-0 br-0 bb bg-near-white pv2 ph2 w-100 outline-0 border-box"
              ! A.label iden
              ! A.name iden
              ! A.id iden
              ! A.type_ "text"
              ! A.placeholder ""

htmlType t = H.span ! A.class_ "silver fw1 ml1" $ H.code $ H.toHtml $
  case t of
    TBool -> "Bool"
    TInt -> "Int"
    TDecimal -> "Decimal"
    TString -> "String"
    TEnum xs -> intercalate "|" xs
    TObject -> "Object"
