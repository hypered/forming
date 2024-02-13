{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Forming.Html where

import qualified Data.Text as T
import Forming.Core (evaluate, gatherUnsets, Computation(..), Rule(..), Input(..), Result(..))
import Forming.Type (Type(..))
import Hypered.Html.Tachyons (nav)
import Protolude hiding (evaluate, Type)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


--------------------------------------------------------------------------------
-- Same as runWithInputs but produces HTML instead of strings to stdout.
runWithInputs' :: Computation -> Either Text (Maybe Text, [Input]) -> Html
runWithInputs' Computation{..} mis = case mis of
  Right (mname, is) ->
    case evaluate [] (fromMaybe cMain mname) cRules is of
      UnsetVariables names -> do
        H.code "ERROR: missing user inputs."
        printUnsetVariables' names
        -- TODO 400 Bad Request
      Result x -> do
        H.code "Result:"
        H.code . H.text . show $ x
      Error stack err -> do
        H.code "ERROR:"
        H.code . H.text . show $ (stack, err)
  Left err -> do
    H.code "ERROR:"
    H.code . H.text . show $ err
    -- TODO 400 Bad Request, possibly 500 if the form is invalid

printUnsetVariables' names = do
  H.code "This computation expects the following user inputs:\n"
  mapM_ (H.code . H.text . ("  " <>)) names


------------------------------------------------------------------------------
landingPage :: Html
landingPage = document "Forming" $ do
  H.header navigationForming
  H.p $ do
    "This is a demo website for Forming."
    " See the "
    H.a ! A.href "/examples" $ "example forms"
    "."


------------------------------------------------------------------------------
namespacePage :: [Computation] -> Html
namespacePage cs = document "Reesd" $ do
  H.header navigationForming
  H.span $ do
    H.code "Examples"
  H.ul $
    mapM_ htmlComputationItem cs

htmlComputationItem :: Computation -> Html
htmlComputationItem Computation{..} =
  H.li $ do
    H.a ! A.href (H.toValue $ "/examples/" <> cSlug <> "/+view") $ H.toHtml cSlug
    H.preEscapedToHtml (" &mdash; " :: Text)
    H.toHtml cName
    H.toHtml (" " :: Text)
    H.a ! A.href (H.toValue $ "/examples/" <> cSlug) $ "View live form."


------------------------------------------------------------------------------
formPage :: Computation -> Html
formPage = document' True "Reesd" . pageComputation

pageComputation :: Computation -> Html
pageComputation c@Computation{..} = do
  H.div ! A.class_ "center mw7" $ do
    H.p $ H.toHtml cName
    htmlComputation c
    H.div ! A.class_ "tc moon-gray pt4" $ "Powered by Reesd"

htmlComputation :: Computation -> Html
htmlComputation Computation{..} = do
  -- Note that `center` seems to require the containing element doesn't use the
  -- flex stuff.
  H.form ! A.class_ "bg-white mw7"
         ! A.method "POST"
         ! A.action (H.toValue ("/examples/" <> cSlug <> "/+submit"))
         $ do
    H.div ! A.class_ "pa4 bt br bl b--black bw1" $ do
      H.h2 $ H.toHtml cName
      case gatherUnsets Nothing cMain cRules of
        Left err -> panic (show err)
        Right unsets -> mapM_ htmlInput unsets
    H.div ! A.class_ "flex justify-between" $ do
      H.div ! A.class_ "bg-white b--black black ph3 pb4 pt3 tl w-100 dib no-underline ba bw1"
          $ ""
      H.button ! A.class_ "bg-black b--black white ph3 pb4 pt3 tl w-100 button-reset ba bw1" $ "Submit →"

htmlInput :: (Rule, Maybe Type) -> Html
htmlInput (Unset{..}, mtype) = do
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
-- TODO This is the same code as above.
htmlInput (Binding{..}, mtype) = do
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
    TEnum xs -> T.intercalate "|" xs
    TObject -> "Object"


--------------------------------------------------------------------------------
formDocPage :: Computation -> Html
formDocPage c = document "Reesd" $ pageComputationDoc c

pageComputationDoc :: Computation -> Html
pageComputationDoc c@Computation{..} = do
  H.header navigationForming
  H.span $ do
    H.a ! A.href (H.toValue @Text "/examples")
        ! A.class_ "black" $
      H.code "Examples"
    " / "
    H.code (H.toHtml cSlug)
  H.p $ do
    H.toHtml cName
    " "
    H.a ! A.href (H.toValue $ "/examples/" <> cSlug) $ "View live form."
  H.div $ do
    "Main rule: "
    H.code . H.toHtml $ cMain
  H.div $ do
    "Inputs: "
  H.code . H.pre $
    case gatherUnsets Nothing cMain cRules of
      Left err -> panic (show err)
      Right unsets -> mapM_ (H.toHtml . (++ "\n") . show) unsets
  H.div $ do
    "Rules:"
  H.code . H.pre $
    mapM_ (H.toHtml . (++ "\n") . show) cRules


----------------------------------------------------------------------
-- TODO Re-use the document function in Hypered.Html.
document :: Text -> Html -> Html
document = document' False

document' :: Bool -> Text -> Html -> Html
document' center title body = do
  H.docType
  H.html $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.title (H.toHtml title)
      H.meta ! A.name "viewport"
             ! A.content "width=device-width, initial-scale=1.0"
      H.style $ do
        mapM_ (\a -> H.toHtml ("@import url(" ++ a ++ ");"))
          [ "/static/css/ibm-plex.css"
          , "/static/css/tachyons.min.v4.11.1.css"
          , "/static/css/style.css"
          , "/static/css/styles.css"
          ]

    H.body ! A.class_ "hy-ibm-plex" $
      if center
      then
        -- It seems that combining the flex stuff with `center` in the content
        -- (`body` here) doesn't work. So we provide this option.
        H.div ! A.class_ "min-height-vh-100 mw8 center pa4 lh-copy" $
          body
      else
        H.div ! A.class_ "flex flex-column justify-between min-height-vh-100 mw8 center pa4 lh-copy" $
          body

navigationForming =
  nav $
    H.div $ do
      H.a ! A.class_ "link black hover-blue mr3" ! A.href "/" $ "Forming"
