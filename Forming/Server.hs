-- |
-- A small HTTP server exposing an endpoint to submit a form.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Forming.Server where

import Control.Concurrent (threadDelay)
import Control.Lens (makeLenses)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, Value, (.=))
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Core
  ( getHeader, getParams, getPostParam, getRequest, ifTop, logError, method
  , modifyResponse, redirect', setHeader, writeBS, writeLazyText, writeText
  , Method(GET, POST))
import Snap.Http.Server
  ( defaultConfig, setAccessLog, setErrorLog, setPort
  , ConfigLog(ConfigFileLog)
  )
import Snap.Snaplet
  ( addRoutes, makeSnaplet, serveSnapletNoArgParsing, Handler, SnapletInit
  )
import Snap.Util.FileServe (serveDirectory, serveFile)
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Hypered.Html
  ( loginForm, navigationReesd, registerForm, resetForm
  )

import Forming.Core
import Forming.Html


------------------------------------------------------------------------------
data App = App

makeLenses ''App


------------------------------------------------------------------------------
-- | This directory is built by `nix-build --attr site` and given to this
-- program by an environment variable. During development, it can also be set
-- manually to ../design-system to provide the static/ files.
{-# NOINLINE _FORMING_SITE_DIR #-}
_FORMING_SITE_DIR = unsafePerformIO $ getEnv "FORMING_SITE_DIR"


------------------------------------------------------------------------------
runServer cs = serveSnapletNoArgParsing defaultConfig $ appInit cs


------------------------------------------------------------------------------
appInit :: [Computation] -> SnapletInit App App
appInit cs =
  makeSnaplet "forming-server" description Nothing $ do
    addRoutes $
      [ ("", ifTop indexPage)
      , ("/about", ifTop aboutPage)

      , ("/login", ifTop loginPage)
      , ("/register", ifTop registerPage)
      , ("/reset", ifTop resetPage)

      ] ++ concatMap makeRoute cs ++
      [

        -- These are packaged in the respan-edit attribute in default.nix.
        -- In practice they will be served by Nginx.
        ("/static/css", serveDirectory' "static/css")
      , ("/static/fonts", serveDirectory' "static/fonts")
      , ("/static/img", serveDirectory' "static/img")
      , ("/favicon.ico", serveFile' "static/favicon.ico")
      , ("/humans.txt", serveFile' "static/humans.txt")

        -- I like to be explicit, but serveDirectory would be shorter.
        -- Would it be faster ?
      , ("/static/respan/respan.js", serveFile' "static/respan/respan.js")
      , ("/static/respan/rts.js", serveFile' "static/respan/rts.js")
      , ("/static/respan/lib.js", serveFile' "static/respan/lib.js")
      , ("/static/respan/out.js", serveFile' "static/respan/out.js")
      , ("/static/respan/runmain.js", serveFile' "static/respan/runmain.js")

      , ( "/checks/about", ifTop $ method GET checksAbout)
      , ( "/checks/sba/page", ifTop $ method GET checksSba)
      ]

    return App

  where

  description = "A simple HTTP server for Forming"

-- TODO Validate slugs are slugs.
makeRoute c =
  [ (B.concat ["/noteed/", B.pack (cSlug c)], ifTop $ formPage c)
  , (B.concat ["/noteed/", B.pack (cSlug c), "/ submit"], ifTop $ submitHandler c) -- TODO ifPost
  ]


------------------------------------------------------------------------------
serveDirectory' = serveDirectory . (_FORMING_SITE_DIR </>)

serveFile' = serveFile . (_FORMING_SITE_DIR </>)

aboutPage :: Handler App App ()
aboutPage = serveFile $ _FORMING_SITE_DIR </> "index.html"


------------------------------------------------------------------------------
indexPage :: Handler App App ()
indexPage = writeLazyText . renderHtml $ document "Reesd" $ do
  H.header $
    navigationReesd
  H.p $ do
    "Reesd is in private alpha. New registrations are currently disabled."
    " You can "
    H.a ! A.href "/login" $ "log in"
    "."

loginPage :: Handler App App ()
loginPage = writeLazyText . renderHtml $ document "Reesd" $ do
  H.header $
    navigationReesd
  H.p "Reesd is in private alpha. New registrations are currently disabled."
  loginForm

registerPage = writeLazyText . renderHtml $ document "Reesd" $ do
  H.header $
    navigationReesd
  H.p "Reesd is in private alpha. New registrations are currently disabled."
  registerForm
  -- There could be a footer, but on simple forms, I think I prefer without.
  -- footer "© Hypered, 2020-2021."

resetPage = writeLazyText . renderHtml $ document "Reesd" $ do
  H.header $
    navigationReesd
  H.p "Enter a verified email address and we'll send a password reset link\
    \ to that address."
  resetForm


----------------------------------------------------------------------
formPage :: Computation -> Handler App App ()
formPage c = writeLazyText . renderHtml $ document "Reesd" $ pageComputation c

submitHandler :: Computation -> Handler App App ()
submitHandler c = do
  logError "Handling .../+submit..."
  params <- getParams
  writeLazyText . renderHtml $ document "Reesd" $ do
    H.header $
      navigationReesd
    H.code . H.toHtml $ cName c
    H.code . H.toHtml $ show params
    runWithInputs' c $ makeInputsFromParams params

-- Same as runWithInputs but produces HTML instead of strings to stdout.
runWithInputs' :: Computation -> Either String (Maybe String, [Input]) -> Html
runWithInputs' Computation{..} mis = case mis of
  Right (mname, is) ->
    case evaluate [] (maybe cMain id mname) cRules is of
      UnsetVariables names -> do
        H.code "ERROR: missing user inputs."
        -- printUnsetVariables' names
        -- TODO 400 Bad Request
      Result x -> do
        H.code "Result:"
        H.code . H.toHtml . show $ x
      Error stack err -> do
        H.code "ERROR:"
        H.code . H.toHtml . show $ (stack, err)
  Left err -> do
    H.code "ERROR:"
    H.code . H.toHtml . show $ err
    -- TODO 400 Bad Request, possibly 500 if the form is invalid

-- I guess I can use `head` here since I assume getParams returns a list only
-- when there is at least one Param.
makeInputsFromParams params = Right (Nothing,
  map (\(k, v) -> Input (B.unpack k) (parseInput . B.unpack $ head v)) (M.toList params))

-- TODO This is a copy of add.hs.
addComputation =
  Computation
    "add"
    "Compute the addition of two integers a and b."
    "value"
    [ Rule "value" (Exp (Add (Name "a") (Name "b")))
    , Rule "a" Unset
    , Rule "b" Unset
    ]

----------------------------------------------------------------------
-- | This route tells what this server is.
checksAbout :: Handler App App ()
checksAbout = do
  logError "Handling GET /checks/about..."
  writeText "This is forming-server."


----------------------------------------------------------------------
-- | This route should be protected by auth_request in Nginx.
checksSba :: Handler App App ()
checksSba = do
  logError "Handling GET /checks/sba..."
  writeText "This page should be visible only when logged in."


----------------------------------------------------------------------
-- TODO Re-use the document function in Hypered.Html.
document :: Text -> Html -> Html
document title body = do
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
      H.div ! A.class_ "flex flex-column justify-between min-height-vh-100 mw8 center pa4 lh-copy" $
        body