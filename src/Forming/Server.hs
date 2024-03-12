-- |
-- A small HTTP server exposing an endpoint to submit a form.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Forming.Server where

import Control.Lens (makeLenses)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Data.List (head)
import Data.String (String)
import qualified Data.Text.Encoding as T
import Protolude hiding (head, Handler, Type)
import Snap.Core
  ( getParams, ifTop, logError, method
  , writeLazyText, writeText
  , Method(GET), Params)
import Snap.Http.Server
  ( defaultConfig
  )
import Snap.Snaplet
  ( addRoutes, makeSnaplet, serveSnapletNoArgParsing, Handler, SnapletInit
  )
import Snap.Util.FileServe (serveDirectory, serveFile)
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath ((</>))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import Text.Pretty.Simple (pShowNoColor)

import Forming.Core
import Forming.IO (parseInput)
import Forming.Html
import Forming.Type (Type)


------------------------------------------------------------------------------
data App = App

makeLenses ''App


------------------------------------------------------------------------------
-- | This directory is built by `nix-build --attr site` and given to this
-- program by an environment variable. During development, it can also be set
-- manually to ../design to provide the static/ files.
{-# NOINLINE _FORMING_SITE_DIR #-}
_FORMING_SITE_DIR :: String
_FORMING_SITE_DIR = unsafePerformIO $ getEnv "FORMING_SITE_DIR"


------------------------------------------------------------------------------
runServer :: [Computation] -> IO ()
runServer cs = serveSnapletNoArgParsing defaultConfig $ appInit cs


------------------------------------------------------------------------------
appInit :: [Computation] -> SnapletInit App App
appInit cs =
  makeSnaplet "forming-server" description Nothing $ do
    addRoutes $
      [ ("", ifTop showLandingPage)
      , ("/about", ifTop showAboutPage)
      , ("/examples", ifTop $ showNamespacePage cs)
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
      ]

    return App

  where

  description = "A simple HTTP server for Forming"

-- TODO Validate slugs are slugs.
makeRoute :: Computation -> [(ByteString, Handler App App ())]
makeRoute c =
  [ (B.concat ["/examples/", T.encodeUtf8 (cSlug c)], ifTop $ showFormPage c)
  , (B.concat ["/examples/", T.encodeUtf8 (cSlug c), "/ view"], ifTop $ showFormDocPage c)
  , (B.concat ["/examples/", T.encodeUtf8 (cSlug c), "/ submit"], ifTop $ submitHandler c) -- TODO ifPost
  ]


------------------------------------------------------------------------------
serveDirectory' :: FilePath -> Handler App App ()
serveDirectory' = serveDirectory . (_FORMING_SITE_DIR </>)

serveFile' :: FilePath -> Handler App App ()
serveFile' = serveFile . (_FORMING_SITE_DIR </>)

showAboutPage :: Handler App App ()
showAboutPage = serveFile $ _FORMING_SITE_DIR </> "index.html"


------------------------------------------------------------------------------
showLandingPage :: Handler App App ()
showLandingPage = writeLazyText . renderHtml $ landingPage


showNamespacePage :: [Computation] -> Handler App App ()
showNamespacePage = writeLazyText . renderHtml . namespacePage

showFormPage :: Computation -> Handler App App ()
showFormPage = writeLazyText . renderHtml . formPage

showFormDocPage :: Computation -> Handler App App ()
showFormDocPage = writeLazyText . renderHtml . formDocPage


----------------------------------------------------------------------
submitHandler :: Computation -> Handler App App ()
submitHandler c = do
  logError "Handling .../+submit..."
  case gatherUnsets Nothing (cMain c) (cRules c) of
    Left err -> panic (show err)
    Right unsets -> do
      params <- getParams
      writeLazyText . renderHtml $ document "Forming" $ do
        H.header navigationForming
        H.code . H.text $ cName c
        H.pre . H.code . H.lazyText $ pShowNoColor params
        runWithInputs' c $ makeInputsFromParams unsets params

-- I guess I can use `head` here since I assume getParams returns a list only
-- when there is at least one Param. We ignore empty strings as they are
-- submitted when users don't do anything.
makeInputsFromParams
  :: [(Rule, Maybe Type)] -> Params -> Either Text (Maybe Text, [Input])
makeInputsFromParams unsets params = Right (Nothing,
  filter (not . isEmptyString) $
  map
    (\(k, v) ->
      let mtype = lookupType (T.decodeUtf8 k) unsets
      in Input (T.decodeUtf8 k) (parseInput mtype . T.decodeUtf8 $ head v))
    (M.toList params))


----------------------------------------------------------------------
-- | This route tells what this server is.
checksAbout :: Handler App App ()
checksAbout = do
  logError "Handling GET /checks/about..."
  writeText "This is forming-server."
