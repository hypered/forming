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
  , Method(GET, POST), Params)
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

import Forming.Core
import Forming.IO (parseInput)
import Forming.Html
import Forming.Syntax (Syntax(..))
import Forming.Type (Type)


------------------------------------------------------------------------------
data App = App

makeLenses ''App


------------------------------------------------------------------------------
-- | This directory is built by `nix-build --attr site` and given to this
-- program by an environment variable. During development, it can also be set
-- manually to ../design to provide the static/ files.
{-# NOINLINE _FORMING_SITE_DIR #-}
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
makeRoute c =
  [ (B.concat ["/examples/", B.pack (cSlug c)], ifTop $ showFormPage c)
  , (B.concat ["/examples/", B.pack (cSlug c), "/ view"], ifTop $ showFormDocPage c)
  , (B.concat ["/examples/", B.pack (cSlug c), "/ submit"], ifTop $ submitHandler c) -- TODO ifPost
  ]


------------------------------------------------------------------------------
serveDirectory' = serveDirectory . (_FORMING_SITE_DIR </>)

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
    Left err -> error (show err)
    Right unsets -> do
      params <- getParams
      writeLazyText . renderHtml $ document "Forming" $ do
        H.header navigationForming
        H.code . H.toHtml $ cName c
        H.code . H.toHtml $ show params
        runWithInputs' c $ makeInputsFromParams unsets params

-- I guess I can use `head` here since I assume getParams returns a list only
-- when there is at least one Param. We ignore empty strings as they are
-- submitted when users don't do anything.
makeInputsFromParams
  :: [(Rule, Maybe Type)] -> Params -> Either String (Maybe String, [Input])
makeInputsFromParams unsets params = Right (Nothing,
  filter (not . isEmptyString) $
  map
    (\(k, v) ->
      let mtype = lookupType (B.unpack k) unsets
      in Input (B.unpack k) (parseInput mtype . B.unpack $ head v))
    (M.toList params))

isEmptyString (Input _ (String [])) = True
isEmptyString _ = False


----------------------------------------------------------------------
-- | This route tells what this server is.
checksAbout :: Handler App App ()
checksAbout = do
  logError "Handling GET /checks/about..."
  writeText "This is forming-server."
