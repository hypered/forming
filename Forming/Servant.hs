{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- A small HTTP server exposing an endpoint to submit a form.
-- This is a Servant version of Server.hs (which uses Snap framework).
module Forming.Servant where

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant ( Get, Handler, Proxy(..), ServerT )
import qualified Servant as Servant
import qualified Servant.Server as Server
import qualified Servant.HTML.Blaze as B
import qualified Text.Blaze.Html5 as H

import Forming.Core ( Computation(..) )


------------------------------------------------------------------------------
-- | This is the main function of this module. It runs a Warp server, serving
-- our `App` API definition.
runServant :: [Computation] -> IO ()
runServant cs =
  Warp.run 9000 waiApp
 where
  waiApp = serve

-- | Turn our `serverT` implementation into a Wai application, suitable for
-- Warp.run.
serve :: Wai.Application
serve =
  Servant.serveWithContext appProxy Server.EmptyContext
    $ Server.hoistServerWithContext appProxy settingsProxy id
    $ serverT


------------------------------------------------------------------------------
type ServerSettings = '[]

settingsProxy ::Proxy ServerSettings
settingsProxy = Proxy


------------------------------------------------------------------------------
type App = Get '[B.HTML] H.Html

appProxy :: Proxy App
appProxy = Proxy


------------------------------------------------------------------------------
serverT :: ServerT App Handler
serverT = showHomePage

showHomePage :: Handler H.Html
showHomePage = pure "Hello."
