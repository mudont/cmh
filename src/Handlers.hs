{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import API
import AppM
import ClassyPrelude
import Handlers.AuthHandler
import Handlers.RajniHandler
import Handlers.TennisHandler (tennisHandler)
import Handlers.WsHandler (wsHandler)
import Network.Wai.Application.Static
import Servant
import Servant.Auth ()
import qualified Servant.Auth.Server as SAS
import Servant.Server ()

server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT (API auths) AppM
server cs jwts =
  authHandler cs jwts
    :<|> apiHandler
    :<|> wsHandler
    :<|> serveDirectoryWith settings
  where
    apiHandler =
      tennisHandler
        :<|> rajniServer

    settings = (defaultFileServerSettings "./elm-client/build") {ssRedirectToIndex = True}
