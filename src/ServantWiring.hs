module ServantWiring where

import API
import AppM
import ClassyPrelude
import Handlers
import Servant
import qualified Servant.Auth.Server as SAS

--
-- Code
--

api :: Proxy (API '[SAS.JWT])
api = Proxy

nt :: AppState -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

mkApp :: Context '[SAS.CookieSettings, SAS.JWTSettings] -> SAS.CookieSettings -> SAS.JWTSettings -> AppState -> Application
mkApp cfg cs jwts state =
  serveWithContext api cfg $
    hoistServerWithContext
      api
      (Proxy :: Proxy '[SAS.CookieSettings, SAS.JWTSettings])
      (`runReaderT` state) -- Peels off AppM layer to leave Handler
      (server cs jwts)
