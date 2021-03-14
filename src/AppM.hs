module AppM where

import           ClassyPrelude
import           Config
import           Database.Selda.Backend    (SeldaConnection)
import           Database.Selda.PostgreSQL
import           Servant                   as S
import           Servant.Auth.Server       (JWTSettings)
import           Types

data AppState = AppState
  { cfg         :: AppConfig,
    oidcEnv     :: OIDCEnv,
    dbConn      :: SeldaConnection PG,
    jwtSettings :: JWTSettings
  }

type AppM = ReaderT AppState S.Handler
