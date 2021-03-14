module API.RajniApi where

import           ClassyPrelude
import           Servant.API
import           Servant.Auth.Server
import           Types

type RajniQuoteApi = "rajni" :> Get '[JSON] String
type RajniApi auths =
  Auth auths UserData :> "chuck" :> Get '[JSON] String
    :<|> RajniQuoteApi
