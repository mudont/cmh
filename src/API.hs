module API where

import           API.AuthApi
import           API.RajniApi
import           API.TennisApi (TennisApi)
import           API.WsApi     (WsApi)
import           Servant.API

type API auths =
  AuthApi
    :<|> "api"
    :> ( TennisApi auths
           :<|> RajniApi auths -- Some fun Rajni/Chuck Norris "facts". Nothing more
       )
    :<|> WsApi
    :<|> Raw
