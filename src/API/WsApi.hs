module API.WsApi where

import ClassyPrelude
import Servant.API
import Servant.API.WebSocket

type WsApi = "stream" :> QueryParam "access-token" Text :> WebSocketPending
