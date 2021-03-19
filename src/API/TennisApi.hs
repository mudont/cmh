module API.TennisApi where

import           ClassyPrelude
import           DB.Selda.CMModels   hiding (email)
import           Servant.API
import           Servant.Auth.Server
import           Types

type TennisApi auths =
  "users" :> Get '[JSON] [User]
  :<|> "user" :> Capture "user" Text :> Get '[JSON] (Maybe User)
  :<|> Auth auths UserData :> "players" :> Get '[JSON] [ContactInfo]
  :<|> Auth auths UserData :> "profile" :> Capture "user" Text :>
      ReqBody '[JSON] ContactInfo :> Put '[JSON] ()
  :<|> Auth auths UserData :> "profile" :> Capture "user" Text :>
      Get '[JSON] (Maybe ContactInfo)
  :<|> Auth auths UserData :> "events" :> QueryParam "event_id" Int :>
      Get '[JSON] [EventInfo]
  :<|> Auth auths UserData :> "event" :>
       ReqBody '[JSON] EventInfo :> Post '[JSON] ()
  :<|> Auth auths UserData :> "event" :>
       ReqBody '[JSON] EventInfo :> Put '[JSON] ()
  :<|> Auth auths UserData :> "event_rsvps" :> Capture "event_id" Int :>
      Get '[JSON] EventWithRsvps
  :<|> Auth auths UserData :> "event_rsvp" :>
       ReqBody '[JSON] EventRsvpInfo :> Post '[JSON] ()

{- TODO:
  events ? org_id, always_show, max_rows, user_id
  rsvps ? event_id

  registrations

  matches
  my_matches
  standings


-}
