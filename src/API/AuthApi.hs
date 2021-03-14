module API.AuthApi where

import           ClassyPrelude
import           Const
import           Servant.API
import           Servant.Auth.Server
import           Servant.HTML.Blaze
import           Types               (LoginForm, RegistrationForm, UserData)
type APIGoogleAuth = "google" :> Get '[JSON] NoContent

type APIGoogleAuthCb =
  "google" :> "cb" :> QueryParam "error" Text
    :> QueryParam "code" Text
    :> QueryParam "state" Text
    :> Get '[HTML] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] UserData)

type AuthApi =
  APIGoogleAuth -- redirect User to the OpenID Provider
  -- Google auth flow
    :<|> APIGoogleAuthCb
    -- login with password
    :<|> "login" :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] UserData)
    -- Register new user
    :<|> "register" :> ReqBody '[JSON] RegistrationForm :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] UserData)
    -- Reset Password
    :<|> "reset_password" :> QueryParam "email" Text :> Get '[JSON] Text
    :<|> "login_with_reset_secret" :> Capture "secret" Text :> Get '[HTML] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] UserData)
