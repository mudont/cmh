module Types where

-- import           ClassyPrelude                             (Bool, ByteString,
--                                                             Eq, Generic, IORef,
--                                                             Map, Maybe,
--                                                             Monad (return), Ord,
--                                                             Semigroup ((<>)),
--                                                             Show, String, Text,
--                                                             fromMaybe, ($),
--                                                             (++))
import           Crypto.Random.AESCtr                      (AESRNG, makeSystem)
import           Crypto.Random.API                         (CPRG, cprgGenBytes)
import           Data.Aeson
import qualified Data.Aeson                                as JSON
import qualified Data.Aeson.Types                          as AeT
import           Data.String.Conv
import           Data.String.Interpolation
import qualified Data.Text.Lazy                            as LT
import           Data.Time
import           Network.HTTP.Client                       (Manager, newManager)
import           Network.HTTP.Client.TLS                   (tlsManagerSettings)
import           Network.Wai
import           Network.Wai.Handler.Warp                  as Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger      (OutputFormat (CustomOutputFormatWithDetails),
                                                            RequestLoggerSettings (autoFlush, destination, outputFormat),
                                                            mkRequestLogger)
import           Network.Wai.Middleware.RequestLogger.JSON
import           Relude
import           Servant
import           Servant.Auth.JWT
import           Text.Blaze
import           Text.Blaze.Html5                          as H
import           Text.Blaze.Html5.Attributes               as HA
import           Text.RawString.QQ
import qualified Web.OIDC.Client                           as O

--
-- Data types
--

type SessionStateMap = Map Text (O.State, O.Nonce)

data OIDCConf = OIDCConf
  { redirectUri    :: ByteString,
    clientId       :: ByteString,
    clientPassword :: ByteString
  }
  deriving (Show, Eq)

data OIDCEnv = OIDCEnv
  { oidc           :: O.OIDC,
    mgr            :: Manager,
    ssm            :: IORef SessionStateMap,
    cprg           :: IORef AESRNG,
    prov           :: O.Provider,
    redirectUri    :: ByteString,
    clientId       :: ByteString,
    clientPassword :: ByteString
  }

-- | @AuthInfo@
data AuthInfo = AuthInfo
  { email         :: Text,
    emailVerified :: Bool,
    name          :: Text,
    picture       :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON AuthInfo where
  parseJSON (JSON.Object v) = do
    email :: Text <- v .: "email"
    email_verified :: Bool <- v .: "email_verified"
    name :: Text <- v .: "name"
    picture :: Text <- v .: "picture"
    return $ AuthInfo (toS email) email_verified (toS name) (toS picture)
  parseJSON invalid = AeT.typeMismatch "Coord" invalid

instance JSON.ToJSON AuthInfo where
  toJSON (AuthInfo e ev n p) =
    JSON.object
      [ "email" JSON..= (toS e :: Text),
        "email_verified" JSON..= ev,
        "name" JSON..= (toS n :: Text),
        "picture" JSON..= (toS p :: Text)
      ]

modUserName :: Text -> UserData -> UserData
modUserName un uD = uD {username = un}

getUserName :: UserData -> Text
getUserName = username

data UserData = UserData
  { username        :: Text,
    email           :: Text,
    userSecret      :: Text,
    localStorageKey :: Text,
    image           :: Text,
    redirectUrl     :: Maybe Text,
    token           :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON UserData

instance FromJSON UserData

instance ToJWT UserData

instance FromJWT UserData

-- | Fancy footwork here. When UserData is returned by the API Handlers
-- | this toMarkup is called to convert it to HTML
-- | which embeds Javascript code to
-- | 1.  set the localStorage on the client side
-- | 2. sets window.location to show the index.html page
instance ToMarkup UserData where
  toMarkup UserData {..} = H.docTypeHtml $ do
    H.head $
      H.title "Logged In"
    H.body $ do
      H.h1 "Logged In"
      H.p (H.toHtml ("Successful login with id " <> username))
      H.script
        ( H.toHtml
            -- localStorage.store = "{"user":{"username":"murali","token":"eyJ...PLQ","image":""}}"
            [str|
                localStorage.store = JSON.stringify({user: {
                  username: '$username$',
                  token: '$fromMaybe "" token$',
                  image: '$image$',
                  email: '$email$'
                }});

                window.location='$fromMaybe "/" redirectUrl$';
                |]
        )

type APIKey = ByteString

type Account = Text

type Conf = [(APIKey, Account)]

data Customer = Customer
  { account  :: Account,
    apiKey   :: APIKey,
    mail     :: Maybe Text,
    fullname :: Maybe Text,
    cPicture :: Maybe Text
  }

bootstrapCss :: H.AttributeValue
bootstrapCss = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"

bootstrapJs :: H.AttributeValue
bootstrapJs = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.min.js"

jQueryJs :: H.AttributeValue
jQueryJs = "https://code.jquery.com/jquery-3.5.1.min.js"

css :: H.AttributeValue -> H.Html
css path = H.link ! HA.href path ! HA.rel "stylesheet" ! HA.type_ "text/css"

js :: H.AttributeValue -> H.Html
js path = H.script ! HA.src path ! HA.type_ "text/javascript" $ H.toHtml ("" :: String)

data Homepage = Homepage

instance ToMarkup Homepage where
  toMarkup Homepage = H.docTypeHtml $ do
    H.head $ do
      H.title "OpenID Connect Servant Example"
      -- H.style (H.toHtml ("body { font-family: monospace; font-size: 18px; }" :: Text.Text))
      css bootstrapCss
      js jQueryJs
      js bootstrapJs
    H.body $ do
      H.h1 "OpenID Connect Servant Example"
      H.div $
        H.a ! HA.href "/google" $ "Click here to login with Google"
      H.ul $ do
        H.li $ do
          H.span "API Key in Local storage: "
          H.script (H.toHtml ("document.write(localStorage.getItem('api-key'));" :: Text))
        H.li $ do
          H.span "User ID in Local storage: "
          H.script (H.toHtml ("document.write(localStorage.getItem('user-id'));" :: Text))
        H.li $ do
          H.span "Picture in Local storage: "
          H.img ! HA.id "user-pic" ! HA.alt "A pic"
          H.script
            [r|
            var img = document.getElementById('user-pic');
            img.src = localStorage.getItem('picture');
           |]

data Err = Err
  { errTitle :: Text,
    errMsg   :: Text
  }

instance ToMarkup Err where
  toMarkup Err {..} = H.docTypeHtml $ do
    H.head $ H.title "Error"
    H.body $ do
      H.h1 (H.a ! HA.href "/" $ "Home")
      H.h2 (H.toHtml errTitle)
      H.p (H.toHtml errMsg)

data LoginForm = LoginForm
  { username :: Text,
    password :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON LoginForm

instance FromJSON LoginForm

data RegistrationForm = RegistrationForm
  { username :: Text,
    password :: Text,
    email    :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON RegistrationForm

instance FromJSON RegistrationForm


data ContactInfo = ContactInfo {
  username    :: Text,
  firstName   :: Text,
  lastName    :: Text,
  password    :: Maybe Text,
  email       :: Text,
  mobilePhone :: Text,
  homePhone   :: Text,
  workPhone   :: Text
} deriving (Eq, Show, Generic)
instance ToJSON ContactInfo

instance FromJSON ContactInfo

data EventInfo = EventInfo {
  eventId :: Maybe Int,
  date       :: UTCTime,
  name       :: Text,
  eventType  :: Text,
  comment    :: Text,
  alwaysShow :: Bool,
  orgId :: Int,
  leagueId :: Maybe Int,
  myRsvp     :: Maybe Text
} deriving (Eq, Show, Generic)

instance ToJSON EventInfo
instance FromJSON EventInfo

data EventRsvpInfo = EventRsvpInfo {
  eventId  :: Int,
  username :: Text,
  response :: Text,
  comment  :: Text
} deriving (Eq, Show, Generic)

instance ToJSON EventRsvpInfo
instance FromJSON EventRsvpInfo

data MatchInfo = MatchInfo {
  matchId :: Maybe Int,
  date :: Day,
  league :: Text,
  homePlayer1 :: Text,
  homePlayer2 :: Text,
  awayPlayer1 :: Text,
  awayPlayer2 :: Text,
  homeWon :: Bool,
  score::Text,
  comment :: Text,
  roundNum :: Int,
  matchNum :: Int
} deriving (Eq, Show, Generic)

instance ToJSON MatchInfo
instance FromJSON MatchInfo