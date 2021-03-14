module Config where

import           ClassyPrelude
import           Dhall

---------------------------------------

data AppConfig = AppConfig
  { port           :: Int,
    dbHost         :: Text,
    dbPort         :: Natural,
    database       :: Text,
    schema         :: Maybe Text,
    dbUsername     :: Maybe Text,
    dbPassword     :: Maybe Text,
    redirectUri    :: Text,
    clientId       :: Text,
    clientPassword :: Text,
    jwkFile        :: Text,
    website        :: Text,
    profileUrl     :: Text
  }
  deriving (Generic, Show)

instance FromDhall AppConfig

config :: Text -> IO AppConfig
config configFile = do
  putStrLn "Reading Dhall config "
  cfg :: AppConfig <- input Dhall.auto configFile
  pure cfg
