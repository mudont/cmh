module Handlers.RajniHandler where

import           API.RajniApi        (RajniApi)
import           AppM
import           ClassyPrelude
import           Data.List           ((!!))
import           Err
import           Servant
import qualified Servant.Auth.Server as SAS
import qualified System.Random       as Random
import           Types

chuckNorrisQuote :: SAS.AuthResult UserData -> AppM String
chuckNorrisQuote (SAS.Authenticated _user) = do
  i <- liftIO $ Random.randomRIO (0, length cQs - 1)
  return (cQs !! i)
chuckNorrisQuote _ = forbidden " Looks like JWT cookie didn't work. No Chuck Norris for you"

rajniQuote :: AppM String
rajniQuote = do
  i <- liftIO $ Random.randomRIO (0, length rQs - 1)
  return (rQs !! i)

rajniServer :: ServerT (RajniApi auths) AppM
rajniServer = chuckNorrisQuote :<|> rajniQuote

cQs :: [String]
cQs =
  [ "Medusa stared at Chuck Norris. She turned into dust",
    "Chuck Norris can divide by zero.",
    "Chuck Norris can kill two stones with one bird.",
    "Chuck Norris once visited the Virgin Islands. They are now The Islands.",
    "When the Boogeyman goes to sleep every night, he checks his closet for Chuck Norris.",
    "Chuck Norris knows Victoria's secret",
    "When Chuck Norris was born, he drove his mom home from the hospital",
    "Time waits for no man, unless that man is Chuck Norris",
    "When God said, ‘Let There Be LIGHT!’ Chuck Norris said, ‘Say Please.’",
    "Chuck Norris has been to Mars. That’s why there are no signs of life there.",
    "Chuck Norris starred in Star Wars. He was the force"
  ]

rQs :: [String]
rQs =
  [ "Naan oru thadava sonna nooru thadava sonna maadiri",
    "Ketta payyan saar inda Kaali",
    "Rajni tested +ve for Corona. Corona is now under quarantine",
    "Rajni once told a boy to keep quiet. He became Manmohan Singh",
    "Idhu eppadi irukku",
    "See-eeviduven",
    "Naan eppo varuven, epdi varuvennu yarukkum theriyaadu. Aana vara vendiya nerathula correcta vandhuduven",
    "Aandavan nallavangala sodhipaan aana kai vida maatan! Kettavangalukku alli alli kodupaan aana kadaisila kai vittuduvaan!",
    "En vazhi, thani vazhi",
    "Kashta padama ethivum kadaikathu kashta padama kedachathu ennikume nelaikaathu",
    "Khatam... Khatam... Mudinjathu mudinju potchu",
    "Kannaaa panniga thaa kutama varu singam singala tha varum",
    "Pera kettaale chumma adhirudhulla?",
    "Arthasasthram unga vazhi, Dharmasasthram en vazhi",
    "Naan vara vendiya neram vandhudichi nee poga vendiya neram nerungidichi",
    "Naan solrathaiyum seiven, sollaathathiyum seiven"
  ]
