{-# LANGUAGE OverloadedStrings #-}

module Handlers.TennisHandler where

import           API.TennisApi              (TennisApi)
import           AppM
import           Control.Monad.Except       (MonadIO (..))
import           Control.Monad.Trans.Reader (asks)
import qualified DB.Selda.CMModels          as CMM
import qualified DB.Selda.Queries           as Query
import           Database.Selda             (ID, Query, Row, SqlRow,
                                             SqlType (fromSql), def, fromId,
                                             query, toId, transaction,
                                             (:*:) ((:*:)))
import           Database.Selda.Backend     (runSeldaT)
import           Database.Selda.PostgreSQL  (PG)
import           Relude                     hiding (asks)

import           Err
-- import           Protolude                  (putText, threadDelay, toS, (&), undefined)
import qualified Data.Text                  as Text
import           Data.Time
import           Servant
import qualified Servant.Auth.Server        as SAS
import           Servant.Server             ()
import           Types                      (ContactInfo (..), EventInfo (..),
                                             EventRsvpInfo (..),
                                             EventWithRsvps (..),
                                             MatchInfo (..),
                                             UserData (email, username),
                                             getUserName, modUserName)
import           Util.Crypto

-- Servant Handlers

tennisHandler :: ServerT (TennisApi auths) AppM
tennisHandler =
  users
  :<|> getUser
  :<|> players
  :<|> updateProfile
  :<|> Handlers.TennisHandler.player
  :<|> events
  :<|> insertEvent
  :<|> updateEvent
  :<|> eventRsvps
  :<|> eventRsvp
  :<|> getMatches


-- Get data from database

users :: AppM [CMM.User]
users = dbQuery Query.allUsers

getUser :: Text -> AppM (Maybe CMM.User)
getUser un = do
  us <- dbQuery (Query.getUser un)
  pure $ case us of
    [u] -> pure u
    _   -> Nothing

playersHelper :: Query PG (Row PG CMM.User :*: Row PG CMM.Player) -> UserData -> AppM [ContactInfo]
playersHelper q _user = do
  conn <- asks dbConn
  res::[CMM.User :*: CMM.Player]  <- runSeldaT (query q) conn
  mapM (
    \(u :*: p) -> do
      -- u <- lift  u'
      -- p <- lift p'
      return $ ContactInfo
        (CMM.username (u::CMM.User))
        (CMM.first_name (u::CMM.User))
        (CMM.last_name (u::CMM.User))
        (Just $ CMM.password (u::CMM.User))
        (fromMaybe "" $ CMM.email (u::CMM.User))
        (fromMaybe "" $ CMM.mobile_phone (p::CMM.Player))
        (fromMaybe "" $ CMM.home_phone (p::CMM.Player))
        (fromMaybe "" $ CMM.work_phone (p::CMM.Player))
    ) res

players :: SAS.AuthResult UserData -> AppM [ContactInfo]
players (SAS.Authenticated _user) = playersHelper Query.allUsersPlayers  _user
players _ = forbidden " Pelase Login to see Contact info"

player :: SAS.AuthResult UserData -> Text -> AppM (Maybe ContactInfo)
player (SAS.Authenticated user) un = do
  res <- playersHelper (Query.userInfo un) user
  case res of
    [p] -> return $ Just p
    _   -> notFound $ "No user " <> un
player _ _ = forbidden " Pelase Login to see Contact info"


getComment :: Maybe CMM.EventRsvp -> Maybe Text
getComment mer =
  case mer of
    Nothing -> Nothing
    Just er -> Just $ CMM.comment (er :: CMM.EventRsvp)

eventFromDb :: CMM.Event :*: Maybe CMM.EventRsvp -> EventInfo
eventFromDb (dbEvt :*: rsvp) =
  EventInfo { eventId= Just $ fromId $ CMM.id (dbEvt:: CMM.Event)
            , date= CMM.date (dbEvt:: CMM.Event)
            , name= CMM.name (dbEvt:: CMM.Event)
            , eventType= CMM.event_type (dbEvt:: CMM.Event)
            , comment= CMM.comment (dbEvt:: CMM.Event)
            , alwaysShow= CMM.always_show (dbEvt:: CMM.Event)
            , orgId = fromId $ CMM.org_id (dbEvt:: CMM.Event)
            , leagueId = fromId <$> CMM.league_id (dbEvt:: CMM.Event)
            , myRsvp= CMM.response <$> rsvp
            , myRsvpComment= getComment rsvp
            }

eventToDb :: EventInfo -> CMM.Event
eventToDb ei =
  CMM.Event {
    id = def,
    date = def,
    name = name ei,
    org_id = toId $ orgId ei,
    event_type = eventType ei,
    comment = Types.comment (ei::EventInfo),
    always_show = alwaysShow ei,
    league_id = toId <$> leagueId ei
  }

events :: SAS.AuthResult UserData -> Maybe Int -> AppM [EventInfo]
events (SAS.Authenticated user) mevent = do
  conn <- asks dbConn
  events <- liftIO $ runSeldaT (Query.getRelevantEvents $ username (user::UserData)) conn
  return $ map eventFromDb events
events _ _ = forbidden " Pelase Login to see Event info"


insertEvent :: SAS.AuthResult UserData -> EventInfo -> AppM ()
insertEvent (SAS.Authenticated user) event = do
  conn <- asks dbConn
  eid <- liftIO $ runSeldaT (Query.insertEvent $ eventToDb event) conn
  return ()

insertEvent _ _  = forbidden " Pelase Login to add Events"

updateEvent :: SAS.AuthResult UserData -> EventInfo -> AppM ()
updateEvent (SAS.Authenticated user) event = do
  conn <- asks dbConn
  eid <- liftIO $ runSeldaT (Query.updateEvent $ eventToDb event) conn
  return ()

updateEvent _ _ = forbidden " Pelase Login to update"

-- Event Rsvps

eventRsvpInfoFromDb:: Text -> CMM.EventRsvp -> EventRsvpInfo
eventRsvpInfoFromDb un rsvp = EventRsvpInfo
  { eventId = fromId $ CMM.event_id (rsvp::CMM.EventRsvp)
  , username = un
  , response = CMM.response (rsvp::CMM.EventRsvp)
  , comment = CMM.comment (rsvp::CMM.EventRsvp)
  }


eventRsvpInfoWithUserFromDb:: CMM.EventRsvp :*: Text-> EventRsvpInfo
eventRsvpInfoWithUserFromDb (rsvp :*: un) = EventRsvpInfo
  { eventId = fromId $ CMM.event_id (rsvp::CMM.EventRsvp)
  , username = un
  , response = CMM.response (rsvp::CMM.EventRsvp)
  , comment = CMM.comment (rsvp::CMM.EventRsvp)
  }

eventRsvpInfoToDb:: ID CMM.Player -> EventRsvpInfo -> CMM.EventRsvp
eventRsvpInfoToDb playerId rsvp = CMM.EventRsvp
  { id = def
  , event_id = toId $ eventId (rsvp::EventRsvpInfo)
  , player_id = playerId
  , response = response (rsvp::EventRsvpInfo)
  , comment = comment (rsvp::EventRsvpInfo)
  }
eventRsvps :: SAS.AuthResult UserData -> Int -> AppM EventWithRsvps
eventRsvps (SAS.Authenticated user) eid = do
  conn <- asks dbConn
  let uname = username (user :: UserData)
  dbRows <- liftIO $ runSeldaT (Query.getEventWithRsvps $ toId eid) conn
  let eventInfo = eventFromDb (fst dbRows :*: Nothing)
  let rsvps = map eventRsvpInfoWithUserFromDb (snd dbRows)
  return EventWithRsvps { event = eventInfo, rsvps = rsvps}

eventRsvp :: SAS.AuthResult UserData -> EventRsvpInfo -> AppM ()
eventRsvp (SAS.Authenticated user) er = do
  let uname = username (user :: UserData)
  ps <- dbQuery (Query.getPlayerByUsername uname)
  case ps of
    [p] -> do
      conn <- asks dbConn
      liftIO $ runSeldaT (Query.recordEventRsvp $ eventRsvpInfoToDb (CMM.id (p::CMM.Player)) er) conn
    _ -> serverError "Didn't get exactly one player for user"

updateProfile :: SAS.AuthResult UserData -> Text -> ContactInfo -> AppM ()
updateProfile (SAS.Authenticated _user) uname ci = do
  conn <- asks dbConn
  liftIO $ print "!!!!!!!!!!!! updateProfile  !!!!!!!!!!!!!!!!"
  liftIO $ print ci
  liftIO $ runSeldaT (Query.updateProfile ci) conn
  return ()

updateProfile _ _ _ = forbidden " Pelase Login to update Profile"
-- usersPlayers :: AppM [UserPlayer]
--usersPlayers = dbQuery Query.allUsersPlayers

-- | Create user in database if not already present
-- | This is useful for registering users authenticated thru OIDC
ensureDBUser :: UserData -> AppM UserData
ensureDBUser uD = do
  mu <- dbQuery $ Query.getUserByEmail (Types.email (uD :: UserData))
  case mu of
    [dbUser] -> do
      let un = CMM.username (dbUser :: CMM.User)
      let (userData :: UserData) = Types.modUserName un uD
      pure userData
    _ -> do
      -- if email, strip domain part
      let un = Text.takeWhile (/= '@') $ getUserName uD
      let em = email (uD :: UserData)
      eU <- createUser un Nothing em
      case eU of
        Left err -> forbidden err
        Right _  -> pure $ Types.modUserName un uD

checkPasswd :: Text -> Text -> AppM (Maybe CMM.User)
checkPasswd username pswd = do
  mu <- getUser username
  pure $ case mu of
    Nothing -> Nothing
    Just u -> if validatePassword pswd (CMM.password (u :: CMM.User)) then Just u else Nothing

createResetSecret :: Text -> AppM (Maybe Text)
createResetSecret email = do
  conn <- asks dbConn
  liftIO $ runSeldaT (Query.createResetSecret email) conn


getUserFromResetToken :: Text -> AppM (Maybe CMM.User)
getUserFromResetToken resetToken = do
  conn <- asks dbConn
  liftIO $ runSeldaT Query.cleanupResetTokens conn
  mu <- dbQuery $ Query.getUserFromResetToken resetToken
  case mu of
    []  -> return Nothing
    [u] -> return $ Just u

createUser :: Text -> Maybe Text -> Text -> AppM (Either Text CMM.User)
createUser username pswd email = do
  mu <- getUser username
  case mu of
    Nothing -> do
      ts <- liftIO getCurrentTime
      conn <- asks dbConn
      hashed <- liftIO $ case pswd of
        Just p -> makeDjangoPassword p
        _      -> pure "-- No Password. User created from OIDC login --"

      let u = CMM.User { id = def
                        , username = username
                        , password = hashed
                        , last_login = Nothing
                        , first_name = ""
                        , last_name = ""
                        , email = Just email
                        , is_staff = True
                        , is_active = True
                        , is_superuser = False
                        , email_verified = False
                        , date_joined = ts
                        }
      liftIO $ Query.insertUserPlayer conn u
      pure $ Right u
    Just _ -> pure $ Left $ "User " <> username <> " exists"

-- MATCH

matchFromDb :: CMM.Match :*: Maybe Text :*: Maybe Text :*: Maybe Text :*: Maybe Text :*: Maybe Text -> MatchInfo
matchFromDb (m :*: mleague :*: mh1un :*: mh2un :*: ma1un :*: ma2un) =
  MatchInfo { matchId = Just $ fromId $ CMM.id (m::CMM.Match)
            , date= CMM.date (m::CMM.Match)
            , league= fromMaybe "" mleague
            , homePlayer1= fromMaybe "" mh1un
            , homePlayer2= fromMaybe "" mh2un
            , awayPlayer1= fromMaybe "" ma1un
            , awayPlayer2= fromMaybe "" ma2un
            , homeWon= CMM.home_won (m::CMM.Match)
            , score= fromMaybe "" $ CMM.score (m::CMM.Match)
            , comment= CMM.comment (m::CMM.Match)
            , roundNum= fromMaybe 0 $ CMM.round_num (m::CMM.Match)
            , matchNum= fromMaybe 0 $ CMM.match_num (m::CMM.Match)
            }

getMatches :: SAS.AuthResult UserData -> Maybe Int -> AppM [MatchInfo]
getMatches (SAS.Authenticated user) mmatch = do
  conn <- asks dbConn
  matches <- liftIO $ runSeldaT (Query.getMatches (username (user::UserData))) conn
  return $ map matchFromDb matches
getMatches _ _ = forbidden " Pelase Login to see Match info"


-- Utility
dbQuery :: SqlRow b => Query PG (Row PG b) -> AppM [b]
dbQuery q = do
  conn <- asks dbConn
  liftIO $ runSeldaT (query q) conn
