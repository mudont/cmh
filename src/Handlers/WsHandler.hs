module Handlers.WsHandler where

import           API
import           AppM
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Except   (MonadIO (..))
import qualified Data.Text              as Text
import           Handlers.AuthHandler
import           Handlers.RajniHandler
import           Handlers.TennisHandler (tennisHandler)
import           Network.WebSockets     (PendingConnection, RejectRequest (..),
                                         acceptRequest, defaultRejectRequest,
                                         pendingRequest, rejectRequestWith,
                                         sendTextData, withPingThread)
import           Relude
import           Servant
import           Servant.Auth           ()
import qualified Servant.Auth.Server    as SAS
import           Servant.Server         ()
import           Types

wsHandler :: Maybe Text -> PendingConnection -> AppM ()
wsHandler = streamData
  where
    hour = 1000000 * 60 * 60
    -- streamData :: MonadIO m => PendingConnection -> m ()
    streamData :: Maybe Text -> PendingConnection -> AppM ()
    streamData mat pc = do
      jwtSettings <- asks jwtSettings
      liftIO . forM_ ([1 ..] :: [Int]) $ \_i -> do
        putStrLn $ "WS Pending conn" ++ show (pendingRequest pc)
        case mat of
          Nothing -> rejectRequestWith pc $ defaultRejectRequest {rejectMessage = "Need access-token"}
          Just jwt -> do
            u::(Maybe UserData) <- SAS.verifyJWT jwtSettings (encodeUtf8 jwt)

            case u of
              Nothing -> do
                print "+++++++ Bad Websocket token ========================="
                rejectRequestWith pc $ defaultRejectRequest {rejectMessage = "Invalid access-token"}
              Just userData -> do
                print "+++++++ Valid Websocket token =========================> "
                print u
                c <- acceptRequest pc
                liftIO $
                  withPingThread c 10 (return ()) $ do
                    putText "DBG ping thread "
                    forM_ ([1 ..] :: [Int]) $ \i -> do
                      -- putStrLn $ "DBG sending " <> show i
                      sendTextData c ("Hello " <> username (userData::UserData) <> show (i :: Int))
                      threadDelay hour
