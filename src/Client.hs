{-# LANGUAGE OverloadedStrings #-}
module Client where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client
import           Servant.Types.SourceT    (foreach)

import           API
import           API.RajniApi
import           API.TennisApi
import           ClassyPrelude
import qualified Servant.Client.Streaming as S
import           Types

api :: Proxy RajniQuoteApi
api = Proxy

rajni :: ClientM String
rajni = client api

getRajniQuote :: ClientM String
getRajniQuote = do
    putStrLn "in client rajni"
    rajni

run :: String -> IO ()
run url = do
  baseUrl <- parseBaseUrl url
  manager' <- newManager defaultManagerSettings
  res <- runClientM getRajniQuote (mkClientEnv manager' baseUrl)
  case res of
    Left err -> print err
    Right quote -> do
      print quote

