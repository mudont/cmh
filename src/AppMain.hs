{-# LANGUAGE OverloadedStrings #-}

module AppMain where

import           AppM
import           ClassyPrelude                             (IO, IORef, Int,
                                                            Monad ((>>=)),
                                                            Semigroup ((<>)),
                                                            Show (show), String,
                                                            fromIntegral,
                                                            liftIO, mapM_,
                                                            newIORef, print,
                                                            putStrLn, return,
                                                            ($))
import           CmdArgs
import           Config
import           Crypto.Random.AESCtr
import           Data.List.Split                           (startsWith)
import qualified Data.Map                                  as M
import           Data.Text                                 (isPrefixOf)
import           Data.Text.Encoding
import           Database.Selda.PostgreSQL
import           Network.HTTP.Client                       (newManager)
import           Network.HTTP.Client.TLS                   (tlsManagerSettings)
import           Network.Wai
import qualified Network.Wai.Handler.Warp                  as Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON
import           Protolude.Conv
import           Servant
import qualified Servant.Auth.Server                       as SAS
import           ServantWiring
import           System.Directory
import           Types
import qualified Web.OIDC.Client                           as O

initOIDC :: AppConfig -> IO OIDCEnv
initOIDC AppConfig {..} = do
  cprg <- makeSystem >>= newIORef
  (ssm :: IORef SessionStateMap) <- newIORef M.empty
  -- ssm' <- readIORef ssm
  -- putText $ "DBG ssm initialized. Map len" <> (show . length . M.elems $ ssm')
  mgr <- newManager tlsManagerSettings
  prov <- O.discover "https://accounts.google.com" mgr
  let ru =
        if "/" `isPrefixOf` redirectUri
          then encodeUtf8 ("http://localhost:" <> toS (show port :: String) <> redirectUri)
          else toS redirectUri
  let clId = encodeUtf8 clientId
  let clPswd = encodeUtf8 clientPassword
  let oidc = O.setCredentials clId clPswd ru $ O.newOIDC prov
  return
    OIDCEnv
      { oidc = oidc,
        mgr = mgr,
        ssm = ssm,
        cprg = cprg,
        --, genState = genRandomBS
        prov = prov,
        redirectUri = ru,
        clientId = clId,
        clientPassword = clPswd
      }

logRequestHeaders :: Application -> Application
logRequestHeaders incoming request outgoing = do
  let headerList = requestHeaders request
  liftIO $ mapM_ print headerList
  incoming request outgoing

jsonRequestLogger :: IO Middleware
jsonRequestLogger = mkRequestLogger $ SAS.def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}

main :: IO ()
main = do
  opts <- cmdArgs
  appCfg <- config $ optConfigFile opts
  let pn :: Int = fromIntegral $ port appCfg

  oidcEnv <- initOIDC appCfg
  warpLogger <- jsonRequestLogger
  -- tstamp <- getCurrentTime
  let jf = show $ jwkFile appCfg
  keyExists <- doesFileExist jf
  if keyExists then return () else SAS.writeKey jf
  myKey <- SAS.readKey jf

  let warpSettings = Warp.defaultSettings
      portSettings = Warp.setPort pn warpSettings
      settings = Warp.setTimeout 55 portSettings
      jwtCfg = SAS.defaultJWTSettings myKey
      cookieCfg = SAS.defaultCookieSettings {SAS.cookieIsSecure = SAS.NotSecure} -- for Dev NotSecure OK
      -- else SAS.defaultCookieSettings
      cfg = cookieCfg :. jwtCfg :. EmptyContext

  putStrLn "Connecting to Postgres"
  conn <-
    pgOpen
      PGConnectInfo
        { pgHost = dbHost appCfg,
          pgPort = fromIntegral $ dbPort appCfg,
          pgDatabase = database appCfg,
          pgSchema = schema appCfg,
          pgUsername = dbUsername appCfg,
          pgPassword = dbPassword appCfg
        }
  let appState = AppState appCfg oidcEnv conn jwtCfg

  print $ "Starting server on " <> show pn
  Warp.runSettings settings $ warpLogger $ {-logRequestHeaders $-} simpleCors $ mkApp cfg cookieCfg jwtCfg appState

  putStrLn "Closing Postgres"
  seldaClose conn
