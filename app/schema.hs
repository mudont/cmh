{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

---------------------------------------
import ClassyPrelude hiding (group, poll)
import Config
import DB.Models
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Database.Selda hiding (Group)
import Database.Selda.Backend
import Database.Selda.PostgreSQL
import Options.Applicative

--import Options.Generic

-- tables :: SqlRow t => [Table t]
-- tables = [person]

dropTables :: PGConnectInfo -> IO ()
dropTables connInfo = withPostgreSQL connInfo $ do
  liftIO $ putStrLn "Dropping Tables"
  dropTable user
  dropTable site
  dropTable person
  dropTable group
  dropTable groupMember
  dropTable permission
  dropTable poll
  dropTable pollChoice
  dropTable pollVote

createTables :: PGConnectInfo -> IO ()
createTables connInfo = withPostgreSQL connInfo $ do
  createTable user
  createTable site
  createTable person
  createTable group
  createTable groupMember
  createTable permission
  createTable poll
  createTable pollChoice
  createTable pollVote

insertUsers :: PGConnectInfo -> IO ()
insertUsers connInfo = withPostgreSQL connInfo $ do
  ts <- liftIO getCurrentTime
  insert_
    user
    [ User def "murali12" "passwd" (Just "donthireddy@yahoo.com") ts ts
    -- Person def "C" (Just "M") "Raje" (Just "cmraje@yahoo.com") (Just "732") "cm" "" def def
    ]

getUser :: Int -> Query s (Row s User)
getUser uid = do
  u <- select user
  let inp = toId uid
   in restrict (u ! #id .== literal inp)
  pure u

insertUser :: [User] -> SeldaM b ()
insertUser u = do
  insert_ user u

dbQuery :: SqlRow b => SeldaConnection PG -> Query PG (Row PG b) -> IO [b]
dbQuery conn q = do
  runSeldaT (query q) conn

data Opts = Opts
  { optGlobalFlag :: !Bool,
    optConfigFile :: !Text,
    optCommand :: !Command
  }
  deriving (Generic)

--deriving instance Show Opts

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption

data Command
  = Create
  | Drop
  | Test

main :: IO ()
main = do
  liftIO $ putStrLn "Reading Dhall config to create tables"
  (opts :: Opts) <- execParser optsParser
  cfg <- config $ optConfigFile opts
  let pgCfg =
        PGConnectInfo
          { pgHost = dbHost cfg,
            pgPort = fromIntegral $ dbPort cfg,
            pgDatabase = database cfg,
            pgSchema = schema cfg,
            pgUsername = dbUsername cfg,
            pgPassword = dbPassword cfg
          }

  conn <- pgOpen pgCfg
  case optCommand opts of
    Create -> do
      createTables pgCfg
      putStrLn "Created Tables "
    Drop -> do
      dropTables pgCfg
      putStrLn "Deleted Tables!"
    Test -> do
      putStrLn "Testing Tables!"
      ts <- getCurrentTime
      runSeldaT
        ( insertUser
            [ User def "murali14" "passwd" (Just "mav@yahoo.com") ts ts
            ]
        )
        conn

      u <- dbQuery conn (getUser 1)
      print u
  print ("global flag: " ++ show (optGlobalFlag opts))
  where
    optsParser :: ParserInfo Opts
    optsParser =
      info
        (helper <*> versionOption <*> programOptions)
        ( fullDesc <> progDesc "Database management"
            <> header
              "create/drop tables"
        )
    versionOption :: Parser (a -> a)
    versionOption = infoOption "0.1" (long "version" <> help "Show version")
    programOptions :: Parser Opts
    programOptions =
      Opts <$> switch (long "global-flag" <> help "Set a global flag")
        <*> textOption
          ( long "config" <> metavar "config-file" <> value "./config.dhall"
              <> help "Config file. defaults to ./config.dhall"
          )
        <*> hsubparser (createCommand <> deleteCommand <> testCommand)
    createCommand :: Mod CommandFields Command
    createCommand =
      command
        "create"
        (info (pure Create) (progDesc "Create all tables"))

    deleteCommand :: Mod CommandFields Command
    deleteCommand =
      command
        "drop"
        (info (pure Drop) (progDesc "Drop all tables"))

    testCommand :: Mod CommandFields Command
    testCommand =
      command
        "test"
        (info (pure Test) (progDesc "Test all tables"))