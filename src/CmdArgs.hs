{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CmdArgs where

import ClassyPrelude
import Config
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Options.Applicative

data Opts = Opts
  { optPort :: !Int, -- Not used. Config file setting is used instead
    optConfigFile :: !T.Text
  }

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption

data Command
  = Create
  | Drop

cmdArgs :: IO Opts
cmdArgs = do
  putStrLn "Reading Dhall config to create tables"
  execParser optsParser
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
      Opts
        <$> option
          auto
          -- !!!!!! port is ignored infavor of config file setting !!!!!!
          (long "portIgnored" <> value 8000 <> help "HTTP port to serve on")
          <*> textOption
            ( long "config" <> metavar "config-file" <> value "./config.dhall"
                <> help "Config file. defaults to ./config.dhall"
            )