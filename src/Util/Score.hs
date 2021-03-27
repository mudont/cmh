module Util.Score where

import           Control.Applicative
import           Control.Monad
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

pScheme :: Parser Text
pScheme = string "data"
  <|> string "file"
  <|> string "ftp"
  <|> string "http"
  <|> string "https"
  <|> string "irc"
  <|> string "mailto"

-- | Parse Tennis scores. valid formats
-- |    6-3 6-1
-- |    6-3 1-6 1-0(11)
-- |    63 16 1311
-- parseTennisScore :: Parser Text
-- parseTennisScore = undefined
