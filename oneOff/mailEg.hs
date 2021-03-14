#!/usr/bin/env stack
-- stack script --resolver lts-17.0

{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Network.Mail.Mime as Mime
import Network.Mail.SMTP
import Prelude

from :: Address
from = Address Nothing "maverickone@gmail.com"

to :: [Address]
to = [Address (Just "Monad Don") "email@domain.com"]

cc :: [Address]
cc = []

bcc :: [Address]
bcc = []

subject :: Text
subject = "Haskell functional starter"

body :: Mime.Part
body = Mime.plainPart "Hi, sending email from haskell"

html :: Mime.Part
html = Mime.htmlPart "<h1>HTML</h1>"

mail :: Mime.Mail
mail = simpleMail from to cc bcc subject [body, html]

main :: IO ()
main = do
  putStrLn "DBG 1"
  sendMail "awani.org" mail
  putStrLn "DBG 1.5"
  renderSendMail mail
  putStrLn "DBG 2"
