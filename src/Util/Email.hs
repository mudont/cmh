{-# LANGUAGE OverloadedStrings #-}

module Util.Email where

import           ClassyPrelude
import           Const
import           Data.String.Conv  (toS)
import           Data.Text         (Text)
import qualified Data.Text.Lazy    as Lazy
import qualified Network.Mail.Mime as Mime
import           Network.Mail.SMTP

from :: Address
from = Address Nothing "murali@mariandrive.com"

makeRecipient :: Text -> [Address]
makeRecipient t = [Address (Just t) t]

cc :: [Address]
cc = []

bcc :: [Address]
bcc = []

body :: Lazy.Text -> Mime.Part
body = Mime.plainPart

html :: Mime.Part
html = Mime.htmlPart "<h1>HTML</h1>"

mail :: Text -> Text -> Lazy.Text -> IO ()
mail to subject b = do
  print $ "Sending email to " <> to <> " about " <> subject
  renderSendMail $ simpleMail from (makeRecipient to) cc bcc subject [body b]

doResetEmail :: Text -> Maybe Text -> Text -> IO ()
doResetEmail website mtoken address = do
  print "+++++++++ Sending Reset email +++++++"
  print mtoken
  print address
  maybe (mailRejectReset address) (mailResetLink website address) mtoken

mailResetLink :: Text -> Text -> Text -> IO ()
mailResetLink website address token  = do
  let subject = "CM Hackers Password Reset link"
  let h = fromStrict $ toS $ "<a href=" <> website <> (toS ("/" <>
          Const.loginWithResetSecretUrlFrag  <> "/") <>  token <>
          "> Click to Login and Reset password</a>")
  renderSendMail $ simpleMail from (makeRecipient address) cc bcc subject [Mime.htmlPart h]



mailRejectReset :: Text -> IO ()
mailRejectReset address =
  mail address "Reset password failed" "This is email address does not belong to a valid user at CM Hackers"

