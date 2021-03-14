{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Text
import DjangoPassword
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Prelude

main :: IO ()
main = hspec spec

aDjangoPassword :: Text
aDjangoPassword = "pbkdf2_sha256$12000$px4WJMvOyTRX$6W2lVN8mEqGc1ReeXevuV9v6DZ59OzZiz4jI9XpmDS0="

clearPassword :: Text
clearPassword = "sachin"

spec :: Spec
spec =
  describe "Django Password encode and validation" $ do
    it "Encodes and succesfuly validates good password against matching hash" $
      validatePassword clearPassword aDjangoPassword
        `shouldBe` True

    it "Rejects bad password" $
      validatePassword (clearPassword <> "x") aDjangoPassword
        `shouldBe` False

    it "make Django password" $
      makeDjangoPasswordWithSalt iters "px4WJMvOyTRX" clearPassword `shouldBe` aDjangoPassword

-- spec = with (return app) $
--   describe "GET /users" $ do
--     it "responds with 200" $
--       get "/users" `shouldRespondWith` 200
--     it "responds with [User]" $ do
--       let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
--       get "/users" `shouldRespondWith` users
