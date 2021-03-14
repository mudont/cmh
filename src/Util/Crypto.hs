{-# LANGUAGE PackageImports #-}

module Util.Crypto where

import           ClassyPrelude                   (ByteString)
import           "fastpbkdf2" Crypto.KDF.PBKDF2
import           "cryptonite" Crypto.Random
import           "base64" Data.ByteString.Base64
import           Data.Either
import           Data.Function
import qualified Data.List                       as List
import           Data.String.Conv                (toS)
import           Data.Text
import qualified Data.Text                       as T
import           Data.Text.Encoding
import qualified Data.Text.Lazy                  as T (toStrict)
import qualified Data.Text.Lazy.Builder          as B
import qualified Data.Text.Lazy.Builder.Int      as B
import           Data.Text.Read
import           Prelude
import qualified System.Random                   as Random

-- | We use Django style password hashing because we wanted
-- | this code to be run on a Django database in parallel with a Django app

djangoHashAlgo :: Text
djangoHashAlgo = "pbkdf2_sha256"

dfltIters :: Int
dfltIters = 12000

--
-- Django passwords in auth.user table look like this:
-- "pbkdf2_sha256$12000$px4WJMvOyTRX$6W2lVN8mEqGc1ReeXevuV9v6DZ59OzZiz4jI9XpmDS0="
-- algorithm, iterations, salt, and hash separated by '$'
-- This example is for cleartext password "sachin"
--
validatePassword :: Text -> Text -> Bool
validatePassword password djangoHash = valid
  where
    valid = case splitOn "$" djangoHash of
      [algo, itersTxt, salt, hash]
        | djangoHashAlgo == algo ->
          hash == makeDjangoHash (fst $ fromRight (100, "") $ decimal itersTxt) salt password
        | otherwise -> False
      _ -> False

intToText :: Integral a => a -> T.Text
intToText = T.toStrict . B.toLazyText . B.decimal


-- | Get random 12 character salt
getSalt :: IO Text
getSalt = getRandTxt 12

getRandTxt :: Int -> IO Text
getRandTxt len = do
  salt <- getRandomBytes len
  return $ encodeBase64 salt

-- | Make a Django style password hash (pbkdf2, sha256, dklen=32)  with given salt
makeDjangoHash :: Int -> Text -> Text -> Text
makeDjangoHash iters salt password = do
  encodeBase64
    ( fastpbkdf2_hmac_sha256
        (encodeUtf8 password)
        (encodeUtf8 salt)
        iters
        32
    )

-- | Make a Django style password hash (pbkdf2, sha256, dklen=32)  with given salt
-- | and prepend, in Django convention, info needed to validate the password
makeDjangoPasswordWithSalt :: Int -> Text -> Text -> Text
makeDjangoPasswordWithSalt iters salt password =
  intercalate "$" [djangoHashAlgo, intToText iters, salt, hash]
  where
    hash = makeDjangoHash iters salt password

-- | Make a Django style password hash (pbkdf2, sha256, dklen=32)  with random salt
-- | and prepend, in Django convention, info needed to validate the password
-- | This value can be store in Django's auth.user.password field
makeDjangoPassword :: Text -> IO Text
makeDjangoPassword password = do
  salt <- getSalt
  return $ makeDjangoPasswordWithSalt dfltIters salt password

-- | Generates somewhat readable random string without special characters
genRandomBS :: IO ByteString
genRandomBS = do
  g <- Random.newStdGen
  Random.randomRs (0, n) g & Prelude.take 42 & fmap toChar & readable 0 & toS & return
  where
    n = Prelude.length letters - 1
    toChar i = letters List.!! i
    letters = ['A' .. 'Z'] <> ['0' .. '9'] <> ['a' .. 'z']
    readable :: Int -> String -> String
    readable _ [] = []
    readable i str =
      let blocksize = case n of
            0 -> 8
            1 -> 4
            2 -> 4
            3 -> 4
            _ -> 12
          block = Prelude.take blocksize str
          rest = Prelude.drop blocksize str
       in if List.null rest
            then str
            else block <> "-" <> readable (i + 1) rest
