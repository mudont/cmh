{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.Opaleye.User where

import ClassyPrelude
import Control.Arrow
import DB.Opaleye.Database -- our database types
-- import qualified Data.Aeson as JSON ()
-- import Data.Profunctor (Profunctor (dimap))
-- import Data.Profunctor.Product (ProductProfunctor (empty, (***!)))
-- import Data.Profunctor.Product.Default (Default (..))
-- import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Scientific ()
import Data.String.Conv (toS)
import Data.Text ()
import Data.Time (getCurrentTimeZone, utcToLocalTime)
import Data.UUID ()
import Database.PostgreSQL.Simple
import GHC.Int ()
import Opaleye

-- USER

selectAllUsers :: Connection -> IO [AuthUser]
selectAllUsers conn = runSelect conn $ selectTable authUserTable

selectByEmail :: Connection -> String -> IO [AuthUser]
selectByEmail conn email = runSelect conn $ proc () -> do
  u <- selectTable authUserTable -< ()
  restrict -< (authUserEmail u .== toFields email)
  returnA -< u

insertUser :: Connection -> AuthUser -> IO Int64
insertUser conn user = do
  n <-
    runInsert_
      conn
      Insert
        { iTable = authUserTable,
          iRows =
            [ AuthUser
                Nothing
                (sqlString $ toS $ authUserPassword user)
                (authUserLastLogin user >>= Just . Opaleye.toNullable . sqlLocalTime)
                (sqlBool $ authUserIsSuperuser user)
                (sqlString $ toS $ authUserUsername user)
                (sqlString $ toS $ authUserFirstName user)
                (sqlString $ toS $ authUserLastName user)
                (sqlString $ toS $ authUserEmail user)
                (sqlBool $ authUserIsStaff user)
                (sqlBool $ authUserIsActive user)
                (sqlLocalTime $ authUserDateJoined user)
            ],
          iReturning = rCount,
          iOnConflict = Nothing
        }
  print n
  pure n

updateUser :: Connection -> AuthUser -> IO Int64
updateUser conn user =
  runUpdate_
    conn
    Update
      { uTable = authUserTable,
        uUpdateWith = \_ ->
          AuthUser
            (Just $ sqlInt4 $ fromIntegral $ authUserId user)
            (sqlString $ toS $ authUserPassword user)
            (authUserLastLogin user >>= Just . Opaleye.toNullable . sqlLocalTime)
            (sqlBool $ authUserIsSuperuser user)
            (sqlString $ toS $ authUserUsername user)
            (sqlString $ toS $ authUserFirstName user)
            (sqlString $ toS $ authUserLastName user)
            (sqlString $ toS $ authUserEmail user)
            (sqlBool $ authUserIsStaff user)
            (sqlBool $ authUserIsActive user)
            (sqlLocalTime $ authUserDateJoined user),
        uWhere = \a -> authUserId a .== sqlInt4 (fromIntegral $ authUserId user),
        uReturning = rCount
      }

deleteUser :: Connection -> Int -> IO Int64
deleteUser conn uid =
  runDelete_
    conn
    Delete
      { dTable = authUserTable,
        dWhere = \user -> authUserId user .== sqlInt4 (fromIntegral uid),
        dReturning = rCount
      }

main :: IO ()
main = do
  putStrLn "Starting"

  conn <-
    connect
      ConnectInfo
        { connectHost = "localhost",
          connectPort = 5432,
          connectDatabase = "tennis",
          connectUser = "murali",
          connectPassword = ""
        }

  us <- selectAllUsers conn

  eus <- selectByEmail conn "rajpal.sachin@gmail.com"
  let meus = fmap (\eu -> eu {authUserFirstName = "Sachin"}) eus
  nu <- sequence $ updateUser conn <$> meus
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  nd <- deleteUser conn 269
  ni <- insertUser conn $ AuthUser 0 "" Nothing True "un" "fn" " ln" "em@a.il" False True (utcToLocalTime tz now)

  print (ni, nu, nd, us)

  mapM_ print eus
