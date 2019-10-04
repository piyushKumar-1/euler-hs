{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module EulerHS.Core.SqlDB.DatabasePlayground where

import EulerHS.Prelude
import qualified EulerHS.Core.Language as L
import qualified EulerHS.Language as L
import qualified EulerHS.Core.Types as T
import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Runtime as R
import qualified EulerHS.Interpreters as I
import qualified Database.Beam as B
import Database.Beam ((==.), (&&.), (<-.))
--import qualified EulerHS.Core.SqlDB.Impl.SQLite as SQLite
--import qualified Database.SQLite.Simple as SQLite
--import qualified Database.Beam.Backend.SQL as B
--import qualified Database.Beam.Schema.Tables as B
--import qualified Database.Beam.Sqlite as BS
--import qualified Database.Beam.Postgres as BP

-- sqlite3 db
-- CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL);

data UserT f
  = User
    {_userId        :: B.C f Int
    ,_userFirstName :: B.C f Text
    ,_userLastName  :: B.C f Text
    } deriving (Generic, B.Beamable)

instance B.Table UserT where
  data PrimaryKey UserT f =
    UserId (B.C f Int) deriving (Generic, B.Beamable)

  primaryKey = UserId . _userId

type User = UserT Identity
type UserId = B.PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

data EulerDb f
  = EulerDb
    {_users :: f (B.TableEntity UserT)
    } deriving (Generic, B.Database be)

eulerDb :: B.DatabaseSettings be EulerDb
eulerDb = B.defaultDbSettings

testDb :: IO (T.DBResult ())
testDb = do
    loggerRuntime <- R.createLoggerRuntime T.defaultLoggerConfig
    coreRuntime   <- R.createCoreRuntime   loggerRuntime
    flowRuntime   <- R.createFlowRuntime   coreRuntime
    I.runFlow flowRuntime $ do
        connection' <- L.connect $ T.SQLiteConfig "db"

        let
            connection = case connection' of
                Left e     -> error $ show e
                Right conn -> conn

        L.runDB connection $ do
            -- Insert users to database
            L.runInsert $ B.insert (_users eulerDb) $
                B.insertExpressions
                    [User B.default_
                        ( B.val_ "John" )
                        ( B.val_ "Doe"  )

                    ,User B.default_
                        ( B.val_ "Doe"  )
                        ( B.val_ "John" )
                    ]

            -- Will cause UNIQUE constraint violation

            -- L.runInsert $ B.insert (_users eulerDb) $
            --     B.insertValues [User 1 "Eve" "Beon"]

            -- Select user John with Id = 1
            let
                predicate User {..} =
                    _userId        ==. B.val_      1 &&.
                    _userFirstName ==. B.val_ "John"

            _ <- L.runSelectOne $ B.select $
                B.limit_ 1 $ B.filter_ predicate $ B.all_ (_users eulerDb)

            -- Update user With Id = 1

            L.runUpdate $ B.update (_users eulerDb)
                (\User {..} -> mconcat
                    [ _userFirstName <-. B.val_ "Leo"
                    , _userLastName  <-. B.val_ "San"
                    ]
                )
                predicate

            -- Two Requests

            muser <- L.runSelectOne $ B.lookup_ (_users eulerDb) $ UserId 2
            case muser of
                Just user -> L.runUpdate $ B.save (_users eulerDb) $
                    user { _userLastName = "Mik" }
                Nothing -> error "Omg"

            -- INNER JOIN

            _ <- L.runSelect $ B.select $ do
                user1 <- B.all_ (_users eulerDb)
                user2 <- B.all_ (_users eulerDb)
                pure (user1, user2)

            pure ()




