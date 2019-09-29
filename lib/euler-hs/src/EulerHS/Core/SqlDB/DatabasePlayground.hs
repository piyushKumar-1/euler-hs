{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module EulerHS.Core.SqlDB.DatabasePlayground where

import EulerHS.Prelude
import qualified EulerHS.Core.Language as L
import qualified EulerHS.Language as L
import qualified EulerHS.Core.Types as T
import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Runtime as R
import qualified EulerHS.Interpreters as I
import qualified Database.Beam as B
import Database.Beam ((==.), (&&.))
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

primaryKey :: UserT f -> B.PrimaryKey UserT f
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

testDb :: IO (T.DBResult (Maybe (UserT Identity)))
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
            L.create (_users eulerDb)
                [User B.default_
                    ( B.val_ "John" )
                    ( B.val_ "Doe"  )

                ,User B.default_
                    ( B.val_ "Doe"  )
                    ( B.val_ "John" )
                ]

            L.findOne (_users eulerDb) $
                B.filter_ (\(User id _ _) -> (B.val_ 1) ==. id )
