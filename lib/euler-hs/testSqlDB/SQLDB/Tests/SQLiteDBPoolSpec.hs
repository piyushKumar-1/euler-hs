{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module SQLDB.Tests.SQLiteDBPoolSpec where

import           EulerHS.Prelude   hiding (getOption)
import           Test.Hspec        hiding (runIO)

import qualified Test.Hspec as HSPEC
import           Data.Aeson               (encode)
import qualified Data.ByteString.Lazy as BSL
import           Unsafe.Coerce

import           EulerHS.Types hiding (error)
import           EulerHS.Interpreters
import           EulerHS.Language


import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as T
import qualified Database.Beam as B
import qualified Database.Beam.Sqlite as BS
import qualified Database.Beam.Backend.SQL as B
import Database.Beam ((==.), (&&.), (<-.), (/=.))
import qualified Data.Map as Map

import qualified SQLDB.Testing.Runtime as TR
-- sqlite3 db
-- CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL);
{-
data UserT f = User
    { _userId        :: B.C f Int
    , _userFirstName :: B.C f Text
    , _userLastName  :: B.C f Text
    } deriving (Generic, B.Beamable)

instance B.Table UserT where
  data PrimaryKey UserT f =
    UserId (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = UserId . _userId

type User = UserT Identity
type UserId = B.PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

data EulerDb f = EulerDb
    { _users :: f (B.TableEntity UserT)
    } deriving (Generic, B.Database be)

eulerDb :: B.DatabaseSettings be EulerDb
eulerDb = B.defaultDbSettings



data SqliteSequenceT f = SqliteSequence
    { _name :: B.C f Text
    , _seq  :: B.C f Int
    } deriving (Generic, B.Beamable)

instance B.Table SqliteSequenceT where
  data PrimaryKey SqliteSequenceT f =
    SqliteSequenceId (B.C f Text) deriving (Generic, B.Beamable)
  primaryKey = SqliteSequenceId . _name

type SqliteSequence = SqliteSequenceT Identity
type SqliteSequenceId = B.PrimaryKey SqliteSequenceT Identity


data SqliteSequenceDb f = SqliteSequenceDb
    { _sqlite_sequence :: f (B.TableEntity SqliteSequenceT)
    } deriving (Generic, B.Database be)

sqliteSequenceDb :: B.DatabaseSettings be SqliteSequenceDb
sqliteSequenceDb = B.defaultDbSettings


testDBName :: String
testDBName = "./test/EulerHS/TestData/test.db"

testDBTemplateName :: String
testDBTemplateName = "./test/EulerHS/TestData/test.db.template"

sqliteCfg = T.mkSQLiteConfig testDBName

poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 1
  }


sqliteDB = mkSQLiteDBName "sqlite"

connectOrFail :: T.DBConfig beM -> Flow (T.SqlConn beM)
connectOrFail cfg = L.initSqlDBConnection cfg >>= \case
    Left e     -> error $ show e -- L.throwException $ toException $ show e
    Right conn -> pure conn

deleteTestValues :: L.Flow ()
deleteTestValues = do
 -- conn <- connectOrFail $ T.mkSQLiteConfig testDBName
  void $ L.runDB sqliteDB
    $ L.deleteRows
    $ B.delete (_users eulerDb) (\u -> _userId u /=. B.val_ 0)
  void $ L.runDB sqliteDB
    $ L.updateRows
    $ B.update (_sqlite_sequence sqliteSequenceDb)
          (\(SqliteSequence {..}) -> mconcat [_seq <-. B.val_ 0])
          (\(SqliteSequence {..}) -> _name ==. B.val_ "users")

rmTestDB :: L.Flow ()
rmTestDB = void $ L.runSysCmd $ "rm -f " <> testDBName

prepareTestDB :: L.Flow ()
prepareTestDB = do
  rmTestDB
  void $ L.runSysCmd $ "cp " <> testDBTemplateName <> " " <> testDBName
  insertTestValues

insertTestValues :: L.Flow ()
insertTestValues = do
 -- conn <- connectOrFail $ T.mkSQLiteConfig testDBName
  void $ L.runDB sqliteDB
    $ L.insertRows
    $ B.insert (_users eulerDb)
    $ B.insertExpressions
          [ User B.default_
              ( B.val_ "John" )
              ( B.val_ "Doe"  )
          , User B.default_
              ( B.val_ "Doe"  )
              ( B.val_ "John" )
          ]

uniqueConstraintViolationDbScript :: L.Flow (T.DBResult ())
uniqueConstraintViolationDbScript = do
 -- connection <- connectOrFail $ T.mkSQLiteConfig testDBName

  L.runDB sqliteDB
    $ L.insertRows
    $ B.insert (_users eulerDb)
    $ B.insertValues [User 1 "Eve" "Beon"]

  L.runDB sqliteDB
    $ L.insertRows
    $ B.insert (_users eulerDb)
    $ B.insertValues [User 1 "Eve" "Beon"]


selectUnknownDbScript :: L.Flow (T.DBResult (Maybe User))
selectUnknownDbScript = do
 -- connection <- connectOrFail $ T.mkSQLiteConfig testDBName

  L.runDB sqliteDB $ do
    let predicate User {..} = _userFirstName ==. B.val_ "Unknown"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (_users eulerDb)

selectOneDbScript :: L.Flow (T.DBResult (Maybe User))
selectOneDbScript = do
 -- connection <- connectOrFail $ T.mkSQLiteConfig testDBName

  L.runDB sqliteDB $ do
    let predicate User {..} = _userFirstName ==. B.val_ "John"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (_users eulerDb)


updateAndSelectDbScript :: L.Flow (T.DBResult (Maybe User))
updateAndSelectDbScript = do
 -- connection <- connectOrFail $ T.mkSQLiteConfig testDBName

  L.runDB sqliteDB $ do
    let predicate1 User {..} = _userFirstName ==. B.val_ "John"

    L.updateRows $ B.update (_users eulerDb)
      (\User {..} -> mconcat
        [ _userFirstName <-. B.val_ "Leo"
        , _userLastName  <-. B.val_ "San"
        ]
      )
      predicate1

    let predicate2 User {..} = _userFirstName ==. B.val_ "Leo"
    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate2
      $ B.all_ (_users eulerDb)


-- innerJoinDbScript :: L.Flow ()
-- innerJoinDbScript = do
--   connection <- connectOrFail $ T.SQLiteConfig testDBName
--
--   L.runDB connection $ do
--     insertTestValues
--
--       -- INNER JOIN
--     L.runSelect $ B.select $ do
--       user1 <- B.all_ (_users eulerDb)
--       user2 <- B.all_ (_users eulerDb)
--       pure (user1, user2)


withEmptyDB :: Map Text T.SqlConnPool -> (R.FlowRuntime -> IO ()) -> IO ()
withEmptyDB p act = TR.withFlowRuntime p Nothing (\rt -> do
  try (runFlow rt prepareTestDB) >>= \case
    Left (e :: SomeException) ->
      runFlow rt rmTestDB
      `finally` error ("Preparing test values failed: " <> show e)
    Right _ -> act rt `finally` runFlow rt rmTestDB
    )

someUser :: Text -> Text -> T.DBResult (Maybe User) -> Bool
someUser f l (Right (Just u)) = _userFirstName u == f && _userLastName u == l
someUser _ _ _ = False


spec :: Spec
spec = do
  sqlitePool <- HSPEC.runIO $ do
    p <- mkNativeConnPool poolConfig sqliteCfg
    pure $ Map.singleton "sqlite" p
  around (withEmptyDB sqlitePool) $

    describe "EulerHS SQLite DB tests" $ do
      it "Unique Constraint Violation" $ \rt -> do
        eRes <- runFlow rt uniqueConstraintViolationDbScript
        eRes `shouldBe` (Left (DBError SomeError "SQLite3 returned ErrorConstraint while attempting to perform step: UNIQUE constraint failed: users.id"))

      it "Select one, row not found" $ \rt -> do
        eRes <- runFlow rt selectUnknownDbScript
        eRes `shouldBe` (Right Nothing)

      it "Select one, row found" $ \rt -> do
        eRes <- runFlow rt selectOneDbScript
        eRes `shouldSatisfy` (someUser "John" "Doe")

      it "Update / Select, row found & changed" $ \rt -> do
        eRes <- runFlow rt updateAndSelectDbScript
        eRes `shouldSatisfy` (someUser "Leo" "San")

        -}