{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module SQLDB.Tests.SQLiteDBSpec where

import           Data.Aeson                (encode)
import qualified Data.ByteString.Lazy      as BSL
import           EulerHS.Prelude           hiding (getOption)
import           Test.Hspec                hiding (runIO)
import           Unsafe.Coerce

import           EulerHS.Interpreters
import           EulerHS.Language
import           EulerHS.Runtime           (withFlowRuntime)
import           EulerHS.Types             hiding (error)

import           Database.Beam             ((&&.), (/=.), (<-.), (==.))
import qualified Database.Beam             as B
import qualified Database.Beam.Backend.SQL as B
import qualified Database.Beam.Sqlite      as BS
import qualified EulerHS.Language          as L
import qualified EulerHS.Runtime           as R
import qualified EulerHS.Types             as T

-- sqlite3 db
-- CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL);

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
deriving instance ToJSON User
deriving instance FromJSON User

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

sqliteCfg = T.mkSQLiteConfig "eulerSQliteDB" testDBName

connectOrFail :: T.DBConfig beM -> Flow (T.SqlConn beM)
connectOrFail cfg = L.initSqlDBConnection cfg >>= \case
    Left e     -> error $ show e -- L.throwException $ toException $ show e
    Right conn -> pure conn

deleteTestValues :: L.Flow ()
deleteTestValues = do
  conn <- connectOrFail sqliteCfg -- $ T.mkSQLiteConfig testDBName
  void $ L.runDB conn
    $ L.deleteRows
    $ B.delete (_users eulerDb) (\u -> _userId u /=. B.val_ 0)
  void $ L.runDB conn
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
  conn <- connectOrFail sqliteCfg
  void $ L.runDB conn
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
  L.deinitSqlDBConnection conn

uniqueConstraintViolationDbScript :: L.Flow (T.DBResult ())
uniqueConstraintViolationDbScript = do
  connection <- connectOrFail sqliteCfg

  L.runDB connection
    $ L.insertRows
    $ B.insert (_users eulerDb)
    $ B.insertValues [User 1 "Eve" "Beon"]

  L.runDB connection
    $ L.insertRows
    $ B.insert (_users eulerDb)
    $ B.insertValues [User 1 "Eve" "Beon"]


selectUnknownDbScript :: L.Flow (T.DBResult (Maybe User))
selectUnknownDbScript = do
  connection <- connectOrFail sqliteCfg

  L.runDB connection $ do
    let predicate User {..} = _userFirstName ==. B.val_ "Unknown"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (_users eulerDb)

selectOneDbScript :: L.Flow (T.DBResult (Maybe User))
selectOneDbScript = do
  connection <- connectOrFail sqliteCfg

  L.runDB connection $ do
    let predicate User {..} = _userFirstName ==. B.val_ "John"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (_users eulerDb)


updateAndSelectDbScript :: L.Flow (T.DBResult (Maybe User))
updateAndSelectDbScript = do
  connection <- connectOrFail sqliteCfg

  L.runDB connection $ do
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


withEmptyDB :: (R.FlowRuntime -> IO ()) -> IO ()
withEmptyDB act = withFlowRuntime Nothing (\rt -> do
  try (runFlow rt prepareTestDB) >>= \case
    Left (e :: SomeException) ->
      runFlow rt rmTestDB
      `finally` error ("Preparing test values failed: " <> show e)
    Right _ -> act rt `finally` runFlow rt rmTestDB
    )

someUser :: Text -> Text -> T.DBResult (Maybe User) -> Bool
someUser f l (Right (Just u)) = _userFirstName u == f && _userLastName u == l
someUser _ _ _                = False


spec :: Spec
spec =
  around withEmptyDB $

    describe "EulerHS SQLite DB tests" $ do
      it "Double connection initialization should fail" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn1 <- L.initSqlDBConnection sqliteCfg
          eConn2 <- L.initSqlDBConnection sqliteCfg
          case (eConn1, eConn2) of
            (Left err, _) -> pure $ Left $ "Failed to connect 1st time: " <> show err
            (_, Left (T.DBError T.ConnectionAlreadyExists msg))
              | msg == "Connection for eulerSQliteDB already created." -> pure $ Right ()
            (_, Left err) -> pure $ Left $ "Unexpected error type on 2nd connect: " <> show err
        eRes `shouldBe` Right ()

      it "Get uninialized connection should fail" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn <- L.getSqlDBConnection sqliteCfg
          case eConn of
            Left (T.DBError T.ConnectionDoesNotExist msg)
              | msg == "Connection for eulerSQliteDB does not exists." -> pure $ Right ()
            Left err -> pure $ Left $ "Unexpected error: " <> show err
            Right _ -> pure $ Left "Unexpected connection success"
        eRes `shouldBe` Right ()

      it "Init and get connection should succeed" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn1 <- L.initSqlDBConnection sqliteCfg
          eConn2 <- L.getSqlDBConnection sqliteCfg
          case (eConn1, eConn2) of
            (Left err, _) -> pure $ Left $ "Failed to connect: " <> show err
            (_, Left err) -> pure $ Left $ "Unexpected error on get connection: " <> show err
            _             -> pure $ Right ()
        eRes `shouldBe` Right ()

      it "Init and double get connection should succeed" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn1 <- L.initSqlDBConnection sqliteCfg
          eConn2 <- L.getSqlDBConnection sqliteCfg
          eConn3 <- L.getSqlDBConnection sqliteCfg
          case (eConn1, eConn2, eConn3) of
            (Left err, _, _) -> pure $ Left $ "Failed to connect: " <> show err
            (_, Left err, _) -> pure $ Left $ "Unexpected error on 1st get connection: " <> show err
            (_, _, Left err) -> pure $ Left $ "Unexpected error on 2nd get connection: " <> show err
            _                -> pure $ Right ()
        eRes `shouldBe` Right ()

      it "getOrInitSqlConn should succeed" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn <- L.getOrInitSqlConn sqliteCfg
          case eConn of
            Left err -> pure $ Left $ "Failed to connect: " <> show err
            _        -> pure $ Right ()
        eRes `shouldBe` Right ()

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
