{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}

module EulerHS.Tests.Framework.SQLArtSpec
  ( spec
  ) where

import           EulerHS.Prelude

import           Data.Aeson as A
import           Data.Aeson.Encode.Pretty
import qualified Data.Map as Map
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import qualified Database.Beam.Query as B
import           Database.Beam.Sqlite.Connection (Sqlite, SqliteM)
import           Test.Hspec

import           EulerHS.Interpreters as I
import           EulerHS.Language as L
import           EulerHS.Runtime
import           EulerHS.Tests.Framework.Common
import           EulerHS.Types as T



-- Prepare custom types for tests

data UserT f = User
    { _userGUID  :: B.C f Int
    , _firstName :: B.C f Text
    , _lastName  :: B.C f Text
    } deriving (Generic, B.Beamable)

instance B.Table UserT where
  data PrimaryKey UserT f =
    UserId (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = UserId . _userGUID

type User = UserT Identity

type UserId = B.PrimaryKey UserT Identity

deriving instance Show UserId
deriving instance Eq UserId
deriving instance ToJSON UserId
deriving instance FromJSON UserId

deriving instance Show User
deriving instance Eq User
deriving instance ToJSON User
deriving instance FromJSON User

userEMod :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity UserT)
userEMod = B.modifyTableFields
  B.tableModification
    { _userGUID = B.fieldNamed "id"
    , _firstName = B.fieldNamed "first_name"
    , _lastName = B.fieldNamed "last_name"
    }

data UserDB f = UserDB
    { users :: f (B.TableEntity UserT)
    } deriving (Generic, B.Database be)

userDB :: B.DatabaseSettings be UserDB
userDB = B.defaultDbSettings `B.withDbModification`
  B.dbModification
    { users = userEMod
    }

-- Prepare connection to database file

testDBName :: String
testDBName = "test/EulerHS/TestData/test.db"

testDBTemplateName :: String
testDBTemplateName = "test/EulerHS/TestData/test.db.template"

poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }

sqliteCfg :: DBConfig SqliteM
sqliteCfg = T.mkSQLitePoolConfig "SQliteDB" testDBName poolConfig

rmTestDB :: L.Flow ()
rmTestDB = void $ L.runSysCmd $ "rm -f " <> testDBName

prepareTestDB :: L.Flow ()
prepareTestDB = do
  rmTestDB
  -- L.runSysCmd "pwd" >>= L.runIO . print
  void $ L.runSysCmd $ "cp " <> testDBTemplateName <> " " <> testDBName
  pure ()

withEmptyDB :: (FlowRuntime -> IO ()) -> IO ()
withEmptyDB act = withFlowRuntime Nothing (\rt -> do
  try (runFlow rt prepareTestDB) >>= \case
    Left (e :: SomeException) ->
      runFlow rt rmTestDB
      `finally` error ("Preparing test values failed: " <> show e)
    Right _ -> act rt `finally` runFlow rt rmTestDB
    )

-- Prepare record log and test returns
connectOrFail :: T.DBConfig beM -> Flow (T.SqlConn beM)
connectOrFail cfg = L.getOrInitSqlConn cfg >>= \case
    Left e     -> error $ show e
    Right conn -> pure conn

runWithSQLConn :: (Show b, Eq b) => Flow b -> IO b
runWithSQLConn flow = do
  (recording, recResult) <- runFlowRecording ($) flow
  -- putStrLn $ encodePretty $ recording
  print $ encode recording
  -- writeFile "recorded" $ show $ encode $ recording
  -- print recResult
  pure recResult

-- Write record to file or to stdout. Choose at 'runWithSQLConn'
-- writeRecord :: IO ()
-- writeRecord = withEmptyDB $ \rt -> do
--   void $ runWithSQLConn $ do
--     conn <- connectOrFail sqliteCfg
--     L.runDB conn $ do
--       L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
--       res <- L.findRow $ B.select $ B.filter_ (\u -> _userGUID u B.==. 1) $ B.all_ (users userDB)
--       L.deleteRows $ B.delete (users userDB) (\u -> _userGUID u B.==. 1)
--       pure res

-- Record and play scenario. Return result.
-- recordAndPlay :: IO ()
-- recordAndPlay = withEmptyDB $ \rt -> do
--   record <- runFlowWithArt $ do
--     conn <- connectOrFail sqliteCfg
--     L.runDB conn $ do
--       L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
--       res <- L.findRow $ B.select $ B.filter_ (\u -> _userGUID u B.==. 1) $ B.all_ (users userDB)
--       L.deleteRows $ B.delete (users userDB) (\u -> _userGUID u B.==. 1)
--       pure res
--   putStrLn $ encodePretty record


run _ = runWithSQLConn

-- Tests

spec :: Spec
spec =
  around (withEmptyDB) $ do
    describe "ART SQL tests" $ do

      it "success to get one correct row" $ \rt -> do
        result <- replayRecording getRowRecord $ do
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ do
            L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
            res <- L.findRow $ B.select $ B.filter_ (\u -> _userGUID u B.==. 1) $ B.all_ (users userDB)
            L.deleteRows $ B.delete (users userDB) (\u -> _userGUID u B.==. 1)
            pure res
        result `shouldBe` Right (Just (User {_userGUID = 1, _firstName = "Bill", _lastName = "Gates"}))

      it "fail to get one wrong row" $ \rt -> do
        result <- replayRecording getWrongRowRecord $ do
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ do
            L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
            res <- L.findRow $ B.select $ B.filter_ (\u -> _userGUID u B.==. 2) $ B.all_ (users userDB)
            L.deleteRows $ B.delete (users userDB) (\u -> _userGUID u B.==. 1)
            pure res
        result `shouldBe` Right Nothing

      it "success to get correct rows" $ \rt -> do
        result <- replayRecording getRowsRecord $ do
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ do
            L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates", User 2 "Stive" "Jobs"]
            res <- L.findRows $ B.select $ B.filter_ (\u -> _userGUID u `B.in_` [1,2]) $ B.all_ (users userDB)
            L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [1,2])
            pure res
        result `shouldBe` Right
          [ User {_userGUID = 1, _firstName = "Bill", _lastName = "Gates"}
          , User {_userGUID = 2, _firstName = "Stive", _lastName = "Jobs"}
          ]

      it "fail to get an uncorrect rows" $ \rt -> do
        result <- replayRecording getWrongRowsRecord $ do
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ do
            L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates", User 2 "Stive" "Jobs"]
            res <- L.findRows $ B.select $ B.filter_ (\u -> _userGUID u `B.in_` [3,4]) $ B.all_ (users userDB)
            L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [1,2])
            pure res
        result `shouldBe` Right []

      it "success to delete existing rows" $ \rt -> do
        result <- replayRecording deleteRowsRecord $ do
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ do
            L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates", User 2 "Stive" "Jobs"]
            L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [1,2])
            res <- L.findRows $ B.select $ B.filter_ (\u -> _userGUID u `B.in_` [1,2]) $ B.all_ (users userDB)
            pure res
        result `shouldBe` Right []

      it "fail to delete wrong rows" $ \rt -> do
        result <- replayRecording deleteWrongRowsRecord $ do
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ do
            L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates", User 2 "Stive" "Jobs"]
            L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [3,4])
            res <- L.findRows $ B.select $ B.filter_ (\u -> _userGUID u `B.in_` [1,2]) $ B.all_ (users userDB)
            L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [1,2])
            pure res
        result `shouldBe` Right
          [ User {_userGUID = 1, _firstName = "Bill", _lastName = "Gates"}
          , User {_userGUID = 2, _firstName = "Stive", _lastName = "Jobs"}
          ]

      it "success to update rows" $ \rt -> do
        result <- replayRecording updateRowRecord $ do
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ do
            L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates", User 2 "Stive" "Jobs"]
            L.updateRows $ B.update (users userDB)
                (\user -> _firstName user B.<-. B.val_ "Robert")
                (\user -> _userGUID user B.==. 1)
            res <- L.findRows $ B.select $ B.filter_ (\u -> _userGUID u `B.in_` [1,2]) $ B.all_ (users userDB)
            L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [1,2])
            pure res
        result `shouldBe` Right
          [ User {_userGUID = 1, _firstName = "Robert", _lastName = "Gates"}
          , User {_userGUID = 2, _firstName = "Stive", _lastName = "Jobs"}
          ]

      it "success to update rows with IO action in between" $ \rt -> do
        result <- replayRecording updateRowWithDelayRecord $ do
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ do
            L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates", User 2 "Stive" "Jobs"]
            L.updateRows $ B.update (users userDB)
                (\user -> _firstName user B.<-. B.val_ "Robert")
                (\user -> _userGUID user B.==. 1)
          L.runIO $ threadDelay (2 * 10 ^ 6)
          L.runDB conn $ do
            res <- L.findRows $ B.select $ B.filter_ (\u -> _userGUID u `B.in_` [1,2]) $ B.all_ (users userDB)
            L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [1,2])
            pure res
        result `shouldBe` Right
          [ User {_userGUID = 1, _firstName = "Robert", _lastName = "Gates"}
          , User {_userGUID = 2, _firstName = "Stive", _lastName = "Jobs"}
          ]


-- Use testRecord to generate record log to 'recorder' file
getRowRecord :: ResultRecording
getRowRecord = fromJust $ decode
  "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\"]\",\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\")=(?);\\n-- With values: [SQLInteger 1]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\")=(?);\\n-- With values: [SQLInteger 1]\"],\"jsonResult\":{\"Right\":{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Bill\"}}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

getWrongRowRecord :: ResultRecording
getWrongRowRecord = fromJust $ decode
  "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\"]\",\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\")=(?);\\n-- With values: [SQLInteger 2]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\")=(?);\\n-- With values: [SQLInteger 1]\"],\"jsonResult\":{\"Right\":null}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

getRowsRecord :: ResultRecording
getRowsRecord = fromJust $ decode
  "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?), (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\",SQLInteger 2,SQLText \\\"Stive\\\",SQLText \\\"Jobs\\\"]\",\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\"],\"jsonResult\":{\"Right\":[{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Bill\"},{\"_userGUID\":2,\"_lastName\":\"Jobs\",\"_firstName\":\"Stive\"}]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

getWrongRowsRecord :: ResultRecording
getWrongRowsRecord = fromJust $ decode
  "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?), (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\",SQLInteger 2,SQLText \\\"Stive\\\",SQLText \\\"Jobs\\\"]\",\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 3,SQLInteger 4]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\"],\"jsonResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

deleteRowsRecord :: ResultRecording
deleteRowsRecord = fromJust $ decode
  "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?), (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\",SQLInteger 2,SQLText \\\"Stive\\\",SQLText \\\"Jobs\\\"]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\",\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\"],\"jsonResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

deleteWrongRowsRecord :: ResultRecording
deleteWrongRowsRecord = fromJust $ decode
  "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?), (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\",SQLInteger 2,SQLText \\\"Stive\\\",SQLText \\\"Jobs\\\"]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 3,SQLInteger 4]\",\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\"],\"jsonResult\":{\"Right\":[{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Bill\"},{\"_userGUID\":2,\"_lastName\":\"Jobs\",\"_firstName\":\"Stive\"}]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

updateRowRecord :: ResultRecording
updateRowRecord = fromJust $ decode
  "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?), (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\",SQLInteger 2,SQLText \\\"Stive\\\",SQLText \\\"Jobs\\\"]\",\"UPDATE \\\"users\\\" SET \\\"first_name\\\"=? WHERE (\\\"id\\\")=(?);\\n-- With values: [SQLText \\\"Robert\\\",SQLInteger 1]\",\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\"],\"jsonResult\":{\"Right\":[{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Robert\"},{\"_userGUID\":2,\"_lastName\":\"Jobs\",\"_firstName\":\"Stive\"}]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

updateRowWithDelayRecord :: ResultRecording
updateRowWithDelayRecord = fromJust $ decode
  "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?), (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\",SQLInteger 2,SQLText \\\"Stive\\\",SQLText \\\"Jobs\\\"]\",\"UPDATE \\\"users\\\" SET \\\"first_name\\\"=? WHERE (\\\"id\\\")=(?);\\n-- With values: [SQLText \\\"Robert\\\",SQLInteger 1]\"],\"jsonResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"jsonResult\":[],\"description\":\"\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":4,\"_entryPayload\":{\"rawSql\":[\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\"],\"jsonResult\":{\"Right\":[{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Robert\"},{\"_userGUID\":2,\"_lastName\":\"Jobs\",\"_firstName\":\"Stive\"}]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"


