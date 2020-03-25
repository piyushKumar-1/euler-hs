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
testDBName = "lib/euler-hs/test/EulerHS/TestData/test.db"

testDBTemplateName :: String
testDBTemplateName = "lib/euler-hs/test/EulerHS/TestData/test.db.template"

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
  putStrLn $ encodePretty $ recording
  -- writeFile "recorded" $ show $ encode $ recording
  print recResult
  pure recResult

-- Write record to file or to stdout. Choose at 'runWithSQLConn'
writeRecord :: IO ()
writeRecord = withEmptyDB $ \rt -> do
  void $ runWithSQLConn $ do
    conn <- connectOrFail sqliteCfg
    L.runDB conn $ do
      L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
      res <- L.findRow $ B.select $ B.filter_ (\u -> _userGUID u B.==. 1) $ B.all_ (users userDB)
      L.deleteRows $ B.delete (users userDB) (\u -> _userGUID u B.==. 1)
      pure res

-- Record and play scenario. Return result.
recordAndPlay :: IO ()
recordAndPlay = withEmptyDB $ \rt -> do
  record <- runFlowWithArt $ do
    conn <- connectOrFail sqliteCfg
    L.runDB conn $ do
      L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
      res <- L.findRow $ B.select $ B.filter_ (\u -> _userGUID u B.==. 1) $ B.all_ (users userDB)
      L.deleteRows $ B.delete (users userDB) (\u -> _userGUID u B.==. 1)
      pure res
  putStrLn $ encodePretty record


-- Tests

spec :: Spec
spec =

    describe "ART SQL tests" $ do

      it "success to get one correct row" $ do
        result <- replayRecording getRowRecord $ do
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ do
            L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
            res <- L.findRow $ B.select $ B.filter_ (\u -> _userGUID u B.==. 1) $ B.all_ (users userDB)
            L.deleteRows $ B.delete (users userDB) (\u -> _userGUID u B.==. 1)
            pure res
        result `shouldBe` Right (Just (User {_userGUID = 1, _firstName = "Bill", _lastName = "Gates"}))

      it "fail to get one wrong row" $ do
        result <- replayRecording getWrongRowRecord $ do
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ do
            L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
            res <- L.findRow $ B.select $ B.filter_ (\u -> _userGUID u B.==. 2) $ B.all_ (users userDB)
            L.deleteRows $ B.delete (users userDB) (\u -> _userGUID u B.==. 1)
            pure res
        result `shouldBe` Right Nothing

      it "success to get correct rows" $ do
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

      it "fail to get an uncorrect rows" $ do
        result <- replayRecording getWrongRowsRecord $ do
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ do
            L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates", User 2 "Stive" "Jobs"]
            res <- L.findRows $ B.select $ B.filter_ (\u -> _userGUID u `B.in_` [3,4]) $ B.all_ (users userDB)
            L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [1,2])
            pure res
        result `shouldBe` Right []

      it "success to delete existing rows" $ do
        result <- replayRecording deleteRowsRecord $ do
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ do
            L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates", User 2 "Stive" "Jobs"]
            L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [1,2])
            res <- L.findRows $ B.select $ B.filter_ (\u -> _userGUID u `B.in_` [1,2]) $ B.all_ (users userDB)
            pure res
        result `shouldBe` Right []

      it "fail to delete wrong rows" $ do
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

      it "success to update rows" $ do
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

      it "success to update rows with IO action in between" $ do
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
getRowRecord = fromJust $ decode "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[\"ConnectionDoesNotExist\",\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Bill\"}}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

getWrongRowRecord :: ResultRecording
getWrongRowRecord = fromJust $ decode "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[\"ConnectionDoesNotExist\",\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":null}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

getRowsRecord :: ResultRecording
getRowsRecord = fromJust $ decode "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[\"ConnectionDoesNotExist\",\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":[{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Bill\"},{\"_userGUID\":2,\"_lastName\":\"Jobs\",\"_firstName\":\"Stive\"}]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

getWrongRowsRecord :: ResultRecording
getWrongRowsRecord = fromJust $ decode "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[\"ConnectionDoesNotExist\",\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

deleteRowsRecord :: ResultRecording
deleteRowsRecord = fromJust $ decode "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[\"ConnectionDoesNotExist\",\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

deleteWrongRowsRecord :: ResultRecording
deleteWrongRowsRecord = fromJust $ decode "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[\"ConnectionDoesNotExist\",\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":[{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Bill\"},{\"_userGUID\":2,\"_lastName\":\"Jobs\",\"_firstName\":\"Stive\"}]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

updateRowRecord :: ResultRecording
updateRowRecord = fromJust $ decode "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[\"ConnectionDoesNotExist\",\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":[{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Robert\"},{\"_userGUID\":2,\"_lastName\":\"Jobs\",\"_firstName\":\"Stive\"}]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

updateRowWithDelayRecord :: ResultRecording
updateRowWithDelayRecord = fromJust $ decode "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[\"ConnectionDoesNotExist\",\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"lib/euler-hs/test/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"description\":\"\", \"jsonResult\":[]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":4,\"_entryPayload\":{\"jsonResult\":{\"Right\":[{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Robert\"},{\"_userGUID\":2,\"_lastName\":\"Jobs\",\"_firstName\":\"Stive\"}]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"
