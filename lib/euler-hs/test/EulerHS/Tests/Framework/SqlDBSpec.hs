{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module EulerHS.Tests.Framework.SqlDBSpec where

import           EulerHS.Prelude   hiding (getOption)
import           Test.Hspec        hiding (runIO)
import           Data.Aeson               (encode)
import qualified Data.ByteString.Lazy as BSL
import           Unsafe.Coerce

import           EulerHS.Types
import           EulerHS.Interpreters
import           EulerHS.Language
import           EulerHS.Runtime (withFlowRuntime)

import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as T
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import Database.Beam ((==.), (&&.), (<-.), (/=.))

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


testDBName :: T.DBName
testDBName = "test.db"

connectOrFail :: T.DBConfig -> Flow T.SqlConn
connectOrFail cfg = L.connect cfg >>= \case
    Left e     -> error $ show e -- L.throwException $ toException $ show e
    Right conn -> pure conn

deleteTestValues :: L.Flow ()
deleteTestValues = do
  conn <- connectOrFail $ T.SQLiteConfig testDBName
  void $ L.runDB conn
      $ L.runDelete $ B.delete (_users eulerDb) (\u -> _userId u /=. B.val_ 0)
  void $ L.runDB conn
    $ L.runUpdate
    $ B.update (_sqlite_sequence sqliteSequenceDb)
          (\(SqliteSequence {..}) -> mconcat [_seq <-. B.val_ 0])
          (\(SqliteSequence {..}) -> _name ==. B.val_ "users")

insertTestValues :: L.Flow ()
insertTestValues = do
  conn <- connectOrFail $ T.SQLiteConfig testDBName
  void $ L.runDB conn
    $ L.runInsert
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
  connection <- connectOrFail $ T.SQLiteConfig testDBName

  L.runDB connection
    $ L.runInsert
    $ B.insert (_users eulerDb)
    $ B.insertValues [User 1 "Eve" "Beon"]

  L.runDB connection
    $ L.runInsert
    $ B.insert (_users eulerDb)
    $ B.insertValues [User 1 "Eve" "Beon"]


selectUnknownDbScript :: L.Flow (T.DBResult (Maybe User))
selectUnknownDbScript = do
  connection <- connectOrFail $ T.SQLiteConfig testDBName

  L.runDB connection $ do
    let predicate User {..} = _userFirstName ==. B.val_ "Unknown"

    L.runSelectOne $ B.select $
        B.limit_ 1 $ B.filter_ predicate $ B.all_ (_users eulerDb)

insertAndSelectDbScript :: L.Flow (T.DBResult (Maybe User))
insertAndSelectDbScript = do
  connection <- connectOrFail $ T.SQLiteConfig testDBName

  L.runDB connection $ do
    let predicate User {..} = _userFirstName ==. B.val_ "John"

    L.runSelectOne $ B.select $
        B.limit_ 1 $ B.filter_ predicate $ B.all_ (_users eulerDb)


updateAndSelectDbScript :: L.Flow (T.DBResult (Maybe User))
updateAndSelectDbScript = do
  connection <- connectOrFail $ T.SQLiteConfig testDBName

  L.runDB connection $ do
    let predicate1 User {..} = _userFirstName ==. B.val_ "John"

    L.runUpdate $ B.update (_users eulerDb)
      (\User {..} -> mconcat
        [ _userFirstName <-. B.val_ "Leo"
        , _userLastName  <-. B.val_ "San"
        ]
      )
      predicate1

    let predicate2 User {..} = _userFirstName ==. B.val_ "Leo"
    L.runSelectOne $ B.select $
      B.limit_ 1 $ B.filter_ predicate2 $ B.all_ (_users eulerDb)


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


withEmptyDB :: (R.FlowRuntime -> IO ()) -> IO ()
withEmptyDB act = withFlowRuntime Nothing (\rt -> do
  try (runFlow rt $ deleteTestValues >> insertTestValues) >>= \case
    Left (e :: SomeException) -> error $ "Deleting rows from DB failed. " <> show e
    Right _ -> do
      act rt
      runFlow rt deleteTestValues
    )

someUser :: Text -> Text -> T.DBResult (Maybe User) -> Bool
someUser f l (Right (Just u)) = _userFirstName u == f && _userLastName u == l
someUser _ _ _ = False

spec :: Spec
spec = do
  around withEmptyDB $ do

    describe "EulerHS SQL DB tests" $ do

      it "Unique Constraint Violation" $ \rt -> do
        eRes <- runFlow rt uniqueConstraintViolationDbScript
        eRes `shouldBe` (Left (DBError SomeError "SQLite3 returned ErrorConstraint while attempting to perform step: UNIQUE constraint failed: users.id"))

      it "Insert / Select, row not found" $ \rt -> do
        eRes <- runFlow rt selectUnknownDbScript
        eRes `shouldBe` (Right Nothing)

      it "Insert / Select, row found" $ \rt -> do
        eRes <- runFlow rt insertAndSelectDbScript
        eRes `shouldSatisfy` (someUser "John" "Doe")

      it "Insert / Update / Select, row found & changed" $ \rt -> do
        eRes <- runFlow rt updateAndSelectDbScript
        eRes `shouldSatisfy` (someUser "Leo" "San")