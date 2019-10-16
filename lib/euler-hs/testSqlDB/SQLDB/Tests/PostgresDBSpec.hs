{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module SQLDB.Tests.PostgresDBSpec where

import           EulerHS.Prelude   hiding (getOption)
import           Test.Hspec        hiding (runIO)
import           Data.Aeson               (encode)
import qualified Data.ByteString.Lazy as BSL
import           Unsafe.Coerce

import           EulerHS.Types hiding (error)
import           EulerHS.Interpreters
import           EulerHS.Language
import           EulerHS.Runtime (withFlowRuntime)

import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as T
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import Database.Beam ((==.), (&&.), (<-.), (/=.))


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

pgConfig = mkPostgresConfig $ T.PostgresConfig
  { connectHost = "localhost" --String
  , connectPort = 5432 --Word16
  , connectUser = "postgres" -- String
  , connectPassword = "postgres" -- String
  , connectDatabase = "testdb" --  String
  }

conPGorFail :: T.DBConfig beM -> Flow (T.SqlConn beM)
conPGorFail cfg = L.initSqlDBConnection cfg >>= \case
  Left e     -> error $ show e -- L.throwException $ toException $ show e
  Right conn -> pure conn


uniqueConstraintViolationDbScript :: L.Flow (T.DBResult ())
uniqueConstraintViolationDbScript = do
  connection <- conPGorFail $ pgConfig

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
  connection <- conPGorFail $ pgConfig

  L.runDB connection $ do
    let predicate User {..} = _userFirstName ==. B.val_ "Unknown"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (_users eulerDb)

selectOneDbScript :: L.Flow (T.DBResult (Maybe User))
selectOneDbScript = do
  connection <- conPGorFail $ pgConfig
  L.runDB connection
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
  L.runDB connection $ do
    let predicate User {..} = _userFirstName ==. B.val_ "John"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (_users eulerDb)


updateAndSelectDbScript :: L.Flow (T.DBResult (Maybe User))
updateAndSelectDbScript = do
  connection <- conPGorFail $ pgConfig

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


someUser :: Text -> Text -> T.DBResult (Maybe User) -> Bool
someUser f l (Right (Just u)) = _userFirstName u == f && _userLastName u == l
someUser _ _ _ = False


spec :: Spec
spec =
  around (withFlowRuntime Nothing) $

    describe "EulerHS Postgres DB tests" $ do
      it "Unique Constraint Violation" $ \rt -> do
        eRes <- runFlow rt uniqueConstraintViolationDbScript
        eRes `shouldBe` (Left (DBError SomeError "SqlError {sqlState = \"23505\", sqlExecStatus = FatalError, sqlErrorMsg = \"duplicate key value violates unique constraint \\\"users_pk\\\"\", sqlErrorDetail = \"Key (id)=(1) already exists.\", sqlErrorHint = \"\"}"))

      it "Select one, row not found" $ \rt -> do
        eRes <- runFlow rt selectUnknownDbScript
        eRes `shouldBe` (Right Nothing)

      it "Select one, row found" $ \rt -> do
        eRes <- runFlow rt selectOneDbScript
        eRes `shouldSatisfy` (someUser "John" "Doe")

      it "Update / Select, row found & changed" $ \rt -> do
        eRes <- runFlow rt updateAndSelectDbScript
        eRes `shouldSatisfy` (someUser "Leo" "San")
