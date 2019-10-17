{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module SQLDB.Tests.MySQLDBPoolSpec where

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
import qualified Database.Beam.Backend.SQL as B
import Database.Beam ((==.), (&&.), (<-.), (/=.))

import qualified Database.Beam.MySQL             as BM
import qualified Data.Map as Map

import qualified SQLDB.Testing.Runtime as TR
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


mySQLCfg :: MySQLConfig
mySQLCfg = MySQLConfig
  { connectHost     = "localhost"
  , connectPort     = 3306
  , connectUser     = "test"
  , connectPassword = "test"
  , connectDatabase = "test"
  , connectOptions  = [CharsetName "utf8"]
  , connectPath     = ""
  , connectSSL      = Nothing
  }

poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }

mysqlCfg = mkMySQLConfig mySQLCfg

mysqlDB = mkMySQLDBName "mysql"

connMySQLorFail :: T.DBConfig beM -> Flow (T.SqlConn beM)
connMySQLorFail cfg = L.initSqlDBConnection cfg >>= \case
  Left e     -> error $ show e -- L.throwException $ toException $ show e
  Right conn -> pure conn


uniqueConstraintViolationDbScript :: L.Flow (T.DBResult ())
uniqueConstraintViolationDbScript = do
 -- connection <- connMySQLorFail $ mysqlPoolCfg

  L.runDB mysqlDB
    $ L.insertRows
    $ B.insert (_users eulerDb)
    $ B.insertValues [User 1 "Eve" "Beon"]

  L.runDB mysqlDB
    $ L.insertRows
    $ B.insert (_users eulerDb)
    $ B.insertValues [User 1 "Eve" "Beon"]


selectUnknownDbScript :: L.Flow (T.DBResult (Maybe User))
selectUnknownDbScript = do
 -- connection <- connMySQLorFail $ mysqlPoolCfg

  L.runDB mysqlDB $ do
    let predicate User {..} = _userFirstName ==.  "Unknown"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (_users eulerDb)


data SimpleUser = SimpleUser {first :: Text, last :: Text}

susers = 
  [ SimpleUser  "John" "Doe"
  , SimpleUser  "Doe" "John"
  ]
mkUser SimpleUser {..} = User B.default_ (B.val_ first) (B.val_ last)

myinsert te expr = L.insertRows
  $ B.insert te
  $ B.insertExpressions (mkUser <$> expr)

selectOneDbScript :: L.Flow (T.DBResult (Maybe User))
selectOneDbScript = do
 -- connection <- connMySQLorFail $ mysqlPoolCfg
  let tableEntity = _users eulerDb
  L.runDB mysqlDB $ myinsert tableEntity susers
     -- [ User B.default_
     --        (B.val_ "John")
     --        (B.val_ "Doe")
     -- , User B.default_
     --        (B.val_ "Doe")
     --        (B.val_ "John")
     -- ]--(mkUser <$> susers)
   -- $ L.insertRows
   -- $ B.insert (te)
   -- $ B.insertExpressions (mkUser <$> susers)
         -- [ User B.default_
         --        "John"
         --        "Doe"
         -- , User B.default_
         --        "Doe"
         --        "John"
         -- ]
  L.runDB mysqlDB $ do
    let predicate User {..} = _userFirstName ==.  "John"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (te)


updateAndSelectDbScript :: L.Flow (T.DBResult (Maybe User))
updateAndSelectDbScript = do
 -- connection <- connMySQLorFail $ mysqlPoolCfg

  L.runDB mysqlDB $ do
    let predicate1 User {..} = _userFirstName ==. "John"

    L.updateRows $ B.update (_users eulerDb)
      (\User {..} -> mconcat
        [ _userFirstName <-. "Leo"
        , _userLastName  <-. "San"
        ]
      )
      predicate1

    let predicate2 User {..} = _userFirstName ==. "Leo"
    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate2
      $ B.all_ (_users eulerDb)


someUser :: Text -> Text -> T.DBResult (Maybe User) -> Bool
someUser f l (Right (Just u)) = _userFirstName u == f && _userLastName u == l
someUser _ _ _ = False


spec :: Spec
spec =  do
  mysqlPool <- HSPEC.runIO $ do
    p <- mkNativeConnPool poolConfig mysqlCfg
    pure $ Map.singleton "mysql" p
  around (TR.withFlowRuntime mysqlPool Nothing) $

    describe "EulerHS MySQL DB with Pool tests" $ do
      it "Unique Constraint Violation" $ \rt -> do
        eRes <- runFlow rt uniqueConstraintViolationDbScript
        eRes `shouldBe` (Left (DBError SomeError "ConnectionError {errFunction = \"query\", errNumber = 1062, errMessage = \"Duplicate entry '1' for key 'PRIMARY'\"}"))

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