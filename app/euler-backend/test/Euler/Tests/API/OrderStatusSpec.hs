module Euler.Tests.API.OrderStatusSpec where

import           EulerHS.Prelude
import           Test.Hspec

import qualified Data.Text as T

import           EulerHS.Interpreters
import           EulerHS.Language
import           EulerHS.Runtime
import           EulerHS.Types hiding (error)

import           Euler.API.RouteParameters
import           Euler.Common.Types.Currency (Currency (..))
import           Euler.Common.Types.DefaultDate
import           Euler.Common.Types.External.Mandate (MandateFeature (..))
import           Euler.Common.Types.External.Order (OrderStatus (..))
import qualified Euler.Playback.MethodPlayer as PB
import qualified Euler.Product.Domain as D

import qualified Euler.API.Order as Api
import qualified Euler.Storage.DBConfig as DB
import qualified Euler.Storage.Repository as Rep
import           Euler.Storage.Types
import qualified Euler.Storage.Types.MerchantAccount as Merchant
import qualified Euler.Storage.Validators.MerchantAccount as Merchant
-- import qualified Euler.Product.OLTP.Order.Create        as OrderCreate
-- import qualified Euler.Product.OLTP.Order.CreateUpdateLegacy as OrderCreateUpdateLegacy
import qualified Euler.Product.OLTP.Order.OrderStatus as OrderStatus
import qualified Euler.Product.OLTP.Services.AuthenticationService as Auth

import           Data.Generics.Product.Fields
import           Data.Time.Clock (NominalDiffTime)
import qualified EulerHS.Extra.Validation as V
import qualified EulerHS.Types as T
import qualified WebService.Language as L
import qualified WebService.Types as T

import qualified Data.Aeson as A
import           Database.MySQL.Base
import qualified Euler.API.Validators.Order as VO
import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Common.Errors.Types as Errs
import           Euler.Lens
import qualified Euler.Options.Options as Opt
import           System.Process
import           Test.HUnit.Base



prepareDB :: (FlowRuntime -> IO ()) -> IO()
prepareDB next = withFlowRuntime (Just defaultLoggerConfig) $ \flowRt ->
  prepareTestDB $
    try (runFlow flowRt prepareDBConnections) >>= \case
      Left (e :: SomeException) -> putStrLn @String $ "Exception thrown: " <> show e
      Right _ -> next flowRt
  where
    prepareTestDB :: IO() -> IO ()
    prepareTestDB action =
      bracket (T.createMySQLConn mySQLRootCfg) close $ \rootConn -> do
        let
          dropTestDbIfExist :: IO ()
          dropTestDbIfExist = do
            query rootConn "drop database if exists euler_test_db"

          createTestDb :: IO ()
          createTestDb = do
            query rootConn "create database euler_test_db"
            query rootConn "grant all privileges on euler_test_db.* to 'cloud'@'%'"


        bracket_
          (dropTestDbIfExist >> createTestDb)
          (dropTestDbIfExist)
          (loadMySQLDump "test/Euler/TestData/orderCreateMySQL.sql" mySQLCfg >> action)


-- Redis config data
redisConn :: IsString a => a
redisConn = "redis"

redisConnConfig :: T.RedisConfig
redisConnConfig = T.RedisConfig
    { connectHost           = "redis"
    , connectPort           = 6379
    , connectAuth           = Nothing
    , connectDatabase       = 0
    , connectMaxConnections = 50
    , connectMaxIdleTime    = 30
    , connectTimeout        = Nothing
    }

prepareDBConnections :: Flow ()
prepareDBConnections = do
  let cfg = T.mkMySQLConfig "eulerMysqlDB" mySQLCfg

  ePool <- initSqlDBConnection cfg
  setOption Opt.EulerDbCfg cfg

  redis <- initKVDBConnection
    $ T.mkKVDBConfig redisConn $ redisConnConfig

  L.throwOnFailedWithLog ePool T.SqlDBConnectionFailedException "Failed to connect to SQLite DB."
  L.throwOnFailedWithLog redis T.KVDBConnectionFailedException "Failed to connect to Redis DB."


mwhen :: Monoid m => Bool -> m -> m
mwhen True a  = a
mwnen False _ = mempty


-- file path relative to app/euler-backend
loadMySQLDump :: String -> T.MySQLConfig -> IO ()
loadMySQLDump path T.MySQLConfig {..} = do
    let cmd = "mysql " <> options <> " " <> connectDatabase <> " 2> /dev/null < " <> path -- ../../init/mysqldump.sql"
    -- putStrLn cmd
    void $ system cmd
  where
    options =
      intercalate " "
        [                                      "--port="     <> show connectPort
        , mwhen (not $ null connectHost    ) $ "--host="     <> connectHost
        , mwhen (not $ null connectUser    ) $ "--user="     <> connectUser
        , mwhen (not $ null connectPassword) $ "--password=" <> connectPassword
        ]

mySQLCfg :: T.MySQLConfig
mySQLCfg = T.MySQLConfig
  { connectHost     = "127.0.0.1"
  , connectPort     = 3306
  , connectUser     = "cloud"
  , connectPassword = "scape"
  , connectDatabase = "jdb"
  , connectOptions  = [T.CharsetName "utf8"]
  , connectPath     = ""
  , connectSSL      = Nothing
  }

mySQLRootCfg :: T.MySQLConfig
mySQLRootCfg =
    T.MySQLConfig
      { connectUser     = "root"
      , connectPassword = "4" -- set your root pass here
      , connectDatabase = ""
      , ..
      }
  where
    T.MySQLConfig {..} = mySQLCfg





-- loop :: IO ()
-- loop = do threadDelay 1000; loop

-- shouldBeAnn ::
{-# INLINE shouldBeAnn #-}
shouldBeAnn s a b = assertEqual s b a

-- shouldStartWithAnn :: _
-- shouldStartWithAnn s v p
--   | plen > length v = assertFailure s
--   | otherwise = assertBool s (take plen v == p)
--   where
--     plen = length p

-- createMerchantAccount :: Flow DM.MerchantAccount
-- createMerchantAccount = case Merchant.transSMaccToDomMacc Merchant.defaultMerchantAccount of
--     V.Failure e -> do
--       logErrorT "DB MerchantAccount Validation" $ show e
--       error "MerchantAccount faulted"
--     V.Success validMAcc -> pure validMAcc

-- withMerchantAccount :: (DM.MerchantAccount -> Flow a) -> Flow a
-- withMerchantAccount flowF = do
--   mAcc <- createMerchantAccount
--   flowF mAcc



-------------------------------------------------------------------------------


ordId :: OrderId
ordId = OrderId "1475240639" -- change it

orderStatusResponse :: Api.OrderStatusResponse
orderStatusResponse = Api.OrderStatusResponse
  {id = "ord_7f3b93351e004877aea384d15f67f05d"
  , merchant_id = Just "altair"
  , amount = Just 111.0
  , currency = Just "INR"
  , order_id = Just "1475240639"
  , date_created = "2012-08-16 18:10:23"
  , return_url = Nothing, product_id = ""
  , customer_email = Just "redbus_user_101@gmail.com"
  , customer_phone = Nothing
  , customer_id = Just "redbus_user_101"
  , payment_links = Api.Paymentlinks
    { iframe = Just "http://localhost:8081/merchant/ipay/ord_7f3b93351e004877aea384d15f67f05d"
    , web = Just "http://localhost:8081/merchant/pay/ord_7f3b93351e004877aea384d15f67f05d"
    , mobile = Just "http://localhost:8081/merchant/pay/ord_7f3b93351e004877aea384d15f67f05d?mobile=true"
    }
  , udf1 = "udf1"
  , udf2 = "udf2"
  , udf3 = "udf3"
  , udf4 = "udf4"
  , udf5 = "udf5"
  , udf6 = "udf6"
  , udf7 = "udf7"
  , udf8 = "udf8"
  , udf9 = "udf9"
  , udf10 = "udf10"
  , txn_id = Nothing
  , status_id = 0
  , status = "SUCCESS"
  , payment_method_type = Nothing
  , auth_type = Just ""
  , card = Nothing
  , payment_method = Nothing
  , refunded = Just False
  , amount_refunded = Just 0.0
  , chargebacks = Nothing
  , refunds = Just []
  , mandate = Nothing
  , promotion = Nothing
  , risk = Nothing
  , bank_error_code = Just ""
  , bank_error_message = Just ""
  , txn_uuid = Nothing
  , gateway_payload = Nothing
  , txn_detail = Nothing
  , payment_gateway_response' = Nothing
  , payment_gateway_response = Nothing
  , gateway_id = Just 0
  , emi_bank = Nothing
  , emi_tenure = Nothing
  , gateway_reference_id = Nothing
  , payer_vpa = Nothing
  , payer_app_name = Nothing
  , juspay = Nothing
  , second_factor_response = Nothing
  , txn_flow_info = Nothing}


spec :: Spec
spec =
  around prepareDB $
    describe "API Order methods" $ do
      let runOrderStatus rt rp = runFlow rt $ do
            -- prepareDBConnections
            Auth.withMacc (PB.getMethod OrderStatus.orderStatus) rp PB.noReqBody
            --withMerchantAccount (OrderCreate.orderCreate rp ordReq)

--------------------------------------------------------------------

      it "OrderStatus. Authorization - invalid" $ \rt -> do
        let rp = collectRPs
              ordId
              (Authorization "BASIC definitelynotvalidbase64string=")
              (Version "2017-07-01")
              (UserAgent "Uagent")
        let err = Errs.eulerAccessDenied "Invalid API key."

        res <- try $ runOrderStatus rt rp
        res `shouldBe` Left err

      it "OrderStatus. Authorization - valid. Get OrderStatusResponse" $ \rt -> do
        let rp = collectRPs
              -- Use https://www.base64encode.org/ to get base64 from api_key
              ordId
              (Authorization "BASIC RTI5QTgzOEU0Qjc2NDM2RThBMkM2NjBBMDYwOTlGRUU=")
              (Version "2017-07-01")
              (UserAgent "Uagent")
        let err = Errs.eulerAccessDenied "Invalid API key."

        (res :: Either Errs.ErrorResponse Api.OrderStatusResponse) <- try $ runOrderStatus rt rp
        res `shouldBe` Right orderStatusResponse


