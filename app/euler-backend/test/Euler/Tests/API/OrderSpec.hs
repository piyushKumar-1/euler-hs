module Euler.Tests.API.OrderSpec where

import           EulerHS.Prelude hiding (show)
import           Test.Hspec

import qualified Prelude as P (show)
import qualified Data.Text as T

import           EulerHS.Language
import           EulerHS.Runtime
import           EulerHS.Interpreters
import           EulerHS.Types hiding (error)

import Euler.Common.Types.DefaultDate
import Euler.Common.Types.Order (OrderStatus(..))
import Euler.API.RouteParameters

import qualified Euler.API.Order                        as OrderAPI
import qualified Euler.Storage.DBConfig                 as DB
import qualified Euler.Storage.Types.MerchantAccount    as Merchant
import qualified Euler.Product.OLTP.Order.CreateUpdate  as OrderCreateUpdate
import qualified Euler.Product.OLTP.Order.OrderStatus   as OrderStatus

import qualified WebService.Types as T
import qualified EulerHS.Types as T
import qualified WebService.Language as L
import           Data.Time.Clock (NominalDiffTime)

testDBName :: String
testDBName = "./test/Euler/TestData/tmp_test.db"

testDBTemplateName :: String
testDBTemplateName = "./test/Euler/TestData/test.db.template"

rmTestDB :: Flow ()
rmTestDB = void $ runSysCmd $ "rm -f " <> testDBName

prepareTestDB :: Flow ()
prepareTestDB = do
  rmTestDB
  void $ runSysCmd $ "cp " <> testDBTemplateName <> " " <> testDBName

withEmptyDB :: (FlowRuntime -> IO ()) -> IO ()
withEmptyDB act = withFlowRuntime Nothing (\rt -> do
  try (runFlow rt prepareTestDB) >>= \case
    Left (e :: SomeException) ->
      runFlow rt rmTestDB
      `finally` error ("Preparing test values failed: " <> (toText $ P.show e))
    Right _ -> act rt `finally` runFlow rt rmTestDB
    )

ordReq :: OrderAPI.OrderCreateRequest
ordReq = OrderAPI.OrderCreateRequest
  { order_id                          = "orderId2" -- :: Text
  , amount                            = 1000 -- :: Double
  , currency                          = Just "INR" -- :: Maybe Text -- EUR, USD, GBP,  Default value: INR
  , customer_id                       = Just "customerId" -- :: Maybe Text
  , customer_email                    = Just "customer@email.com" -- :: Maybe Text
  , customer_phone                    = Just "" -- :: Maybe Text
  , description                       = Just "some descr" -- :: Maybe Text
  , return_url                        = Just "http://example.com" -- :: Maybe Text
  , product_id                        = Just "prodId" -- :: Maybe Text
  , billing_address_first_name        = Nothing -- :: Maybe Text
  , billing_address_last_name         = Nothing -- :: Maybe Text
  , billing_address_line1             = Nothing -- :: Maybe Text
  , billing_address_line2             = Nothing -- :: Maybe Text
  , billing_address_line3             = Nothing -- :: Maybe Text
  , billing_address_city              = Nothing -- :: Maybe Text
  , billing_address_state             = Nothing -- :: Maybe Text
  , billing_address_country           = Nothing -- :: Maybe Text
  , billing_address_postal_code       = Nothing -- :: Maybe Text
  , billing_address_phone             = Nothing -- :: Maybe Text
  , billing_address_country_code_iso  = Nothing -- :: Maybe Text -- Default value: IND
  , shipping_address_first_name       = Nothing -- :: Maybe Text
  , shipping_address_last_name        = Nothing -- :: Maybe Text
  , shipping_address_line1            = Nothing -- :: Maybe Text
  , shipping_address_line2            = Nothing -- :: Maybe Text
  , shipping_address_line3            = Nothing -- :: Maybe Text
  , shipping_address_city             = Nothing -- :: Maybe Text
  , shipping_address_state            = Nothing -- :: Maybe Text
  , shipping_address_country          = Nothing -- :: Maybe Text
  , shipping_address_postal_code      = Nothing -- :: Maybe Text
  , shipping_address_phone            = Nothing -- :: Maybe Text
  , shipping_address_country_code_iso = Nothing -- :: Maybe Text -- Default value: IND
  , udf1                              = Just "udf1" -- :: Maybe Text
  , udf2                              = Just "udf2" -- :: Maybe Text
  , udf3                              = Just "udf3" -- :: Maybe Text
  , udf4                              = Just "udf4" -- :: Maybe Text
  , udf5                              = Just "udf5" -- :: Maybe Text
  , udf6                              = Just "udf6" -- :: Maybe Text
  , udf7                              = Just "udf7" -- :: Maybe Text
  , udf8                              = Just "udf8" -- :: Maybe Text
  , udf9                              = Just "udf9" -- :: Maybe Text
  , udf10                             = Just "udf10" -- :: Maybe Text
  , metaData                          = Nothing -- :: Maybe Text
  , gateway_id                        = Nothing -- :: Maybe Text -- converted to Int, why Text?
  , mandate_max_amount                = Nothing -- :: Maybe Text
  , auto_refund                       = Nothing -- :: Maybe Bool
  , options_create_mandate            = Nothing
  , options_get_client_auth_token     = Just True
  }

pLinksCreate :: OrderAPI.Paymentlinks
pLinksCreate = OrderAPI.Paymentlinks
  { iframe = Just "https://defaulthost/merchant/ipay/ordeu_someUUID32"
  , web = Just "https://defaulthost/merchant/pay/ordeu_someUUID32"
  , mobile = Just "https://defaulthost/merchant/pay/ordeu_someUUID32?mobile=true"
  }


ordCreateResp :: OrderAPI.OrderCreateResponse
ordCreateResp = OrderAPI.OrderCreateResponse
  { status = NEW
  , status_id = 10
  , id = "ordeu_someUUID32"
  , order_id = "orderId"
  , payment_links = pLinksCreate
  , udf9 = Just "udf9"
  , udf8 = Just "udf8"
  , udf7 = Just "udf7"
  , udf6 = Just "udf6"
  , udf5 = Just "udf5"
  , udf4 = Just "udf4"
  , udf3 = Just "udf3"
  , udf2 = Just "udf2"
  , udf10 = Just "udf10"
  , udf1 = Just "udf1"
  , return_url = Just "http://example.com"
  , refunded = Just False
  , product_id = Just "prodId"
  , merchant_id = Just "1"
  , date_created = Just defaultDate
  , customer_phone = Just ""
  , customer_id = Just "customerId"
  , customer_email = Just "customer@email.com"
  , currency = Just "INR"
  , amount_refunded = Just 0.0
  , amount = Just 1000.0
  , juspay = Nothing
  }

pLinksStatus  :: OrderAPI.Paymentlinks
pLinksStatus = OrderAPI.Paymentlinks
  {iframe = Just "https://defaulthost/merchant/ipay/order_uuid123"
  , web = Just "https://defaulthost/merchant/pay/order_uuid123"
  , mobile = Just "https://defaulthost/merchant/pay/order_uuid123?mobile=true"
  }

orderStatusResp :: OrderAPI.OrderStatusResponse
orderStatusResp = OrderAPI.OrderStatusResponse
  {id = "order_uuid123"
  , merchant_id = Just "merchantId"
  , amount = Just 10.0
  , currency = Just "INR"
  , order_id = Just "orderId"
  , date_created = "1858-11-18 01:01:01"
  , return_url = Just ""
  , product_id = ""
  , customer_email = Just "customer@email.com"
  , customer_phone = Just ""
  , customer_id = Just "customerId"
  , payment_links = pLinksStatus
  , udf1 = "udf1"
  , udf2 = ""
  , udf3 = ""
  , udf4 = ""
  , udf5 = ""
  , udf6 = ""
  , udf7 = ""
  , udf8 = ""
  , udf9 = ""
  , udf10 = "udf10"
  , txn_id = Nothing
  , status_id = 1
  , status = "CREATED"
  , payment_method_type = Nothing
  , auth_type = Nothing
  , card = Nothing
  , payment_method = Nothing
  , refunded = Nothing
  , amount_refunded = Nothing
  , chargebacks = Nothing
  , refunds = Nothing
  , mandate = Nothing
  , promotion = Nothing
  , risk = Nothing
  , bank_error_code = Nothing
  , bank_error_message = Nothing
  , txn_uuid = Nothing
  , gateway_payload = Nothing
  , txn_detail = Nothing
  , payment_gateway_response' = Nothing
  , payment_gateway_response = Nothing
  , gateway_id = Nothing
  , emi_bank = Nothing
  , emi_tenure = Nothing
  , gateway_reference_id = Nothing
  , payer_vpa = Nothing
  , payer_app_name = Nothing
  , juspay = Nothing
  }


sqliteConn :: IsString a => a
sqliteConn = "sqlite"

keepConnsAliveForSecs :: NominalDiffTime
keepConnsAliveForSecs = 60 * 10 -- 10 mins

maxTotalConns :: Int
maxTotalConns = 8

-- Redis config data
redisConn :: IsString a => a
redisConn = "redis"

redisConnConfig :: T.RedisConfig
redisConnConfig = T.RedisConfig
    { connectHost           = "localhost"
    , connectPort           = 6379
    , connectAuth           = Nothing
    , connectDatabase       = 0
    , connectMaxConnections = 50
    , connectMaxIdleTime    = 30
    , connectTimeout        = Nothing
    }

prepareDBConnections :: Flow ()
prepareDBConnections = do
  ePool <- initSqlDBConnection
    $ T.mkSQLitePoolConfig sqliteConn "./test/Euler/TestData/tmp_test.db"
    $ T.PoolConfig 1 keepConnsAliveForSecs maxTotalConns
  redis <- initKVDBConnection
    $ T.mkKVDBConfig redisConn
    $ redisConnConfig
  L.throwOnFailedWithLog ePool T.SqlDBConnectionFailedException "Failed to connect to SQLite DB."
  L.throwOnFailedWithLog redis T.KVDBConnectionFailedException "Failed to connect to Redis DB."

spec :: Spec
spec =
  around withEmptyDB $

    describe "API Order methods" $ do
      it "OrderCreate" $ \rt -> do
        let rp = collectRPs (Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI=")
                            (Version "2018-07-01")
                            (UserAgent "Uagent")
        eRes <- runFlow rt $ prepareDBConnections *> OrderCreateUpdate.orderCreate ordReq rp Merchant.defaultMerchantAccount
        eRes `shouldSatisfy` (\OrderAPI.OrderCreateResponse{..} ->
          status == NEW
          && status_id == 10
         -- && id ==
          && order_id == "orderId2"
         -- && payment_links ==
          && udf9 == Just "udf9"
          && udf8 == Just "udf8"
          && udf7 == Just "udf7"
          && udf6 == Just "udf6"
          && udf5 == Just "udf5"
          && udf4 == Just "udf4"
          && udf3 == Just "udf3"
          && udf2 == Just "udf2"
          && udf10 == Just "udf10"
          && udf1 == Just "udf1"
          && return_url == Just "http://example.com"
          && refunded == Just False
          && product_id == Just "prodId"
          && merchant_id == Just "1"
         -- && date_created ==
          && customer_phone == Just ""
          && customer_id == Just "customerId"
          && customer_email == Just "customer@email.com"
          && currency == Just "INR"
          && amount_refunded == Just 0.0
          && amount == Just 1000.0
         -- && juspay ==
          )

      it "OrderStatus" $ \rt -> do
        eRes <- runFlow rt $ prepareDBConnections *> OrderStatus.processOrderStatusGET "orderId" "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI="
        eRes `shouldBe` orderStatusResp
