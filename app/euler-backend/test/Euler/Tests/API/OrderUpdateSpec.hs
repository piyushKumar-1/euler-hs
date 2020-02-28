module Euler.Tests.API.OrderUpdateSpec
  ( spec
  )
  where

import           EulerHS.Prelude
import           Test.Hspec

import qualified Data.Text as T

import           EulerHS.Language
import           EulerHS.Runtime
import           EulerHS.Interpreters
import           EulerHS.Types hiding (error)


import qualified Euler.Product.Domain as DM


import qualified Euler.API.Order                        as OrderAPI
import           Euler.API.Order                        (OrderUpdateRequest(..), Paymentlinks(..)
                                                        , OrderTokenResp(..), OrderStatusResponse(..))
import           Euler.API.RouteParameters
import qualified Euler.API.Validators.Order             as VO
import qualified Euler.Common.Errors.PredefinedErrors   as Errs
import qualified Euler.Storage.DBConfig                 as DB
import qualified Euler.Storage.Types.MerchantAccount    as Merchant
import qualified Euler.Product.OLTP.Services.AuthenticationService as Auth
import qualified Euler.Product.OLTP.Order.Update        as OrderUpdate


import qualified WebService.Types as T
import qualified WebService.Language as L
import qualified EulerHS.Extra.Validation as V
import qualified EulerHS.Types as T
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
      `finally` error ("Preparing test values failed: " <> show e)
    Right _ -> act rt `finally` runFlow rt rmTestDB
    )

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


-- createMerchantAccount :: Flow DM.MerchantAccount
-- createMerchantAccount = case Merchant.transSMaccToDomMacc Merchant.defaultMerchantAccount of
--     V.Failure e -> do
--       logError "DB MerchantAccount Validation" $ show e
--       error "MerchantAccount faulted"
--     V.Success validMAcc -> pure validMAcc

--withMerchantAccount :: (DM.MerchantAccount -> Flow a) -> Flow a
--withMerchantAccount flowF = do
--  mAcc <- createMerchantAccount
--  flowF mAcc

-- prepared orders in db
orderForUpdate :: OrderId
orderForUpdate = OrderId "orderForUpdate"

orderForCleanup :: OrderId
orderForCleanup = OrderId "orderForCleanup"


-- Versions
oldVersion :: Version
oldVersion = Version "2015-01-09"

newVersion :: Version
newVersion = Version "2018-10-25"

-- API key
auth :: Authorization
auth = Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI="

runOrderUpdateWithValidation rp req mAcc = do
  let validReq = VO.apiOrderUpdToOrderUpdT req
  case validReq of
    V.Failure err -> do
      logError "OrderUpdateRequest validation" $ show err
      throwException $ Errs.mkValidationError err
    V.Success validatedOrder -> OrderUpdate.orderUpdate rp validatedOrder mAcc

-- update all address, udf fields and amount
updateRequest :: OrderUpdateRequest
updateRequest = OrderUpdateRequest
  { amount                            = Just 999.99

  , billing_address_first_name        = Just "new_billing_address_first_name"
  , billing_address_last_name         = Just "new_billing_address_last_name"
  , billing_address_line1             = Just "new_billing_address_line1"
  , billing_address_line2             = Just "new_billing_address_line2"
  , billing_address_line3             = Just "new_billing_address_line3"
  , billing_address_city              = Just "new_billing_address_city"
  , billing_address_state             = Just "new_billing_address_state"
  , billing_address_country           = Just "new_billing_address_country"
  , billing_address_postal_code       = Just "new_billing_address_postal_code"
  , billing_address_phone             = Just "new_billing_address_phone"
  , billing_address_country_code_iso  = Just "new_billing_address_country_code_iso"

  , shipping_address_first_name       = Just "new_shipping_address_first_name"
  , shipping_address_last_name        = Just "new_shipping_address_last_name"
  , shipping_address_line1            = Just "new_shipping_address_line1"
  , shipping_address_line2            = Just "new_shipping_address_line2"
  , shipping_address_line3            = Just "new_shipping_address_line3"
  , shipping_address_city             = Just "new_shipping_address_city"
  , shipping_address_state            = Just "new_shipping_address_state"
  , shipping_address_country          = Just "new_shipping_address_country"
  , shipping_address_postal_code      = Just "new_shipping_address_postal_code"
  , shipping_address_phone            = Just "new_shipping_address_phone"
  , shipping_address_country_code_iso = Just "new_shipping_address_country_code_iso"

  , udf1                              = Just "new_udf1"
  , udf2                              = Just "new_udf2"
  , udf3                              = Just "new_udf3"
  , udf4                              = Just "new_udf4"
  , udf5                              = Just "new_udf5"
  , udf6                              = Just "new_udf6"
  , udf7                              = Just "new_udf7"
  , udf8                              = Just "new_udf8"
  , udf9                              = Just "new_udf9"
  , udf10                             = Just "new_udf10"
  }


-- should clean all address and udf fields, amount not changed
cleanupRequest :: OrderUpdateRequest
cleanupRequest = OrderUpdateRequest
  { amount                            = Nothing

  , billing_address_first_name        = Nothing
  , billing_address_last_name         = Nothing
  , billing_address_line1             = Nothing
  , billing_address_line2             = Nothing
  , billing_address_line3             = Nothing
  , billing_address_city              = Nothing
  , billing_address_state             = Nothing
  , billing_address_country           = Nothing
  , billing_address_postal_code       = Nothing
  , billing_address_phone             = Nothing
  , billing_address_country_code_iso  = Nothing

  , shipping_address_first_name       = Nothing
  , shipping_address_last_name        = Nothing
  , shipping_address_line1            = Nothing
  , shipping_address_line2            = Nothing
  , shipping_address_line3            = Nothing
  , shipping_address_city             = Nothing
  , shipping_address_state            = Nothing
  , shipping_address_country          = Nothing
  , shipping_address_postal_code      = Nothing
  , shipping_address_phone            = Nothing
  , shipping_address_country_code_iso = Nothing

  , udf1                              = Nothing
  , udf2                              = Nothing
  , udf3                              = Nothing
  , udf4                              = Nothing
  , udf5                              = Nothing
  , udf6                              = Nothing
  , udf7                              = Nothing
  , udf8                              = Nothing
  , udf9                              = Nothing
  , udf10                             = Nothing
  }

emptyPaymentlinks :: Paymentlinks
emptyPaymentlinks = Paymentlinks
  { iframe = Nothing
  , web    = Nothing
  , mobile = Nothing
  }

emptyOrderTokenResp :: OrderTokenResp
emptyOrderTokenResp = OrderTokenResp
  { client_auth_token        = Nothing -- :: Maybe Text
  , client_auth_token_expiry = Nothing -- :: Maybe Text
  }

emptyOrderStatusResponse :: OrderStatusResponse
emptyOrderStatusResponse = OrderStatusResponse
  {  id                        = "" -- :: Text
  ,  merchant_id               = Nothing -- :: Maybe Text
  ,  amount                    = Nothing -- :: Maybe Double
  ,  currency                  = Nothing -- :: Maybe Text
  ,  order_id                  = Nothing -- :: Maybe Text
  ,  date_created              = "" -- :: Text
  ,  return_url                = Nothing -- :: Maybe Text
  ,  product_id                = "" -- :: Text
  ,  customer_email            = Nothing -- :: Maybe Text -- Foreign
  ,  customer_phone            = Nothing -- :: Maybe Text -- Foreign
  ,  customer_id               = Nothing -- :: Maybe Text -- Foreign
  ,  payment_links             = emptyPaymentlinks -- :: Paymentlinks
  ,  udf1                      = "" -- :: Text
  ,  udf2                      = "" -- :: Text
  ,  udf3                      = "" -- :: Text
  ,  udf4                      = "" -- :: Text
  ,  udf5                      = "" -- :: Text
  ,  udf6                      = "" -- :: Text
  ,  udf7                      = "" -- :: Text
  ,  udf8                      = "" -- :: Text
  ,  udf9                      = "" -- :: Text
  ,  udf10                     = "" -- :: Text
  ,  txn_id                    = Nothing -- :: Maybe Text
  ,  status_id                 = 0       -- :: Int
  ,  status                    = "DEFAULT" -- :: Text
  ,  payment_method_type       = Nothing -- :: Maybe Text
  ,  auth_type                 = Nothing -- :: Maybe Text
  ,  card                      = Nothing -- :: Maybe Card
  ,  payment_method            = Nothing -- :: Maybe Text
  ,  refunded                  = Nothing -- :: Maybe Bool
  ,  amount_refunded           = Nothing -- :: Maybe Double
  ,  chargebacks               = Nothing -- :: Maybe [Chargeback']
  ,  refunds                   = Nothing -- :: Maybe [Refund']
  ,  mandate                   = Nothing -- :: Maybe Mandate'
  ,  promotion                 = Nothing -- :: Maybe Promotion'
  ,  risk                      = Nothing -- :: Maybe Risk
  ,  bank_error_code           = Nothing -- :: Maybe Text
  ,  bank_error_message        = Nothing -- :: Maybe Text
  ,  txn_uuid                  = Nothing -- :: Maybe Text
  ,  gateway_payload           = Nothing -- :: Maybe Text
  ,  txn_detail                = Nothing -- :: Maybe TxnDetail'
  ,  payment_gateway_response' = Nothing -- :: Maybe MerchantPaymentGatewayResponse'
  ,  payment_gateway_response  = Nothing -- :: Maybe MerchantPaymentGatewayResponse
  ,  gateway_id                = Nothing -- :: Maybe Int
  ,  emi_bank                  = Nothing -- :: Maybe Text
  ,  emi_tenure                = Nothing -- :: Maybe Int
  ,  gateway_reference_id      = Nothing -- :: Maybe Text -- Foreign
  ,  payer_vpa                 = Nothing -- :: Maybe Text -- Foreign
  ,  payer_app_name            = Nothing -- :: Maybe Text -- Foreign
  ,  juspay                    = Nothing -- :: Maybe OrderTokenResp
  }


-- TODO: replace OrderAPI.defaultOrderStatusResponse with correct responses
spec :: Spec
spec =
  describe "API OrderUpdate" $ do

    describe "Old version" $ do

      around withEmptyDB $ do

        xit "Update" $ \rt -> do
          let rp = collectRPs orderForUpdate auth oldVersion
          eRes <- runFlow rt $ prepareDBConnections *> Auth.withMacc runOrderUpdateWithValidation rp updateRequest
          eRes `shouldBe` OrderAPI.defaultOrderStatusResponse

        xit "Cleanup" $ \rt -> do
          let rp = collectRPs orderForCleanup auth oldVersion
          eRes <- runFlow rt $ prepareDBConnections *> Auth.withMacc runOrderUpdateWithValidation rp cleanupRequest
          eRes `shouldBe` OrderAPI.defaultOrderStatusResponse

    describe "New version" $ do

      around withEmptyDB $ do

        xit "Update" $ \rt -> do
          let rp = collectRPs orderForUpdate auth newVersion
          eRes <- runFlow rt $ prepareDBConnections *> Auth.withMacc runOrderUpdateWithValidation rp updateRequest
          eRes `shouldBe` OrderAPI.defaultOrderStatusResponse

        xit "Cleanup" $ \rt -> do
          let rp = collectRPs orderForCleanup auth newVersion
          eRes <- runFlow rt $ prepareDBConnections *> Auth.withMacc runOrderUpdateWithValidation rp cleanupRequest
          eRes `shouldBe` OrderAPI.defaultOrderStatusResponse
