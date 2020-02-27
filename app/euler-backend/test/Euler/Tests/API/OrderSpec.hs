module Euler.Tests.API.OrderSpec where

import           EulerHS.Prelude
import           Test.Hspec

import qualified Data.Text as T

import           EulerHS.Language
import           EulerHS.Runtime
import           EulerHS.Interpreters
import           EulerHS.Types hiding (error)

import Euler.Common.Types.DefaultDate
import Euler.Common.Types.Currency (Currency(..))
import Euler.Common.Types.External.Mandate (MandateFeature(..))
import Euler.Common.Types.External.Order (OrderStatus(..))
import qualified Euler.Product.Domain as DM
import Euler.API.RouteParameters

import qualified Euler.API.Order                        as OrderAPI
import           Euler.API.Order                        (OrderCreateRequest(..))
import qualified Euler.Storage.DBConfig                 as DB
import qualified Euler.Storage.Types.MerchantAccount    as Merchant
import qualified Euler.Storage.Validators.MerchantAccount as Merchant
import qualified Euler.Product.OLTP.Order.Create        as OrderCreate
import qualified Euler.Product.OLTP.Order.CreateUpdateLegacy  as OrderCreateUpdateLegacy
import qualified Euler.Product.OLTP.Order.OrderStatus   as OrderStatus

import qualified WebService.Types as T
import qualified WebService.Language as L
import qualified EulerHS.Extra.Validation as V
import qualified EulerHS.Types as T
import           Data.Time.Clock (NominalDiffTime)

import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Common.Errors.Types as Errs
import qualified Euler.API.Validators.Order as VO
import qualified Data.Aeson as A
import Test.HUnit.Base


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

createMerchantAccount :: Flow DM.MerchantAccount
createMerchantAccount = case Merchant.transSMaccToDomMacc Merchant.defaultMerchantAccount of
    V.Failure e -> do
      logError "DB MerchantAccount Validation" $ show e
      error "MerchantAccount faulted"
    V.Success validMAcc -> pure validMAcc

withMerchantAccount :: (DM.MerchantAccount -> Flow a) -> Flow a
withMerchantAccount flowF = do
  mAcc <- createMerchantAccount
  flowF mAcc

orderReqTemplate :: OrderAPI.OrderCreateRequest
orderReqTemplate = OrderAPI.OrderCreateRequest
  { order_id                          = "" -- :: Text
  , amount                            = 0 -- :: Double
  , currency                          = Nothing -- :: Maybe Text -- EUR, USD, GBP,
--  Default value: MerchantIframePreferences defaultCurrency or INR
  , customer_id                       = Nothing -- :: Maybe Text
  , customer_email                    = Nothing -- :: Maybe Text
  , customer_phone                    = Nothing -- :: Maybe Text
  , description                       = Nothing -- :: Maybe Text
  , return_url                        = Nothing -- :: Maybe Text
  , product_id                        = Nothing -- :: Maybe Text
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
  , udf1                              = Nothing -- Just "udf1" -- :: Maybe Text
  , udf2                              = Nothing -- Just "udf2" -- :: Maybe Text
  , udf3                              = Nothing -- Just "udf3" -- :: Maybe Text
  , udf4                              = Nothing -- Just "udf4" -- :: Maybe Text
  , udf5                              = Nothing -- Just "udf5" -- :: Maybe Text
  , udf6                              = Nothing -- Just "udf6" -- :: Maybe Text
  , udf7                              = Nothing -- Just "udf7" -- :: Maybe Text
  , udf8                              = Nothing -- Just "udf8" -- :: Maybe Text
  , udf9                              = Nothing -- Just "udf9" -- :: Maybe Text
  , udf10                             = Nothing -- Just "udf10" -- :: Maybe Text
  , metaData                          = Nothing -- :: Maybe Text
  , gateway_id                        = Nothing -- :: Maybe Text -- converted to Int, why Text?
  , mandate_max_amount                = Nothing -- :: Maybe Text
  , auto_refund                       = Nothing -- :: Maybe Bool
  , options_create_mandate            = Nothing
  , options_get_client_auth_token     = Nothing -- Just True
  }

-- order_id textNotEmpty
-- amount max2DecimalDigits gteOne
-- customer_id notBlank if present
-- options_create_mandate default: DISABLED
ordReqForValidators :: OrderCreateRequest
ordReqForValidators = orderReqTemplate
  { amount = 0.0104
  , customer_id = Just "  "
  }

ordReqExisted :: OrderCreateRequest
ordReqExisted = orderReqTemplate
  { order_id = "orderId"
  , amount = 1.0
  }

ordReqMandateFeatureFailData :: OrderCreateRequest
ordReqMandateFeatureFailData = orderReqTemplate
  { order_id = "wrongMandateParams"
  , amount = 500.0
  , mandate_max_amount = Nothing
  , options_create_mandate = Just OPTIONAL
  }

ordReqMandateFeatureTooBigMandateMaxAmount :: OrderCreateRequest
ordReqMandateFeatureTooBigMandateMaxAmount = orderReqTemplate
  { order_id = "wrongMandateParams"
  , amount = 50000.0
  , mandate_max_amount = Just "100500"
  , options_create_mandate = Just OPTIONAL
  }

ordReqUdf :: OrderCreateRequest
ordReqUdf = orderReqTemplate
  { order_id = "udfId"
  , amount = 100.0
  , udf1  = Just "filtered~!#%^=+\\|:;,\"'()-.&/"
  , udf2  = Just "filtered~!#%^=+\\|:;,\"'()-.&/"
  , udf3  = Just "filtered~!#%^=+\\|:;,\"'()-.&/"
  , udf4  = Just "filtered~!#%^=+\\|:;,\"'()-.&/"
  , udf5  = Just "filtered~!#%^=+\\|:;,\"'()-.&/"
  , udf6  = Just "unfiltered~!#%^=+\\|:;,\"'()-.&/"
  , udf7  = Just "unfiltered~!#%^=+\\|:;,\"'()-.&/"
  , udf8  = Just "unfiltered~!#%^=+\\|:;,\"'()-.&/"
  , udf9  = Just "unfiltered~!#%^=+\\|:;,\"'()-.&/"
  , udf10 = Just "unfiltered~!#%^=+\\|:;,\"'()-.&/"
  }

ordReqWithAddresses :: OrderCreateRequest
ordReqWithAddresses = orderReqTemplate
  { order_id = "orderWithAddr"
  , amount = 100.0
  , billing_address_first_name = Just "billing_address_first_name"
  , billing_address_last_name = Just "billing_address_last_name"
  , billing_address_line1 = Just "billing_address_line1"
  , billing_address_line2 = Just "billing_address_line2"
  , billing_address_line3 = Just "billing_address_line3"
  , billing_address_city = Just "billing_address_city"
  , billing_address_state = Just "billing_address_state"
  , billing_address_country = Just "billing_address_country"
  , billing_address_postal_code = Just "billing_address_postal_code"
  , billing_address_phone = Just "billing_address_phone"
  , billing_address_country_code_iso = Just "billing_address_country_code_iso"
  , shipping_address_first_name = Just "shipping_address_first_name"
  , shipping_address_last_name = Just "shipping_address_last_name"
  , shipping_address_line1 = Just "shipping_address_line1"
  , shipping_address_line2 = Just "shipping_address_line2"
  , shipping_address_line3 = Just "shipping_address_line3"
  , shipping_address_city = Just "shipping_address_city"
  , shipping_address_state = Just "shipping_address_state"
  , shipping_address_country = Just "shipping_address_country"
  , shipping_address_postal_code = Just "shipping_address_postal_code"
  , shipping_address_phone = Just "shipping_address_phone"
  , shipping_address_country_code_iso = Just "shipping_address_country_code_iso"
  }

ordReq :: OrderAPI.OrderCreateRequest
ordReq = OrderAPI.OrderCreateRequest
  { order_id                          = "orderId2" -- :: Text
  , amount                            = 1000.0 -- :: Double
  , currency                          = Just INR -- EUR, USD, GBP,  Default value: INR
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
  { iframe = Just "https://defaulthost/merchant/ipay/order_uuid123"
  , web = Just "https://defaulthost/merchant/pay/order_uuid123"
  , mobile = Just "https://defaulthost/merchant/pay/order_uuid123?mobile=true"
  }

orderStatusResp :: OrderAPI.OrderStatusResponse
orderStatusResp = OrderAPI.OrderStatusResponse
  { id = "order_uuid123"
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


loop :: IO ()
loop = do threadDelay 1000; loop

-- shouldBeAnn ::
shouldBeAnn s a b = assertEqual s b a

-- shouldStartWithAnn :: _
shouldStartWithAnn s v p
  | plen > length v = assertFailure s
  | otherwise = assertBool s (take plen v == p)
  where
    plen = length p


spec :: Spec
spec =
  around withEmptyDB $
    describe "API Order methods" $ do
      let runOrderCreate rt ordReq rp = runFlow rt $ do
            prepareDBConnections
            withMerchantAccount (OrderCreate.orderCreate rp ordReq)

----------------------------------------------------------------------

      xit "OrderCreate. Authorization - invalid" $ \rt -> do
        let rp = collectRPs
              (Authorization "BASIC definitelynotvalidbase64string=")
              (Version "2017-07-01")
              (UserAgent "Uagent")

        let check Errs.ECErrorResponse{code, response} =
              case A.decode response of
                Just Errs.ECErrorPayload{error_code} ->
                  code == 401 -- && error_code == Just "...
                Nothing -> False

        runOrderCreate rt ordReq rp `shouldThrow` check


----------------------------------------------------------------------

      it "OrderCreate. Old version. customer_id = Nothing" $ \rt -> do
        let rp = collectRPs
              (Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI=")
              (Version "2017-07-01")
              (UserAgent "Uagent")

        resp <- runOrderCreate rt ordReq{customer_id = Nothing} rp

        -- Do no threat this as a source of truth. Just fixing current behavior
        do
          let OrderAPI.OrderCreateResponse{..} = resp

          -- For old version udf's set to nothing (in defaultOrderCreateResponse)
          -- Also, udfs present in db

          shouldBeAnn "status"          status           $ CREATED
          shouldBeAnn "order_id"        order_id         $ "orderId2"
          shouldBeAnn "udf10"           udf10            $ Nothing
          shouldBeAnn "udf9"            udf9             $ Nothing
          shouldBeAnn "udf8"            udf8             $ Nothing
          shouldBeAnn "udf7"            udf7             $ Nothing
          shouldBeAnn "udf6"            udf6             $ Nothing
          shouldBeAnn "udf5"            udf5             $ Nothing
          shouldBeAnn "udf4"            udf4             $ Nothing
          shouldBeAnn "udf3"            udf3             $ Nothing
          shouldBeAnn "udf2"            udf2             $ Nothing
          shouldBeAnn "udf1"            udf1             $ Nothing
          shouldBeAnn "return_url"      return_url       $ Just "http://example.com"
          shouldBeAnn "refunded"        refunded         $ Just False
          shouldBeAnn "product_id"      product_id       $ Just "prodId"
          shouldBeAnn "merchant_id"     merchant_id      $ Just "1"

          shouldBeAnn "customer_phone"  customer_phone   $ Nothing
          shouldBeAnn "customer_id"     customer_id      $ Nothing -- Just "customerId"
          shouldBeAnn "customer_email"  customer_email   $ Nothing

          shouldBeAnn "currency"        currency         $ Just "INR"
          shouldBeAnn "amount_refunded" amount_refunded  $ Nothing
          shouldBeAnn "amount"          amount           $ Just 1000.0

----------------------------------------------------------------------

      it "OrderCreate. Old version. customer_id existing in db" $ \rt -> do
        let rp = collectRPs
              (Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI=")
              (Version "2017-07-01")
              (UserAgent "Uagent")

        resp <- runOrderCreate rt ordReq{customer_id = Just "1"} rp

        -- Do no threat this as a source of truth. Just fixing current behavior
        do
          let OrderAPI.OrderCreateResponse{..} = resp

          -- For old version udf's set to nothing (in defaultOrderCreateResponse)
          -- Also, udfs present in db

          shouldBeAnn "status"          status           $ CREATED
          shouldBeAnn "order_id"        order_id         $ "orderId2"
          shouldBeAnn "udf10"           udf10            $ Nothing
          shouldBeAnn "udf9"            udf9             $ Nothing
          shouldBeAnn "udf8"            udf8             $ Nothing
          shouldBeAnn "udf7"            udf7             $ Nothing
          shouldBeAnn "udf6"            udf6             $ Nothing
          shouldBeAnn "udf5"            udf5             $ Nothing
          shouldBeAnn "udf4"            udf4             $ Nothing
          shouldBeAnn "udf3"            udf3             $ Nothing
          shouldBeAnn "udf2"            udf2             $ Nothing
          shouldBeAnn "udf1"            udf1             $ Nothing
          shouldBeAnn "return_url"      return_url       $ Just "http://example.com"
          shouldBeAnn "refunded"        refunded         $ Just False
          shouldBeAnn "product_id"      product_id       $ Just "prodId"
          shouldBeAnn "merchant_id"     merchant_id      $ Just "1"

          shouldBeAnn "customer_phone"  customer_phone   $ Just ""
          shouldBeAnn "customer_id"     customer_id      $ Just "1"
          shouldBeAnn "customer_email"  customer_email   $ Just "customer@email.com"

          shouldBeAnn "currency"        currency         $ Just "INR"
          shouldBeAnn "amount_refunded" amount_refunded  $ Nothing
          shouldBeAnn "amount"          amount           $ Just 1000.0

----------------------------------------------------------------------

      it "OrderCreate. Old version. customer_id not existing in db" $ \rt -> do
        let rp = collectRPs
              (Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI=")
              (Version "2017-07-01")
              (UserAgent "Uagent")

        resp <- runOrderCreate rt ordReq{customer_id = Just "notExists"} rp

        -- Do no threat this as a source of truth. Just fixing current behavior
        do
          let OrderAPI.OrderCreateResponse{..} = resp

          -- For old version udf's set to nothing (in defaultOrderCreateResponse)
          -- Also, udfs present in db

          shouldBeAnn "status"          status           $ CREATED
          shouldBeAnn "order_id"        order_id         $ "orderId2"
          shouldBeAnn "udf10"           udf10            $ Nothing
          shouldBeAnn "udf9"            udf9             $ Nothing
          shouldBeAnn "udf8"            udf8             $ Nothing
          shouldBeAnn "udf7"            udf7             $ Nothing
          shouldBeAnn "udf6"            udf6             $ Nothing
          shouldBeAnn "udf5"            udf5             $ Nothing
          shouldBeAnn "udf4"            udf4             $ Nothing
          shouldBeAnn "udf3"            udf3             $ Nothing
          shouldBeAnn "udf2"            udf2             $ Nothing
          shouldBeAnn "udf1"            udf1             $ Nothing
          shouldBeAnn "return_url"      return_url       $ Just "http://example.com"
          shouldBeAnn "refunded"        refunded         $ Just False
          shouldBeAnn "product_id"      product_id       $ Just "prodId"
          shouldBeAnn "merchant_id"     merchant_id      $ Just "1"

          shouldBeAnn "customer_phone"  customer_phone   $ Nothing
          shouldBeAnn "customer_id"     customer_id      $ Nothing
          shouldBeAnn "customer_email"  customer_email   $ Nothing

          shouldBeAnn "currency"        currency         $ Just "INR"
          shouldBeAnn "amount_refunded" amount_refunded  $ Nothing
          shouldBeAnn "amount"          amount           $ Just 1000.0


----------------------------------------------------------------------

      it "OrderCreate. New version. customer_id existing in db" $ \rt -> do
        let rp = collectRPs
              (Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI=")
              (Version "2018-07-01")
              (UserAgent "Uagent")

        resp <- runOrderCreate rt ordReq{customer_id = Just "1"} rp

        do
          let OrderAPI.OrderCreateResponse{..} = resp

          shouldBeAnn "status"          status          $ NEW
          shouldBeAnn "status_id"       status_id       $ 10
          shouldBeAnn "order_id"        order_id        $ "orderId2"
          shouldBeAnn "udf10"           udf10           $ Just "udf10"
          shouldBeAnn "udf9"            udf9            $ Just "udf9"
          shouldBeAnn "udf8"            udf8            $ Just "udf8"
          shouldBeAnn "udf7"            udf7            $ Just "udf7"
          shouldBeAnn "udf6"            udf6            $ Just "udf6"
          shouldBeAnn "udf5"            udf5            $ Just "udf5"
          shouldBeAnn "udf4"            udf4            $ Just "udf4"
          shouldBeAnn "udf3"            udf3            $ Just "udf3"
          shouldBeAnn "udf2"            udf2            $ Just "udf2"
          shouldBeAnn "udf1"            udf1            $ Just "udf1"
          shouldBeAnn "return_url"      return_url      $ Just "http://example.com"
          shouldBeAnn "refunded"        refunded        $ Just False
          shouldBeAnn "product_id"      product_id      $ Just "prodId"
          shouldBeAnn "merchant_id"     merchant_id     $ Just "1"

          shouldBeAnn "customer_phone"  customer_phone  $ Just ""
          shouldBeAnn "customer_id"     customer_id     $ Just "1"
          shouldBeAnn "customer_email"  customer_email  $ Just "customer@email.com"

          shouldBeAnn "currency"        currency        $ Just "INR"
          shouldBeAnn "amount_refunded" amount_refunded $ Just 0.0
          shouldBeAnn "amount"          amount          $ Just 1000.0


----------------------------------------------------------------------

      it "OrderCreate. New version. customer_id not existing in db" $ \rt -> do
        let rp = collectRPs
              (Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI=")
              (Version "2018-07-01")
              (UserAgent "Uagent")

        resp <- runOrderCreate rt ordReq{customer_id = Just "notExists"} rp

        do
          let OrderAPI.OrderCreateResponse{..} = resp

          shouldBeAnn "status"          status          $ NEW
          shouldBeAnn "status_id"       status_id       $ 10
          shouldBeAnn "order_id"        order_id        $ "orderId2"
          shouldBeAnn "udf10"           udf10           $ Just "udf10"
          shouldBeAnn "udf9"            udf9            $ Just "udf9"
          shouldBeAnn "udf8"            udf8            $ Just "udf8"
          shouldBeAnn "udf7"            udf7            $ Just "udf7"
          shouldBeAnn "udf6"            udf6            $ Just "udf6"
          shouldBeAnn "udf5"            udf5            $ Just "udf5"
          shouldBeAnn "udf4"            udf4            $ Just "udf4"
          shouldBeAnn "udf3"            udf3            $ Just "udf3"
          shouldBeAnn "udf2"            udf2            $ Just "udf2"
          shouldBeAnn "udf1"            udf1            $ Just "udf1"
          shouldBeAnn "return_url"      return_url      $ Just "http://example.com"
          shouldBeAnn "refunded"        refunded        $ Just False
          shouldBeAnn "product_id"      product_id      $ Just "prodId"
          shouldBeAnn "merchant_id"     merchant_id     $ Just "1"

          shouldBeAnn "customer_phone"  customer_phone  $ Just ""
          shouldBeAnn "customer_id"     customer_id     $ Just ""
          shouldBeAnn "customer_email"  customer_email  $ Just ""

          shouldBeAnn "currency"        currency        $ Just "INR"
          shouldBeAnn "amount_refunded" amount_refunded $ Just 0.0
          shouldBeAnn "amount"          amount          $ Just 1000.0

----------------------------------------------------------------------

      it "OrderCreate. New version. customer_id = Nothing" $ \rt -> do
        let rp = collectRPs
              (Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI=")
              (Version "2018-07-01")
              (UserAgent "Uagent")

        resp <- runOrderCreate rt ordReq{customer_id = Nothing} rp

        do
          let OrderAPI.OrderCreateResponse{..} = resp

          shouldBeAnn "status"          status          $ NEW
          shouldBeAnn "status_id"       status_id       $ 10
          shouldBeAnn "order_id"        order_id        $ "orderId2"
          shouldBeAnn "udf10"           udf10           $ Just "udf10"
          shouldBeAnn "udf9"            udf9            $ Just "udf9"
          shouldBeAnn "udf8"            udf8            $ Just "udf8"
          shouldBeAnn "udf7"            udf7            $ Just "udf7"
          shouldBeAnn "udf6"            udf6            $ Just "udf6"
          shouldBeAnn "udf5"            udf5            $ Just "udf5"
          shouldBeAnn "udf4"            udf4            $ Just "udf4"
          shouldBeAnn "udf3"            udf3            $ Just "udf3"
          shouldBeAnn "udf2"            udf2            $ Just "udf2"
          shouldBeAnn "udf1"            udf1            $ Just "udf1"
          shouldBeAnn "return_url"      return_url      $ Just "http://example.com"
          shouldBeAnn "refunded"        refunded        $ Just False
          shouldBeAnn "product_id"      product_id      $ Just "prodId"
          shouldBeAnn "merchant_id"     merchant_id     $ Just "1"

          shouldBeAnn "customer_phone"  customer_phone  $ Just "" -- same here ???
          shouldBeAnn "customer_id"     customer_id     $ Just "" -- We can get Just customerId, Nothing, Just ""
          shouldBeAnn "customer_email"  customer_email  $ Just "" -- same here ???

          shouldBeAnn "currency"        currency        $ Just "INR"
          shouldBeAnn "amount_refunded" amount_refunded $ Just 0.0
          shouldBeAnn "amount"          amount          $ Just 1000.0

----------------------------------------------------------------------

      it "Order with ordReqForValidators" $ \rt -> do
        let rp = collectRPs
              (Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI=")
              (Version "2018-07-01")
              (UserAgent "Uagent")

        let check Errs.ECErrorResponse{code, response} =
              case A.decode response of
                Just Errs.ECErrorPayload{error_code} ->
                  code == 400 && error_code == Just "INVALID_REQUEST"
                Nothing -> False

        runOrderCreate rt ordReqForValidators rp `shouldThrow` check


----------------------------------------------------------------------

      xit "Order with ordReqMandateFeatureFailData" $ \rt -> do
        let rp = collectRPs
              (Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI=")
              (Version "2018-07-01")
              (UserAgent "Uagent")

        let check Errs.ECErrorResponse{code, response} =
              case A.decode response of
                Just Errs.ECErrorPayload{error_code} ->
                  code == 400 && error_code == Just "INVALID_MANDATE_MAX_AMOUNT"
                Nothing -> False

        runOrderCreate rt ordReqMandateFeatureFailData rp `shouldThrow` check

----------------------------------------------------------------------

-- OrderReference get from DB

-- OrderReference
--   { id = Just 1
--   , version = 1
--   , amount = Just 10.0
--   , currency = Just INR
--   , dateCreated = 1858-11-18 01:01:01
--   , lastModified = 1858-11-18 01:01:01
--   , merchantId = Just "1"
--   , orderId = Just "orderId"
--   , status = CREATED
--   , customerEmail = Just "customer@email.com"
--   , customerId = Just "customerId"
--   , browser = Nothing
--   , browserVersion = Nothing
--   , popupLoaded = Nothing
--   , popupLoadedTime = Nothing
--   , description = Just "some description"
--   , udf1 = Just "udf1"
--   , udf2 = Nothing
--   , udf3 = Nothing
--   , udf4 = Nothing
--   , udf5 = Nothing
--   , udf6 = Nothing
--   , udf7 = Nothing
--   , udf8 = Nothing
--   , udf9 = Nothing
--   , udf10 = Just "udf10"
--   , returnUrl = Nothing
--   , amountRefunded = Nothing
--   , refundedEntirely = Nothing
--   , preferredGateway = Nothing
--   , customerPhone = Nothing
--   , productId = Nothing
--   , billingAddressId = Nothing
--   , shippingAddressId = Nothing
--   , orderUuid = Just "order_uuid123"
--   , lastSynced = Nothing
--   , orderType = Nothing
--   , mandateFeature = Nothing
--   , autoRefund = Nothing
--   }

-- Validation failes internally due to

-- ["orderType not present","mandateFeature not present"]

      xit "Order with ordReqExisted" $ \rt -> do
        let rp = collectRPs
              (Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI=")
              (Version "2018-07-01")
              (UserAgent "Uagent")

        resp <- runOrderCreate rt ordReqExisted rp
        resp `shouldBe` ordCreateResp

----------------------------------------------------------------------

      it "Order with ordReqMandateFeatureTooBigMandateMaxAmount" $ \rt -> do
        let rp = collectRPs
                (Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI=")
                (Version "2018-07-01")
                (UserAgent "Uagent")


        let check Errs.ECErrorResponse{code, response} =
              case A.decode response of
                Just Errs.ECErrorPayload{error_code} ->
                  code == 400 && error_code == Just "INVALID_MANDATE_MAX_AMOUNT"
                Nothing -> False

        runOrderCreate rt ordReqMandateFeatureTooBigMandateMaxAmount rp `shouldThrow` check

----------------------------------------------------------------------

      xit "Order UDF cleaned" $ \rt -> do
          let rp = collectRPs
                (Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI=")
                (Version "2018-07-01")
                (UserAgent "Uagent")

          resp <- runOrderCreate rt ordReqUdf rp

          do
            let OrderAPI.OrderCreateResponse{..} = resp

            shouldBeAnn "status"      status      $ CREATED
            shouldBeAnn "status_id"   status_id   $ 1
            shouldBeAnn "order_id"    order_id    $ "udfId"
            shouldBeAnn "refunded"    refunded    $ Just False
            shouldBeAnn "merchant_id" merchant_id $ Just "1"
            shouldBeAnn "currency"    currency    $ Just "INR"
            shouldBeAnn "amount"      amount      $ Just 100.0

            shouldBeAnn "udf10"       udf10       $ Just ""
            shouldBeAnn "udf9"        udf9        $ Just ""
            shouldBeAnn "udf8"        udf8        $ Just ""
            shouldBeAnn "udf7"        udf7        $ Just ""
            shouldBeAnn "udf6"        udf6        $ Just ""
            shouldBeAnn "udf5"        udf5        $ Nothing
            shouldBeAnn "udf4"        udf4        $ Nothing
            shouldBeAnn "udf3"        udf3        $ Nothing
            shouldBeAnn "udf2"        udf2        $ Nothing
            shouldBeAnn "udf1"        udf1        $ Nothing

            shouldStartWithAnn "id" (T.unpack id) "ordeu_"

----------------------------------------------------------------------

      it "Order with addresses" $ \rt -> do
          let rp = collectRPs
                (Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI=")
                (Version "2018-07-01")
                (UserAgent "Uagent")

          resp <- runOrderCreate rt ordReqWithAddresses rp

          do
            let OrderAPI.OrderCreateResponse{..} = resp

            shouldBeAnn "status"      status      $ CREATED
            shouldBeAnn "status_id"   status_id   $ 1
            shouldBeAnn "order_id"    order_id    $ "orderWithAddr"
            shouldBeAnn "refunded"    refunded    $ Just False
            shouldBeAnn "merchant_id" merchant_id $ Just "1"
            shouldBeAnn "currency"    currency    $ Just "INR"
            shouldBeAnn "amount"      amount      $ Just 100.0

            shouldStartWithAnn "id" (T.unpack id) "ordeu_"

-- ----------------------------------------------------------------------

      it "Order with addresses. Valid customerId" $ \rt -> do
          let rp = collectRPs
                (Authorization "BASIC RjgyRjgxMkFBRjI1NEQ3QTlBQzgxNEI3OEE0Qjk0MUI=")
                (Version "2018-07-01")
                (UserAgent "Uagent")

          resp <- runOrderCreate rt ordReqWithAddresses{customer_id = Just "1"} rp

          do
            let OrderAPI.OrderCreateResponse{..} = resp

            shouldBeAnn "status"         status          $ CREATED
            shouldBeAnn "status_id"      status_id       $ 1
            shouldBeAnn "order_id"       order_id        $ "orderWithAddr"
            shouldBeAnn "refunded"       refunded        $ Just False
            shouldBeAnn "merchant_id"    merchant_id     $ Just "1"
            shouldBeAnn "currency"       currency        $ Just "INR"
            shouldBeAnn "amount"         amount          $ Just 100.0
            shouldBeAnn "customer_phone" customer_phone  $ Just "555-555-555"

            shouldStartWithAnn "id" (T.unpack id) "ordeu_"
