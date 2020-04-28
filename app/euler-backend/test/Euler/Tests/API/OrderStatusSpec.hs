module Euler.Tests.API.OrderStatusSpec where

import           EulerHS.Prelude
import           Test.Hspec

import           Euler.Tests.Common
import           Euler.AppEnv

import           EulerHS.Interpreters
import           EulerHS.Language
import           EulerHS.Runtime
import           EulerHS.Types

import           Euler.API.RouteParameters
import qualified Euler.API.Order as Api
import qualified Euler.API.MerchantPaymentGatewayResponse as M

import qualified Euler.Common.Types as C

import qualified EulerHS.Types as T
import qualified WebService.Language as L
import qualified WebService.Types as T

import           Database.MySQL.Base
import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Common.Errors.Types as Errs
import qualified Euler.Options.Options as Opt
import           System.Process
import           Test.HUnit.Base
import           EulerHS.Extra.Test


testDBName :: String
testDBName = "order_status_spec_test_db"

spec :: Spec
spec = do
  let prepare next =
        withMysqlDb testDBName "test/Euler/TestData/mysqldump.sql" (mySQLRootCfg testDBName) $
          withFlowRuntime Nothing $ \rt -> do
            runFlow rt $ prepareDBConnections
            next rt

  around prepare $
    describe "API Order methods" $ do

--------------------------------------------------------------------

      it "OrderStatus. Authorization - invalid" $ \rt -> do
        let rp = collectRPs
              (OrderId ordId1)
              (Authorization "BASIC definitelynotvalidbase64string=")
              (Version "2017-07-01")
              (UserAgent "Uagent")
        let err = Errs.eulerAccessDenied "Invalid API key."

        res <- try $ runOrderStatus rt rp False
        res `shouldBe` Left err

      it "OrderStatus. Authorization - valid. Get OrderStatusResponse" $ \rt -> do
        (res :: Either Errs.ErrorResponse Api.OrderStatusResponse) <- try $ runOrderStatus rt rps1 False
        res `shouldBe` Right orderStatusResponse


ordId1 :: C.OrderId
ordId1 = "1475240639"

rps1 :: RouteParameters
rps1 = collectRPs
  (OrderId ordId1)
  -- Use https://www.base64encode.org/ to get base64 from api_key
  (Authorization "BASIC RTI5QTgzOEU0Qjc2NDM2RThBMkM2NjBBMDYwOTlGRUU=")
  (Version "2017-07-01")
  (UserAgent "Uagent")

orderStatusResponse :: Api.OrderStatusResponse
orderStatusResponse = Api.OrderStatusResponse
  {id = "ord_7f3b93351e004877aea384d15f67f05d", merchant_id = Just "altair", amount = Just 1.0, currency = Just "INR", order_id = Just "1475240639", date_created = "2016-09-30 17:53:39", return_url = Just "", product_id = "", customer_email = Just "azharamin_user_101@gmail.com", customer_phone = Nothing, customer_id = Just "azharamin_user_101", payment_links = Api.Paymentlinks {iframe = Just "http://api.juspay.in/merchant/ipay/ord_7f3b93351e004877aea384d15f67f05d", web = Just "http://api.juspay.in/merchant/pay/ord_7f3b93351e004877aea384d15f67f05d", mobile = Just "http://api.juspay.in/merchant/pay/ord_7f3b93351e004877aea384d15f67f05d?mobile=true"}, udf1 = "udf1", udf2 = "udf2", udf3 = "udf3", udf4 = "udf4", udf5 = "udf5", udf6 = "udf6", udf7 = "udf7", udf8 = "udf8", udf9 = "udf9", udf10 = "udf10", txn_id = Just "azharamin-1475240639-8", status_id = 20, status = "STARTED", payment_method_type = Nothing, auth_type = Just "", card = Nothing, payment_method = Nothing, refunded = Just False, amount_refunded = Just 0.0, chargebacks = Nothing, refunds = Just [], mandate = Nothing, promotion = Nothing, risk = Nothing, bank_error_code = Just "", bank_error_message = Just "", txn_uuid = Just "txn_b0adad7c0614471d81b265b1289b675a", gateway_payload = Just "", txn_detail = Nothing, payment_gateway_response' = Nothing, payment_gateway_response = Just (M.MerchantPaymentGatewayResponse {resp_code = Just "success", rrn = Just "success", created = Just "success", epg_txn_id = Just "success", resp_message = Just "success", auth_id_code = Just "success", txn_id = Just "success", offer = Nothing, offer_type = Nothing, offer_availed = Nothing, discount_amount = Nothing, offer_failure_reason = Nothing, gateway_response = Nothing}), gateway_id = Just 12, emi_bank = Nothing, emi_tenure = Nothing, gateway_reference_id = Nothing, payer_vpa = Nothing, payer_app_name = Nothing, juspay = Nothing, second_factor_response = Nothing, txn_flow_info = Nothing}

runOrderStatus :: FlowRuntime -> RouteParameters -> Bool -> IO Api.OrderStatusResponse
runOrderStatus rt rps isAsync = runFlow rt $ orderStatusMethod (mkAppEnv' isAsync) rps T.emptyReq

prepareDBConnections :: Flow ()
prepareDBConnections = do
  let cfg = T.mkMySQLConfig "eulerMysqlDB" (mySQLCfg testDBName)

  ePool <- initSqlDBConnection cfg
  setOption Opt.EulerDbCfg cfg

  redis <- initKVDBConnection
    $ T.mkKVDBConfig redisConn $ redisConnConfig

  L.throwOnFailedWithLog ePool T.SqlDBConnectionFailedException "Failed to connect to SQLite DB."
  L.throwOnFailedWithLog redis T.KVDBConnectionFailedException "Failed to connect to Redis DB."

-- shouldBeAnn ::
{-# INLINE shouldBeAnn #-}
shouldBeAnn :: (Eq a, Show a) => String -> a -> a -> Assertion
shouldBeAnn s a b = assertEqual s b a
