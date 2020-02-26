module Euler.Services.Version.OrderStatusResponse
  ( OrderStatusService
  , mkOrderStatusService
  , transformOrderStatus
  )
  where

import EulerHS.Prelude
import qualified EulerHS.Prelude as P

import           EulerHS.Language

import           Data.Generics.Product.Fields

import Euler.API.Order (OrderStatusResponse(..), OrderTokenResp)
import Euler.Common.Types as C
import Euler.Services.Version.MerchantPaymentGatewayResponse
import Euler.Services.Version.OrderTokenResp
import Euler.Services.Version.Refund

-- move to common types?
-- change to newtype?
type Version = Text

-- -- Euler.Product.Domain.Order
type OrderPId = Int

-- -- Euler.Common.Types.Merchant
type MerchantId = Text

transformOrderStatus :: OrderStatusService -> OrderStatusResponse -> OrderStatusResponse
transformOrderStatus OrderStatusService{..}
  = setRefunds
  . setPaymentGatewayResponse
  . setChargebacks
  . setTxnDetail
  . setGatewayReferenceId

tokenizeOrderStatusResponse :: OrderStatusService -> C.OrderPId -> C.MerchantId -> OrderStatusResponse -> Flow OrderStatusResponse
tokenizeOrderStatusResponse OrderStatusService{..}  orderPId merchantId orderStatusResp = do
  token <- getToken tokenService orderPId merchantId
  pure $ updateResponse token orderStatusResp

data OrderStatusService = OrderStatusService
  { setRefunds :: OrderStatusResponse -> OrderStatusResponse
  , setPaymentGatewayResponse :: OrderStatusResponse -> OrderStatusResponse
  , setChargebacks :: OrderStatusResponse -> OrderStatusResponse
  , setTxnDetail :: OrderStatusResponse -> OrderStatusResponse
  , setGatewayReferenceId :: OrderStatusResponse -> OrderStatusResponse
  , tokenService :: OrderTokenRespService
  , updateResponse :: Maybe OrderTokenResp -> OrderStatusResponse -> OrderStatusResponse
  }

mkOrderStatusService :: Version -> C.GatewayId -> OrderStatusService
mkOrderStatusService version gwId = OrderStatusService
  { setRefunds = setRefunds' (mkRefundService version)
  , setPaymentGatewayResponse = setPaymentGatewayResponse' (mkMerchantPGRService version gwId)
  , setChargebacks = setChargebacks' version
  , setTxnDetail = setTxnDetail' version
  , setGatewayReferenceId = setGatewayReferenceId' version
  , tokenService = mkOrderTokenRespService version
  , updateResponse = updateResponse' version
  }

updateResponse' version
  | version >= "2018-07-01" = setField @"juspay"
  | otherwise = \ _ orderStatusResp  -> orderStatusResp

setRefunds' :: RefundService -> OrderStatusResponse -> OrderStatusResponse
setRefunds' rh orderStatus
  | length newRefundStatuses > 0 = setField @"refunds" (Just newRefundStatuses) orderStatus
  | otherwise = orderStatus
  where
    newRefundStatuses = transformRefunds rh
     $ fromMaybe [] $ getField @"refunds" orderStatus

setPaymentGatewayResponse' :: MerchantPGRService -> OrderStatusResponse -> OrderStatusResponse
setPaymentGatewayResponse' mpgrh otderStatus = setField @"payment_gateway_response" mpgr otderStatus
  where
    mpgr = transformMPGR mpgrh <$> getField @"payment_gateway_response" otderStatus

setChargebacks' :: Version -> OrderStatusResponse -> OrderStatusResponse
setChargebacks' version
  | (version < "2017-07-26"  || version == "") = setField @"chargebacks" Nothing
  | otherwise = P.id

setTxnDetail' :: Version -> OrderStatusResponse -> OrderStatusResponse
setTxnDetail' version
  | version < "2018-07-16"  || version == "" = setField @"txn_detail" Nothing
  | otherwise = P.id

setGatewayReferenceId' :: Version -> OrderStatusResponse -> OrderStatusResponse
setGatewayReferenceId' version
  | (version < "2018-10-25" || version == "") = setField @"gateway_reference_id" Nothing
  | otherwise = P.id
