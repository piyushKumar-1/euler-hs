module Euler.Version.Services.OrderStatusResponse
  ( OrderStatusService
  , mkOrderStatusService
  , transformOrderStatus
  )
  where

import EulerHS.Prelude
import qualified EulerHS.Prelude as P

import           Data.Generics.Product.Fields

import Euler.API.Order (OrderStatusResponse(..))
import Euler.Version.Services.MerchantPaymentGatewayResponse
import Euler.Version.Services.Refund

-- move to common types?
-- change to newtype?
type Version = Text
type GatewayId = Int

transformOrderStatus :: OrderStatusService -> OrderStatusResponse -> OrderStatusResponse
transformOrderStatus OrderStatusService{..}
  = setRefunds
  . setPaymentGatewayResponse
  . setChargebacks
  . setTxnDetail
  . setGatewayReferenceId

data OrderStatusService = OrderStatusService
  { setRefunds :: OrderStatusResponse -> OrderStatusResponse
  , setPaymentGatewayResponse :: OrderStatusResponse -> OrderStatusResponse
  , setChargebacks :: OrderStatusResponse -> OrderStatusResponse
  , setTxnDetail :: OrderStatusResponse -> OrderStatusResponse
  , setGatewayReferenceId :: OrderStatusResponse -> OrderStatusResponse
  }

mkOrderStatusService :: Version -> GatewayId -> OrderStatusService
mkOrderStatusService version gwId = OrderStatusService
  { setRefunds = setRefunds' (mkRefundService version)
  , setPaymentGatewayResponse = setPaymentGatewayResponse' (mkMerchantPGRService version gwId)
  , setChargebacks = setChargebacks' version
  , setTxnDetail = setTxnDetail' version
  , setGatewayReferenceId = setGatewayReferenceId' version
  }

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