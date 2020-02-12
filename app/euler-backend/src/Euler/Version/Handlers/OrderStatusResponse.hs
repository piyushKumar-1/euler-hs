module Euler.Version.Handlers.OrderStatusResponse
  ( OrderStatusHandler
  , mkOrderStatusHandler
  , transformOrderStatus
  )
  where

import EulerHS.Prelude
import qualified EulerHS.Prelude as P

import           Data.Generics.Product.Fields

import Euler.API.Order (OrderStatusResponse(..))
import Euler.Version.Handlers.MerchantPaymentGatewayResponse
import Euler.Version.Handlers.Refund

-- move to common types?
-- change to newtype?
type Version = Text
type GatewayId = Int

transformOrderStatus :: OrderStatusHandler -> OrderStatusResponse -> OrderStatusResponse
transformOrderStatus OrderStatusHandler{..}
  = setRefunds
  . setPaymentGatewayResponse
  . setChargebacks
  . setTxnDetail
  . setGatewayReferenceId

data OrderStatusHandler = OrderStatusHandler
  { setRefunds :: OrderStatusResponse -> OrderStatusResponse
  , setPaymentGatewayResponse :: OrderStatusResponse -> OrderStatusResponse
  , setChargebacks :: OrderStatusResponse -> OrderStatusResponse
  , setTxnDetail :: OrderStatusResponse -> OrderStatusResponse
  , setGatewayReferenceId :: OrderStatusResponse -> OrderStatusResponse
  }

mkOrderStatusHandler :: Version -> GatewayId -> OrderStatusHandler
mkOrderStatusHandler version gwId = OrderStatusHandler
  { setRefunds = setRefunds' (mkRefundHandler version)
  , setPaymentGatewayResponse = setPaymentGatewayResponse' (mkMerchantPGRHandler version gwId)
  , setChargebacks = setChargebacks' version
  , setTxnDetail = setTxnDetail' version
  , setGatewayReferenceId = setGatewayReferenceId' version
  }

setRefunds' :: RefundHandler -> OrderStatusResponse -> OrderStatusResponse
setRefunds' rh orderStatus
  | length newRefundStatuses > 0 = setField @"refunds" (Just newRefundStatuses) orderStatus
  | otherwise = orderStatus
  where
    newRefundStatuses = transformRefunds rh
     $ fromMaybe [] $ getField @"refunds" orderStatus

setPaymentGatewayResponse' :: MerchantPGRHandler -> OrderStatusResponse -> OrderStatusResponse
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