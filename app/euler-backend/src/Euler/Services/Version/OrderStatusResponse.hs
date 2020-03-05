module Euler.Services.Version.OrderStatusResponse
  ( OrderStatusService(..)
  , mkOrderStatusService
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


transformOrderStatus' :: Version  -> C.GatewayId -> OrderStatusResponse -> OrderStatusResponse
transformOrderStatus' version gwId
  = setRefunds' (mkRefundService version)
  . setPaymentGatewayResponse' (mkMerchantPGRService version gwId)
  . setChargebacks' version
  . setTxnDetail' version
  . setGatewayReferenceId' version

tokenizeOrderStatusResponse' :: Version -> C.OrderPId -> C.MerchantId -> OrderStatusResponse -> Flow OrderStatusResponse
tokenizeOrderStatusResponse' version orderPId merchantId orderStatusResp = do
  token <- getToken (mkOrderTokenRespService version) orderPId merchantId
  pure $ updateResponse' version token orderStatusResp

data OrderStatusService = OrderStatusService
  { transformOrderStatus :: C.GatewayId -> OrderStatusResponse -> OrderStatusResponse
  , tokenizeOrderStatusResponse :: C.OrderPId -> C.MerchantId -> OrderStatusResponse -> Flow OrderStatusResponse
  }


mkOrderStatusService :: Version -> OrderStatusService
mkOrderStatusService version = OrderStatusService
  { transformOrderStatus = transformOrderStatus' version
  , tokenizeOrderStatusResponse = tokenizeOrderStatusResponse' version
  }

updateResponse' :: Version -> Maybe OrderTokenResp -> OrderStatusResponse -> OrderStatusResponse
updateResponse' version
  | version >= "2018-07-01" = setField @"juspay"
  | otherwise = \ _ orderStatusResp  -> orderStatusResp

setRefunds' :: RefundService -> OrderStatusResponse -> OrderStatusResponse
setRefunds' RefundService{transformRefunds} orderStatus
  | length newRefundStatuses > 0 = setField @"refunds" (Just newRefundStatuses) orderStatus
  | otherwise = orderStatus
  where
    newRefundStatuses = transformRefunds
     $ fromMaybe [] $ getField @"refunds" orderStatus

setPaymentGatewayResponse' :: MerchantPGRService -> OrderStatusResponse -> OrderStatusResponse
setPaymentGatewayResponse' MerchantPGRService{..} otderStatus = setField @"payment_gateway_response" mpgr otderStatus
  where
    mpgr = transformMPGR <$> getField @"payment_gateway_response" otderStatus

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
