module Euler.Services.OrderStatus
  ( OrderStatusService(..)
  , defaultOrderStatusService
  )
  where

import EulerHS.Prelude

import           EulerHS.Language
import qualified Euler.API.Order           as API
import qualified Euler.API.RouteParameters as RP
import qualified Euler.Product.Domain      as D


getOrderStatusResponse' :: Text
  -> D.MerchantAccount
  -> Bool
  -> RP.RouteParameters
  ->Flow API.OrderStatusResponse
getOrderStatusResponse' orderId mAcc isAuth rp = do
  let ordStatReq = getEmptyOrderStatusRequest' orderId
  ordStatusResp <- getOrdStatusResp' ordStatReq mAcc isAuth rp
  void $ addOrderStatusResponseToCache' ordStatReq isAuth mAcc rp ordStatusResp
  pure ordStatusResp

data OrderStatusService = OrderStatusService
  { getOrdStatusResp :: API.OrderStatusRequestLegacy -- default with current order orderId
                        --  getOrderStatusRequest
                        --  from src/Types/Communication/OLTP/OrderStatus.purs
                     -> D.MerchantAccount
                     -> Bool -- true
                     -> RP.RouteParameters
                     -> Flow API.OrderStatusResponse
  , addOrderStatusResponseToCache :: API.OrderStatusRequestLegacy
                                  -> Bool
                                  -> D.MerchantAccount
                                  -> RP.RouteParameters
                                  -> API.OrderStatusResponse
                                  -> Flow ()
  , getEmptyOrderStatusRequest :: Text -> API.OrderStatusRequestLegacy
  }


defaultOrderStatusService :: OrderStatusService
defaultOrderStatusService = OrderStatusService
  { getOrderStatusResponse = getOrderStatusResponse'
  }



--  getOrderStatusRequest
--  from src/Types/Communication/OLTP/OrderStatus.purs
getEmptyOrderStatusRequest' :: Text -> API.OrderStatusRequestLegacy
getEmptyOrderStatusRequest' orderId = API.OrderStatusRequestLegacy
  { txn_uuid =  Nothing
  , merchant_id =  Nothing
  , order_id =  (Just orderId)
  , txnUuid =  Nothing
  , merchantId =  Nothing
  , orderId =  Nothing
 -- , "options.add_full_gateway_response" = Nothing
  }

getOrdStatusResp' = undefined

addOrderStatusResponseToCache' = undefined
