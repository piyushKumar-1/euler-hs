module Euler.Services.OrderStatus
  ( OrderStatusService(..)
  , defaultOrderStatusService
  , getOrderStatusResponse
  )
  where

import EulerHS.Prelude

import           EulerHS.Language
import qualified Euler.API.Order           as API
import qualified Euler.API.RouteParameters as RP
import qualified Euler.Product.Domain      as D


getOrderStatusResponse :: OrderStatusService
  -> Text
  -> D.MerchantAccount
  -> Bool
  -> RP.RouteParameters
  ->Flow API.OrderStatusResponse
getOrderStatusResponse srv orderId mAcc isAuth rp = do
  let ordStatReq = getEmptyOrderStatusRequest srv orderId
  ordStatusResp <- getOrdStatusResp srv ordStatReq mAcc isAuth rp
  addOrderStatusResponseToCache srv ordStatReq isAuth mAcc rp ordStatusResp
  pure ordStatusResp

data OrderStatusService = OrderStatusService
  { getOrdStatusResp :: API.OrderStatusRequest -- default with current order orderId
                        --  getOrderStatusRequest
                        --  from src/Types/Communication/OLTP/OrderStatus.purs
                     -> D.MerchantAccount
                     -> Bool -- true
                     -> RP.RouteParameters
                     -> Flow API.OrderStatusResponse
  , addOrderStatusResponseToCache :: API.OrderStatusRequest
                                  -> Bool
                                  -> D.MerchantAccount
                                  -> RP.RouteParameters
                                  -> API.OrderStatusResponse
                                  -> Flow ()
  , getEmptyOrderStatusRequest :: Text -> API.OrderStatusRequest
  }

defaultOrderStatusService :: OrderStatusService
defaultOrderStatusService = OrderStatusService
  { getOrdStatusResp = getOrdStatusResp'
  , addOrderStatusResponseToCache = addOrderStatusResponseToCache'
  , getEmptyOrderStatusRequest = getEmptyOrderStatusRequest'
  }


--  getOrderStatusRequest
--  from src/Types/Communication/OLTP/OrderStatus.purs
getEmptyOrderStatusRequest' :: Text -> API.OrderStatusRequest
getEmptyOrderStatusRequest' orderId = API.OrderStatusRequest
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