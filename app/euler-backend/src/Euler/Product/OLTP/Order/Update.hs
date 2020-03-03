{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Euler.Product.OLTP.Order.Update where

import EulerHS.Prelude hiding (id, get)
import EulerHS.Language
import WebService.Language

import           Data.Generics.Product.Fields
import           Data.Generics.Product.Subtype

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Prelude  as P (show)

import Euler.Product.OLTP.Services.RedisService


-- EHS: Should not depend on API?
import qualified Euler.API.RouteParameters  as RP
import qualified Euler.API.Order as API (OrderStatusResponse, defaultOrderStatusResponse)

import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Common.Types                   as C
-- import qualified Euler.Common.Metric                  as Metric
import qualified Euler.Product.Domain                 as D
import qualified Euler.Product.Domain.Templates       as Ts
import qualified Euler.Services.OrderStatus           as OSSrv
import qualified Euler.Services.Version.OrderStatusResponse as VSrv
import qualified Euler.Storage.Repository as Rep

import Euler.Lens
-- Beam
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import Database.Beam ((==.), (&&.), (||.), (<-.), (/=.))

-- EHS: since original update function consumed orderCreate type - fields "amount" and "order_id"
-- was mandatory. But what if we dont whant to update amount? Also we dont need order_id field
-- because we take order_id from route parameters.
-- We can update only amount, address and UDF fields.
-- All this fields in new OrderUpdateRequest type are optional.

-- EHS: we did not check amount like in orderCreate it's ok?

orderUpdate :: RP.RouteParameters -> Ts.OrderUpdateTemplate -> D.MerchantAccount -> Flow API.OrderStatusResponse
orderUpdate  routeParams orderUpdateT mAccnt = do
  let merchantId' = getField @"merchantId" mAccnt
  orderId' <- maybe
    (throwException Errs.orderIdNotFoundInPath)
    pure
    $ RP.lookupRP @RP.OrderId routeParams
  (mOrder :: Maybe D.Order) <- Rep.loadOrder orderId' merchantId'
  resp <- case mOrder of
    Just order' -> do
      doOrderUpdate orderUpdateT order' mAccnt
      resp' <- OSSrv.getOrderStatusResponse
        OSSrv.defaultOrderStatusService
        orderId'
        mAccnt
        True
        routeParams
      let version = fromMaybe "" $ RP.lookupRP @RP.Version routeParams
      let gatewayId = fromMaybe 0 $ resp' ^. _gateway_id
      pure $ VSrv.transformOrderStatus
        (VSrv.mkOrderStatusService version gatewayId) resp'
    Nothing -> throwException $ Errs.orderDoesNotExist orderId'
  logInfo "order update response: " $ show resp
  pure API.defaultOrderStatusResponse --resp

doOrderUpdate :: Ts.OrderUpdateTemplate -> D.Order -> D.MerchantAccount -> Flow ()
doOrderUpdate orderUpdateT order@D.Order {..}  mAccnt = do
  case orderStatus of
    C.OrderStatusSuccess -> do
      logError "not_updating_successful_order" $ Text.pack("Order: " <> P.show ( orderId) <> " has already succeeded. Not updating any field.")
    C.OrderStatusAutoRefunded ->
      logError "not_updating_auto_refunded_order" $ Text.pack ("Order: " <> P.show ( orderId) <> " has already been autoRefunded. Not updating any field.")
    _ ->  do
      let mNewAmount = getField @"amount" orderUpdateT
      let newUDF = orderUpdateT ^. _udf
      mbCustomer <- Rep.loadCustomer customerId (mAccnt ^. _id)
      billingAddressId' <- Rep.updateAddress mbCustomer billingAddressId (orderUpdateT ^. _billingAddr) (orderUpdateT ^. _billingAddrHolder)
      shippingAddressId' <- Rep.updateAddress Nothing shippingAddressId (orderUpdateT ^. _shippingAddr) (orderUpdateT ^. _shippingAddrHolder)
      Rep.updateOrder (order ^. _id) newUDF mNewAmount billingAddressId' shippingAddressId'
      invalidateOrderStatusCache (order ^. _orderId) (order ^. _merchantId)
