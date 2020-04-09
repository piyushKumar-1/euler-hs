{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Euler.Product.OLTP.Order.Update where

import EulerHS.Prelude hiding (id, get)

import           EulerHS.Language
import qualified EulerHS.Extra.Validation as V

import           Data.Generics.Product.Fields

import qualified Data.Text as Text
import qualified Prelude  as P (show)

-- EHS: Should not depend on API?
import qualified Euler.API.RouteParameters  as RP
import qualified Euler.API.Order as API (OrderStatusResponse, OrderUpdateRequest, defaultOrderStatusResponse)
import qualified Euler.API.Validators.Order as VO

import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Common.Types                   as C
-- import qualified Euler.Common.Metric                  as Metric
import qualified Euler.Product.Domain.OrderStatusResponse as DO
import qualified Euler.Product.Domain                 as D
import qualified Euler.Product.Domain.Templates       as Ts
import qualified Euler.Product.OLTP.Services.OrderStatusCacheService as OSCS
--import qualified Euler.Services.OrderStatus           as OSSrv

-- EHS: shall we abstract this as a service?
import qualified Euler.Product.OLTP.Order.OrderStatus   as OrderStatus

--import qualified Euler.Services.Version.OrderStatusResponse as VSrv
import qualified Euler.Storage.Repository as Rep

import Euler.Lens


-- EHS: since original update function consumed orderCreate type - fields "amount" and "order_id"
-- was mandatory. But what if we dont whant to update amount? Also we dont need order_id field
-- because we take order_id from route parameters.
-- We can update only amount, address and UDF fields.
-- All this fields in new OrderUpdateRequest type are optional.

-- EHS: we did not check amount like in orderCreate it's ok?

runOrderUpdate :: RP.RouteParameters
  -> API.OrderUpdateRequest
  -> D.MerchantAccount
  -> Flow API.OrderStatusResponse
runOrderUpdate routeParams req mAcc = do
  -- EHS: use maybe as it is
  --let version = fromMaybe "" $ RP.lookupRP @RP.Version routeParams
  --let service = VSrv.mkOrderStatusService version
  case VO.apiOrderUpdToOrderUpdT req of
    V.Failure err -> do
      logError @String "OrderUpdateRequest validation" $ show err
      throwException $ Errs.mkValidationError err
    V.Success validatedOrder -> orderUpdate routeParams validatedOrder mAcc

orderUpdate :: RP.RouteParameters
            -- -> VSrv.OrderStatusService
            -> Ts.OrderUpdateTemplate
            -> D.MerchantAccount
            -> Flow API.OrderStatusResponse
orderUpdate
 routeParams
 -- VSrv.OrderStatusService{transformOrderStatus}
 orderUpdateT
 mAccnt = do
    let merchantId' = getField @"merchantId" mAccnt
    orderId' <- maybe
      (throwException Errs.orderIdNotFoundInPath)
      pure
      $ RP.lookupRP @RP.OrderId routeParams
    (mOrder :: Maybe D.Order) <- Rep.loadOrder orderId' merchantId'
    resp <- case mOrder of
      Just order' -> do
        doOrderUpdate orderUpdateT order' mAccnt
        callOrderStatus orderId' merchantId'
      Nothing -> throwException $ Errs.orderDoesNotExist orderId'
    logInfo @String "order update response: " $ show resp
    pure API.defaultOrderStatusResponse --resp
  where
    callOrderStatus orderId merchantId = do
      -- EHS: provide a "default" template for query?
      let query = D.OrderStatusRequest
            { orderId = orderId
            , merchantId = merchantId
            , resellerId = Nothing
            , isAuthenticated = True   -- EHS: is it the case?
            , sendCardIsin = False
            , sendFullGatewayResponse = False
            , sendAuthToken = True
            , version = RP.lookupRP @RP.Version routeParams
            }
      OrderStatus.orderStatusRequest query


doOrderUpdate :: Ts.OrderUpdateTemplate -> D.Order -> D.MerchantAccount -> Flow ()
doOrderUpdate orderUpdateT order@D.Order {..}  mAccnt = do
  case orderStatus of
    C.OrderStatusSuccess -> do
      logError @String "not_updating_successful_order" $ Text.pack("Order: " <> P.show ( orderId) <> " has already succeeded. Not updating any field.")
    C.OrderStatusAutoRefunded ->
      logError @String "not_updating_auto_refunded_order" $ Text.pack ("Order: " <> P.show ( orderId) <> " has already been autoRefunded. Not updating any field.")
    _ ->  do
      let mNewAmount = getField @"amount" orderUpdateT
      let newUDF = orderUpdateT ^. _udf
      mbCustomer <- Rep.loadCustomer customerId (mAccnt ^. _id)
      billingAddressId' <- Rep.updateAddress mbCustomer billingAddressId (orderUpdateT ^. _billingAddr) (orderUpdateT ^. _billingAddrHolder)
      shippingAddressId' <- Rep.updateAddress Nothing shippingAddressId (orderUpdateT ^. _shippingAddr) (orderUpdateT ^. _shippingAddrHolder)
      Rep.updateOrder (order ^. _id) newUDF mNewAmount billingAddressId' shippingAddressId'
      OSCS.invalidateCache (order ^. _orderId) (order ^. _merchantId)
