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
import qualified Euler.API.Order as API (OrderStatusResponse, OrderUpdateRequest)
import qualified Euler.API.Validators.Order as VO

import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Common.Types                   as C
import           Euler.Lens
import qualified Euler.Product.Domain.OrderStatusResponse as DO
import qualified Euler.Product.Domain                 as D
import qualified Euler.Product.Domain.Templates       as Ts


-- API
import qualified Euler.Product.OLTP.Order.UpdateApi           as UpdateApi

-- deps
import qualified Euler.Product.Cache.CacheApi                 as CacheApi
import qualified Euler.Product.OLTP.Order.StatusApi           as StatusApi
import qualified Euler.Product.OLTP.Services.Auth.AuthService as AuthApi



--import qualified Euler.Services.Version.OrderStatusResponse as VSrv
import qualified Euler.Storage.Repository as Rep

import           WebService.Language



-- | So called "implementation handle" to hold all dependencies together
data IHandle = IHandle
  { authServiceH      :: AuthApi.SHandle
  , orderStatusCacheH :: CacheApi.SHandle
  , orderStatusH      :: StatusApi.SHandle
  }

mkHandle :: IHandle -> UpdateApi.SHandle
mkHandle iHandle =
  UpdateApi.SHandle
  { updateOrder = \rps req -> do
      let AuthApi.SHandle { authenticate } = authServiceH iHandle
      authResult <- authenticate rps
      case (authResult) of
        Left err -> do
          logErrorT "AuthService" $ "authentication failed with error: " <> err
          throwException Errs.internalError
        Right macc ->
          runOrderUpdate (orderStatusH iHandle) (orderStatusCacheH iHandle) rps req macc
  }


-- ----------------------------------------------------------------------------


-- EHS: since original update function consumed orderCreate type - fields "amount" and "order_id"
-- was mandatory. But what if we dont whant to update amount? Also we dont need order_id field
-- because we take order_id from route parameters.
-- We can update only amount, address and UDF fields.
-- All this fields in new OrderUpdateRequest type are optional.

-- EHS: we did not check amount like in orderCreate it's ok?

runOrderUpdate
  :: StatusApi.SHandle
  -> CacheApi.SHandle
  -> RP.RouteParameters
  -> API.OrderUpdateRequest
  -> D.MerchantAccount
  -> Flow API.OrderStatusResponse
runOrderUpdate sHandle cHandle routeParams req mAcc = do
  -- EHS: use maybe as it is
  --let version = fromMaybe "" $ RP.lookupRP @RP.Version routeParams
  --let service = VSrv.mkOrderStatusService version
  case VO.apiOrderUpdToOrderUpdT req of
    V.Failure err -> do
      logErrorT "OrderUpdateRequest validation" $ show err
      throwException $ Errs.mkValidationError err
    V.Success validatedOrder -> orderUpdate sHandle cHandle routeParams validatedOrder mAcc

orderUpdate :: StatusApi.SHandle
            -> CacheApi.SHandle
            -> RP.RouteParameters
            -> Ts.OrderUpdateTemplate
            -> D.MerchantAccount
            -> Flow API.OrderStatusResponse
orderUpdate
  StatusApi.SHandle { statusByRequest }
  cacheH
  routeParams
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
        doOrderUpdate cacheH orderUpdateT order' mAccnt
        callOrderStatus orderId' merchantId'
      Nothing -> throwException $ Errs.orderDoesNotExist orderId'
    logInfoT "order update response: " $ show resp
    pure resp
  where
    callOrderStatus orderId merchantId = do
      -- EHS: provide a "default" template for query?
      let query = D.OrderStatusRequest
            { orderId = orderId
            , merchantId = merchantId
            , merchantReturnUrl = mAccnt ^. _returnUrl
            , resellerId = mAccnt ^. _resellerId
            , isAuthenticated = True   -- EHS: is it the case?
            , sendCardIsin = False
            , sendFullGatewayResponse = False
            , sendAuthToken = True
            , version = RP.lookupRP @RP.Version routeParams
            }
      statusByRequest query


doOrderUpdate
  :: CacheApi.SHandle
  -> Ts.OrderUpdateTemplate
  -> D.Order
  -> D.MerchantAccount
  -> Flow ()
doOrderUpdate
  CacheApi.SHandle { invalidateCache }
  orderUpdateT
  order@D.Order {..}
  mAccnt = do
    case orderStatus of
      C.OrderStatusSuccess -> do
        logErrorT "not_updating_successful_order"
          $ Text.pack("Order: " <> P.show ( orderId) <> " has already succeeded. Not updating any field.")
      C.OrderStatusAutoRefunded ->
        logErrorT "not_updating_auto_refunded_order"
          $ Text.pack ("Order: " <> P.show ( orderId) <> " has already been autoRefunded. Not updating any field.")
      _ ->  do
        let mNewAmount = getField @"amount" orderUpdateT
        let newUDF = orderUpdateT ^. _udf
        mbCustomer <- Rep.loadCustomer customerId (mAccnt ^. _id)
        billingAddressId' <- Rep.updateAddress mbCustomer billingAddressId (orderUpdateT ^. _billingAddr) (orderUpdateT ^. _billingAddrHolder)
        shippingAddressId' <- Rep.updateAddress Nothing shippingAddressId (orderUpdateT ^. _shippingAddr) (orderUpdateT ^. _shippingAddrHolder)
        Rep.updateOrder (order ^. _id) newUDF mNewAmount billingAddressId' shippingAddressId'
        invalidateCache (order ^. _orderId) (order ^. _merchantId)
