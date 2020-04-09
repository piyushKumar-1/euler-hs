module Euler.Storage.Repository.Order
  ( loadOrder
  , loadOrderById
  , saveOrder
  , updateOrder
  )
  where

import EulerHS.Prelude hiding (id)

import           Euler.Storage.DBConfig
import           EulerHS.Language
import           WebService.Language
import qualified EulerHS.Extra.Validation as V

import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Common.Types                   as C
import qualified Euler.Product.Domain.Order           as D
import qualified Euler.Storage.Types                  as DB
import qualified Euler.Storage.Types.OrderReference   as DBO
import qualified Euler.Storage.Validators.Order       as SV

import           Database.Beam ((==.), (&&.), (<-.))
import qualified Database.Beam as B
import           Euler.Lens


-- EHS name `findOrder` would be more accurate
loadOrder :: C.OrderId -> C.MerchantId -> Flow (Maybe D.Order)
loadOrder orderId' merchantId' = do
    mbOrderRef <- withDB eulerDB $ do
      let predicate DB.OrderReference {orderId, merchantId} =
            (orderId    ==. B.just_ (B.val_ orderId')) &&.
            (merchantId ==. B.just_ (B.val_ merchantId'))
      findRow
        $ B.select
        $ B.limit_ 1
        $ B.filter_ predicate
        $ B.all_ (DB.order_reference DB.eulerDBSchema)
    case mbOrderRef of
      Nothing -> pure Nothing
      Just ordRef -> transOrder mbOrderRef

-- | Load an order by a surrogate ID value
loadOrderById :: Int -> Flow (Maybe D.Order)
loadOrderById id' = do
  mbOrderRef <- withDB eulerDB $ do
    let predicate DB.OrderReference {id} =
          (id ==. B.just_ (B.val_ id'))
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.order_reference DB.eulerDBSchema)
  transOrder mbOrderRef

transOrder :: Maybe DB.OrderReference -> Flow (Maybe D.Order)
transOrder mbOrderRef = do
  case mbOrderRef of
    Nothing     -> pure Nothing
    Just ordRef -> do
      case SV.transSOrderToDOrder ordRef of
        V.Success order -> pure $ Just order
        V.Failure e     -> do
          logErrorT "Incorrect order in DB"
            $  " id: "         <> (maybe "no value" show $ ordRef ^. _id)
            <> " orderId: "    <> (fromMaybe "no value" $ ordRef ^. _orderId)
            <> " merchantId: " <> (fromMaybe "no value" $ ordRef ^. _merchantId)
            <> " , validation errors: " <> show e
          throwException Errs.internalError

-- For compatibility with other backends, we should return types that we use together through Redis
-- If the returned data is not expected to be saved to the Redis, then only domain type should be returned.
saveOrder :: DB.OrderReference -> Flow (DB.OrderReference, D.Order)
saveOrder orderRefVal = do
  orderRef <- unsafeInsertRow (Errs.mkDBError "Inserting order reference failed.") eulerDB
    $ B.insert (DB.order_reference DB.eulerDBSchema)
    $ B.insertExpressions [(B.val_ orderRefVal) & _id .~ B.default_]

  -- EHS: should not happen, ideally.
  -- EHS: should we skip the validation and just return the orderRefVal updated with the id from insertion?
  case SV.transSOrderToDOrder orderRef of
    V.Success order -> pure (orderRef, order)
    V.Failure _ -> do
      logErrorT "orderCreate" $ "Unexpectedly got an invalid order reference after save: " <> show orderRef
      throwException Errs.internalError

updateOrder :: Int -> C.UDF -> Maybe C.Money -> Maybe C.AddressId -> Maybe C.AddressId -> Flow ()
updateOrder orderRefId newUdf mAmount mbBillingAddrId mbShippingAddrId = do
  currentDate' <- getCurrentTimeUTC
  withDB eulerDB
    $ updateRows
    $ B.update (DB.order_reference DB.eulerDBSchema)
      (    (\oRef -> DBO.amount oRef  <-. case (C.fromMoney <$> mAmount) of
                Just m -> B.val_ $ Just m
                Nothing -> (B.current_ (DBO.amount oRef))
           )
        <> (\oRef -> DBO.billingAddressId oRef <-. B.val_ mbBillingAddrId)
        <> (\oRef -> DBO.shippingAddressId oRef <-. B.val_ mbShippingAddrId)
        <> (\oRef -> DBO.udf1 oRef <-. B.val_ (C.udf1 newUdf))
        <> (\oRef -> DBO.udf2 oRef <-. B.val_ (C.udf2 newUdf))
        <> (\oRef -> DBO.udf3 oRef <-. B.val_ (C.udf3 newUdf))
        <> (\oRef -> DBO.udf4 oRef <-. B.val_ (C.udf4 newUdf))
        <> (\oRef -> DBO.udf5 oRef <-. B.val_ (C.udf5 newUdf))
        <> (\oRef -> DBO.udf6 oRef <-. B.val_ (C.udf6 newUdf))
        <> (\oRef -> DBO.udf7 oRef <-. B.val_ (C.udf7 newUdf))
        <> (\oRef -> DBO.udf8 oRef <-. B.val_ (C.udf8 newUdf))
        <> (\oRef -> DBO.udf9 oRef <-. B.val_ (C.udf9 newUdf))
        <> (\oRef -> DBO.udf10 oRef <-. B.val_ (C.udf10 newUdf))
        <> (\oRef -> DBO.lastModified oRef <-. B.val_ currentDate')
      )
      (\oRef ->  oRef ^. _id ==. B.val_ (Just orderRefId))
  pure ()
