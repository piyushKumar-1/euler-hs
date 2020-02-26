module Euler.Storage.Repository.Order
  ( loadOrder
  , saveOrder
  , updateOrder
  )
  where

import EulerHS.Prelude

import           Euler.Storage.DBConfig
import           EulerHS.Extra.Validation
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
      Just ordRef -> do
        case SV.transSOrderToDOrder ordRef of
          V.Success order -> pure $ Just order
          V.Failure e -> do
            logError "Incorrect order in DB"
              $  " orderId: "    <> orderId'
              <> " merchantId: " <> merchantId'
              <> " error: "      <> show e
            throwException Errs.internalError


saveOrder :: DB.OrderReference -> Flow D.Order
saveOrder orderRefVal = do
  orderRef <- unsafeInsertRow (Errs.mkDBError "Inserting order reference failed.") eulerDB
    $ B.insert (DB.order_reference DB.eulerDBSchema)
    $ B.insertExpressions [(B.val_ orderRefVal) & _id .~ B.default_]

  -- EHS: should not happen, ideally.
  -- EHS: should we skip the validation and just return the orderRefVal updated with the id from insertion?
  case SV.transSOrderToDOrder orderRef of
    V.Success order -> pure order
    V.Failure e -> do
      logError "orderCreate" $ "Unexpectedly got an invalid order reference after save: " <> show orderRef
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
                -- B.maybe_
                --  (B.current_ (DBO.amount oRef))
                --  (\a -> B.val_ $ a)
                --  (C.fromMoney <$> mAmount)
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

-- -- another variant of update
-- -- Domain Order should contain fields like storage OrderReference
-- updateDBOrder2 :: Order -> C.UDF -> Maybe Money -> Maybe AddressId -> Maybe AddressId -> Flow () --OrderReference
-- updateDBOrder2 currOrd@Order{..} newUdf mAmount mbBillingAddrId mbShippingAddrId = do
--   currentDate' <- getCurrentDateUTC
--   withDB eulerDB
--     $ updateRows
--     $ B.save (order_reference eulerDBSchema) newOrder
--   pure ()
--   where
--     newOrder = smash newUdf $ OrderReference
--         { amount = fromMoney <$> (mAmount <|> (Just amount))
--         , billingAddressId = mbBillingAddrId
--         , shippingAddressId = mbShippingAddrId
--         , lastModified = currentDate'
--         , ..
--         }