{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Euler.Product.OLTP.Order.Update where

import EulerHS.Prelude hiding (id, get)
import EulerHS.Language
import WebService.Language

import           Data.Generics.Product.Fields
import           Data.Generics.Product.Subtype

import qualified Data.Set as Set
import qualified Prelude  as P (show)

-- EHS: Storage interaction should be extracted from here into separate modules

import Euler.Storage.DBConfig
import Euler.Storage.Types.EulerDB
import Euler.Storage.Types.OrderAddress
import qualified Euler.Storage.Validators.Order as SV


-- -- EHS: Should not depend on API?
import qualified Euler.API.RouteParameters  as RP
import qualified Euler.API.Order as API (OrderStatusResponse)

import Euler.Common.Errors.PredefinedErrors
import qualified Euler.Common.Types.Order   as C
-- import qualified Euler.Common.Metric        as Metric
import Euler.Product.Domain.Order
import Euler.Product.Domain.MerchantAccount
import Euler.Product.OLTP.Order.OrderStatus (getOrderStatusWithoutAuth)
import Euler.Product.OLTP.Services.RedisService

import qualified Euler.Product.Domain.Templates as Ts

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

-- ### EHS: functions like in orderCreate - move to separate module ###

-- loadCustomer
-- fillBillingAddressHolder

loadOrder :: OrderId -> MerchantId -> Flow (Maybe Order)
loadOrder orderId' merchantId' = withDB eulerDB $ do
  let predicate OrderReference {orderId, merchantId} =
            (orderId    ==. B.just_ (B.val_ orderId')) &&.
            (merchantId ==. B.just_ (B.val_ merchantId'))
  findRow
    $ B.select
    $ B.limit_ 1
    $ B.filter_ predicate
    $ B.all_ (order_reference eulerDBSchema)


cleanUpUDF :: C.UDF -> C.UDF
cleanUpUDF UDF {..} = UDF
  { udf1 = cleanUp udf1
  , udf2 = cleanUp udf2
  , udf3 = cleanUp udf3
  , udf4 = cleanUp udf4
  , udf5 = cleanUp udf5
  , ..
  }

cleanUp :: Maybe Text -> Maybe Text
cleanUp mStr =  cleanUpSpecialChars <$>  mStr

cleanUpSpecialChars :: Text -> Text
cleanUpSpecialChars = Text.filter (`Set.notMember` specialChars)

specialChars :: Set Char
specialChars = Set.fromList "~!#%^=+\\|:;,\"'()-.&/"

-- createAddress :: Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Flow (Maybe AddressId)
-- createAddress addr addrHolder =
--   case toDBAddress addr addrHolder of
--     Nothing -> pure Nothing
--     Just dbAddr -> do
--       mAddr <- withDB eulerDB
--           $ insertRowsReturningList
--           $ B.insert (order_address eulerDBSchema)
--           $ B.insertExpressions [(B.val_ dbAddr) & _id .~ B.default_]
--       pure $ (^. _id) =<< (safeHead mAddr)

-- in update we :
--   should check incoming address, if it is empty then return Nothing
--   if it contain new info then we add customer info and update current address in DB
--   but if in current orderRef addresId is empty we create new
updateAddress :: Maybe Ts.CustomerTemplate -> Maybe AddressId -> Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Flow (Maybe AddressId)
updateAddress mCT currAddrId addrT addrHolderT =
  case (currAddrId, toDBAddress' mCT addrT addrHolderT) of
    (_, Nothing) -> pure Nothing
    (Just addrId, Just dbAddr) -> do
      withDB eulerDB
        $ updateRows
        $ B.save (order_address eulerDBSchema)
        $ dbAddr & _id .~ (Just addrId)
      pure addrId
    (Nothing, Just dbAddr) -> do
      mAddr <- withDB eulerDB
          $ insertRowsReturningList
          $ B.insert (order_address eulerDBSchema)
          $ B.insertExpressions [(B.val_ dbAddr) & _id .~ B.default_]
      pure $ (^. _id) =<< (safeHead mAddr)


-- toDBAddress :: Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Maybe OrderAddress
-- toDBAddress (Ts.AddressTemplate {..}) (Ts.AddressHolderTemplate {..})
--   | nothingSet = Nothing
--   | otherwise = Just $ OrderAddress
--         { id             = Nothing  -- in DB it's not Null, Auto increment
--         , version        = 1        -- defaultVersion
--       -- from src/Config/Constants.purs
--       --  defaultVersion :: Int
--       --  defaultVersion = 1
--         ..
--         }
--   where
--     nothingSet = isNothing
--       $   firstName
--       <|> lastName
--       <|> line1
--       <|> line2
--       <|> line3
--       <|> city
--       <|> state
--       <|> country
--       <|> postalCode
--       <|> phone
--       <|> countryCodeIso

toDBAddress' :: Maybe Ts.CustomerTemplate -> Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Maybe OrderAddress
toDBAddress' mCT aT@Ts.AddressTemplate {..} ahT
  | addressEmpty aT ahT = Nothing
  | otherwise = Just $ OrderAddress
        { id             = Nothing  -- in DB it's not Null, Auto increment
        , version        = 1        -- defaultVersion
      -- from src/Config/Constants.purs
      --  defaultVersion :: Int
      --  defaultVersion = 1
        , ..
        }
  where
    Ts.AddressHolderTemplate {..} = fillBillingAddressHolder mCT aht

-- better name?
addressEmpty :: Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Bool
addressEmpty (Ts.AddressTemplate {..}) (Ts.AddressHolderTemplate {..}) =
  isNothing
    $   firstName
    <|> lastName
    <|> line1
    <|> line2
    <|> line3
    <|> city
    <|> state
    <|> country
    <|> postalCode
    <|> phone
    <|> countryCodeIso
-- ### ###



orderUpdate :: RP.RouteParameters -> Ts.OrderUpdateTemplate -> MerchantAccount -> Flow API.OrderStatusResponse
orderUpdate  routeParams orderUpdateT mAccnt = do
  let merchantId' = getField @"merchantId" mAccnt
  let orderId' = lookupRP @OrderId routeParams
  (mOrder :: Maybe OrderReference) <- loadOrder orderId' merchantId'
  resp <- case mOrder of
            Just orderRef -> do
              case SV.transSOrderToDOrder orderRef of
                Success ord -> do
                  doOrderUpdate orderUpdateT ord mAccnt
                -- ordStatusResp <- from orderStatus not ready -- getOrderStatusWithoutAuth (getOrderStatusRequest orderId) routeParams mAccnt True (Just updatedOrderCreateReq) (Just orderRef)
                -- better to use:
                -- ordStatusResp <- getOrdStatusResp ::
                --     OrderStatusRequest -- default with current order orderId
                --          getOrderStatusRequest
                --          from src/Types/Communication/OLTP/OrderStatus.purs
                --  -> MerchantAccount
                --  -> Bool -- true
                --  -> RouteParameters
                --  -> Flow OrderStatusResponse
                -- and
                -- addToCache :: OrderStatusRequest
                  --  -> Bool -- true
                  --  -> MerchantAccount
                  --  -> RouteParameters
                  --  -> OrderStatusResponse
                  --  -> Flow ()
                  -- pure ordStatusResp
                Failure e -> do
                  logError "Incorrect order in DB"
                    $  "orderId: " <> orderId'
                    <> "merchantId: " merchantId'
                    <> "error: " <> show err
                  throwException internalError
            Nothing -> throwException $ orderDoesNotExist orderId'
  logInfo "order update response: " $ show resp
  pure resp


doOrderUpdate :: Ts.OrderUpdateTemplate -> Order -> -> MerchantAccount -> Flow ()
doOrderUpdate orderUpdateT order@Order {..}  mAccnt = do
  case status of
    C.SUCCESS -> do
      logError "not_updating_successful_order" $ Text.pack("Order: " <> P.show ( orderId) <> " has already succeeded. Not updating any field.")
    _ ->  do
      let mNewAmount <- getField @"amount" orderUpdateT
      let newUDF = cleanUpUDF $ udf orderUpdateT
      mbCustomer <- loadCustomer customerId (mAccnt ^. _id)
      billingAddressId' <- updateAddress mbCustomer billingAddressId (orderUpdateT ^. _billingAddr) (orderUpdateT ^. _billingAddrHolder)
      shippingAddressId' <- updateAddress Nothing shippingAddressId (orderUpdateT ^. _shippingAddr) (orderUpdateT ^. _shippingAddrHolder)
      updateDBOrder2 (order ^. _id) newUDF mNewAmount billingAddressId' shippingAddressId'
      invalidateOrderStatusCache (order ^. _orderId) (order ^. _merchantId)



-- Domain Order should contain fields like storage OrderReference
updateDBOrder :: Order -> UDF -> Maybe Money -> Maybe AddressId -> Maybe AddressId -> Flow () --OrderReference
updateDBOrder currOrd@Order{..} newUdf mAmount mbBillingAddrId mbShippingAddrId = do
  currentDate' <- getCurrentDateUTC
  withDB eulerDB
    $ updateRows
    $ B.save (order_reference eulerDBSchema) newOrder
  pure ()
  where
    newOrder = smash newUdf $ OrderReference
        { amount = fromMoney <$> (mAmount <|> (Just amount))
        , billingAddressId = mbBillingAddrId
        , shippingAddressId = mbShippingAddrId
        , lastModified = currentDate'
        , ..
        }

-- another variant of update
updateDBOrder2 :: Int -> UDF -> Maybe Money -> Maybe AddressId -> Maybe AddressId -> Flow ()
updateDBOrder2 orderRefId newUdf mAmount mbBillingAddrId mbShippingAddrId = do
  currentDate' <- getCurrentDateUTC
  withDB eulerDB
    $ updateRows
    $ B.update (order_reference eulerDBSchema)
      (    (\oRef -> amount oRef <-. B.maybe_
                 (B.current_ (amount oRef))
                 (\a -> B.val_ $ Just $ fromMoney a)
                 mAmount
           )
        <> (\oRef -> billingAddressId oRef <-. B.val_ mbBillingAddrId)
        <> (\oRef -> shippingAddressId oRef <-. B.val_ mbShippingAddrId)
        <> (\oRef -> udf1 oRef <-. B.val_ (C.udf1 newUdf))
        <> (\oRef -> udf2 oRef <-. B.val_ (C.udf2 newUdf))
        <> (\oRef -> udf3 oRef <-. B.val_ (C.udf3 newUdf))
        <> (\oRef -> udf4 oRef <-. B.val_ (C.udf4 newUdf))
        <> (\oRef -> udf5 oRef <-. B.val_ (C.udf5 newUdf))
        <> (\oRef -> udf6 oRef <-. B.val_ (C.udf6 newUdf))
        <> (\oRef -> udf7 oRef <-. B.val_ (C.udf7 newUdf))
        <> (\oRef -> udf8 oRef <-. B.val_ (C.udf8 newUdf))
        <> (\oRef -> udf9 oRef <-. B.val_ (C.udf9 newUdf))
        <> (\oRef -> udf10 oRef <-. B.val_ (C.udf10 newUdf))
        <> (\oRef -> lastModified oRef <-. B.val_ currentDate')
      )
      (\oRef -> id oRef ==. val_ (Just orderRefId))
  pure ()
