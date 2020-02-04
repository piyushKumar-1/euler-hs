{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Euler.Product.OLTP.Order.Update where

import EulerHS.Prelude hiding (id, show, get)
-- import qualified EulerHS.Prelude as EHP
-- import EulerHS.Language
-- import WebService.Language
--
-- import           Data.Generics.Product.Fields
-- import           Data.Generics.Product.Subtype
-- import           Data.List (lookup, span)
-- import           Servant.Server
--
-- -- EHS: Storage interaction should be extracted from here into separate modules
-- import Euler.Storage.Types.Customer
-- import Euler.Storage.Types.Mandate
-- import qualified Euler.Storage.Types.MerchantAccount as DBMA
-- import qualified Euler.Storage.Types.OrderAddress as Address (id)
-- import Euler.Storage.Types.MerchantIframePreferences
import Euler.Storage.Types.OrderAddress
-- import Euler.Storage.Types.OrderMetadataV2
-- import Euler.Storage.Types.OrderReference
-- import Euler.Storage.Types.ResellerAccount
--
-- import Euler.KVDB.Redis (rGet, setCacheWithExpiry)
-- import Euler.Storage.Types.EulerDB
--
-- import qualified Prelude              as P (show, notElem)
-- import qualified Data.Aeson           as A
-- import qualified Data.Text            as Text
-- import qualified Text.Read            as TR
--
-- -- EHS: Should not depend on API?
import qualified Euler.API.RouteParameters  as RP
import qualified Euler.API.Order            as API
--
-- import Euler.Common.Types.DefaultDate
-- import Euler.Common.Types.Gateway
-- import Euler.Common.Types.Order
-- import Euler.Common.Types.OrderMetadata
-- import qualified Euler.Common.Types.Mandate as M
import qualified Euler.Common.Types.Order   as C
-- import qualified Euler.Common.Metric        as Metric
-- import Euler.Product.Domain.Order
import Euler.Product.Domain.MerchantAccount
-- import Euler.Product.OLTP.Order.OrderStatus (getOrderStatusRequest, getOrderStatusWithoutAuth)
import Euler.Product.OLTP.Services.RedisService
-- import qualified Euler.Config.Config        as Config
-- import qualified Euler.Config.ServiceConfiguration as SC
--
import qualified Euler.Product.Domain.Templates as Ts
--
-- import Euler.Lens
-- import Euler.Storage.DBConfig
-- import qualified Database.Beam as B
-- import qualified Database.Beam.Backend.SQL as B
-- import Database.Beam ((==.), (&&.), (||.), (<-.), (/=.))


-- EHS: since original update function consumed orderCreate type - fields "amount" and "order_id"
-- was mandatory. But what if we dont whant to update amount? Also we dont need order_id field
-- because we take order_id from route parameters.
-- We can update only amount, address and UDF fields.
-- All this fields in new OrderUpdateRequest type are optional.

-- EHS: we did not check amount, first/last names like in orderCreate its ok?

-- ### EHS: functions like in orderCreate - move to separate module ###
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
cleanUpSpecialChars = Text.filter (`P.notElem` ("~!#%^=+\\|:;,\"'()-.&/" :: String))

createAddress :: Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Flow (Maybe AddressId)
createAddress addr addrHolder =
  case toDBAddress addr addrHolder of
    Nothing -> pure Nothing
    Just dbAddr -> do
      mAddr <- withDB eulerDB
          $ insertRowsReturningList
          $ B.insert (order_address eulerDBSchema)
          $ B.insertExpressions [(B.val_ dbAddr) & _id .~ B.default_]
      pure $ (^. _id) =<< (safeHead mAddr)


updateAddress :: Maybe AddressId -> Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Flow (Maybe AddressId)
updateAddress curAddrId addrT addrHolderT =
  case (currAddrId, toDBAddress addrT addrHolderT) of
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


toDBAddress :: Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Maybe OrderAddress
toDBAddress (Ts.AddressTemplate {..}) (Ts.AddressHolderTemplate {..})
  | nothingSet = Nothing
  | otherwise = Just $ OrderAddress
        { id             = Nothing  -- in DB it's not Null, Auto increment
        , version        = 1        -- defaultVersion
      -- from src/Config/Constants.purs
      --  defaultVersion :: Int
      --  defaultVersion = 1
        ..
        }
  where
    nothingSet = isNothing
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



orderUpdate :: RP.RouteParameters -> Text -> Ts.OrderUpdateTemplate -> MerchantAccount -> Flow OrderStatusResponse
orderUpdate  routeParams orderId orderUpdateT@OrderUpdateTemplate {..} mAccnt = do
  let merchantId' = getField @"merchantId" mAccnt
  let orderId' = lookupRP @OrderId routeParams
  (orderIfExists :: Maybe OrderReference) <- loadOrder orderId' merchantId'
  resp <- case (orderIfExists) of
            Just orderRef -> do
              doOrderUpdate orderUpdateT orderRef mAccnt
             -- from orderStatus not ready -- getOrderStatusWithoutAuth (getOrderStatusRequest orderId) routeParams mAccnt True (Just updatedOrderCreateReq) (Just orderRef)
            Nothing -> throwException err400 {errBody = "Order does not exists."}
  logInfo "createOrUpdateOrder response: " $ show resp
  pure resp


-- EHS: Should we update udf fields if they are not present (all Nothing)?
doOrderUpdate :: Ts.OrderUpdateTemplate -> OrderReference -> -> MerchantAccount -> Flow OrderReference -- OrderStatusResponse --Order
doOrderUpdate orderUpdateT order@OrderReference {..}  mAccnt = do
  case status of
    C.SUCCESS -> do
      logError "not_updating_successful_order" $ Text.pack("Order: " <> P.show ( orderId) <> " has already succeeded. Not updating any field.")
      pure order
    _ ->  do
      let mNewAmount <- getField @"amount" orderUpdateT
      let newUDF = cleanUpUDF $ udf orderUpdateT
      shippingAddressId' <- updateAddress shippingAddressId (orderUpdateT ^. _shippingAddr) (orderUpdateT ^. _shippingAddrHolder)
      billingAddressId' <- updateAddress billingAddressId (orderUpdateT ^. _billingAddr) (orderUpdateT ^. _billingAddrHolder)
      currentDate' <-  getCurrentDateUTC
      updateOrderRefAndInValidateCache (smash udf $ order
        { amount = newAmount <|> (Just amount)
        , billingAddressId = billingAddressId'
        , shippingAddressId = shippingAddressId'
        , lastModified = currentDate'
        })




-- from src/Types/Storage/EC/OrderReference.purs
updateOrderRefAndInValidateCache :: OrderReference -> Flow OrderReference
updateOrderRefAndInValidateCache order = do
  _ <- withDB eulerDB
         $ updateRows
         $ B.save (order_reference eulerDBSchema) order
        --DB.updateOne' ecDB order $ where_ := WHERE ["id" /\ Int (order .^. _id)] :: WHERE OrderReference
  _     <- invalidateOrderStatusCache (order .^. _orderId) (order .^. _merchantId)
  pure order

-- from src/Types/Storage/EC/OrderAddress.purs
-- updateOrderAddress :: OrderAddress -> Flow OrderAddress
-- updateOrderAddress orderAddress = do --pure orderAddress --do
--   withDB eulerDB
--     $ updateRows
--     $ B.save (order_address eulerDBSchema)
--     $ orderAddress
--   pure orderAddress
--  let opts = getOrderAddressOpts orderAddress
--  (update "ECRDB" opts $ where_ := WHERE ["id" /\ Int id] :: WHERE OrderAddress) >>= extractUpdateOneObj
