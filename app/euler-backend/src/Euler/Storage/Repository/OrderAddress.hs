module Euler.Storage.Repository.OrderAddress
  ( createAddress
  , updateAddress
  , fillBillingAddressHolder
  , loadAddress
  )
  where

import EulerHS.Prelude hiding (id, state)

import           EulerHS.Language

import           Euler.Constants (defaultVersion)
import           Euler.Storage.DBConfig

import qualified Euler.Common.Types                   as C
import qualified Euler.Product.Domain.Templates       as Ts
import qualified Euler.Storage.Types                  as DB

import           Database.Beam ((==.))
import qualified Database.Beam as B
import           Euler.Lens



createAddress :: Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Flow (Maybe C.AddressId)
createAddress addr addrHolder =
  case toDBAddress Nothing addr addrHolder of
    Nothing -> pure Nothing
    Just dbAddr -> do
      mAddr <- withDB eulerDB
          $ insertRowsReturningList
          $ B.insert (DB.order_address DB.eulerDBSchema)
          $ B.insertExpressions [(B.val_ dbAddr) & _id .~ B.default_]
      pure $ (^. _id) =<< (safeHead mAddr)

updateAddress :: Maybe Ts.CustomerTemplate -> Maybe C.AddressId -> Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Flow (Maybe C.AddressId)
updateAddress mCT currAddrId addrT addrHolderT =
  case (currAddrId, toDBAddress mCT addrT addrHolderT) of
    (_, Nothing) -> pure Nothing
    (Just addrId, Just dbAddr) -> do
      withDB eulerDB
        $ updateRows
        $ B.save (DB.order_address DB.eulerDBSchema)
        $ dbAddr & _id .~ (Just addrId)
      pure $ Just addrId
    (Nothing, Just dbAddr) -> do
      mAddr <- withDB eulerDB
          $ insertRowsReturningList
          $ B.insert (DB.order_address DB.eulerDBSchema)
          -- EHS : if in db OrderAddres id changed from Int to Text (so it is not auto-incremented) we should generate uuid or what?
          $ B.insertExpressions [(B.val_ dbAddr) & _id .~ B.default_]
      pure $ (^. _id) =<< (safeHead mAddr)

toDBAddress :: Maybe Ts.CustomerTemplate -> Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Maybe DB.OrderAddress
toDBAddress mCT aT@Ts.AddressTemplate {..} ahT
  | addressEmpty aT ahT = Nothing
  | otherwise = Just $ DB.OrderAddress
        { DB.id             = Nothing  -- in DB it's not Null, Auto increment
        , DB.version        = defaultVersion
        , DB.firstName      = firstName
        , DB.lastName       = lastName
        , DB.line1          = line1
        , DB.line2          = line2
        , DB.line3          = line3
        , DB.city           = city
        , DB.state          = state
        , DB.country        = country
        , DB.countryCodeIso = countryCodeIso
        , DB.postalCode     = postalCode
        , DB.phone          = phone
        }
  where
    Ts.AddressHolderTemplate {..} = fillBillingAddressHolder mCT ahT

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

-- EHS: move to Templates?
fillBillingAddressHolder :: Maybe Ts.CustomerTemplate -> Ts.AddressHolderTemplate -> Ts.AddressHolderTemplate
fillBillingAddressHolder Nothing addressHolder = addressHolder
fillBillingAddressHolder (Just customer) (Ts.AddressHolderTemplate {..})
  = Ts.AddressHolderTemplate
      (firstName <|> (customer ^. _firstName))
      (lastName <|> (customer ^. _lastName))

loadAddress :: C.AddressId -> Flow (Maybe DB.OrderAddress)
loadAddress addrId = withDB eulerDB $ do
  let predicate DB.OrderAddress {id} = id ==. B.just_ (B.val_ addrId)
  findRow
    $ B.select
    $ B.limit_ 1
    $ B.filter_ predicate
    $ B.all_ (DB.order_address DB.eulerDBSchema)
