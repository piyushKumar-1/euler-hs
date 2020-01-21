{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.OrderAddress where

import EulerHS.Prelude hiding (id, state)

import qualified Database.Beam as B

data OrderAddressT f = OrderAddress
  { id             :: B.C f (Maybe Int)
  , version        :: B.C f Int
  , firstName      :: B.C f (Maybe Text)
  , lastName       :: B.C f (Maybe Text)
  , line1          :: B.C f (Maybe Text)
  , line2          :: B.C f (Maybe Text)
  , line3          :: B.C f (Maybe Text)
  , city           :: B.C f (Maybe Text)
  , state          :: B.C f (Maybe Text)
  , country        :: B.C f (Maybe Text)
  , countryCodeIso :: B.C f (Maybe Text)
  , postalCode     :: B.C f (Maybe Text)
  , phone          :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)


instance B.Table OrderAddressT where
  data PrimaryKey OrderAddressT f =
    Id (B.C f (Maybe Int)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type OrderAddress = OrderAddressT Identity
type Id = B.PrimaryKey OrderAddressT Identity

deriving instance Show OrderAddress
deriving instance Eq OrderAddress
deriving instance ToJSON OrderAddress
deriving instance FromJSON OrderAddress
deriving instance Read OrderAddress
deriving instance Ord OrderAddress

orderAddressEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity OrderAddressT)
orderAddressEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , firstName = B.fieldNamed "first_name"
    , lastName = B.fieldNamed "last_name"
    , line1 = B.fieldNamed "line1"
    , line2 = B.fieldNamed "line2"
    , line3 = B.fieldNamed "line3"
    , city = B.fieldNamed "city"
    , state = B.fieldNamed "state"
    , country = B.fieldNamed "country"
    , countryCodeIso = B.fieldNamed "country_code_iso"
    , postalCode = B.fieldNamed "postal_code"
    , phone = B.fieldNamed "phone"
    }


defaultOrderAddress :: OrderAddress
defaultOrderAddress = OrderAddress
  { id             = Just 1 -- :: Maybe Int
  , version        = 1 -- :: Int
  , firstName      = Nothing -- :: Maybe Text
  , lastName       = Nothing -- :: Maybe Text
  , line1          = Nothing -- :: Maybe Text
  , line2          = Nothing -- :: Maybe Text
  , line3          = Nothing -- :: Maybe Text
  , city           = Nothing -- :: Maybe Text
  , state          = Nothing -- :: Maybe Text
  , country        = Nothing -- :: Maybe Text
  , countryCodeIso = Nothing -- :: Maybe Text
  , postalCode     = Nothing -- :: Maybe Text
  , phone          = Nothing -- :: Maybe Text
  }

data APIBillingAddress = APIBillingAddress
  { billing_address_first_name        :: Maybe Text
  , billing_address_last_name         :: Maybe Text
  , billing_address_line1             :: Maybe Text
  , billing_address_line2             :: Maybe Text
  , billing_address_line3             :: Maybe Text
  , billing_address_city              :: Maybe Text
  , billing_address_state             :: Maybe Text
  , billing_address_country           :: Maybe Text
  , billing_address_postal_code       :: Maybe Text
  , billing_address_phone             :: Maybe Text
  , billing_address_country_code_iso  :: Maybe Text -- Default value: IND
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data APIShippingAddress = APIShippingAddress
  { shipping_address_first_name       :: Maybe Text
  , shipping_address_last_name        :: Maybe Text
  , shipping_address_line1            :: Maybe Text
  , shipping_address_line2            :: Maybe Text
  , shipping_address_line3            :: Maybe Text
  , shipping_address_city             :: Maybe Text
  , shipping_address_state            :: Maybe Text
  , shipping_address_country          :: Maybe Text
  , shipping_address_postal_code      :: Maybe Text
  , shipping_address_phone            :: Maybe Text
  , shipping_address_country_code_iso :: Maybe Text -- Default value: IND
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

-- Usefull with 'upcast' and 'smash' functions from generic lens.
-- They help extract and insert sub-types.

mkShippingAddress :: APIShippingAddress -> OrderAddress 
mkShippingAddress APIShippingAddress {..} = OrderAddress 
  { id             = Nothing
  , version        = 1 -- defaultVersion -- from src/Config/Constants.purs = 1 :: Int
  , firstName      = shipping_address_first_name
  , lastName       = shipping_address_last_name
  , line1          = shipping_address_line1
  , line2          = shipping_address_line2
  , line3          = shipping_address_line3
  , city           = shipping_address_city
  , state          = shipping_address_state
  , country        = shipping_address_country
  , postalCode     = shipping_address_postal_code
  , phone          = shipping_address_phone
  , countryCodeIso = shipping_address_country_code_iso
  }

mkBillingAddress :: APIBillingAddress -> OrderAddress -- OrderCreateRequest -> OrderAddress
mkBillingAddress APIBillingAddress {..} = OrderAddress
  { id             = Nothing -- in DB it Not Null, Auto increment
  , version        = 1 -- defaultVersion  -- from src/Config/Constants.purs = 1 :: Int
  , firstName      = billing_address_first_name
  , lastName       = billing_address_last_name
  , line1          = billing_address_line1
  , line2          = billing_address_line2
  , line3          = billing_address_line3
  , city           = billing_address_city
  , state          = billing_address_state
  , country        = billing_address_country
  , postalCode     = billing_address_postal_code
  , phone          = billing_address_phone
  , countryCodeIso = billing_address_country_code_iso
  }

isAddressEmpty :: OrderAddress -> Bool
isAddressEmpty OrderAddress {..}
  =  isNothing firstName
  && isNothing lastName
  && isNothing line1
  && isNothing line2
  && isNothing line3
  && isNothing city
  && isNothing state
  && isNothing country
  && isNothing postalCode
  && isNothing phone
  && isNothing countryCodeIso