{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Address where

import EulerHS.Prelude hiding (id, state)

data OrderAddress = OrderAddress
  { id             :: Maybe Int
  , version        :: Int
  , firstName      :: Maybe Text
  , lastName       :: Maybe Text
  , line1          :: Maybe Text
  , line2          :: Maybe Text
  , line3          :: Maybe Text
  , city           :: Maybe Text
  , state          :: Maybe Text
  , country        :: Maybe Text
  , countryCodeIso :: Maybe Text
  , postalCode     :: Maybe Text
  , phone          :: Maybe Text
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

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