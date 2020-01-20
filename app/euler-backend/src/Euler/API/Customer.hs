{-# LANGUAGE DeriveAnyClass #-}

module Euler.API.Customer
  ( CustomerWithTokenData (..)
  , TokenData (..)
  , CustomerReqSignaturePayload(..)
  , CustomerReq(..)
  )
  where

import EulerHS.Prelude hiding (id)

import Data.Time

-- From graphh/.../grails-app/domain/juspay/model/Customer.groovy
-- static constraints = {
--         mobileNumber nullable: false, blank: false, digits: true, maxSize: 16
--         objectReferenceId nullable: false, size: 1..128, unique: "merchantAccount"
--         emailAddress nullable: true, blank: true, maxSize: 128
--         firstName nullable: true, blank: true, maxSize: 64
--         lastName nullable: true, blank: true, maxSize: 64
--     }

--     String id
--     String object = "customer"
--     String objectReferenceId // customerId defined by merchant
--     String mobileCountryCode = "91"
--     String mobileNumber
--     String emailAddress
--     String firstName
--     String lastName

-- domain
-- data CreateCustomerReq = CreateCustomerReq
--   { object_reference_id :: Text -- customer_id
--   , mobile_number       :: Text
--   -- email_address required in https://www.juspay.in/docs/api/ec/?shell#create-customer
--   -- email_address not required in https://developer.juspay.in/reference#customer
--   , email_address       :: Maybe Text
--   , first_name          :: Maybe Text
--   , last_name           :: Maybe Text
--   -- No need to prefix “+”.
--   , mobile_country_code :: Maybe Text
--   }
--   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Get Customer Response
data CustomerWithTokenData = CustomerWithTokenData
    { id                  :: Text
    , object              :: Text -- "customer"
    , object_reference_id :: Text -- customer_id
    , mobile_country_code :: Text
    , mobile_number       :: Text
    , email_address       :: Text -- Foreign
    , first_name          :: Text -- Foreign
    , last_name           :: Text -- Foreign
    , date_created        :: LocalTime
    , last_updated        :: LocalTime
    , juspay              :: Maybe TokenData
    }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Looks like Order OrderTokenResp
data TokenData = TokenData
    { client_auth_token        :: Maybe Text
    , client_auth_token_expiry :: Maybe Text
    }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data CustomerReqSignaturePayload = CustomerReqSignaturePayload
  { merchant_id :: Text
  , customer_id :: Text -- object_reference_id
  , timestamp  :: Text
  , nonce :: Maybe Text
  , mobile_number :: Maybe Text
  , email_address :: Maybe Text
  , first_name :: Maybe Text
  , last_name :: Maybe Text
  , mobile_country_code :: Maybe Text
}
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- GetCustomerReq
data CustomerReq = CustomerReq
  { -- ?? options_get_client_auth_token :: Maybe Bool -- "options.get_client_auth_token" --??
  , merchant_key_id     :: Maybe Text -- ??

-- Used if api key present in Authorization header
  , object_reference_id :: Maybe Text
  , mobile_number       :: Maybe Text --
  , email_address       :: Maybe Text --
  , first_name          :: Maybe Text --
  , last_name           :: Maybe Text --
  , mobile_country_code :: Maybe Text --

 -- Used if api key not present in Authorization header
  , signature           :: Maybe Text
  , signature_payload   :: Maybe Text -- CustomerReqSignaturePayload
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


