{-# LANGUAGE DeriveAnyClass #-}

module Euler.API.Card where

import EulerHS.Prelude

import Data.Aeson
import Web.FormUrlEncoded



-- There is big differance with api docs and euler-ps what fields required.
-- Here are requirements from working graphh
-- https://www.juspay.in/docs/api/ec/?shell#add-card

-- Legasy fields.
-- Left is legacy, Right is new ones.
-- cardId = cardToken
-- externalId = cardReference
-- isin = cardIsin

----------------------------------------------------------------------
-- Get Card Details
----------------------------------------------------------------------

-- grails-app/controllers/juspay/CardController.groovy
-- def getCard
data GetCardRequest = GetCardRequest
  { card_token :: Text
  , merchant_id :: Text
  , customer_id :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)

--  Composed with some GetDetail fields
data GetCardResponse = GetCardResponse
  { card_number :: Text
  , card_reference :: Text
  , card_exp_year :: Text
  , card_exp_month :: Text
  , card_isin :: Text
  , name_on_card :: Maybe Text
  , card_type :: Maybe Text
  , card_issuer :: Maybe Text
  , card_brand :: Maybe Text
  , nickname :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- src/groovy/juspay/CardDetail.groovy
-- it is auxiliary type most likely
data CardDetail = CardDetail
  { cardNumber :: Text
  , cardExpMonth :: Text
  , cardExpYear :: Text
  , cardSecurityCode :: Maybe Text
  , nameOnCard :: Maybe Text
  , nickname :: Maybe Text
  , maskedCardNumber :: Text -- can be made with cardNumber
  , cardLastFourDigits :: Text -- can be made with cardNumber or maskedCardNumber
  , cardIsin :: Text -- can be made with cardNumber
  , cardType :: Maybe Text
  , cardBrand :: Maybe Text
  , cardIssuer :: Maybe Text
  , cardReference :: Maybe Text
  , cardFingerprint :: Maybe Text
  , cardGlobalFingerprint :: Maybe Text
  , cardToken :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

----------------------------------------------------------------------
-- Add Card
----------------------------------------------------------------------

-- src/groovy/juspay/command/AddCardCommand.groovy
data AddCard = AddCard
  { merchant_id :: Maybe Text
  , customer_id :: Text
  , customer_email :: Maybe Text
  , card_number :: Text
  , card_exp_year :: Maybe Integer
  , card_exp_month :: Maybe Integer
  , name_on_card :: Maybe Text
  , nickname :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)

data AddCardUsingToken = AddCardUsingToken
  { customer_id :: Text
  , customer_email :: Maybe Text
  , card_token :: Text
  , merchant_id :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)

data AddCardResponse = AddCardResponse
  { card_token :: Maybe Text
  , card_reference :: Maybe Text
  , card_fingerprint :: Maybe Text
  , card_global_fingerprint :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- it is auxiliary type most likely
-- for making request with all needed data
data AddCardInputRequest = AddCardInputRequest
  { cardDetail :: CardDetail
  , customerId :: Text
  , customerEmail :: Text
  , merchantAccountId :: Int

  -- MerchantAccount used for authentication
  -- , merchantAccount :: MerchantAccount
  -- , lockerAccount :: MerchantLockerAccount
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

------------------------------------------------------------------------
-- List stored cards
------------------------------------------------------------------------

data ListCardRequest = ListCardRequest
  { customerId :: Text
  , merchantId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)

data ListCardResp = ListCardResp
  { cards :: [ListCardResponse]
  , customerId :: Text
  , merchantId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- ?: "" converted to Maybe
data ListCardResponse = ListCardResponse
  { card_token :: Maybe Text
  , card_reference :: Text
  , unMaskedCardNumber :: Maybe Text
  , card_number :: Text
  , card_isin :: Text
  , card_exp_year :: Text
  , card_exp_month :: Text
  , card_type :: Maybe Text
  , card_issuer :: Maybe Text
  , card_brand :: Maybe Text
  , nickname :: Maybe Text
  , name_on_card :: Maybe Text
  , expired :: Bool
  , card_fingerprint :: Maybe Text
  , card_global_fingerprint :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

----------------------------------------------------------------------
-- Delete Card
----------------------------------------------------------------------

-- Authentication needed
data CardDeleteReq = CardDeleteReq
  { card_token :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)

data CardDeleteResponseScheme = CardDeleteResponseScheme
  { card_token :: Text
  , card_reference :: Maybe Text
  , deleted :: Bool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


----------------------------------------------------------------------
-- Tokenize Card
----------------------------------------------------------------------

-- data CreateCardToken = CreateCardToken
--   { merchant_id :: Text
--   , card_number :: Text
--   , card_exp_year :: Text
--   , card_exp_month :: Text
--   , card_security_code :: Text
--   , stored_card_token :: Text
--   , customer_id :: Text
--   -- , merchantAccount :: MerchantAccount -- for authentication
--   , card_brand :: Text
--   , save_to_locker :: Bool
--   , name_on_card :: Text
--   }
