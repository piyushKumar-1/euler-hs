{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.CardPayment where

import EulerHS.Prelude

import qualified Euler.Product.Domain.PaymentMethod.Card as CPM
import qualified Euler.Product.Domain.PaymentMethod.ATMCard as ATMCPM
import qualified Euler.API.Types as AT

-- Was Transaction
data CardPayment = CardPayment
  { payment_method         :: CPM.CardPaymentMethod
  , card_payment_type      :: CardPaymentType

  , is_emi                 :: Maybe Bool
  , emi_bank               :: Maybe Text
  , emi_tenure             :: Maybe Int
  , auth_type              :: Maybe AT.AuthType

  -- , payment_method_type    :: PaymentMethodType   -- ^ it's CARD here
  -- , redirect_after_payment :: Bool                -- ^ These types are not about domain
  -- , format                 :: Text                -- ^ These types are not about domain
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data CardPaymentType
  = NewCardPayment
  { card_number            :: Text
  , name_on_card           :: Text
  , card_exp_month         :: Text
  , card_exp_year          :: Text
  , card_security_code     :: Text
  , save_to_locker         :: Bool
  }
  | SavedCardPayment                  -- ^ With /card/list
  { card_token             :: Text
  , card_security_code     :: Text
  }
 -- | TokenizedCardPayment              -- ^ With /card/tokenize
 -- { card_token             :: Text   -- Not possible
 -- }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data ATMSeamlessPayment = ATMSeamlessPayment
  { payment_method         :: CPM.CardPaymentMethod
  , card_payment_type      :: CardPaymentType

  , auth_type              :: AT.AuthType

  -- , payment_method_type    :: PaymentMethodType   -- ^ it's CARD here
  -- , redirect_after_payment :: Bool                -- ^ These types are not about domain
  -- , format                 :: Text                -- ^ These types are not about domain
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data ATMRedirectionPayment = ATMRedirectionPayment
  { payment_method         :: ATMCPM.ATMCardPaymentMethod
  , auth_type              :: AT.AuthType
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)