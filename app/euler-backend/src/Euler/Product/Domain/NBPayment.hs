{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.NBPayment where

import EulerHS.Prelude

import qualified Euler.Product.Domain.PaymentMethod.NB as NBPM

data NBPayment = NBPayment
  { payment_method         :: NBPM.NBPaymentMethod

  -- , payment_method_type    :: PaymentMethodType   -- ^ it's CARD here
  -- , redirect_after_payment :: Bool                -- ^ These types are not about domain
  -- , format                 :: Text                -- ^ These types are not about domain
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
