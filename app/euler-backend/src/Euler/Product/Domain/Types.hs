{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Types where

import           EulerHS.Prelude


-- TODO: rework PaymentMethod / PaymentMethodType approach

data PaymentMethodType
  = WALLET
  | NB
  | CARD
  | UPI
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


newtype OrderId = OrderId Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype MerchantId = MerchantId Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
