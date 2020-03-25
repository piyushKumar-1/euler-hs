{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Paymentlinks where

import           EulerHS.Prelude


import           Data.Time (LocalTime)


data Paymentlinks = Paymentlinks
  { iframe :: Text
  , web    :: Text
  , mobile :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

defaultPaymentlinks :: Paymentlinks
defaultPaymentlinks = Paymentlinks
  { iframe = mempty
  , web    = mempty
  , mobile = mempty
  }
