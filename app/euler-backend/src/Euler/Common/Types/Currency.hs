{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Types.Currency where

import EulerHS.Prelude

data Currency
  = INR
  | USD
  | GBP
  | EUR
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)
