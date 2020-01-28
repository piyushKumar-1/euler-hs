{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Euler.Common.Types.Currency where

import EulerHS.Prelude

import Data.Data

data Currency
  = INR
  | USD
  | GBP
  | EUR
  deriving (Data, Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)
