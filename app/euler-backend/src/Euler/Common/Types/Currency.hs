{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Types.Currency where


data Currency
  = INR
  | USD
  | GBP
  | EUR
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)
