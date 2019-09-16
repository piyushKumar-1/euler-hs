{-# LANGUAGE DeriveAnyClass #-}

module CreditPlatform.Domain.Types where

import EulerHS.Prelude

-- Demo types

newtype GSTIN = GSTIN Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype AuthToken = AuthToken Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
