{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Chargeback where


import           EulerHS.Prelude

import           Euler.Product.Domain.Money

import           Data.Time


newtype ChargebackId = ChargebackId
  { chargebackId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Chargeback = Chargeback
  { id                :: ChargebackId
  , version           :: Int
  , amount            :: Money
  , dateCreated       :: LocalTime
  , dateResolved      :: Maybe LocalTime
  , disputeStatus     :: Text
  , lastUpdated       :: LocalTime
  , merchantAccountId :: Int
  , txnDetailId       :: Maybe Text
  , objectReferenceId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
