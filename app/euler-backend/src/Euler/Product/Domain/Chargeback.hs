{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Chargeback where


import           EulerHS.Prelude

import           Euler.Product.Domain.Money

import           Data.Time


newtype ChargebackPId = ChargebackPId
  { chargebackPId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Chargeback = Chargeback
  { id                :: ChargebackPId
  , version           :: Int
  , amount            :: Money
  , dateCreated       :: LocalTime
  , dateResolved      :: Maybe LocalTime
  , disputeStatus     :: Text
  , lastUpdated       :: LocalTime
  , merchantAccountId :: Int
  , txnDetailId       :: Maybe Int
  , objectReferenceId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
