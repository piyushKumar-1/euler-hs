{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.RiskManagementAccount where

import EulerHS.Prelude

import Data.Time


newtype RiskManagementAccountPId = RiskManagementAccountPId
  { riskManagementAccountPId :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data RiskManagementAccount = RiskManagementAccount
  { id                 :: RiskManagementAccountPId
  , version            :: Int
  , accountDetailsJson :: Text
  , merchantAccountId  :: Int
  , provider           :: Text
  , dateCreated        :: Maybe LocalTime
  , lastUpdated        :: Maybe LocalTime
  , riskDsl            :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
