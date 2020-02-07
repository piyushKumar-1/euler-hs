{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.MerchantAccount where

import EulerHS.Prelude

import Euler.Common.Types.Merchant (MerchantId)

-- EHS: this is a DB type. Move somewhere?
type MerchantAccountId = Int
type ResellerId = Text

data MerchantAccount = MerchantAccount
  { id                        :: MerchantAccountId
  , merchantId                :: MerchantId
  , resellerId                :: Maybe ResellerId
  }
  deriving (Eq, Show, Ord, Generic)
