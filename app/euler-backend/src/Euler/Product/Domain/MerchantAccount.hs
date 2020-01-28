{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.MerchantAccount where

import EulerHS.Prelude



data MerchantAccount = MerchantAccount
  { id                                  :: Int
  , merchantId                          :: Text
  }
  deriving (Eq, Show, Ord, Generic)
