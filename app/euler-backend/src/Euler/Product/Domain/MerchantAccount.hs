{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.MerchantAccount where





data MerchantAccount = MerchantAccount
  { id                                  :: Int
  , merchantId                          :: Text
  }
  deriving (Eq, Show, Ord, Generic)
