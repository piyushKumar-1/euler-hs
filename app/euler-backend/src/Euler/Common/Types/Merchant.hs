{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Types.Merchant where

import EulerHS.Prelude

-- from src/Types/Storage/EC/MerchantKey.purs
data KeyScope = MERCHANT | CLIENT | DASHBOARD
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)



