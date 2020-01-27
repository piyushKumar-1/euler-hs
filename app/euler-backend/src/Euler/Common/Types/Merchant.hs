{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Types.Merchant where

import EulerHS.Prelude

type MerchantId = Text

-- from src/Types/Storage/EC/MerchantKey.purs
data KeyScope = MERCHANT | CLIENT | DASHBOARD
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)
