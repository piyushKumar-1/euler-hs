{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Types.Promotion where

import EulerHS.Prelude

-- from src/Types/Storage/EC/Promotions.purs
data Rules = Rules
  { dimension :: Text
  , value     :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

