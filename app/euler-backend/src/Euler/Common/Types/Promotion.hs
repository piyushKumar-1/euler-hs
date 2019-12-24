{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Types.Promotion where

import EulerHS.Prelude

-- from src/Types/Storage/EC/Promotions.purs
data Rules = Rules
  { dimension :: Text
  , value :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
-- from src/Types/Communication/OLTP/OrderStatus.purs
data Promotion' = Promotion'
  { id :: Maybe Text
  , order_id :: Maybe Text
  , rules :: Maybe [Rules]
  , created :: Maybe Text
  , discount_amount :: Maybe Double
  , status :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

defaultPromotion' :: Promotion'
defaultPromotion' = Promotion'
  { id              = Nothing -- :: Maybe Text
  , order_id        = Nothing -- :: Maybe Text
  , rules           = Nothing -- :: Maybe [Rules]
  , created         = Nothing -- :: Maybe Text
  , discount_amount = Nothing -- :: Maybe Double
  , status          = Nothing -- :: Maybe Text
  }
