{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Promotion where

import           EulerHS.Prelude

import qualified Euler.Common.Types as C

import           Data.Time (LocalTime)


newtype PromotionPId = PromotionPId
  { promotionPId :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Promotion = Promotion
  { id               :: PromotionPId
  , dateCreated      :: LocalTime
  , discountAmount   :: C.Money
  , lastModified     :: LocalTime
  , orderId          :: Maybe Int
  , rules            :: Text
  , status           :: Text
  , orderReferenceId :: Maybe Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data PromotionActive = PromotionActive
  { id             :: PromotionPId
  , orderId        :: C.OrderId
  , rules          :: C.Rules
  , dateCreated    :: LocalTime
  , discountAmount :: C.Money
  , status         :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


-- from src/Types/Storage/EC/Promotions.purs
--data Rules = Rules
--  { dimension :: Text
--  , value :: Text
--  }