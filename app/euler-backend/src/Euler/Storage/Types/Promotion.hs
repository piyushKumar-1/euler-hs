{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.Promotion
  ( PromotionT(..)
  , Promotion
  -- , Id
  , promotionEMod
  , defaultPromotion
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import Euler.Common.Types.DefaultDate (defaultDate)
import qualified Database.Beam as B


data PromotionT f = Promotion
  { id               :: B.C f Int
  , dateCreated      :: B.C f LocalTime
  , discountAmount   :: B.C f Double
  , lastModified     :: B.C f LocalTime
  , orderId          :: B.C f (Maybe Int)
  , rules            :: B.C f Text
  , status           :: B.C f Text
  , orderReferenceId :: B.C f (Maybe Int)
  }
  deriving (Generic, B.Beamable)

instance B.Table PromotionT where
  data PrimaryKey PromotionT f =
    Id (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Promotion = PromotionT Identity
-- type Id = B.PrimaryKey PromotionT Identity

deriving instance Show Promotion
deriving instance Eq Promotion
deriving instance ToJSON Promotion
deriving instance FromJSON Promotion
deriving instance Read Promotion
deriving instance Ord Promotion

promotionEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity PromotionT)
promotionEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , dateCreated = B.fieldNamed "date_created"
    , discountAmount = B.fieldNamed "discount_amount"
    , lastModified = B.fieldNamed "last_modified"
    , orderId = B.fieldNamed "order_id"
    , rules = B.fieldNamed "rules"
    , status = B.fieldNamed "status"
    , orderReferenceId = B.fieldNamed "order_reference_id"
    }

defaultPromotion :: Promotion
defaultPromotion = Promotion
  { id               = 1 -- :: B.C f Int
  , dateCreated      = defaultDate  -- :: B.C f LocalTime
  , discountAmount   = 0  -- :: B.C f Double
  , lastModified     = defaultDate  -- :: B.C f LocalTime
  , orderId          = Nothing  -- :: B.C f (Maybe Int)
  , rules            = ""  -- :: B.C f Text
  , status           = ""  -- :: B.C f Text
  , orderReferenceId = Nothing  -- :: B.C f (Maybe Int)
  }
