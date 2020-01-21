{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.Promotions
  ( PromotionsT(..)
  , Promotions
  , Id
  , promotionsEMod
  , defaultPromotions
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import Euler.Common.Types.DefaultDate (defaultDate)
import qualified Database.Beam as B


data PromotionsT f = Promotions
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

instance B.Table PromotionsT where
  data PrimaryKey PromotionsT f =
    Id (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Promotions = PromotionsT Identity
type Id = B.PrimaryKey PromotionsT Identity

deriving instance Show Promotions
deriving instance Eq Promotions
deriving instance ToJSON Promotions
deriving instance FromJSON Promotions
deriving instance Read Promotions
deriving instance Ord Promotions

promotionsEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity PromotionsT)
promotionsEMod = B.modifyTableFields
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

defaultPromotions :: Promotions
defaultPromotions = Promotions
  { id               = 1 -- :: B.C f Int
  , dateCreated      = defaultDate  -- :: B.C f LocalTime
  , discountAmount   = 0  -- :: B.C f Double
  , lastModified     = defaultDate  -- :: B.C f LocalTime
  , orderId          = Nothing  -- :: B.C f (Maybe Int)
  , rules            = ""  -- :: B.C f Text
  , status           = ""  -- :: B.C f Text
  , orderReferenceId = Nothing  -- :: B.C f (Maybe Int)
  }