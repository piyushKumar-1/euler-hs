{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Euler.Storage.Types.Chargeback
  ( ChargebackT(..)
  , Chargeback
  -- , Id
  , chargebackEMod
  ) where

import           EulerHS.Prelude hiding (id)
import           Data.Time
import qualified Database.Beam as B

data ChargebackT f = Chargeback
  { id                :: B.C f Text
  , version           :: B.C f Int
  , amount            :: B.C f Double
  , dateCreated       :: B.C f LocalTime
  , dateResolved      :: B.C f (Maybe LocalTime)
  , disputeStatus     :: B.C f Text
  , lastUpdated       :: B.C f LocalTime
  , merchantAccountId :: B.C f Int
  , txnDetailId       :: B.C f (Maybe Int)
  , objectReferenceId :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table ChargebackT where
  data PrimaryKey ChargebackT f =
    Id (B.C f Text) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Chargeback = ChargebackT Identity
-- type Id = B.PrimaryKey ChargebackT Identity

deriving instance Show Chargeback
deriving instance Eq Chargeback
deriving instance ToJSON Chargeback
deriving instance FromJSON Chargeback
deriving instance Read Chargeback
deriving instance Ord Chargeback

chargebackEMod :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity ChargebackT)
chargebackEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , amount = B.fieldNamed "amount"
    , dateCreated = B.fieldNamed "date_created"
    , dateResolved = B.fieldNamed "date_resolved"
    , disputeStatus = B.fieldNamed "dispute_status"
    , lastUpdated = B.fieldNamed "last_updated"
    , merchantAccountId = B.fieldNamed "merchant_account_id"
    , txnDetailId = B.fieldNamed "txn_detail_id"
    , objectReferenceId = B.fieldNamed "object_reference_id"
    }
