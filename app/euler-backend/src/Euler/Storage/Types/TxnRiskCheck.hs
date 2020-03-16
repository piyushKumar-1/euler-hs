{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.TxnRiskCheck
  ( TxnRiskCheckT(..)
  , TxnRiskCheck
  -- , Id
  , txnRiskCheckEMod
  , defaultTxnRiskCheck
  ) where

import EulerHS.Prelude hiding (id)

import Data.Time
import qualified Data.Text as T
import Euler.Common.Types.DefaultDate (defaultDate)
import qualified Database.Beam as B


data TxnRiskCheckT f = TxnRiskCheck
  { id                              :: B.C f Int
  , completeResponse                :: B.C f Text
  , dateCreated                     :: B.C f LocalTime
  , flagged                         :: B.C f (Maybe Bool)
  , lastUpdated                     :: B.C f LocalTime
  , recommendedAction               :: B.C f (Maybe Text)
  , resultJson                      :: B.C f (Maybe Text)
  , riskManagementAccountId         :: B.C f Int
  , txnDetailId                     :: B.C f Int
  , message                         :: B.C f (Maybe Text)
  , status                          :: B.C f (Maybe Text) -- FIXME enum?
  , riskStatus                      :: B.C f (Maybe Text) -- FIXME enum?
  , domestic                        :: B.C f (Maybe Bool)
  , invocationMode                  :: B.C f (Maybe Text)
  , paymentStatusUpdateResponseCode :: B.C f (Maybe Text)
  , paymentStatusUpdated            :: B.C f (Maybe Bool)
  }
  deriving (Generic, B.Beamable)

instance B.Table TxnRiskCheckT where
  data PrimaryKey TxnRiskCheckT f =
    Id (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type TxnRiskCheck = TxnRiskCheckT Identity
-- type Id = B.PrimaryKey TxnRiskCheckT Identity

deriving instance Show TxnRiskCheck
deriving instance Eq TxnRiskCheck
deriving instance ToJSON TxnRiskCheck
deriving instance FromJSON TxnRiskCheck
deriving instance Read TxnRiskCheck
deriving instance Ord TxnRiskCheck

txnRiskCheckEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity TxnRiskCheckT)
txnRiskCheckEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , completeResponse = B.fieldNamed "complete_response"
    , dateCreated = B.fieldNamed "date_created"
    , flagged = B.fieldNamed "flagged"
    , lastUpdated = B.fieldNamed "last_updated"
    , recommendedAction = B.fieldNamed "recommended_action"
    , resultJson = B.fieldNamed "result_json"
    , riskManagementAccountId = B.fieldNamed "risk_management_account_id"
    , txnDetailId = B.fieldNamed "txn_detail_id"
    , message = B.fieldNamed "message"
    , status = B.fieldNamed "status"
    , riskStatus = B.fieldNamed "risk_status"
    , domestic = B.fieldNamed "domestic"
    , invocationMode = B.fieldNamed "invocation_mode"
    , paymentStatusUpdateResponseCode = B.fieldNamed "payment_status_update_response_code"
    , paymentStatusUpdated = B.fieldNamed "payment_status_updated"
    }

defaultTxnRiskCheck :: Int -> Int -> TxnRiskCheck
defaultTxnRiskCheck pId txnId = TxnRiskCheck
  { id = pId
  , completeResponse = T.empty
  , dateCreated = defaultDate
  , flagged = Nothing
  , lastUpdated = defaultDate
  , recommendedAction = Nothing
  , resultJson = Nothing
  , riskManagementAccountId = 0
  , txnDetailId = txnId
  , message = Nothing
  , status = Nothing
  , riskStatus = Nothing
  , domestic = Nothing
  , invocationMode = Nothing
  , paymentStatusUpdateResponseCode = Nothing
  , paymentStatusUpdated = Nothing
  }
