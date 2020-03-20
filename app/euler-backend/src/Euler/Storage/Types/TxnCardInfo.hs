{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.TxnCardInfo
  ( TxnCardInfoT(..)
  , TxnCardInfo
  -- , Id
  , txnCardInfoEMod
  , defaultTxnCardInfo
  ) where

import EulerHS.Prelude hiding (id)

import Euler.Common.Types.External.Mandate (PaymentMethodType)
import Data.Time
import qualified Database.Beam as B


data TxnCardInfoT f = TxnCardInfo
  { id :: B.C f (Maybe Int)
  , txnId :: B.C f Text
  , cardIsin :: B.C f (Maybe Text)
  , cardIssuerBankName :: B.C f (Maybe Text)
  , cardExpYear :: B.C f (Maybe Text)
  , cardExpMonth :: B.C f (Maybe Text)
  , cardSwitchProvider :: B.C f (Maybe Text)
  , cardType :: B.C f (Maybe Text)
  , cardLastFourDigits :: B.C f (Maybe Text)
  , nameOnCard :: B.C f (Maybe Text)
  , cardFingerprint :: B.C f (Maybe Text)
  , cardReferenceId :: B.C f (Maybe Text)
  , txnDetailId :: B.C f (Maybe Int)
  , dateCreated :: B.C f (Maybe LocalTime)
  , paymentMethodType :: B.C f (Maybe PaymentMethodType)
  , paymentMethod :: B.C f (Maybe Text)
  , cardGlobalFingerprint :: B.C f (Maybe Text)
  , paymentSource :: B.C f (Maybe Text)
  , authType :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table TxnCardInfoT where
  data PrimaryKey TxnCardInfoT f =
    Id (B.C f (Maybe Int)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type TxnCardInfo = TxnCardInfoT Identity
-- type Id = B.PrimaryKey TxnCardInfoT Identity

deriving instance Show TxnCardInfo
deriving instance Eq TxnCardInfo
deriving instance ToJSON TxnCardInfo
deriving instance FromJSON TxnCardInfo
deriving instance Read TxnCardInfo
deriving instance Ord TxnCardInfo

txnCardInfoEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity TxnCardInfoT)
txnCardInfoEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , txnId = B.fieldNamed "txn_id"
    , cardIsin = B.fieldNamed "card_isin"
    , cardIssuerBankName = B.fieldNamed "card_issuer_bank_name"
    , cardExpYear = B.fieldNamed "card_exp_year"
    , cardExpMonth = B.fieldNamed "card_exp_month"
    , cardSwitchProvider = B.fieldNamed "card_switch_provider"
    , cardType = B.fieldNamed "card_type"
    , cardLastFourDigits = B.fieldNamed "card_last_four_digits"
    , nameOnCard = B.fieldNamed "name_on_card"
    , cardFingerprint = B.fieldNamed "card_fingerprint"
    , cardReferenceId = B.fieldNamed "card_reference_id"
    , txnDetailId = B.fieldNamed "txn_detail_id"
    , dateCreated = B.fieldNamed "date_created"
    , paymentMethodType = B.fieldNamed "payment_method_type"
    , paymentMethod = B.fieldNamed "payment_method"
    , cardGlobalFingerprint = B.fieldNamed "card_global_fingerprint"
    , paymentSource = B.fieldNamed "payment_source"
    , authType = B.fieldNamed "auth_type"
    }

defaultTxnCardInfo :: Maybe Int -> Text -> TxnCardInfo
defaultTxnCardInfo mId txnId = TxnCardInfo
  { id = mId
  , txnId = txnId
  , cardIsin = Nothing
  , cardIssuerBankName = Nothing
  , cardExpYear = Nothing
  , cardExpMonth = Nothing
  , cardSwitchProvider = Nothing
  , cardType = Nothing
  , cardLastFourDigits = Nothing
  , nameOnCard = Nothing
  , cardFingerprint = Nothing
  , cardReferenceId = Nothing
  , txnDetailId = Nothing
  , dateCreated = Nothing
  , paymentMethodType = Nothing
  , paymentMethod = Nothing
  , cardGlobalFingerprint = Nothing
  , paymentSource = Nothing
  , authType = Nothing
  }
