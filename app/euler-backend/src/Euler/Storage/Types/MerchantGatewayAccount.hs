{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.MerchantGatewayAccount
  ( MerchantGatewayAccountT(..)
  , MerchantGatewayAccount
  , merchantGatewayAccountEMod
  , defaultMerchantGatewayAccount
  ) where

import EulerHS.Prelude hiding (id)

import Data.Time
import Euler.Common.Types.EulerAccountDetails (EulerAccountDetails(..))
import qualified Database.Beam as B

data MerchantGatewayAccountT f =
  MerchantGatewayAccount
    { id :: B.C f (Maybe Int)
    , version :: B.C f Int
    , accountDetails :: B.C f Text
    , gateway :: B.C f Text
    , merchantId :: B.C f Text
    , paymentMethods :: B.C f (Maybe Text)
    , testMode :: B.C f (Maybe Bool)
    , disabled :: B.C f (Maybe Bool)
    , disabledBy :: B.C f (Maybe Text)
    , disabledAt :: B.C f (Maybe LocalTime)
    , isJuspayAccount :: B.C f (Maybe Bool)
    , enforcePaymentMethodAcceptance :: B.C f (Maybe Bool)
    , eulerAccountDetails :: B.C f (Maybe EulerAccountDetails)
    , referenceId :: B.C f (Maybe Text)
    , supportedCurrencies :: B.C f (Maybe Text)
    } deriving (Generic, B.Beamable)

instance B.Table MerchantGatewayAccountT where
  data PrimaryKey MerchantGatewayAccountT f = Id (B.C f (Maybe Int))
                                              deriving (Generic, B.Beamable)
  primaryKey = Id . id

type MerchantGatewayAccount = MerchantGatewayAccountT Identity

deriving instance Show MerchantGatewayAccount
deriving instance Eq MerchantGatewayAccount
deriving instance ToJSON MerchantGatewayAccount
deriving instance FromJSON MerchantGatewayAccount
deriving instance Read MerchantGatewayAccount
deriving instance Ord MerchantGatewayAccount

merchantGatewayAccountEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity MerchantGatewayAccountT)
merchantGatewayAccountEMod =
  B.modifyTableFields
    B.tableModification
      { id = B.fieldNamed "id"
      , version = B.fieldNamed "version"
      , accountDetails = B.fieldNamed "account_details"
      , gateway = B.fieldNamed "gateway"
      , merchantId = B.fieldNamed "merchant_id"
      , paymentMethods = B.fieldNamed "payment_methods"
      , testMode = B.fieldNamed "test_mode"
      , disabled = B.fieldNamed "disabled"
      , disabledAt = B.fieldNamed "disabled_at"
      , disabledBy = B.fieldNamed "disabled_by"
      , isJuspayAccount = B.fieldNamed "is_juspay_account"
      , enforcePaymentMethodAcceptance =
          B.fieldNamed "enforce_payment_method_acceptance"
      , eulerAccountDetails = B.fieldNamed "euler_account_details"
      , referenceId = B.fieldNamed "reference_id"
      , supportedCurrencies = B.fieldNamed "supported_currencies"
      }

defaultMerchantGatewayAccount :: MerchantGatewayAccount
defaultMerchantGatewayAccount = MerchantGatewayAccount
  { id                             = Just 1 -- :: (Maybe Int)
  , version                        = 1 -- :: Int
  , accountDetails                 = "accountDetails" -- :: Text
  , gateway                        = "gateway" -- :: Text
  , merchantId                     = "merchant_id" -- :: Text
  , paymentMethods                 = Nothing -- :: (Maybe Text)
  , testMode                       = Nothing -- :: (Maybe Bool)
  , disabled                       = Nothing -- :: (Maybe Bool)
  , disabledBy                     = Nothing -- :: (Maybe Text)
  , disabledAt                     = Nothing -- :: (Maybe LocalTime)
  , isJuspayAccount                = Nothing -- :: (Maybe Bool)
  , enforcePaymentMethodAcceptance = Nothing -- :: (Maybe Bool)
  , eulerAccountDetails            = Nothing -- :: (Maybe EulerAccountDetails)
  , referenceId                    = Nothing -- :: (Maybe Text)
  , supportedCurrencies            = Nothing -- :: (Maybe Text)
  }
