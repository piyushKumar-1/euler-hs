{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.MerchantIframePreferences
  ( MerchantIframePreferencesT(..)
  , MerchantIframePreferences
  , Id
  , merchantIframePreferencesEMod
  , defaultMerchantIframePreferences
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import qualified Database.Beam as B

data MerchantIframePreferencesT f = MerchantIframePreferences
  { id                          :: B.C f (Maybe Int)
  , version                     :: B.C f Int
  , customStylesheet            :: B.C f (Maybe Text)
  , returnUrl                   :: B.C f (Maybe Text)
  , paymentButtonLabel          :: B.C f (Maybe Text)
  , merchantLogoUrl             :: B.C f (Maybe Text)
  , showCancelButton            :: B.C f (Maybe Bool)
  , emergencyMessage            :: B.C f (Maybe Text)
  , redirectModeOnly            :: B.C f (Maybe Bool)
  , defaultCurrency             :: B.C f (Maybe Text)
  , termsAndConditionsLink      :: B.C f (Maybe Text)
  , orderSessionTimeout         :: B.C f (Maybe Double)
  , walletTopupReturnUrl        :: B.C f (Maybe Text)
  , saveCardEnabled             :: B.C f (Maybe Bool)
  , merchantId                  :: B.C f Text
  , headingText                 :: B.C f (Maybe Text)
  , orderCreationUrl            :: B.C f (Maybe Text)
  , dynamicSwitchingEnabled     :: B.C f (Maybe Bool)
  , isinRoutingEnabled          :: B.C f (Maybe Bool)
  , issuerRoutingEnabled        :: B.C f (Maybe Bool)
  , mirrorGatewayResponse       :: B.C f (Maybe Bool)
  , txnFailureGatewayPenality   :: B.C f (Maybe Bool)
  , passThroughMode             :: B.C f (Maybe Bool)
  , deleteCardOnExpiry          :: B.C f (Maybe Bool)
  , canAcceptAmex               :: B.C f (Maybe Bool)
  , canAcceptDiners             :: B.C f (Maybe Bool)
  , canAcceptDiscover           :: B.C f (Maybe Bool)
  , canAcceptJcb                :: B.C f (Maybe Bool)
  , canAcceptMaster             :: B.C f (Maybe Bool)
  , canAcceptRupay              :: B.C f (Maybe Bool)
  , canAcceptVisa               :: B.C f (Maybe Bool)
  , canAcceptMaestro            :: B.C f (Maybe Bool)
  , customMobileStylesheet      :: B.C f (Maybe Text)
  , cardBrandRoutingEnabled     :: B.C f (Maybe Bool)
  , alwaysAcceptCards           :: B.C f (Maybe Bool)
  , additionalParamsInResponse  :: B.C f (Maybe Text)
  , additionalParamsInResponse1 :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantIframePreferencesT where
  data PrimaryKey MerchantIframePreferencesT f =
    Id (B.C f (Maybe Int)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type MerchantIframePreferences = MerchantIframePreferencesT Identity
type Id = B.PrimaryKey MerchantIframePreferencesT Identity

deriving instance Show MerchantIframePreferences
deriving instance Eq MerchantIframePreferences
deriving instance ToJSON MerchantIframePreferences
deriving instance FromJSON MerchantIframePreferences
deriving instance Read MerchantIframePreferences
deriving instance Ord MerchantIframePreferences

merchantIframePreferencesEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity MerchantIframePreferencesT)
merchantIframePreferencesEMod = B.modifyTableFields
  B.tableModification 
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , customStylesheet = B.fieldNamed "custom_stylesheet"
    , returnUrl = B.fieldNamed "return_url"
    , paymentButtonLabel = B.fieldNamed "payment_button_label"
    , merchantLogoUrl = B.fieldNamed "merchant_logo_url"
    , showCancelButton = B.fieldNamed "show_cancel_button"
    , emergencyMessage = B.fieldNamed "emergency_message"
    , redirectModeOnly = B.fieldNamed "redirect_mode_only"
    , defaultCurrency = B.fieldNamed "default_currency"
    , termsAndConditionsLink = B.fieldNamed "terms_and_conditions_link"
    , orderSessionTimeout = B.fieldNamed "order_session_timeout"
    , walletTopupReturnUrl = B.fieldNamed "wallet_topup_return_url"
    , saveCardEnabled = B.fieldNamed "save_card_enabled"
    , merchantId = B.fieldNamed "merchant_id"
    , headingText = B.fieldNamed "heading_text"
    , orderCreationUrl = B.fieldNamed "order_creation_url"
    , dynamicSwitchingEnabled = B.fieldNamed "dynamic_switching_enabled"
    , isinRoutingEnabled = B.fieldNamed "isin_routing_enabled"
    , issuerRoutingEnabled = B.fieldNamed "issuer_routing_enabled"
    , mirrorGatewayResponse = B.fieldNamed "mirror_gateway_response"
    , txnFailureGatewayPenality = B.fieldNamed "txn_failure_gateway_penality"
    , passThroughMode = B.fieldNamed "pass_through_mode"
    , deleteCardOnExpiry = B.fieldNamed "delete_card_on_expiry"
    , canAcceptAmex = B.fieldNamed "can_accept_amex"
    , canAcceptDiners = B.fieldNamed "can_accept_diners"
    , canAcceptDiscover = B.fieldNamed "can_accept_discover"
    , canAcceptJcb = B.fieldNamed "can_accept_jcb"
    , canAcceptMaster = B.fieldNamed "can_accept_master"
    , canAcceptRupay = B.fieldNamed "can_accept_rupay"
    , canAcceptVisa = B.fieldNamed "can_accept_visa"
    , canAcceptMaestro = B.fieldNamed "can_accept_maestro"
    , customMobileStylesheet = B.fieldNamed "custom_mobile_stylesheet"
    , cardBrandRoutingEnabled = B.fieldNamed "card_brand_routing_enabled"
    , alwaysAcceptCards = B.fieldNamed "always_accept_cards"
    , additionalParamsInResponse = B.fieldNamed "additional_params_in_response"
    , additionalParamsInResponse1 = B.fieldNamed "additional_params_in_response1"
    }

defaultMerchantIframePreferences :: MerchantIframePreferences
defaultMerchantIframePreferences = MerchantIframePreferences
  { id                           = Nothing -- :: Maybe Int
  , version                      = 0       -- :: Int
  , customStylesheet             = Nothing -- :: Maybe Text
  , returnUrl                    = Nothing -- :: Maybe Text
  , paymentButtonLabel           = Nothing -- :: Maybe Text
  , merchantLogoUrl              = Nothing -- :: Maybe Text
  , showCancelButton             = Nothing -- :: Maybe Bool
  , emergencyMessage             = Nothing -- :: Maybe Text
  , redirectModeOnly             = Nothing -- :: Maybe Bool
  , defaultCurrency              = Nothing -- :: Maybe Text
  , termsAndConditionsLink       = Nothing -- :: Maybe Text
  , orderSessionTimeout          = Nothing -- :: Maybe Double
  , walletTopupReturnUrl         = Nothing -- :: Maybe Text
  , saveCardEnabled              = Nothing -- :: Maybe Bool
  , merchantId                   = ""      -- :: Text
  , headingText                  = Nothing -- :: Maybe Text
  , orderCreationUrl             = Nothing -- :: Maybe Text
  , dynamicSwitchingEnabled      = Nothing -- :: Maybe Bool
  , isinRoutingEnabled           = Nothing -- :: Maybe Bool
  , issuerRoutingEnabled         = Nothing -- :: Maybe Bool
  , mirrorGatewayResponse        = Nothing -- :: Maybe Bool
  , txnFailureGatewayPenality    = Nothing -- :: Maybe Bool
  , passThroughMode              = Nothing -- :: Maybe Bool
  , deleteCardOnExpiry           = Nothing -- :: Maybe Bool
  , canAcceptAmex                = Nothing -- :: Maybe Bool
  , canAcceptDiners              = Nothing -- :: Maybe Bool
  , canAcceptDiscover            = Nothing -- :: Maybe Bool
  , canAcceptJcb                 = Nothing -- :: Maybe Bool
  , canAcceptMaster              = Nothing -- :: Maybe Bool
  , canAcceptRupay               = Nothing -- :: Maybe Bool
  , canAcceptVisa                = Nothing -- :: Maybe Bool
  , canAcceptMaestro             = Nothing -- :: Maybe Bool
  , customMobileStylesheet       = Nothing -- :: Maybe Text
  , cardBrandRoutingEnabled      = Nothing -- :: Maybe Bool
  , alwaysAcceptCards            = Nothing -- :: Maybe Bool
  , additionalParamsInResponse   = Nothing -- :: Maybe Text
  , additionalParamsInResponse1  = Nothing -- :: Maybe Text
  }