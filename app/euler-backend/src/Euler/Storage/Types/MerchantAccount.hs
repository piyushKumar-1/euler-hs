{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.MerchantAccount
  (
    MerchantAccountT(..)
  , MerchantAccount
  , Id
  , merchantAccountEMod
  , defaultMerchantAccount
  ) where

import EulerHS.Prelude hiding (id, state, zip)

import Data.Time

import qualified Database.Beam as B

data MerchantAccountT f = MerchantAccount
  { id                                  :: B.C f (Maybe Int)
  , merchantName                        :: B.C f (Maybe Text)
  , apiKey                              :: B.C f (Maybe Text)
  , userId                              :: B.C f (Maybe Int)
  , alternativeEmail                    :: B.C f (Maybe Text)
  , contactPerson                       :: B.C f (Maybe Text)
  , mobile                              :: B.C f (Maybe Text)
  , telephone                           :: B.C f (Maybe Text)
  , website                             :: B.C f (Maybe Text)
  , aboutBusiness                       :: B.C f (Maybe Text)
  , adminContactEmail                   :: B.C f (Maybe Text)
  , city                                :: B.C f (Maybe Text)
  , contactPersonEmail                  :: B.C f (Maybe Text)
  , contactPersonPrimary                :: B.C f (Maybe Text)
  , contactPersonSecondary              :: B.C f (Maybe Text)
  , contactPersonSecondaryEmail         :: B.C f (Maybe Text)
  , landmark                            :: B.C f (Maybe Text)
  , officeLine1                         :: B.C f (Maybe Text)
  , officeLine2                         :: B.C f (Maybe Text)
  , officeLine3                         :: B.C f (Maybe Text)
  , state                               :: B.C f (Maybe Text) -- 20
  , zip                                 :: B.C f (Maybe Text)
  , dateCreated                         :: B.C f (Maybe LocalTime)
  , lastModified                        :: B.C f (Maybe LocalTime)
  , paymentResponseHashKey              :: B.C f (Maybe Text)
  , returnUrl                           :: B.C f (Maybe Text)
  , autoRefundConflictTransactions      :: B.C f (Maybe Bool)
  , enablePaymentResponseHash           :: B.C f (Maybe Bool)
  , enableAutomaticRetry                :: B.C f (Maybe Bool)
  , enableConflictStatusNotification    :: B.C f (Maybe Bool)
  , webHookCustomHeaders                :: B.C f (Maybe Text) -- 30
  , conflictStatusEmail                 :: B.C f (Maybe Text)
  , webHookPassword                     :: B.C f (Maybe Text)
  , webHookUsername                     :: B.C f (Maybe Text)
  , enableSaveCardBeforeAuth            :: B.C f (Maybe Bool)
  , webHookapiversion                   :: B.C f (Maybe Text)
  , webHookurl                          :: B.C f (Maybe Text)
  , cardEncodingKey                     :: B.C f (Maybe Text)
  , txnIdCustomPrefix                   :: B.C f (Maybe Text)
  , includeSurchargeAmountForRefund     :: B.C f (Maybe Bool)
  , shouldAddSurcharge                  :: B.C f (Maybe Bool) -- 40
  , showSurchargeBreakupScreen          :: B.C f (Maybe Bool)
  , enabledInstantRefund                :: B.C f (Maybe Bool)
  , enableTxnFilter                     :: B.C f (Maybe Bool)
  , version                             :: B.C f (Int)
  , bankAccountNumber                   :: B.C f (Maybe Text)
  , creditBalance                       :: B.C f (Maybe Double)
  , merchantId                          :: B.C f (Maybe Text)
  , expressCheckoutEnabled              :: B.C f (Maybe Bool)
  , inlineCheckoutEnabled               :: B.C f (Maybe Bool)
  , inlineCheckoutHtml                  :: B.C f (Maybe Text) -- 50
  , iciciKey                            :: B.C f (Maybe Text)
  , iciciMerchantId                     :: B.C f (Maybe Text)
  , iciciSecondFactorId                 :: B.C f (Maybe Text)
  , iciciVerified                       :: B.C f (Maybe Bool)
  , lockerId                            :: B.C f (Maybe Text)
  , axisMerchantId                      :: B.C f (Maybe Text)
  , axisVerified                        :: B.C f (Maybe Bool)
  , axisSecureSecret                    :: B.C f (Maybe Text)
  , axisAccessCode                      :: B.C f (Maybe Text)
  , hdfcMerchantId                      :: B.C f (Maybe Text) -- 60
  , hdfcPassword                        :: B.C f (Maybe Text)
  , hdfcVerified                        :: B.C f (Maybe Bool)
  , ebsAccountId                        :: B.C f (Maybe Text)
  , ebsHash                             :: B.C f (Maybe Text)
  , ebsVerified                         :: B.C f (Maybe Bool)
  , enableReauthentication              :: B.C f (Maybe Bool)
  , enableReauthorization               :: B.C f (Maybe Bool)
  , enableRecapture                     :: B.C f (Maybe Bool)
  , enableSendingLastFourDigits         :: B.C f (Maybe Bool)
  , domain                              :: B.C f (Maybe Text) -- 70
  , whitelabelEnabled                   :: B.C f (Maybe Bool)
  , payuMerchantKey                     :: B.C f (Maybe Text)
  , payuSalt                            :: B.C f (Maybe Text)
  , payuVerified                        :: B.C f (Maybe Bool)
  , otpEnabled                          :: B.C f (Maybe Bool)
  , reverseTokenEnabled                 :: B.C f (Maybe Bool)
  , hdfcTestMode                        :: B.C f (Maybe Bool)
  , realModeOnly                        :: B.C f (Maybe Bool)
  , gatewayDecidedByHealthEnabled       :: B.C f (Maybe Bool)
  , gatewayPriority                     :: B.C f (Maybe Text) -- 80
  , amexMerchantId                      :: B.C f (Maybe Text)
  , amexVerified                        :: B.C f (Maybe Bool)
  , mustUseGivenOrderIdForTxn           :: B.C f (Maybe Bool)
  , enable3DsecureHelpMail              :: B.C f (Maybe Bool)
  , amexAccessCode                      :: B.C f (Maybe Text) -- 85
  , amexPassword                        :: B.C f (Maybe Text)
  , amexSecureSecret                    :: B.C f (Maybe Text)
  , amexUsername                        :: B.C f (Maybe Text)
  , paypalClientId                      :: B.C f (Maybe Text)
  , paypalSecret                        :: B.C f (Maybe Text) -- 90
  , hdfcIvrMerchantId                   :: B.C f (Maybe Text)
  , hdfcIvrPassword                     :: B.C f (Maybe Text)
  , hdfcIvrVerified                     :: B.C f (Maybe Bool)
  , settlementAccountId                 :: B.C f (Maybe Int)
  , enableRefundsInDashboard            :: B.C f (Maybe Bool) -- 95
  , hdfcMerchantCode                    :: B.C f (Maybe Text)
  , merchantLegalName                   :: B.C f (Maybe Text)
  , orderFieldsInSettlementReport       :: B.C f (Maybe Text)
  , enableSendingCardIsin               :: B.C f (Maybe Bool)
  , ccavenueAccountId                   :: B.C f (Maybe Text) -- 100
  , ccavenueSecretKey                   :: B.C f (Maybe Text)
  , ccavenueVerified                    :: B.C f (Maybe Bool)
  , mobileVersion                       :: B.C f (Maybe Text)
  , enableOrderNotification             :: B.C f (Maybe Bool)
  , orderNotificationEmail              :: B.C f (Maybe Text) -- 105
  , ccavenueBackendGateway              :: B.C f (Maybe Text)
  , payuTestMode                        :: B.C f (Maybe Bool)
  , ebsBackendGateway                   :: B.C f (Maybe Text)
  , gatewayPriorityLogic                :: B.C f (Maybe Text)
  , useCodeForGatewayPriority           :: B.C f (Bool)      -- 110
  , enableExternalRiskCheck             :: B.C f (Maybe Bool)
  , citiKey                             :: B.C f (Maybe Text)
  , citiMerchantCode                    :: B.C f (Maybe Text)
  , citiTestMode                        :: B.C f (Maybe Bool)
  , citiVerified                        :: B.C f (Maybe Bool) -- 115
  , fetchCardsFromPayu                  :: B.C f (Maybe Bool)
  , prefixMerchantIdForCardKey          :: B.C f (Maybe Bool)
  , secondaryMerchantAccountId          :: B.C f (Maybe Int)
  , resellerId                          :: B.C f (Maybe Text)
  , fingerprintOnTokenize               :: B.C f (Maybe Bool) -- 120
  , enableUnauthenticatedOrderStatusApi :: B.C f (Maybe Bool) -- should be present and defaulted to False, not maybe?
  , enableUnauthenticatedCardAdd        :: B.C f (Maybe Bool)
  , timezone                            :: B.C f (Maybe Text)
  , mandatory2FA                        :: B.C f (Maybe Bool)
  }
  deriving (Generic, B.Beamable)



instance B.Table MerchantAccountT where
  data PrimaryKey MerchantAccountT f =
    Id (B.C f (Maybe Int)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type MerchantAccount = MerchantAccountT Identity
type Id = B.PrimaryKey MerchantAccountT Identity

deriving instance Show MerchantAccount
deriving instance Eq MerchantAccount
deriving instance ToJSON MerchantAccount
deriving instance FromJSON MerchantAccount
deriving instance Read MerchantAccount
deriving instance Ord MerchantAccount

merchantAccountEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity MerchantAccountT)
merchantAccountEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , merchantName = B.fieldNamed "merchant_name"
    , apiKey = B.fieldNamed "api_key"
    , userId = B.fieldNamed "user_id"
    , alternativeEmail = B.fieldNamed "alternative_email"
    , contactPerson = B.fieldNamed "contact_person"
    , mobile = B.fieldNamed "mobile"
    , telephone = B.fieldNamed "telephone"
    , website = B.fieldNamed "website"
    , aboutBusiness = B.fieldNamed "about_business"
    , adminContactEmail = B.fieldNamed "admin_contact_email"
    , city = B.fieldNamed "city"
    , contactPersonEmail = B.fieldNamed "contact_person_email"
    , contactPersonPrimary = B.fieldNamed "contact_person_primary"
    , contactPersonSecondary = B.fieldNamed "contact_person_secondary"
    , contactPersonSecondaryEmail = B.fieldNamed "contact_person_secondary_email"
    , landmark = B.fieldNamed "landmark"
    , officeLine1 = B.fieldNamed "office_line1"
    , officeLine2 = B.fieldNamed "office_line2"
    , officeLine3 = B.fieldNamed "office_line3"
    , state = B.fieldNamed "state"
    , zip = B.fieldNamed "zip"
    , dateCreated = B.fieldNamed "date_created"
    , lastModified = B.fieldNamed "last_modified"
    , paymentResponseHashKey = B.fieldNamed "payment_response_hash_key"
    , returnUrl = B.fieldNamed "return_url"
    , autoRefundConflictTransactions = B.fieldNamed "auto_refund_conflict_transactions"
    , enablePaymentResponseHash = B.fieldNamed "enable_payment_response_hash"
    , enableAutomaticRetry = B.fieldNamed "enable_automatic_retry"
    , enableConflictStatusNotification = B.fieldNamed "enable_conflict_status_notification"
    , webHookCustomHeaders = B.fieldNamed "webhook_custom_headers"
    , conflictStatusEmail = B.fieldNamed "conflict_status_email"
    , webHookPassword = B.fieldNamed "web_hook_password"
    , webHookUsername = B.fieldNamed "web_hook_username"
    , enableSaveCardBeforeAuth = B.fieldNamed "enable_save_card_before_auth"
    , webHookapiversion = B.fieldNamed "web_hookapiversion"
    , webHookurl = B.fieldNamed "web_hookurl"
    , cardEncodingKey = B.fieldNamed "card_encoding_key"
    , txnIdCustomPrefix = B.fieldNamed "txn_id_custom_prefix"
    , includeSurchargeAmountForRefund = B.fieldNamed "include_surcharge_amount_for_refund"
    , shouldAddSurcharge = B.fieldNamed "should_add_surcharge"
    , showSurchargeBreakupScreen = B.fieldNamed "show_surcharge_breakup_screen"
    , enabledInstantRefund = B.fieldNamed "enabled_instant_refund"
    , enableTxnFilter = B.fieldNamed "enable_txn_filter"
    , version = B.fieldNamed "version"
    , bankAccountNumber = B.fieldNamed "bank_account_number"
    , creditBalance = B.fieldNamed "credit_balance"
    , merchantId = B.fieldNamed "merchant_id"
    , expressCheckoutEnabled = B.fieldNamed "express_checkout_enabled"
    , inlineCheckoutEnabled = B.fieldNamed "inline_checkout_enabled"
    , inlineCheckoutHtml = B.fieldNamed "inline_checkout_html"
    , iciciKey = B.fieldNamed "icici_key"
    , iciciMerchantId = B.fieldNamed "icici_merchant_id"
    , iciciSecondFactorId = B.fieldNamed "icici_second_factor_id"
    , iciciVerified = B.fieldNamed "icici_verified"
    , lockerId = B.fieldNamed "locker_id"
    , axisMerchantId = B.fieldNamed "axis_merchant_id"
    , axisVerified = B.fieldNamed "axis_verified"
    , axisSecureSecret = B.fieldNamed "axis_secure_secret"
    , axisAccessCode = B.fieldNamed "axis_access_code"
    , hdfcMerchantId = B.fieldNamed "hdfc_merchant_id"
    , hdfcPassword = B.fieldNamed "hdfc_password"
    , hdfcVerified = B.fieldNamed "hdfc_verified"
    , ebsAccountId = B.fieldNamed "ebs_account_id"
    , ebsHash = B.fieldNamed "ebs_hash"
    , ebsVerified = B.fieldNamed "ebs_verified"
    , enableReauthentication = B.fieldNamed "enable_reauthentication"
    , enableReauthorization = B.fieldNamed "enable_reauthorization"
    , enableRecapture = B.fieldNamed "enable_recapture"
    , enableSendingLastFourDigits = B.fieldNamed "enable_sending_last_four_digits"
    , domain = B.fieldNamed "domain"
    , whitelabelEnabled = B.fieldNamed "whitelabel_enabled"
    , payuMerchantKey = B.fieldNamed "payu_merchant_key"
    , payuSalt = B.fieldNamed "payu_salt"
    , payuVerified = B.fieldNamed "payu_verified"
    , otpEnabled = B.fieldNamed "otp_enabled"
    , reverseTokenEnabled = B.fieldNamed "reverse_token_enabled"
    , hdfcTestMode = B.fieldNamed "hdfc_test_mode"
    , realModeOnly = B.fieldNamed "real_mode_only"
    , gatewayDecidedByHealthEnabled = B.fieldNamed "gateway_decided_by_health_enabled"
    , gatewayPriority = B.fieldNamed "gateway_priority"
    , amexMerchantId = B.fieldNamed "amex_merchant_id"
    , amexVerified = B.fieldNamed "amex_verified"
    , mustUseGivenOrderIdForTxn = B.fieldNamed "must_use_given_order_id_for_txn"
    , enable3DsecureHelpMail = B.fieldNamed "enable3dsecure_help_mail"
    , amexAccessCode = B.fieldNamed "amex_access_code"
    , amexPassword = B.fieldNamed "amex_password"
    , amexSecureSecret = B.fieldNamed "amex_secure_secret"
    , amexUsername = B.fieldNamed "amex_username"
    , paypalClientId = B.fieldNamed "paypal_client_id"
    , paypalSecret = B.fieldNamed "paypal_secret"
    , hdfcIvrMerchantId = B.fieldNamed "hdfc_ivr_merchant_id"
    , hdfcIvrPassword = B.fieldNamed "hdfc_ivr_password"
    , hdfcIvrVerified = B.fieldNamed "hdfc_ivr_verified"
    , settlementAccountId = B.fieldNamed "settlement_account_id"
    , enableRefundsInDashboard = B.fieldNamed "enable_refunds_in_dashboard"
    , hdfcMerchantCode = B.fieldNamed "hdfc_merchant_code"
    , merchantLegalName = B.fieldNamed "merchant_legal_name"
    , orderFieldsInSettlementReport = B.fieldNamed "order_fields_in_settlement_report"
    , enableSendingCardIsin = B.fieldNamed "enable_sending_card_isin"
    , ccavenueAccountId = B.fieldNamed "ccavenue_account_id"
    , ccavenueSecretKey = B.fieldNamed "ccavenue_secret_key"
    , ccavenueVerified = B.fieldNamed "ccavenue_verified"
    , mobileVersion = B.fieldNamed "mobile_version"
    , enableOrderNotification = B.fieldNamed "enable_order_notification"
    , orderNotificationEmail = B.fieldNamed "order_notification_email"
    , ccavenueBackendGateway = B.fieldNamed "ccavenue_backend_gateway"
    , payuTestMode = B.fieldNamed "payu_test_mode"
    , ebsBackendGateway = B.fieldNamed "ebs_backend_gateway"
    , gatewayPriorityLogic = B.fieldNamed "gateway_priority_logic"
    , useCodeForGatewayPriority = B.fieldNamed "use_code_for_gateway_priority"
    , enableExternalRiskCheck = B.fieldNamed "enable_external_risk_check"
    , citiKey = B.fieldNamed "citi_key"
    , citiMerchantCode = B.fieldNamed "citi_merchant_code"
    , citiTestMode = B.fieldNamed "citi_test_mode"
    , citiVerified = B.fieldNamed "citi_verified"
    , fetchCardsFromPayu = B.fieldNamed "fetch_cards_from_payu"
    , prefixMerchantIdForCardKey = B.fieldNamed "prefix_merchant_id_for_card_key"
    , secondaryMerchantAccountId = B.fieldNamed "secondary_merchant_account_id"
    , resellerId = B.fieldNamed "reseller_id"
    , fingerprintOnTokenize = B.fieldNamed "fingerprint_on_tokenize"
    , enableUnauthenticatedOrderStatusApi = B.fieldNamed "enable_unauthenticated_order_status_api"
    , enableUnauthenticatedCardAdd = B.fieldNamed "enable_unauthenticated_card_add"
    , timezone = B.fieldNamed "timezone"
    , mandatory2FA = B.fieldNamed "mandatory_2FA"
  }

defaultMerchantAccount :: MerchantAccount
defaultMerchantAccount = MerchantAccount
  { id                                  = Just 1  -- :: Maybe Int
  , merchantName                        = Just "merchantName" -- :: Maybe Text
  , apiKey                              = Just "apiKey" -- :: Maybe Text
  , userId                              = Nothing -- :: Maybe Int
  , alternativeEmail                    = Nothing -- :: Maybe Text
  , contactPerson                       = Nothing -- :: Maybe Text
  , mobile                              = Nothing -- :: Maybe Text
  , telephone                           = Nothing -- :: Maybe Text
  , website                             = Nothing -- :: Maybe Text
  , aboutBusiness                       = Nothing -- :: Maybe Text
  , adminContactEmail                   = Nothing -- :: Maybe Text
  , city                                = Nothing -- :: Maybe Text
  , contactPersonEmail                  = Nothing -- :: Maybe Text
  , contactPersonPrimary                = Nothing -- :: Maybe Text
  , contactPersonSecondary              = Nothing -- :: Maybe Text
  , contactPersonSecondaryEmail         = Nothing -- :: Maybe Text
  , landmark                            = Nothing -- :: Maybe Text
  , officeLine1                         = Nothing -- :: Maybe Text
  , officeLine2                         = Nothing -- :: Maybe Text
  , officeLine3                         = Nothing -- :: Maybe Text
  , state                               = Nothing -- :: Maybe Text
  , zip                                 = Nothing -- :: Maybe Text
  , dateCreated                         = Nothing -- :: Maybe LocalTime
  , lastModified                        = Nothing -- :: Maybe LocalTime
  , paymentResponseHashKey              = Nothing -- :: Maybe Text
  , returnUrl                           = Nothing -- :: Maybe Text
  , autoRefundConflictTransactions      = Nothing -- :: Maybe Bool
  , enablePaymentResponseHash           = Nothing -- :: Maybe Bool
  , enableAutomaticRetry                = Nothing -- :: Maybe Bool
  , enableConflictStatusNotification    = Nothing -- :: Maybe Bool
  , webHookCustomHeaders                = Nothing -- :: Maybe Text
  , conflictStatusEmail                 = Nothing -- :: Maybe Text
  , webHookPassword                     = Nothing -- :: Maybe Text
  , webHookUsername                     = Nothing -- :: Maybe Text
  , enableSaveCardBeforeAuth            = Nothing -- :: Maybe Bool
  , webHookapiversion                   = Nothing -- :: Maybe Text
  , webHookurl                          = Nothing -- :: Maybe Text
  , cardEncodingKey                     = Nothing -- :: Maybe Text
  , txnIdCustomPrefix                   = Nothing -- :: Maybe Text
  , includeSurchargeAmountForRefund     = Nothing -- :: Maybe Bool
  , shouldAddSurcharge                  = Nothing -- :: Maybe Bool
  , showSurchargeBreakupScreen          = Nothing -- :: Maybe Bool
  , enabledInstantRefund                = Nothing -- :: Maybe Bool
  , enableTxnFilter                     = Nothing -- :: Maybe Bool
  , version                             = 0       -- :: Int
  , bankAccountNumber                   = Nothing -- :: Maybe Text
  , creditBalance                       = Nothing -- :: Maybe Double
  , merchantId                          = Just "1" -- Nothing -- :: Maybe Text
  , expressCheckoutEnabled              = Nothing -- :: Maybe Bool
  , inlineCheckoutEnabled               = Nothing -- :: Maybe Bool
  , inlineCheckoutHtml                  = Nothing -- :: Maybe Text
  , iciciKey                            = Nothing -- :: Maybe Text
  , iciciMerchantId                     = Nothing -- :: Maybe Text
  , iciciSecondFactorId                 = Nothing -- :: Maybe Text
  , iciciVerified                       = Nothing -- :: Maybe Bool
  , lockerId                            = Nothing -- :: Maybe Text
  , axisMerchantId                      = Nothing -- :: Maybe Text
  , axisVerified                        = Nothing -- :: Maybe Bool
  , axisSecureSecret                    = Nothing -- :: Maybe Text
  , axisAccessCode                      = Nothing -- :: Maybe Text
  , hdfcMerchantId                      = Nothing -- :: Maybe Text
  , hdfcPassword                        = Nothing -- :: Maybe Text
  , hdfcVerified                        = Nothing -- :: Maybe Bool
  , ebsAccountId                        = Nothing -- :: Maybe Text
  , ebsHash                             = Nothing -- :: Maybe Text
  , ebsVerified                         = Nothing -- :: Maybe Bool
  , enableReauthentication              = Nothing -- :: Maybe Bool
  , enableReauthorization               = Nothing -- :: Maybe Bool
  , enableRecapture                     = Nothing -- :: Maybe Bool
  , enableSendingLastFourDigits         = Nothing -- :: Maybe Bool
  , domain                              = Nothing -- :: Maybe Text
  , whitelabelEnabled                   = Nothing -- :: Maybe Bool
  , payuMerchantKey                     = Nothing -- :: Maybe Text
  , payuSalt                            = Nothing -- :: Maybe Text
  , payuVerified                        = Nothing -- :: Maybe Bool
  , otpEnabled                          = Nothing -- :: Maybe Bool
  , reverseTokenEnabled                 = Nothing -- :: Maybe Bool
  , hdfcTestMode                        = Nothing -- :: Maybe Bool
  , realModeOnly                        = Nothing -- :: Maybe Bool
  , gatewayDecidedByHealthEnabled       = Nothing -- :: Maybe Bool
  , gatewayPriority                     = Nothing -- :: Maybe Text
  , amexMerchantId                      = Nothing -- :: Maybe Text
  , amexVerified                        = Nothing -- :: Maybe Bool
  , mustUseGivenOrderIdForTxn           = Nothing -- :: Maybe Bool
  , enable3DsecureHelpMail              = Nothing -- :: Maybe Bool
  , amexAccessCode                      = Nothing -- :: Maybe Text
  , amexPassword                        = Nothing -- :: Maybe Text
  , amexSecureSecret                    = Nothing -- :: Maybe Text
  , amexUsername                        = Nothing -- :: Maybe Text
  , paypalClientId                      = Nothing -- :: Maybe Text
  , paypalSecret                        = Nothing -- :: Maybe Text
  , hdfcIvrMerchantId                   = Nothing -- :: Maybe Text
  , hdfcIvrPassword                     = Nothing -- :: Maybe Text
  , hdfcIvrVerified                     = Nothing -- :: Maybe Bool
  , settlementAccountId                 = Nothing -- :: Maybe Int
  , enableRefundsInDashboard            = Nothing -- :: Maybe Bool
  , hdfcMerchantCode                    = Nothing -- :: Maybe Text
  , merchantLegalName                   = Nothing -- :: Maybe Text
  , orderFieldsInSettlementReport       = Nothing -- :: Maybe Text
  , enableSendingCardIsin               = Nothing -- :: Maybe Bool
  , ccavenueAccountId                   = Nothing -- :: Maybe Text
  , ccavenueSecretKey                   = Nothing -- :: Maybe Text
  , ccavenueVerified                    = Nothing -- :: Maybe Bool
  , mobileVersion                       = Nothing -- :: Maybe Text
  , enableOrderNotification             = Nothing -- :: Maybe Bool
  , orderNotificationEmail              = Nothing -- :: Maybe Text
  , ccavenueBackendGateway              = Nothing -- :: Maybe Text
  , payuTestMode                        = Nothing -- :: Maybe Bool
  , ebsBackendGateway                   = Nothing -- :: Maybe Text
  , gatewayPriorityLogic                = Nothing -- :: Maybe Text
  , useCodeForGatewayPriority           = False   -- :: Bool
  , enableExternalRiskCheck             = Nothing -- :: Maybe Bool
  , citiKey                             = Nothing -- :: Maybe Text
  , citiMerchantCode                    = Nothing -- :: Maybe Text
  , citiTestMode                        = Nothing -- :: Maybe Bool
  , citiVerified                        = Nothing -- :: Maybe Bool
  , fetchCardsFromPayu                  = Nothing -- :: Maybe Bool
  , prefixMerchantIdForCardKey          = Nothing -- :: Maybe Bool
  , secondaryMerchantAccountId          = Nothing -- :: Maybe Int
  , resellerId                          = Nothing -- :: Maybe Text
  , fingerprintOnTokenize               = Nothing -- :: Maybe Bool
  , enableUnauthenticatedOrderStatusApi = Nothing -- :: Maybe Bool -- should be present and defaulted to False, not maybe?
  , enableUnauthenticatedCardAdd        = Nothing -- :: Maybe Bool
  , timezone                            = Nothing -- :: Maybe Text
  , mandatory2FA                        = Nothing -- :: Maybe Bool
  }
