{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Types.Merchant where

import EulerHS.Prelude
import Data.Time

--------------
-- from src/Types/Storage/EC/ResellerAccount.purs

-- newtype ResellerAccount = ResellerAccount (RAUser (resellerId :: Text))
-- newtype ResellerAccountFiltered = ResellerAccountFiltered (RAUser ())
-- newtype ResellerAccountLogin = ResellerAccountLogin (RAUser (
--   merchants :: Array Text
--   ))

-- type RAUser a =
--   { userId :: Maybe Int
--   , resellerName :: Maybe Text
--   , resellerApiEndpoint :: Maybe Text
--   , dateCreated :: Maybe Date
--   , lastModified :: Maybe Date
--     | a
--   }

data ResellerAccount = ResellerAccount
  { userId              :: Maybe Int
  , resellerName        :: Maybe Text
  , resellerApiEndpoint :: Maybe Text
  , dateCreated         :: Maybe LocalTime
  , lastModified        :: Maybe LocalTime
  , resellerId          :: Text
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

defaultResellerAccount = ResellerAccount
  { userId              = Nothing -- :: Maybe Int
  , resellerName        = Nothing -- :: Maybe Text
  , resellerApiEndpoint = Just "testEndpoint" -- Nothing -- :: Maybe Text
  , dateCreated         = Nothing -- :: Maybe LocalTime
  , lastModified        = Nothing -- :: Maybe LocalTime
  , resellerId          = "" -- :: Text
  }
------------
-- why we need wrapper?
newtype MerchantKey = MerchantKey MKey

data MKey = MKey
  { id :: Maybe Int
  , version :: Int
  , apiKey :: Maybe Text
  , status :: Maybe Text
  , dateCreated :: LocalTime
  , lastUpdated :: LocalTime
  , merchantAccountId :: Maybe Int
  , scope :: Maybe Text
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

-- newtype MerchantAccount = MerchantAccount (MAUser MADB)
-- some fields that exists in DB not present in type 
data MerchantAccount = MerchantAccount
  { id                                  :: Maybe Int
  , merchantName                        :: Maybe Text
  , apiKey                              :: Maybe Text
  , userId                              :: Maybe Int
  , alternativeEmail                    :: Maybe Text
  , contactPerson                       :: Maybe Text
  , mobile                              :: Maybe Text
  , telephone                           :: Maybe Text
  , website                             :: Maybe Text
  , aboutBusiness                       :: Maybe Text
  , adminContactEmail                   :: Maybe Text
  , city                                :: Maybe Text
  , contactPersonEmail                  :: Maybe Text
  , contactPersonPrimary                :: Maybe Text
  , contactPersonSecondary              :: Maybe Text
  , contactPersonSecondaryEmail         :: Maybe Text
  , landmark                            :: Maybe Text
  , officeLine1                         :: Maybe Text
  , officeLine2                         :: Maybe Text
  , officeLine3                         :: Maybe Text
  , state                               :: Maybe Text
  , zip                                 :: Maybe Text
  , dateCreated                         :: Maybe LocalTime
  , lastModified                        :: Maybe LocalTime
  , paymentResponseHashKey              :: Maybe Text
  , returnUrl                           :: Maybe Text
  , autoRefundConflictTransactions      :: Maybe Bool
  , enablePaymentResponseHash           :: Maybe Bool
  , enableAutomaticRetry                :: Maybe Bool
  , enableConflictStatusNotification    :: Maybe Bool
  , webHookCustomHeaders                :: Maybe Text
  , conflictStatusEmail                 :: Maybe Text
  , webHookPassword                     :: Maybe Text
  , webHookUsername                     :: Maybe Text
  , enableSaveCardBeforeAuth            :: Maybe Bool
  , webHookapiversion                   :: Maybe Text
  , webHookurl                          :: Maybe Text
  , cardEncodingKey                     :: Maybe Text
  , txnIdCustomPrefix                   :: Maybe Text
  , includeSurchargeAmountForRefund     :: Maybe Bool
  , shouldAddSurcharge                  :: Maybe Bool
  , showSurchargeBreakupScreen          :: Maybe Bool
  , enabledInstantRefund                :: Maybe Bool
  , enableTxnFilter                     :: Maybe Bool
  , version                             :: Int
  , bankAccountNumber                   :: Maybe Text
  , creditBalance                       :: Maybe Double
  , merchantId                          :: Maybe Text
  , expressCheckoutEnabled              :: Maybe Bool
  , inlineCheckoutEnabled               :: Maybe Bool
  , inlineCheckoutHtml                  :: Maybe Text
  , iciciKey                            :: Maybe Text
  , iciciMerchantId                     :: Maybe Text
  , iciciSecondFactorId                 :: Maybe Text
  , iciciVerified                       :: Maybe Bool
  , lockerId                            :: Maybe Text
  , axisMerchantId                      :: Maybe Text
  , axisVerified                        :: Maybe Bool
  , axisSecureSecret                    :: Maybe Text
  , axisAccessCode                      :: Maybe Text
  , hdfcMerchantId                      :: Maybe Text
  , hdfcPassword                        :: Maybe Text
  , hdfcVerified                        :: Maybe Bool
  , ebsAccountId                        :: Maybe Text
  , ebsHash                             :: Maybe Text
  , ebsVerified                         :: Maybe Bool
  , enableReauthentication              :: Maybe Bool
  , enableReauthorization               :: Maybe Bool
  , enableRecapture                     :: Maybe Bool
  , enableSendingLastFourDigits         :: Maybe Bool
  , domain                              :: Maybe Text
  , whitelabelEnabled                   :: Maybe Bool
  , payuMerchantKey                     :: Maybe Text
  , payuSalt                            :: Maybe Text
  , payuVerified                        :: Maybe Bool
  , otpEnabled                          :: Maybe Bool
  , reverseTokenEnabled                 :: Maybe Bool
  , hdfcTestMode                        :: Maybe Bool
  , realModeOnly                        :: Maybe Bool
  , gatewayDecidedByHealthEnabled       :: Maybe Bool
  , gatewayPriority                     :: Maybe Text
  , amexMerchantId                      :: Maybe Text
  , amexVerified                        :: Maybe Bool
  , mustUseGivenOrderIdForTxn           :: Maybe Bool
  , enable3DsecureHelpMail              :: Maybe Bool
  , amexAccessCode                      :: Maybe Text
  , amexPassword                        :: Maybe Text
  , amexSecureSecret                    :: Maybe Text
  , amexUsername                        :: Maybe Text
  , paypalClientId                      :: Maybe Text
  , paypalSecret                        :: Maybe Text
  , hdfcIvrMerchantId                   :: Maybe Text
  , hdfcIvrPassword                     :: Maybe Text
  , hdfcIvrVerified                     :: Maybe Bool
  , settlementAccountId                 :: Maybe Int
  , enableRefundsInDashboard            :: Maybe Bool
  , hdfcMerchantCode                    :: Maybe Text
  , merchantLegalName                   :: Maybe Text
  , orderFieldsInSettlementReport       :: Maybe Text
  , enableSendingCardIsin               :: Maybe Bool
  , ccavenueAccountId                   :: Maybe Text
  , ccavenueSecretKey                   :: Maybe Text
  , ccavenueVerified                    :: Maybe Bool
  , mobileVersion                       :: Maybe Text
  , enableOrderNotification             :: Maybe Bool
  , orderNotificationEmail              :: Maybe Text
  , ccavenueBackendGateway              :: Maybe Text
  , payuTestMode                        :: Maybe Bool
  , ebsBackendGateway                   :: Maybe Text
  , gatewayPriorityLogic                :: Maybe Text
  , useCodeForGatewayPriority           :: Bool
  , enableExternalRiskCheck             :: Maybe Bool
  , citiKey                             :: Maybe Text
  , citiMerchantCode                    :: Maybe Text
  , citiTestMode                        :: Maybe Bool
  , citiVerified                        :: Maybe Bool
  , fetchCardsFromPayu                  :: Maybe Bool
  , prefixMerchantIdForCardKey          :: Maybe Bool
  , secondaryMerchantAccountId          :: Maybe Int
  , resellerId                          :: Maybe Text
  , fingerprintOnTokenize               :: Maybe Bool
  , enableUnauthenticatedOrderStatusApi :: Maybe Bool -- should be present and defaulted to False, not maybe?
  , enableUnauthenticatedCardAdd        :: Maybe Bool
  , timezone                            :: Maybe Text
  , mandatory2FA                        :: Maybe Bool
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

  
{-
DATABASE REPRESENTATION :


CREATE TABLE `merchant_account` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `version` bigint(20) NOT NULL,
  `bank_account_number` varchar(32) DEFAULT NULL,
  `credit_balance` double NOT NULL,
  `merchant_id` varchar(64) DEFAULT NULL,
  `merchant_name` varchar(64) DEFAULT NULL,
  `api_key` varchar(255) DEFAULT NULL,
  `express_checkout_enabled` bit(1) DEFAULT NULL,
  `inline_checkout_enabled` bit(1) DEFAULT NULL,
  `inline_checkout_html` longtext,
  `user_id` bigint(20) DEFAULT NULL,
  `alternative_email` varchar(255) DEFAULT NULL,
  `contact_person` varchar(255) DEFAULT NULL,
  `mobile` varchar(255) DEFAULT NULL,
  `telephone` varchar(255) DEFAULT NULL,
  `website` varchar(255) DEFAULT NULL,
  `icici_key` text,
  `icici_merchant_id` varchar(255) DEFAULT NULL,
  `icici_second_factor_id` varchar(255) DEFAULT NULL,
  `about_business` varchar(255) DEFAULT NULL,
  `admin_contact_email` varchar(255) DEFAULT NULL,
  `city` varchar(255) DEFAULT NULL,
  `contact_person_email` varchar(255) DEFAULT NULL,
  `contact_person_primary` varchar(255) DEFAULT NULL,
  `contact_person_secondary` varchar(255) DEFAULT NULL,
  `contact_person_secondary_email` varchar(255) DEFAULT NULL,
  `landmark` varchar(255) DEFAULT NULL,
  `office_line1` varchar(255) DEFAULT NULL,
  `office_line2` varchar(255) DEFAULT NULL,
  `office_line3` varchar(255) DEFAULT NULL,
  `state` varchar(255) DEFAULT NULL,
  `zip` varchar(255) DEFAULT NULL,
  `icici_verified` bit(1) DEFAULT NULL,
  `locker_id` varchar(15) DEFAULT NULL,
  `date_created` datetime NOT NULL,
  `last_modified` datetime NOT NULL,
  `axis_merchant_id` varchar(255) DEFAULT NULL,
  `secure_secret` varchar(255) DEFAULT NULL,
  `axis_verified` bit(1) DEFAULT NULL,
  `axis_secure_secret` varchar(255) DEFAULT NULL,
  `axis_access_code` varchar(255) DEFAULT NULL,
  `hdfc_merchant_id` varchar(255) DEFAULT NULL,
  `hdfc_password` varchar(255) DEFAULT NULL,
  `hdfc_verified` bit(1) DEFAULT NULL,
  `ebs_account_id` varchar(12) DEFAULT NULL,
  `ebs_hash` varchar(32) DEFAULT NULL,
  `ebs_verified` bit(1) DEFAULT NULL,
  `enable_reauthentication` bit(1) DEFAULT NULL,
  `enable_reauthorization` bit(1) DEFAULT NULL,
  `enable_recapture` bit(1) DEFAULT NULL,
  `enable_sending_last_four_digits` bit(1) DEFAULT NULL,
  `return_url` varchar(128) DEFAULT NULL,
  `domain` varchar(64) DEFAULT NULL,
  `whitelabel_enabled` bit(1) DEFAULT NULL,
  `payu_merchant_key` varchar(64) DEFAULT NULL,
  `payu_salt` varchar(64) DEFAULT NULL,
  `payu_verified` bit(1) DEFAULT NULL,
  `otp_enabled` bit(1) DEFAULT NULL,
  `get_card_enabled` bit(1) DEFAULT NULL,
  `reverse_card_enabled` bit(1) DEFAULT NULL,
  `reverse_token_enabled` bit(1) DEFAULT NULL,
  `hdfc_test_mode` bit(1) DEFAULT NULL,
  `real_mode_only` bit(1) DEFAULT NULL,
  `gateway_decided_by_health_enabled` bit(1) DEFAULT NULL,
  `gateway_priority` varchar(255) DEFAULT NULL,
  `amex_merchant_id` varchar(255) DEFAULT NULL,
  `amex_verified` bit(1) DEFAULT NULL,
  `must_use_given_order_id_for_txn` bit(1) DEFAULT NULL,
  `enable3dsecure_help_mail` bit(1) DEFAULT NULL,
  `amex_access_code` varchar(255) DEFAULT NULL,
  `amex_password` varchar(255) DEFAULT NULL,
  `amex_secure_secret` varchar(255) DEFAULT NULL,
  `amex_username` varchar(255) DEFAULT NULL,
  `paypal_client_id` varchar(255) DEFAULT NULL,
  `paypal_secret` varchar(255) DEFAULT NULL,
  `hdfc_ivr_merchant_id` varchar(255) DEFAULT NULL,
  `hdfc_ivr_password` varchar(255) DEFAULT NULL,
  `hdfc_ivr_verified` bit(1) DEFAULT NULL,
  `settlement_account_id` bigint(20) DEFAULT NULL,
  `country` varchar(255) DEFAULT NULL,
  `enable_refunds_in_dashboard` bit(1) DEFAULT NULL,
  `hdfc_merchant_code` varchar(255) DEFAULT NULL,
  `merchant_legal_name` varchar(255) DEFAULT NULL,
  `order_fields_in_settlement_report` varchar(255) DEFAULT NULL,
  `enable_sending_card_isin` bit(1) DEFAULT NULL,
  `ccavenue_account_id` varchar(255) DEFAULT NULL,
  `ccavenue_secret_key` varchar(255) DEFAULT NULL,
  `ccavenue_verified` bit(1) DEFAULT NULL,
  `mobile_version` varchar(255) DEFAULT NULL,
  `web_version` varchar(255) DEFAULT NULL,
  `iframe_version` varchar(255) DEFAULT NULL,
  `enable_order_notification` bit(1) DEFAULT NULL,
  `order_notification_email` varchar(255) DEFAULT NULL,
  `ccavenue_backend_gateway` varchar(255) DEFAULT NULL,
  `web_hookurl` varchar(255) DEFAULT NULL,
  `payu_test_mode` bit(1) DEFAULT NULL,
  `ebs_backend_gateway` varchar(255) DEFAULT NULL,
  `enable_automatic_retry` bit(1) DEFAULT NULL,
  `gateway_priority_logic` text,
  `use_code_for_gateway_priority` bit(1) NOT NULL DEFAULT b'0',
  `enable_storing_failed_card` bit(1) DEFAULT NULL,
  `enable_failed_card_storage` bit(1) DEFAULT NULL,
  `enable_save_card_before_auth` bit(1) DEFAULT NULL,
  `enable_save_card_before_authentication` bit(1) DEFAULT b'0',
  `auto_refund_conflict_transactions` bit(1) DEFAULT NULL,
  `payment_response_hash_key` varchar(255) DEFAULT NULL,
  `enable_payment_response_hash` bit(1) DEFAULT NULL,
  `enable_external_risk_check` bit(1) DEFAULT NULL,
  `citi_key` varchar(255) DEFAULT NULL,
  `citi_merchant_code` varchar(255) DEFAULT NULL,
  `citi_test_mode` bit(1) DEFAULT NULL,
  `citi_verified` bit(1) DEFAULT NULL,
  `conflict_status_email` varchar(255) DEFAULT NULL,
  `enable_conflict_status_notification` bit(1) DEFAULT NULL,
  `web_hook_password` varchar(255) DEFAULT NULL,
  `web_hook_username` varchar(255) DEFAULT NULL,
  `web_hookapiversion` varchar(255) DEFAULT NULL,
  `fetch_cards_from_payu` bit(1) DEFAULT NULL,
  `prefix_merchant_id_for_card_key` bit(1) DEFAULT NULL,
  `txn_id_custom_prefix` varchar(10) DEFAULT NULL,
  `secondary_merchant_account_id` bigint(20) DEFAULT NULL,
  `reseller_id` int(11) DEFAULT NULL,
  `reseller_id1` varchar(64) DEFAULT NULL,
  `fingerprint_on_tokenize` bit(1) DEFAULT NULL,
  `card_encoding_key` varchar(255) DEFAULT NULL,
  `enable_unauthenticated_order_status_api` bit(1) DEFAULT NULL,
  `enable_unauthenticated_card_add` bit(1) DEFAULT NULL,
  `capture_authorizing_response` bit(1) DEFAULT NULL,
  `enable_unauthenticated_card_add2` bit(1) DEFAULT NULL,
  `process_authorizing_response` bit(1) DEFAULT NULL,
  `webhook_custom_headers` longtext,
  `timezone` varchar(255) DEFAULT NULL,
  `mandatory_2FA` tinyint(1) DEFAULT NULL,
  `order_session_timeout` tinyint(1) DEFAULT NULL,
  `include_surcharge_amount_for_refund` bit(1) DEFAULT NULL,
  `show_surcharge_breakup_screen` bit(1) DEFAULT NULL,
  `should_add_surcharge` bit(1) DEFAULT NULL,
  `enabled_instant_refund` tinyint(1) DEFAULT NULL,
  `enable_txn_filter` tinyint(1) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FK47E2B2B67727E594` (`settlement_account_id`),
  KEY `FK47E2B2B6EB94360B` (`secondary_merchant_account_id`),
  CONSTRAINT `FK47E2B2B67727E594` FOREIGN KEY (`settlement_account_id`) REFERENCES `settlement_account` (`id`),
  CONSTRAINT `FK47E2B2B6EB94360B` FOREIGN KEY (`secondary_merchant_account_id`) REFERENCES `merchant_account` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=103 DEFAULT CHARSET=utf8;
-}


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

--type MAUser a =
--  { id :: Maybe Int
--  , merchantName :: Maybe Text
--  , apiKey :: Maybe Text
--  , userId :: Maybe Int
--  , alternativeEmail :: Maybe Text
--  , contactPerson :: Maybe Text
--  , mobile :: Maybe Text
--  , telephone :: Maybe Text
--  , website :: Maybe Text
--  , aboutBusiness :: Maybe Text
--  , adminContactEmail :: Maybe Text
--  , city :: Maybe Text
--  , contactPersonEmail :: Maybe Text
--  , contactPersonPrimary :: Maybe Text
--  , contactPersonSecondary :: Maybe Text
--  , contactPersonSecondaryEmail :: Maybe Text
--  , landmark :: Maybe Text
--  , officeLine1 :: Maybe Text
--  , officeLine2 :: Maybe Text
--  , officeLine3 :: Maybe Text
--  , state :: Maybe Text
--  , zip :: Maybe Text
--  , dateCreated :: Maybe LocalTime
--  , lastModified :: Maybe LocalTime
--  , paymentResponseHashKey :: Maybe Text
--  , returnUrl :: Maybe Text
--  , autoRefundConflictTransactions :: Maybe Bool
--  , enablePaymentResponseHash :: Maybe Bool
--  , enableAutomaticRetry :: Maybe Bool
--  , enableConflictStatusNotification :: Maybe Bool
--  , webHookCustomHeaders :: Maybe Text
--  , conflictStatusEmail :: Maybe Text
--  , webHookPassword :: Maybe Text
--  , webHookUsername :: Maybe Text
--  , enableSaveCardBeforeAuth :: Maybe Bool
--  , webHookapiversion :: Maybe Text
--  , webHookurl :: Maybe Text
--  , cardEncodingKey :: Maybe Text
--  , txnIdCustomPrefix :: Maybe Text
--  , includeSurchargeAmountForRefund :: Maybe Bool
--  , shouldAddSurcharge :: Maybe Bool
--  , showSurchargeBreakupScreen :: Maybe Bool
--  , enabledInstantRefund :: Maybe Bool
--  , enableTxnFilter :: Maybe Bool
--  | a
--  }

-- type MADB = (
--     version :: Int
--   , bankAccountNumber :: Maybe Text
--   , creditBalance :: Maybe Double
--   , merchantId :: Maybe Text
--   , expressCheckoutEnabled :: Maybe Bool
--   , inlineCheckoutEnabled :: Maybe Bool
--   , inlineCheckoutHtml :: Maybe Text
--   , iciciKey :: Maybe Text
--   , iciciMerchantId :: Maybe Text
--   , iciciSecondFactorId :: Maybe Text
--   , iciciVerified :: Maybe Bool
--   , lockerId :: Maybe Text
--   , axisMerchantId :: Maybe Text
--   , axisVerified :: Maybe Bool
--   , axisSecureSecret :: Maybe Text
--   , axisAccessCode :: Maybe Text
--   , hdfcMerchantId :: Maybe Text
--   , hdfcPassword :: Maybe Text
--   , hdfcVerified :: Maybe Bool
--   , ebsAccountId :: Maybe Text
--   , ebsHash :: Maybe Text
--   , ebsVerified :: Maybe Bool
--   , enableReauthentication :: Maybe Bool
--   , enableReauthorization :: Maybe Bool
--   , enableRecapture :: Maybe Bool
--   , enableSendingLastFourDigits :: Maybe Bool
--   , domain :: Maybe Text
--   , whitelabelEnabled :: Maybe Bool
--   , payuMerchantKey :: Maybe Text
--   , payuSalt :: Maybe Text
--   , payuVerified :: Maybe Bool
--   , otpEnabled :: Maybe Bool
--   , reverseTokenEnabled :: Maybe Bool
--   , hdfcTestMode :: Maybe Bool
--   , realModeOnly :: Maybe Bool
--   , gatewayDecidedByHealthEnabled :: Maybe Bool
--   , gatewayPriority :: Maybe Text
--   , amexMerchantId :: Maybe Text
--   , amexVerified :: Maybe Bool
--   , mustUseGivenOrderIdForTxn :: Maybe Bool
--   , enable3DsecureHelpMail :: Maybe Bool
--   , amexAccessCode :: Maybe Text
--   , amexPassword :: Maybe Text
--   , amexSecureSecret :: Maybe Text
--   , amexUsername :: Maybe Text
--   , paypalClientId :: Maybe Text
--   , paypalSecret :: Maybe Text
--   , hdfcIvrMerchantId :: Maybe Text
--   , hdfcIvrPassword :: Maybe Text
--   , hdfcIvrVerified :: Maybe Bool
--   , settlementAccountId :: Maybe Int
--   , enableRefundsInDashboard :: Maybe Bool
--   , hdfcMerchantCode :: Maybe Text
--   , merchantLegalName :: Maybe Text
--   , orderFieldsInSettlementReport :: Maybe Text
--   , enableSendingCardIsin :: Maybe Bool
--   , ccavenueAccountId :: Maybe Text
--   , ccavenueSecretKey :: Maybe Text
--   , ccavenueVerified :: Maybe Bool
--   , mobileVersion :: Maybe Text
--   , enableOrderNotification :: Maybe Bool
--   , orderNotificationEmail :: Maybe Text
--   , ccavenueBackendGateway :: Maybe Text
--   , payuTestMode :: Maybe Bool
--   , ebsBackendGateway :: Maybe Text
--   , gatewayPriorityLogic :: Maybe Text
--   , useCodeForGatewayPriority :: Bool
--   , enableExternalRiskCheck :: Maybe Bool
--   , citiKey :: Maybe Text
--   , citiMerchantCode :: Maybe Text
--   , citiTestMode :: Maybe Bool
--   , citiVerified :: Maybe Bool
--   , fetchCardsFromPayu :: Maybe Bool
--   , prefixMerchantIdForCardKey :: Maybe Bool
--   , secondaryMerchantAccountId :: Maybe Int
--   , resellerId :: Maybe Text
--   , fingerprintOnTokenize :: Maybe Bool
--   , enableUnauthenticatedOrderStatusApi :: Maybe Bool -- should be present and defaulted to False, not maybe?
--   , enableUnauthenticatedCardAdd :: Maybe Bool
--   , timezone :: Maybe Text
--   , mandatory2FA :: Maybe Bool
--   )

-- from src/Types/Storage/EC/MerchantIframePreferences.purs
data MerchantIframePreferences = MerchantIframePreferences
  { id                          :: Maybe Int
  , version                     :: Int
  , customStylesheet            :: Maybe Text
  , returnUrl                   :: Maybe Text
  , paymentButtonLabel          :: Maybe Text
  , merchantLogoUrl             :: Maybe Text
  , showCancelButton            :: Maybe Bool
  , emergencyMessage            :: Maybe Text
  , redirectModeOnly            :: Maybe Bool
  , defaultCurrency             :: Maybe Text
  , termsAndConditionsLink      :: Maybe Text
  , orderSessionTimeout         :: Maybe Double
  , walletTopupReturnUrl        :: Maybe Text
  , saveCardEnabled             :: Maybe Bool
  , merchantId                  :: Text
  , headingText                 :: Maybe Text
  , orderCreationUrl            :: Maybe Text
  , dynamicSwitchingEnabled     :: Maybe Bool
  , isinRoutingEnabled          :: Maybe Bool
  , issuerRoutingEnabled        :: Maybe Bool
  , mirrorGatewayResponse       :: Maybe Bool
  , txnFailureGatewayPenality   :: Maybe Bool
  , passThroughMode             :: Maybe Bool
  , deleteCardOnExpiry          :: Maybe Bool
  , canAcceptAmex               :: Maybe Bool
  , canAcceptDiners             :: Maybe Bool
  , canAcceptDiscover           :: Maybe Bool
  , canAcceptJcb                :: Maybe Bool
  , canAcceptMaster             :: Maybe Bool
  , canAcceptRupay              :: Maybe Bool
  , canAcceptVisa               :: Maybe Bool
  , canAcceptMaestro            :: Maybe Bool
  , customMobileStylesheet      :: Maybe Text
  , cardBrandRoutingEnabled     :: Maybe Bool
  , alwaysAcceptCards           :: Maybe Bool
  , additionalParamsInResponse  :: Maybe Text
  , additionalParamsInResponse1 :: Maybe Text
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)
  
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
