{-# LANGUAGE DeriveAnyClass #-}
module Euler.Constants where

import EulerHS.Prelude
import Data.Scientific as S

defaultCurrency :: IsString s => s
defaultCurrency = "INR"

ecDB :: String
ecDB = "ECRDB"

eulerDB :: IsString s => s
eulerDB = "Euler"

defaultVersion :: Int
defaultVersion = 1

ecRedis :: Text
ecRedis = "ECRRedis"

kvRedis :: Text
kvRedis = "KVRedis"

ecRedisDBQueue :: IsString s => s
ecRedisDBQueue = "db-sync-queue"

ecRedisDBStream :: IsString s => s
ecRedisDBStream = "db-sync-stream"

ecRedisDBStreamCounter :: IsString s => s
ecRedisDBStreamCounter = "db-sync-stream-counter"

ecRedisErrorStream :: IsString s => s
ecRedisErrorStream = "db-sync-error-stream"

ecRedisDBConsumerGroup :: IsString s => s
ecRedisDBConsumerGroup = "db-sync-consumergroup"

txnStatusReconFeature :: IsString s => s
txnStatusReconFeature = "TXN_STATUS_RECON"

updateLastSyncedFeature :: IsString s => s
updateLastSyncedFeature = "UPDATE_LAST_SYNC"

eulerOrderStatusCachingKey :: IsString s => s
eulerOrderStatusCachingKey = "EULER_ORDER_STATUS_CACHING"

directWalletDebit :: IsString s => s
directWalletDebit = "DIRECT_WALLET_DEBIT"

orderStatusCacheTTL :: Integer
orderStatusCacheTTL = 45 * 60

-- EHS: hardcoded currency
invalidMandateMaxAmount :: IsString s => s
invalidMandateMaxAmount = "maxAmount is mandatory and should be greater than Rs 0.00 and less than Rs "

invalidMandateTxnAmount :: IsString s => s
invalidMandateTxnAmount = "amount not valid for mandate transaction"

invalidMandateTxnCurrency :: IsString s => s
invalidMandateTxnCurrency = "currency not valid for mandate transaction"

decisionCodeToMessageMap :: String -> String
decisionCodeToMessageMap "Y:" = "Executed the transaction"
decisionCodeToMessageMap "N:" = "Dropped the transaction"
decisionCodeToMessageMap "C:" = "Checksum is incorrect, so rectify and send"
decisionCodeToMessageMap "E:" = "Error thrown"
decisionCodeToMessageMap "ND" = "Delay in processing transaction"
decisionCodeToMessageMap "R:" = "Transaction not found or already reversed"
decisionCodeToMessageMap "NP" = "Card not permitted for that merchant"
decisionCodeToMessageMap "NR" = "Record not available in the table"
decisionCodeToMessageMap "I1" = "Card No not available"
decisionCodeToMessageMap "I2" = "Exp date not available"
decisionCodeToMessageMap "I4" = "CVV not available"
decisionCodeToMessageMap "U:" = "Unique Constraint Violation, same Duplicate Trace Number used"
decisionCodeToMessageMap "Y1" = "The transaction has been already cancelled"
decisionCodeToMessageMap  _   = ""

successPath :: IsString s => s
successPath = "pay/success"

finishPath :: IsString s => s
finishPath = "pay/finish"

voidPath :: IsString s => s
voidPath = "pay/voided"

noSuitablePM :: IsString s => s
noSuitablePM = "No suitable payment method found."

paymentStartPath :: IsString s => s
paymentStartPath = "pay/start"

viesPaymentStartPath :: IsString s => s
viesPaymentStartPath = "vies/pay/start"

eulerPaymentStartPath :: IsString s => s
eulerPaymentStartPath = "v2/pay/start"

eulerPaymentStartPathV3 :: IsString s => s
eulerPaymentStartPathV3 = "v3/pay/start"

handleResponsePath :: IsString s => s
handleResponsePath = "pay/response"

redisEnabledGateway :: IsString s => s
redisEnabledGateway = "redis_enabled_gateway"

decideGatewayVersion :: IsString s => s
decideGatewayVersion = "decide_gateway_version"

eulerHandleResponsePath :: IsString s => s
eulerHandleResponsePath = "v2/pay/response"

eulerHandleResponsePathV3 :: IsString s => s
eulerHandleResponsePathV3 = "v3/pay/response"

eulerHandleRegResponsePath :: IsString s => s
eulerHandleRegResponsePath = "v2/reg/response"

viesFailResponsePath :: IsString s => s
viesFailResponsePath = "vies/pay/response" --TODO: fail_response

viesResponsePath :: IsString s => s
viesResponsePath = "vies/pay/response"

vcoResponsePath :: IsString s => s
vcoResponsePath = "v1/pay/vco_response"

eulerHandleRegResponsePathV3 :: IsString s => s
eulerHandleRegResponsePathV3 = "v3/reg/response"

authenticatePath :: IsString s => s
authenticatePath = "txns/:id/authenticate"

cardTokenPrefix :: IsString s => s
cardTokenPrefix = "ctkn_"

invalid_card_number :: IsString s => s
invalid_card_number = "invalid_card_number"

invalid_card_details :: IsString s => s
invalid_card_details = "invalid card details"

card_info_required :: IsString s => s
card_info_required = "card.info.required"

card_number_is_invalid :: IsString s => s
card_number_is_invalid = "Card number is invalid."

missing_card_number :: IsString s => s
missing_card_number = "missing_card_number"

card_number_is_required :: IsString s => s
card_number_is_required = "Card number is required."

required :: IsString s => s
required = "required"

emi_bank_is_missing_ :: IsString s => s
emi_bank_is_missing_ = "EMI Bank is missing."

emi_tenure_is_missing_ :: IsString s => s
emi_tenure_is_missing_ = "EMI Tenure is missing."

missing :: IsString s => s
missing = "missing"

customer_vpa_is_required_for_collect_request_ :: IsString s => s
customer_vpa_is_required_for_collect_request_ = "Customer VPA is required for Collect Request."

invalid :: IsString s => s
invalid = "invalid"

txn_type_is_missing_invalid :: IsString s => s
txn_type_is_missing_invalid = "Txn type is missing/invalid"

invalid_card_cvv :: IsString s => s
invalid_card_cvv = "invalid_card_cvv"

invalid_card_cvv_message :: IsString s => s
invalid_card_cvv_message = "invalid card cvv."

missing_card_expiry_year :: IsString s => s
missing_card_expiry_year = "Missing card expiry year"

missing_card_exp_year :: IsString s => s
missing_card_exp_year = "missing_card_exp_year"

invalid_expiry_year :: IsString s => s
invalid_expiry_year = "invalid_expiry_year"

invalid_expiry_year_ :: IsString s => s
invalid_expiry_year_ = "Invalid expiry year."

missing_card_expiry_month :: IsString s => s
missing_card_expiry_month = "Missing card expiry month"

missing_card_exp_month :: IsString s => s
missing_card_exp_month = "missing_card_exp_month"

invalid_expiry_month :: IsString s => s
invalid_expiry_month = "invalid_expiry_month"

invalid_expiry_month_ :: IsString s => s
invalid_expiry_month_ = "Invalid expiry month."

missing_payment_method_message :: IsString s => s
missing_payment_method_message = "Payment method cannot be null"

missing_payment_method :: IsString s => s
missing_payment_method = "missing_payment_method"

missing_payment_method_type_message :: IsString s => s
missing_payment_method_type_message = "Payment method type cannot be null"

missing_payment_method_type :: IsString s => s
missing_payment_method_type = "missing_payment_method_type"

blank :: IsString s => s
blank = "blank"

orderIdCannotBeNull :: IsString s => s
orderIdCannotBeNull = "[order id] cannot be null"


merchantIdCannotBeNull :: IsString s => s
merchantIdCannotBeNull = "[merchantId] cannot be null"

nullable :: IsString s => s
nullable = "nullable"

orderOrderIdCannotBeNull :: IsString s => s
orderOrderIdCannotBeNull = "order.order_id cannot be null"

missing_card_security_code :: IsString s => s
missing_card_security_code = "missing_card_security_code"

-- Txns Flags

txnEnabledMids :: IsString s => s
txnEnabledMids = "txns_enabled_merchants"

nonCardSupportedMids :: IsString s => s
nonCardSupportedMids = "non_card_supported_merchants"

emiEnabledGws :: IsString s => s
emiEnabledGws = "emi_enabled_gw"

supportedGws :: IsString s => s
supportedGws = "euler_supported_gw"

eulerSid :: IsString s => s
eulerSid = "X-Euler-SessionId"

refundEnabledMerchants :: IsString s => s
refundEnabledMerchants = "euler_refund_enabled_merchants"

refundSupportedGws :: IsString s => s
refundSupportedGws = "euler_refund_supported_gw"

enabledPMT :: IsString s => s
enabledPMT = "enabled_payment_method_types"

enabledPM :: IsString s => s
enabledPM = "enabled_payment_method"

enabledUPIMerchants :: IsString s => s
enabledUPIMerchants = "enabled_upi_merchants"

-- TODO: FIXME: invalid design (another list of constants)
upiCollectSupportedGateways :: [String]
upiCollectSupportedGateways = ["RAZORPAY","AXIS_UPI", "HDFC_UPI", "INDUS_UPI", "KOTAK_UPI", "SBI_UPI", "ICICI_UPI", "HSBC_UPI", "VIJAYA_UPI", "PAYTM_UPI", "YESBANK_UPI"]

emiPlansPrefix :: IsString s => s
emiPlansPrefix = "emi_plans_"

-- TODO
-- emiPlansExpiry :: Milliseconds
-- emiPlansExpiry = convertDuration $ Hours 24.0

siStoredCardPrefix :: IsString s => s
siStoredCardPrefix = "si_stored_cards_"

redis_token_expiry_default :: Int
redis_token_expiry_default = 900

token_max_usage_default :: Int
token_max_usage_default = 30

-- TODO
-- transactionSourceHeader :: Header
-- transactionSourceHeader = Header "X-Transaction-Source" "EULERFORWARDED"

eligibilitySupportedGateways :: IsString s => s
eligibilitySupportedGateways = "eligibility_supported_gateways"

pmSupportsSDKParams :: IsString s => s
pmSupportsSDKParams = "euler_pm_supports_sdk_params"

visaAuthenticatableAmount :: IsString s => s
visaAuthenticatableAmount = "visa_authenticatable_amount"

visaDefaultAuthenticatableAmount :: S.Scientific
visaDefaultAuthenticatableAmount = 2000.0

internalNetworkAuthorizeUrl :: IsString s => s
internalNetworkAuthorizeUrl = "/internal/txns/:txnUuid/authorize"

eulerEnabledUpiPsps :: IsString s => s
eulerEnabledUpiPsps = "euler_enabled_upi_psps"

transactionIdMismatch :: IsString s => s
transactionIdMismatch = "transaction id mismatch"

amount_mismatch :: IsString s => s
amount_mismatch = "order_amount_mismatch"

amount_mismatch_message :: IsString s => s
amount_mismatch_message = "Order amount changed during authentication"

sfr_status_failed :: IsString s => s
sfr_status_failed = "Second Factor Response status failed"

paresDecodingFailed :: IsString s => s
paresDecodingFailed = "Unable to decode Pares"

enrollmentCheckError :: IsString s => s
enrollmentCheckError = "Error occured while checking for enrollment"

internalServerError :: IsString s => s
internalServerError = "INTERNAL_SERVER_ERROR"

invalidRequest :: IsString s => s
invalidRequest = "INVALID_REQUEST"

credentialsNotFound :: String -> String
credentialsNotFound gateway = gateway <> " credentials not found"

-- TODO
-- getProtocolAndhost :: String
-- getProtocolAndhost = (getECRConfig ^. _protocol) <> "://" <> (getECRConfig ^. _host)

-- TODO
-- redisTtl :: Seconds
-- redisTtl = convertDuration $ Hours 24.0 --"86400" --24hrs

unexpectedResponseformat :: IsString s => s
unexpectedResponseformat = "unexpected Response type"

alreadyProcessed :: IsString s => s
alreadyProcessed = "already_processed"

autoCreateCustomerMerchants :: IsString s => s
autoCreateCustomerMerchants = "euler_auto_create_customer_merchants"

cannotProcessResponse :: IsString s => s
cannotProcessResponse = "cannot_process_response"

maxLimitExceeded :: IsString s => s
maxLimitExceeded = "max_limit_exceeded"

otpMissing :: IsString s => s
otpMissing = "otp_missing"

answerMissing :: IsString s => s
answerMissing = "answer_missing"

cannotAcceptResponse :: IsString s => s
cannotAcceptResponse =  "Cannot accept response."

maxAuthenticationAttemptExceeded :: IsString s => s
maxAuthenticationAttemptExceeded = "Max authentication attempt exceeded."

formatNotSupported :: IsString s => s
formatNotSupported = "format_not_supported"

maxOtpSendLimitExceeded :: IsString s => s
maxOtpSendLimitExceeded = "Max OTP send limit exceeded"

otp_missing :: IsString s => s
otp_missing = "otp_missing"

challenge_id_missing :: IsString s => s
challenge_id_missing = "challenge_id_missing"

resendOtpFailed :: IsString s => s
resendOtpFailed = "Resend OTP failed"

maxSubmitOtpAttempts :: Int
maxSubmitOtpAttempts = 3

maxResendOtpAttempts :: Int
maxResendOtpAttempts = 3

-- TODO
-- getResendOtpEnabledGateways :: [String]
-- getResendOtpEnabledGateways = ["RAZORPAY"]

gatewayOutage :: IsString s => s
gatewayOutage = "gateway_outage_"

-- Redis controlled features
redisEnabledMerchants :: IsString s => s
redisEnabledMerchants = "redis_enabled_merchants"

kvSupportedGws :: IsString s => s
kvSupportedGws = "kv_supported_gateways"

memCacheEnabled :: IsString s => s
memCacheEnabled = "e_mem_cache_enabled"

gatewayRolloutPrefix :: IsString s => s
gatewayRolloutPrefix = "euler_gateway_cutover_"

eulerTxnsMerchantCutoverPrefix :: IsString s => s
eulerTxnsMerchantCutoverPrefix = "euler_txns_merchant_cutover"

eulerDirectOtpSupportedMids :: IsString s => s
eulerDirectOtpSupportedMids = "euler_direct_otp_supported_merchants"

anomalyLogVersion :: IsString s => s
anomalyLogVersion = "0.0.1"

-- KV Store Constants --
-- Template for data in Redis: "kv_" <> shard <> "_oid_" <> oid <> "_mid_" <> mid
-- Template for stream in Redis:
numberOfStreamsForKV :: Int
numberOfStreamsForKV = 128

eulerRefundsGatewayRolloutPrefix :: IsString s => s
eulerRefundsGatewayRolloutPrefix = "euler_refunds_gateway_cutover_"

eulerHandlePayMerchantCutoverPrefix :: IsString s => s
eulerHandlePayMerchantCutoverPrefix = "euler_handle_pay_merchant_cutover"

-- TODO
-- nonRefundSupportedGateways :: [String]
-- nonRefundSupportedGateways = ["ICICINB", "AXISNB"]

-- This gateway list is being maintained for webhooks.
-- In these gateways we can get either txnId or TxnUuid in the webhook response
-- TODO
-- txnUuidWebhookQueryGatewayList :: [String]
-- txnUuidWebhookQueryGatewayList = ["AXIS_UPI", "HDFC_UPI", "INDUS_UPI", "KOTAK_UPI", "SBI_UPI", "ICICI_UPI", "HSBC_UPI", "VIJAYA_UPI", "PAYTM_UPI", "YESBANK_UPI", "UPI", "GOOGLEPAY"]

upiDirectRolloutPrefix :: IsString s => s
upiDirectRolloutPrefix = "use_direct_upi_"

-- TODO
-- conCurrentRequestUniqueRequestIdExpiry :: Milliseconds
-- conCurrentRequestUniqueRequestIdExpiry = convertDuration $ Seconds $ 2.0
