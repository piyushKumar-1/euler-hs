{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Euler.API.MerchantPaymentGatewayResponse where

import           EulerHS.Prelude

import           Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL

import           Euler.API.Types
import           Euler.Common.Types.PaymentGatewayResponseXml
import           Euler.Product.Domain as D
import           Euler.Storage.Types.PaymentGatewayResponse
import           Euler.Storage.Types.TxnDetail



data MerchantPaymentGatewayResponse' = MerchantPaymentGatewayResponse'
  {  resp_code            :: Maybe Text
  ,  rrn                  :: Maybe Text
  ,  created              :: Maybe Text
  ,  epg_txn_id           :: Maybe Text
  ,  resp_message         :: Maybe Text
  ,  auth_id_code         :: Maybe Text
  ,  txn_id               :: Maybe Text
  ,  offer                :: Maybe Text
  ,  offer_type           :: Maybe Text
  ,  offer_availed        :: Maybe Text -- Foreign
  ,  discount_amount      :: Maybe Text -- Foreign
  ,  offer_failure_reason :: Maybe Text
  ,  gateway_response     :: Maybe Text -- Foreign
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- from src/Types/Communication/OLTP/OrderStatus.purs
-- former defaultPaymentGatewayResponse
defaultMerchantPaymentGatewayResponse' :: MerchantPaymentGatewayResponse'
defaultMerchantPaymentGatewayResponse' = MerchantPaymentGatewayResponse'
  { resp_code = Nothing
  , rrn = Nothing
  , created = Nothing
  , epg_txn_id = Nothing
  , resp_message = Nothing
  , auth_id_code = Nothing
  , txn_id = Nothing
  , offer = Nothing
  , offer_type = Nothing
  , offer_availed = Nothing
  , discount_amount = Nothing
  , offer_failure_reason = Nothing
  , gateway_response = Nothing
  }

-- from src/Types/Communication/OLTP/OrderStatus.purs
data MerchantPaymentGatewayResponse = MerchantPaymentGatewayResponse
  {   resp_code            :: Maybe Text -- Foreign
   ,  rrn                  :: Maybe Text -- Foreign
   ,  created              :: Maybe Text -- Foreign
   ,  epg_txn_id           :: Maybe Text -- Foreign
   ,  resp_message         :: Maybe Text -- Foreign
   ,  auth_id_code         :: Maybe Text -- Foreign
   ,  txn_id               :: Maybe Text -- Foreign
   ,  offer                :: Maybe Text
   ,  offer_type           :: Maybe Text
   ,  offer_availed        :: Maybe Text -- Foreign
   ,  discount_amount      :: Maybe Text -- Foreign
   ,  offer_failure_reason :: Maybe Text
   ,  gateway_response     :: Maybe Text -- Foreign
   }
   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- EPS
-- | Type for PGR_Key_Mapping
-- | To fetch value from xml string we have following constructors
-- |    Fn      - To give custom function implementation for some key
-- |    XmlKey  key defaultValue
-- |    Xmlkeys key1 key2 defaultValue
-- |    Value   (just value)

data PGRField = PGRField
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
-- data PGRField a b =
  -- Fn (a → b) a
  -- | FromKeysOrObj String                   String                   (NullOrUndefined String) String
  -- | FromObjOrkeys (NullOrUndefined String) String                   String                   String
  -- | XmlKeys                                String                   String                   String
  -- | FromKeyOrObj  String                   (NullOrUndefined String)                          String
  -- | FromObjOrkey  (NullOrUndefined String) String                                            String
  -- | XmlKey                                 String                                            String
  -- | Value b

data MerchantPaymentGatewayResponseTemp = MerchantPaymentGatewayResponseTemp
  { txnId       :: PGRField
  , rrn         :: PGRField
  , epgTxnId    :: PGRField
  , authId      :: PGRField
  , respCode    :: PGRField
  , respMessage :: PGRField
  }
   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

executePGR
  :: MerchantPaymentGatewayResponse'
  -> (Map.Map TL.Text EValue, Map.Map TL.Text EValue)
  -> MerchantPaymentGatewayResponseTemp
  -> MerchantPaymentGatewayResponse'
executePGR merchPGR xmls merchTemp =
  merchPGR
    { txn_id       = undefined -- runPGR xmls (getField @"txnId" merchTemp)
    , rrn          = undefined -- runPGR xmls (getField @"rrn" merchTemp)
    , epg_txn_id   = undefined -- runPGR xmls (getField @"epgTxnId" merchTemp)
    , auth_id_code = undefined -- runPGR xmls (getField @"authId" merchTemp)
    , resp_code    = undefined -- runPGR xmls (getField @"respCode" merchTemp)
    , resp_message = undefined -- runPGR xmls (getField @"respMessage" merchTemp)
    }

runPGR
  :: (Map.Map TL.Text EValue, Map.Map TL.Text EValue)
  -> PGRField
  -> Maybe Text
runPGR _ _= undefined
-- runPGR xmls (Value b) = b
-- runPGR xmls (Fn f a) = f a
-- runPGR xmls (FromObjOrkeys ndf key_1 key_2 df) = just $ unNull ndf $ lookupXMLKeys xmls key_1 key_2 df
-- runPGR xmls (FromKeysOrObj key_1 key_2 ndf df) = just $ lookupXMLKeys xmls key_1 key_2 $ unNull ndf df
-- runPGR xmls (FromObjOrkey ndf key df) = just $ unNull ndf $ lookupXML xmls key df
-- runPGR xmls (FromKeyOrObj key ndf df) = just $ lookupXML xmls key $ unNull ndf df
-- runPGR xmls (XmlKeys key_1 key_2 df)  = just $ lookupXMLKeys xmls key_1 key_2 df
-- runPGR xmls (XmlKey key df)           = just $ lookupXML xmls key df


lookupXML
  :: (Map.Map TL.Text EValue, Map.Map TL.Text EValue)
  -> Text
  -> Text
  -> Text
lookupXML (xml_1, xml_2) key defaultValue = undefined
-- lookupXML (xml_1, xml_2) key defaultValue =
--   (<|>)
--   (matchKeyfn_1 xml_1 key # previewValue)
--   (matchKeyfn_2 xml_2 key >>= (getRecordValues >>> preview _1))

  -- # maybe defaultValue id

lookupXMLKeys
  :: (Map.Map TL.Text EValue, Map.Map TL.Text EValue)
  -> Text
  -> Text
  -> Text
  -> Text
lookupXMLKeys xmls key_1 key_2 defaultValue = undefined
  -- on chooseOne (\key -> lookupXML xmls key defaultValue) key_1 key_2
  -- where
        -- chooseOne a b = if a == defaultValue then b else a



-- EPS: ** PGR Key Mapping Configs **

zaakpay :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
zaakpay txn pgr = undefined
  -- { txnId       = Value (txn ^. _txnId # just)
  -- , rrn         = Value justEmpty
  -- , epgTxnId    = XmlKeys "pgTransId" "txnId" "null"
  -- , authId      = Value justNa
  -- , respCode    = FromKeyOrObj  "responseCode"                          (pgr ^.  _respCode)     "null" -- extra check
  -- , respMessage = FromKeysOrObj "responseMessage" "responseDescription" (pgr ^.  _respMessage)  "null" -- EPS: extra check
  -- }

atom :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
atom txn pgr = undefined
--   -- { txnId       :: Value (txn ^. _txnId # just)
  -- -- , rrn         :: XmlKeys "bank_txn"  "BID"       "null"
  -- -- , epgTxnId    :: XmlKeys "mmp_txn"   "atomtxnId" "null"
  -- -- , authId      :: XmlKey  "auth_code" "null"
  -- -- , respCode    :: Value (pgr ^.  _respCode)
  -- -- , respMessage :: Value (pgr ^.  _respMessage)
  -- }


paylater :: TxnDetail -> MerchantPaymentGatewayResponseTemp
paylater txn = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justEmpty
  -- , epgTxnId    : Value justNa
  -- , authId      : Value justNa
  -- , respCode    : Value justNa
  -- , respMessage : Value justNa
  -- }

mobikwik :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
mobikwik txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justEmpty
  -- , epgTxnId    : XmlKeys "refid" "refId" "null"
  -- , authId      : Value justNa
  -- , respCode    : FromKeyOrObj "statuscode"    (pgr ^.  _respCode)    "null" -- EPS extra check
  -- , respMessage : FromKeyOrObj "statusmessage" (pgr ^.  _respMessage) "null" -- EPS extra check
  -- }

ebs_v3 :: TxnDetail -> MerchantPaymentGatewayResponseTemp
ebs_v3 pgr = undefined
  -- { txnId       : Value (pgr ^.  _txnId)
  -- , rrn         : XmlKeys "PaymentID"     "paymentId"     "null"
  -- , epgTxnId    : XmlKeys "TransactionID" "transactionId" "null"
  -- , authId      : Value justEmpty
  -- , respCode    : Value (pgr ^.  _respCode)
  -- , respMessage : Value (pgr ^.  _respMessage)
  -- }

ebs :: TxnDetail -> MerchantPaymentGatewayResponseTemp
ebs pgr = undefined
  -- { txnId       : Value (pgr ^.  _txnId)
  -- , rrn         : XmlKey "vpc_TransactionId" "null"
  -- , epgTxnId    : XmlKey "vpc_PaymentId"     "null"
  -- , authId      : Value justEmpty
  -- , respCode    : Value (pgr ^.  _respCode)
  -- , respMessage : Value (pgr ^.  _respMessage)
  -- }

paytm_v2 :: TxnDetail -> MerchantPaymentGatewayResponseTemp
paytm_v2 pgr = undefined
  -- { txnId       : Value (pgr ^. _txnId)
  -- , rrn         : XmlKey  "BANKTXNID" ""
  -- , epgTxnId    : XmlKeys "TXNID" "TxnId" ""
  -- , authId      : Value justEmpty
  -- , respCode    : Value (pgr ^.  _respCode)
  -- , respMessage : Value (pgr ^.  _respMessage)
  -- }

tpsl :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
tpsl txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : XmlKey "clnt_txn_ref" ""
  -- , epgTxnId    : XmlKey "tpsl_txn_id"  ""
  -- , authId      : Value  justEmpty
  -- , respCode    : FromKeyOrObj "txn_status" (pgr ^. _respCode)    ""
  -- , respMessage : FromKeyOrObj "txn_msg"    (pgr ^. _respMessage) ""
  -- }

amex :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
amex txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : XmlKey "vpc_ReceiptNo"        "null"
  -- , epgTxnId    : XmlKey "vpc_TransactionNo"    "null"
  -- , authId      : XmlKey "vpc_AuthorizeId"      "null"
  -- , respCode    : FromKeyOrObj "vpc_TxnResponseCode" (pgr ^.  _respCode)    "null" -- EPS: extra check
  -- , respMessage : FromKeyOrObj "vpc_Message"         (pgr ^.  _respMessage) "null" -- EPS: extra check
  -- }

dummy :: MerchantPaymentGatewayResponseTemp
dummy = undefined
  -- { txnId       : XmlKey "txnId"       "null"
  -- , rrn         : XmlKeys "RRN"  "rrn" "null"
  -- , epgTxnId    : XmlKey "epgTxnId"    "null"
  -- , authId      : XmlKey "authIdCode"  "null"
  -- , respCode    : XmlKey "respCode"    "null"
  -- , respMessage : XmlKey "respMessage" "null"
  -- }

hdfc_ebs_vas :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
hdfc_ebs_vas txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justEmpty
  -- , epgTxnId    : Value justNa
  -- , authId      : Value justNa
  -- -- , respCode    : FromKeyOrObj  "ResponseCode"            (pgr ^.  _respCode)    "null" -- EPS:  extra check
  -- -- , respMessage : FromKeysOrObj "ResponseMessage" "Error" (pgr ^.  _respMessage) ""     -- EPS:  extra check
  -- }

stripe
  :: TxnDetail
  -> PaymentGatewayResponse
  -> (Map.Map TL.Text EValue, Map.Map TL.Text EValue)
  -> MerchantPaymentGatewayResponseTemp
stripe txn pgr xmls = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : XmlKey "id" "null"
  -- , epgTxnId    : XmlKey "id" "null"
  -- , authId      : Value justNa
  -- , respCode    : Value respCode
  -- , respMessage : FromKeyOrObj "failureMessage" (pgr ^.  _respMessage) "null" -- EPS: extra check
  -- }
  -- where
  --       status      = lookupXML xmls "status" "null"
  --       failureCode = lookupXML xmls "failureCode" "null"
  --       respCode    = just $ if status == "failed" then failureCode else status


ipg :: TxnDetail -> (Map.Map TL.Text EValue, Map.Map TL.Text EValue) -> MerchantPaymentGatewayResponseTemp
ipg txn xmls = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKey "ipgTransactionId" "null"
  -- , authId      : Value justNa
  -- -- , respCode    : Value $ ipgRespCodeAndMessage xmls "fail_rc"     "approval_code" (lookupXML xmls "ApprovalCode"     "null")
  -- -- , respMessage : Value $ ipgRespCodeAndMessage xmls "fail_reason" "status"        (lookupXML xmls "TransactionState" "null")
  -- }
  -- where
        -- ipgRespCodeAndMessage xmls keys_1 key_2 df = lookupXMLKeys xmls keys_1 key_2 df # just

axisnb :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
axisnb txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKey "BID" ""
  -- , authId      : Value justNa
  -- , respCode    : Fn (view _axisRespCode) pgr
  -- , respMessage : Fn (view _axisRespMessage) pgr
  -- }

zestmoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
zestmoney txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : Value justNa
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^.  _respCode)
  -- , respMessage : Value (pgr ^.  _respMessage)
  -- }

ccavenue_v2
  :: TxnDetail
  -> PaymentGatewayResponse
  -> (Map.Map TL.Text EValue, Map.Map TL.Text EValue)
  -> MerchantPaymentGatewayResponseTemp
ccavenue_v2 txn pgr xmls = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : XmlKeys "bank_ref_no" "order_bank_ref_no" "null"
  -- , epgTxnId    : XmlKeys "tracking_id" "reference_no"      "null"
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^.  _respCode)
  -- , respMessage : FromObjOrkey (pgr ^. _respMessage) "order_status" "null" -- extra check
  -- }

cybersource :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
cybersource txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKeys "requestID" "RequestID" "null"
  -- , authId      : XmlKeys "ccAuthReply_authorizationCode" "AuthorizationCode" "NA"
  -- , respCode    : Value (pgr ^.  _respCode)
  -- , respMessage : Value (pgr ^.  _respMessage)
  -- }

paypal :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
paypal txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justEmpty
  -- , epgTxnId    : XmlKey "paymentId"  ""
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^.  _respCode)
  -- , respMessage : Value (pgr ^.  _respMessage)
  -- }

olapostpaid :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
olapostpaid txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justEmpty
  -- , epgTxnId    : XmlKeys "transactionId" "globalMerchantId" ""
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^.  _respCode)
  -- , respMessage : Value (pgr ^.  _respMessage)
  -- }

blazepay
  :: TxnDetail
  -> PaymentGatewayResponse
  -> (Map.Map TL.Text EValue, Map.Map TL.Text EValue)
  -> MerchantPaymentGatewayResponseTemp
blazepay txn pgr xmls = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : XmlKeys "issuerRefNo" "RRN"     "null"
  -- , epgTxnId    : XmlKeys "pgTxnNo"     "pgTxnId" "null"
  -- , authId      : XmlKey "authIdCode"             "null"
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : FromObjOrkey (pgr ^. _respMessage) "respMsg" "null" -- EPS: extra check
  -- }

simpl :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
simpl txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justEmpty
  -- , epgTxnId    : XmlKey "id"  ""
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

gocashfree :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
gocashfree txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justEmpty
  -- , epgTxnId    : XmlKey "referenceId"  ""
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }


razorpay :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
razorpay txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justEmpty
  -- , epgTxnId    : XmlKeys "razorpay_payment_id" "id" "null"
  -- , authId      : Value justNa
  -- , respCode    : Value (unNull (pgr ^. _respCode) "null" # just)
  -- , respMessage : Value (unNull (pgr ^. _respMessage) "null" # just)
  -- }

olmoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
olmoney txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId)
  -- , rrn         : Value justEmpty
  -- , epgTxnId    : XmlKey "transactionId"  "NA"
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

mpesa :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
mpesa txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKeys "mcompgtransid" "mcomPgTransID" "null"
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

sbibuddy :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
sbibuddy txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKey "transactionId"  "NA"
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

citrus :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
citrus txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : XmlKey "issuerRefNo"    "null"
  -- , epgTxnId    : XmlKey "pgTxnNo"        "null"
  -- , authId      : XmlKey "authIdCode"     "null"
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

axis :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
axis pgr = undefined
  -- { txnId       : XmlKey "vpc_MerchTxnRef"      "null"
  -- , rrn         : XmlKey "vpc_ReceiptNo"        "null"
  -- , epgTxnId    : XmlKey "vpc_TransactionNo"    "null"
  -- , authId      : XmlKey "vpc_AuthorizeId"      "null"
  -- -- , respCode    : FromKeyOrObj "vpc_TxnResponseCode" (pgr ^. _respCode)    "null" -- EPS: extra check
  -- -- , respMessage : FromKeyOrObj "vpc_Message"         (pgr ^. _respMessage) "null" -- EPS: extra check
  -- }

migs :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
migs pgr = undefined
  -- { txnId       : XmlKey "vpc_MerchTxnRef"      "null"
  -- , rrn         : XmlKey "vpc_ReceiptNo"        "null"
  -- , epgTxnId    : XmlKey "vpc_TransactionNo"    "null"
  -- , authId      : XmlKey "vpc_AuthorizeId"      "null"
  -- , respCode    : FromKeyOrObj "vpc_TxnResponseCode" (pgr ^. _respCode)    "null" -- EPS: extra check
  -- , respMessage : FromKeyOrObj "vpc_Message"         (pgr ^. _respMessage) "null" -- EPS: extra check
  -- }

hdfc :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
hdfc pgr = undefined
  -- { txnId       : XmlKey "trackid"    "null"
  -- , rrn         : XmlKey "ref"        "null"
  -- , epgTxnId    : XmlKey "paymentid"  "null"
  -- , authId      : XmlKey "auth"       "null"
  -- , respCode    : FromKeyOrObj "result" (pgr ^. _respCode)    "null" -- EPS: extra check
  -- , respMessage : FromKeyOrObj "result" (pgr ^. _respMessage) "null" -- EPS: extra check
  -- }

icici :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
icici pgr = undefined
  -- { txnId       : XmlKey "txnId"        "null"
  -- , rrn         : XmlKey "RRN"          "null"
  -- , epgTxnId    : XmlKey "epgTxnId"     "null"
  -- , authId      : XmlKey "authIdCode"   "null"
  -- , respCode    : FromKeyOrObj "respCode"    (pgr ^. _respCode)    "null" -- EPS: extra check
  -- , respMessage : FromKeyOrObj "respMessage" (pgr ^. _respMessage) "null" -- EPS: extra check
  -- }

icicinb :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
icicinb txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKey "BID" "null"
  -- , authId      : Value justNa
  -- , respCode    : FromKeyOrObj "PAID" (pgr ^. _respCode) "null"
  -- , respMessage : Fn (unNull (pgr ^. _respMessage) >>> just) ""
  -- }

olamoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
olamoney txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justEmpty
  -- , epgTxnId    : XmlKey "transactionId" "NA"
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

paytm :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
paytm pgr = undefined
  -- { txnId       : Value (pgr ^. _txnId)
  -- , rrn         : XmlKeys "TXNID" "TxnId" "null"
  -- , epgTxnId    : XmlKeys "TXNID" "TxnId" "null"
  -- , authId      : Value justEmpty
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

payu :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
payu pgr = undefined
  -- { txnId       : Value (pgr ^. _txnId)
  -- , rrn         : XmlKey "bank_ref_num" "null"
  -- , epgTxnId    : XmlKey "mihpayid"     "null"
  -- , authId      : XmlKey "field2"       "NA"
  -- , respCode    : Fn (unNull (pgr ^. _respCode)    >>> just) "null"
  -- , respMessage : Fn (unNull (pgr ^. _respMessage) >>> just) "null"
  -- }

freecharge :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
freecharge txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justEmpty
  -- , epgTxnId    : XmlKey "txnId" "null"
  -- , authId      : Value justNa
  -- , respCode    : FromKeysOrObj "status"       "errorCode" (pgr ^. _respCode)    "null" -- EPS: extra check
  -- , respMessage : FromKeysOrObj "errorMessage" "status"    (pgr ^. _respMessage) "null" -- EPS: extra check
  -- }

billdesk :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
billdesk txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : XmlKey "TxnReferenceNo"  "null"
  -- , epgTxnId    : XmlKey "TxnReferenceNo"  "null"
  -- , authId      : XmlKey "BankReferenceNo" "NA"
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

kotak :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
kotak txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : XmlKey "RetRefNo" "NA"
  -- , epgTxnId    : XmlKey "RetRefNo" "NA"
  -- , authId      : XmlKey "AuthCode" "NA"
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

jiomoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
jiomoney txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKey "jm_tran_ref_no" "NA"
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

amazonpay :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
amazonpay txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKeys "amazonOrderId" "transactionId" "NA"
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

airtelmoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
airtelmoney txn pgr = undefined
  -- { txnId       : Fn (view _txnId >>> onlyAlphaNumeric >>> just) txn
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKeys "TRAN_ID" "FDC_TXN_ID" "null"
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

phonepe :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
phonepe txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKey "providerReferenceId" "null"
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

epaylater :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
epaylater txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKeys "eplOrderId" "id" "null"
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

sodexo :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
sodexo txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : XmlKey "retrievalReferenceNumber" "null"
  -- , epgTxnId    : XmlKey "transactionId" "null"
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

fssatmpinv2 :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
fssatmpinv2 txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : XmlKey "ref"    "null"
  -- , epgTxnId    : XmlKey "tranid" "null"
  -- , authId      : XmlKey "auth"   "null"
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

fssatmpin :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
fssatmpin txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : XmlKey "ref"    "null"
  -- , epgTxnId    : XmlKey "tranid" "null"
  -- , authId      : XmlKey "auth"   "null"
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

linepay :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
linepay txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : XmlKey "transactionId" ""
  -- , epgTxnId    : XmlKey "transactionId" ""
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

citi
  :: TxnDetail
  -> (Map.Map TL.Text EValue, Map.Map TL.Text EValue)
  -> MerchantPaymentGatewayResponseTemp
citi txn xmls = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justEmpty
  -- , epgTxnId    : Value justEmpty
  -- , authId      : Fn (fromMaybe "" >>> just) (result !! 7)
  -- , respCode    : Fn (just) decisionCode
  -- , respMessage : Fn (decisionCodeToMessageMap >>> just) decisionCode
  -- }
  -- where
  -- :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp"null")
  --       result       = undefined S.split (S.Pattern "|") (lookupXML xmls "CitiToMall" "null")
  -- :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
  --       decisionCode = undefined fromMaybe "" (result !! 6)

cash :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
cash txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : Value justNa
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

axisupi :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
axisupi txn pgr = undefined
  -- { txnId       : XmlKeys "merchantRequestId" "transactionRef" (txn ^. _txnId)
  -- , rrn         : XmlKey  "custRef" "null"
  -- , epgTxnId    : XmlKeys "gatewayTransactionId" "upiRequestId" "null"
  -- , authId      : Value   justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

otherGateways :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
otherGateways pgr = undefined
  -- { txnId       : Value (pgr ^. _txnId)
  -- , rrn         : XmlKeys "rrn"        "RRN"        "null"
  -- , epgTxnId    : XmlKeys "epgTxnId"   "EpgTxnId"   "null"
  -- , authId      : XmlKeys "authIdCode" "AuthIdCode" "null"
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

eulerUpiGWs :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
eulerUpiGWs txn pgr = undefined
  -- { txnId       : XmlKey "transactionRef" (txn ^. _txnId)
  -- , rrn         : XmlKey "custRef"        "null"
  -- , epgTxnId    : XmlKey "upiRequestId"   "null"
  -- , authId      : Value justNa
  -- , respCode    : Value (pgr ^. _respCode)
  -- , respMessage : Value (pgr ^. _respMessage)
  -- }

fsspay :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
fsspay txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKey "tranid" ""
  -- , authId      : XmlKey "auth" ""
  -- , respCode    : Value (unNull (pgr ^. _respCode) "" # just)
  -- , respMessage : Value (unNull (pgr ^. _respMessage) "" # just)
  -- }

lazypay :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
lazypay txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKeys "pgTxnNo" "transactionId" ""
  -- , authId      : XmlKey  "authIdCode" "NA"
  -- , respCode    : Value (unNull (pgr ^. _respCode) "" # just)
  -- , respMessage : Value (unNull (pgr ^. _respMessage) "" # just)
  -- }

pinelabs :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
pinelabs txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justEmpty
  -- , epgTxnId    : XmlKey "ppc_UniqueMerchantTxnID" ""
  -- , authId      : Value justNa
  -- , respCode    : FromKeyOrObj "ppc_TxnResponseCode" (pgr ^. _respCode)       "null"
  -- , respMessage : FromObjOrkey (pgr ^. _respMessage) "ppc_TxnResponseMessage" "null"
  -- }

airpay :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
airpay txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKey "APTRANSACTIONID" ""
  -- , authId      : Value justNa
  -- , respCode    : FromKeyOrObj "TRANSACTIONSTATUS" (pgr ^. _respCode)    ""
  -- , respMessage : FromKeyOrObj "MESSAGE"           (pgr ^. _respMessage) ""
  -- }

freechargev2 :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
freechargev2 txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKey "txnId" ""
  -- , authId      : XmlKey "authCode" "NA"
  -- , respCode    : Value (unNull (pgr ^. _respCode) "" # just)
  -- , respMessage : Value (unNull (pgr ^. _respMessage) "" # just)
  -- }

hdfcnb :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
hdfcnb txn pgr = undefined
  -- { txnId       : Value (txn ^. _txnId # just)
  -- , rrn         : Value justNa
  -- , epgTxnId    : XmlKey "BankRefNo" ""
  -- , authId      : Value justNa
  -- , respCode    : Value (unNull (pgr ^. _respCode) "" # just)
  -- , respMessage : Value (unNull (pgr ^. _respMessage) "" # just)
  -- }



