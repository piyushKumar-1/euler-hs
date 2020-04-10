module Euler.Services.Gateway.MerchantPaymentGatewayResponse
  ( MerchantPGRServiceByGateway(..)
  , mkMerchantPGRService
  )
  where

import           EulerHS.Prelude hiding (id)

import           Euler.Lens

import           Data.Generics.Product.Fields

-- import           Euler.API.MerchantPaymentGatewayResponse (MerchantPaymentGatewayResponse' (..))

import           Euler.Common.Types.Gateway
import           Euler.Common.Types.PaymentGatewayResponseXml

import           Euler.Storage.Types.PaymentGatewayResponse

import qualified Euler.Product.Domain as D
import           Euler.Product.Domain.TxnDetail

import qualified Data.Char as C
import           Data.Text (Text)
import qualified Data.Text as T

eulerUpiGatewaysList :: [Gateway]
eulerUpiGatewaysList =
  [ HDFC_UPI
  , INDUS_UPI
  , KOTAK_UPI
  , SBI_UPI
  , ICICI_UPI
  , HSBC_UPI
  , VIJAYA_UPI
  , YESBANK_UPI
  , PAYTM_UPI
  , GOOGLEPAY
  ]


data MerchantPGRTemp = MerchantPGRTemp
  { txnId                :: PGRField
  , rrn                  :: PGRField
  , epgTxnId             :: PGRField
  , authId               :: PGRField
  , respCode             :: PGRField
  , respMessage          :: PGRField
  , offer                :: PGRField
  , offer_type           :: PGRField
  , offer_availed        :: PGRField
  , discount_amount      :: PGRField
  , offer_failure_reason :: PGRField
  }
   deriving (Show, Eq, Ord, Generic)

defaultMerchantPGRTemp :: MerchantPGRTemp
defaultMerchantPGRTemp = MerchantPGRTemp
  { txnId                = Default
  , rrn                  = Default
  , epgTxnId             = Default
  , authId               = Default
  , respCode             = Default
  , respMessage          = Default
  , offer                = Default
  , offer_type           = Default
  , offer_availed        = Default
  , discount_amount      = Default
  , offer_failure_reason = Default
  }

-- EPS
-- | Type for PGR_Key_Mapping
-- | To fetch value from xml string we have following constructors
-- |    Fn      - To give custom function implementation for some key
-- |    XmlKey  key defaultValue
-- |    Xmlkeys key1 key2 defaultValue
-- |    Value   (just value)

data PGRField
  = FromKeysOrObj Text Text (Maybe Text) Text
  | FromObjOrkeys (Maybe Text) Text Text Text
  | XmlKeys Text Text Text
  | FromKeyOrObj Text (Maybe Text) Text
  | FromObjOrkey (Maybe Text) Text Text
  | XmlKey Text Text
  | Value (Maybe Text)
  | Default
  deriving (Show, Eq, Ord, Generic)



mkMerchantPGRService :: Maybe Gateway -> MerchantPGRServiceByGateway
mkMerchantPGRService mGW = MerchantPGRServiceByGateway $ mkMPGRTransform mGW

mkMPGRTransform :: Maybe Gateway -> TxnDetail -> PaymentGatewayResponse -> EValueMap -> D.MerchantPaymentGatewayResponse
mkMPGRTransform mGW txn pgr xmls = executePGR defMPGR xmls (mkMerchantPGRTemp mGW txn pgr xmls)
  where
    date = show <$> pgr ^. _dateCreated
    defMPGR = D.defaultMerchantPaymentGatewayResponse & _created .~ date

mkMerchantPGRTemp :: Maybe Gateway -> TxnDetail -> PaymentGatewayResponse -> EValueMap -> MerchantPGRTemp
mkMerchantPGRTemp Nothing _ pgr _ = otherGW' pgr
mkMerchantPGRTemp (Just gv) txn pgr xmls = case gv of
  CCAVENUE_V2    -> ccavenue_v2' txn pgr
  BLAZEPAY       -> blazepay' txn pgr
  STRIPE         -> stripe' txn pgr xmls
  CITI           -> citi' txn xmls
  IPG            -> ipg' txn xmls
  FSS_ATM_PIN_V2 -> fssatmpin' txn pgr
  FSS_ATM_PIN    -> fssatmpin' txn pgr
  HDFC_EBS_VAS   -> hdfc_ebs_vas' txn pgr
  FREECHARGE_V2  -> freechargev2' txn pgr
  AIRTELMONEY    -> airtelmoney' txn pgr
  CYBERSOURCE    -> cybersource' txn pgr
  OLAPOSTPAID    -> olapostpaid' txn pgr
  GOCASHFREE     -> gocashfree' txn pgr
  EPAYLATER      -> epaylater' txn pgr
  ZESTMONEY      -> zestmoney' txn pgr
  BILLDESK       -> billdesk' txn pgr
  JIOMONEY       -> jiomoney' txn pgr
  SBIBUDDY       -> sbibuddy' txn pgr
  RAZORPAY       -> razorpay' txn pgr
  AXIS_UPI       -> axisupi' txn pgr
--  PINELABS       -> pinelabs'  txn pgr -- PINELABS constructor is missing in Gateway data type
  MOBIKWIK       -> mobikwik' txn pgr
  LINEPAY        -> linepay' txn pgr
  PHONEPE        -> phonepe' txn pgr
  ICICINB        -> icicinb' txn pgr
  ZAAKPAY        -> zaakpay' txn pgr
--  AIRPAY         -> airpay' txn pgr -- AIRPAY constructor is missing in Gateway data type
  AXISNB         -> axisnb' txn pgr
  SODEXO         -> sodexo' txn pgr
  CITRUS         -> citrus' txn pgr
  PAYPAL         -> paypal' txn pgr
--  HDFCNB         -> hdfcnb' txn pgr -- HDFCNB constructor is missing in Gateway data type
  KOTAK          -> kotak' txn pgr
  MPESA          -> mpesa' txn pgr
  SIMPL          -> simpl' txn pgr
  CASH           -> cash' txn pgr
  TPSL           -> tpsl' txn pgr
  LAZYPAY        -> lazypay' txn pgr
  FSSPAY         -> fsspay' txn pgr
  AMEX           -> amex' txn pgr
  PAYFORT        -> payfort' txn pgr
  ITZCASH        -> itzcash' txn pgr
  ATOM           -> atom' txn pgr
  PAYTM_V2       -> paytm_v2' pgr
  EBS_V3         -> ebs_v3' pgr
  AXIS           -> axis' pgr
  HDFC           -> hdfc' pgr
  EBS            -> ebs' pgr
  MIGS           -> migs' pgr
  ICICI          -> icici' pgr
  PAYLATER       -> paylater' txn
  DUMMY          -> dummy'
  FREECHARGE     -> freecharge' txn pgr xmls
  PAYU           -> payu' pgr xmls
  PAYTM          -> paytm' pgr xmls
  OLAMONEY       -> olamoney' txn pgr xmls
  AMAZONPAY      -> amazonpay' txn pgr xmls
  _              -> otherGateways' gv txn pgr

data MerchantPGRServiceByGateway = MerchantPGRServiceByGateway
  { transformMpgrByGateway' :: TxnDetail
                            -> PaymentGatewayResponse
                            -> EValueMap
                            -> D.MerchantPaymentGatewayResponse
  }

executePGR
  :: D.MerchantPaymentGatewayResponse
  -> EValueMap
  -> MerchantPGRTemp
  -> D.MerchantPaymentGatewayResponse
executePGR merchPGR xmls merchTemp =
  merchPGR
    { D.txnId              = runPGR xmls $ getField @"txnId" merchTemp
    , D.rrn                = runPGR xmls $ getField @"rrn" merchTemp
    , D.epgTxnId           = runPGR xmls $ getField @"epgTxnId" merchTemp
    , D.authIdCode         = runPGR xmls $ getField @"authId" merchTemp
    , D.respCode           = runPGR xmls $ getField @"respCode" merchTemp
    , D.respMessage        = runPGR xmls $ getField @"respMessage" merchTemp
    , D.offer              = runPGR xmls $ getField @"offer" merchTemp
    , D.offerType          = runPGR xmls $ getField @"offer_type" merchTemp
    , D.offerAvailed       = runPGR xmls $ getField @"offer_availed" merchTemp
    , D.discountAmount     = runPGR xmls $ getField @"discount_amount" merchTemp
    , D.offerFailureReason = runPGR xmls $ getField @"offer_failure_reason" merchTemp
    }

runPGR
  :: EValueMap
  -> PGRField
  -> Maybe Text
runPGR xmls (FromObjOrkeys ndf key_1 key_2 df) = Just $ fromMaybe (lookupXMLKeys xmls key_1 key_2 df) ndf
runPGR xmls (FromKeysOrObj key_1 key_2 ndf df) = Just $ lookupXMLKeys xmls key_1 key_2 $ fromMaybe df ndf
runPGR xmls (FromObjOrkey ndf key df) = Just $ fromMaybe (lookupXML xmls key df) ndf
runPGR xmls (FromKeyOrObj key ndf df) = Just $ lookupXML xmls key $ fromMaybe df ndf
runPGR xmls (XmlKeys key_1 key_2 df)  = Just $ lookupXMLKeys xmls key_1 key_2 df
runPGR xmls (XmlKey key df)           = Just $ lookupXML xmls key df
runPGR _ (Value b) = b
runPGR _ Default = Nothing


justEmpty, justNa :: Maybe Text
justEmpty = Just T.empty
justNa = Just "NA"

onlyAlphaNumeric :: Text -> Text
onlyAlphaNumeric = T.filter (\c -> C.isDigit c || C.isAsciiLower c)
-- exports["onlyAlphaNumeric"] = function(str) {
--   return str.replace(/[^a-z0-9]/gi, '');
-- }

-- from Config.Constants
decisionCodeToMessageMap :: Text -> Text
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



-- EPS: ** PGR Key Mapping Configs **

zaakpay' ::TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
zaakpay' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKeys "pgTransId" "txnId" "null"
    , authId      = Value justNa
    , respCode    = FromKeyOrObj  "responseCode" (getField @"respCode" pgr) "null"
    , respMessage = FromKeysOrObj "responseMessage" "responseDescription" (getField @"respMessage" pgr)  "null"
    }

atom' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
atom' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKeys "bank_txn" "BID" "null"
    , epgTxnId    = XmlKeys "mmp_txn" "atomtxnId" "null"
    , authId      = XmlKey  "auth_code" "null"
    , respCode    = Value $ getField @"respCode" pgr
    , respMessage = Value $ getField @"respMessage" pgr
    }

paylater' :: TxnDetail -> MerchantPGRTemp
paylater' txn = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = Value justNa
    , authId      = Value justNa
    , respCode    = Value justNa
    , respMessage = Value justNa
    }

mobikwik' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
mobikwik' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKeys "refid" "refId" "null"
    , authId      = Value justNa
    , respCode    = FromKeyOrObj "statuscode" (getField @"respCode" pgr) "null" -- EPS extra check
    , respMessage = FromKeyOrObj "statusmessage" (getField @"respMessage" pgr) "null" -- EPS extra check
    }

ebs_v3' :: PaymentGatewayResponse -> MerchantPGRTemp
ebs_v3' pgr = defaultMerchantPGRTemp
    { txnId       = Value $ getField @"txnId" pgr
    , rrn         = XmlKeys "PaymentID" "paymentId" "null"
    , epgTxnId    = XmlKeys "TransactionID" "transactionId" "null"
    , authId      = Value justEmpty
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

ebs' :: PaymentGatewayResponse -> MerchantPGRTemp
ebs'  pgr = defaultMerchantPGRTemp
    { txnId       = Value (getField @"txnId" pgr)
    , rrn         = XmlKey "vpc_TransactionId" "null"
    , epgTxnId    = XmlKey "vpc_PaymentId" "null"
    , authId      = Value justEmpty
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

paytm_v2' :: PaymentGatewayResponse -> MerchantPGRTemp
paytm_v2' pgr = defaultMerchantPGRTemp
    { txnId       = Value (getField @"txnId" pgr)
    , rrn         = XmlKey  "BANKTXNID" ""
    , epgTxnId    = XmlKeys "TXNID" "TxnId" ""
    , authId      = Value justEmpty
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

tpsl' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
tpsl'  txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "clnt_txn_ref" ""
    , epgTxnId    = XmlKey "tpsl_txn_id" ""
    , authId      = Value  justEmpty
    , respCode    = FromKeyOrObj "txn_status" (getField @"respCode" pgr) ""
    , respMessage = FromKeyOrObj "txn_msg" (getField @"respMessage" pgr) ""
    }

amex' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
amex'  txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "vpc_ReceiptNo" "null"
    , epgTxnId    = XmlKey "vpc_TransactionNo" "null"
    , authId      = XmlKey "vpc_AuthorizeId" "null"
    , respCode    = FromKeyOrObj "vpc_TxnResponseCode" (getField @"respCode" pgr)    "null" -- EPS: extra check
    , respMessage = FromKeyOrObj "vpc_Message"         (getField @"respMessage" pgr) "null" -- EPS: extra check
    }

dummy' :: MerchantPGRTemp
dummy' = defaultMerchantPGRTemp
    { txnId       = XmlKey "txnId"       "null"
    , rrn         = XmlKeys "RRN"  "rrn" "null"
    , epgTxnId    = XmlKey "epgTxnId"    "null"
    , authId      = XmlKey "authIdCode"  "null"
    , respCode    = XmlKey "respCode"    "null"
    , respMessage = XmlKey "respMessage" "null"
    }

hdfc_ebs_vas' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
hdfc_ebs_vas'  txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = Value justNa
    , authId      = Value justNa
    , respCode    = FromKeyOrObj  "ResponseCode" (getField @"respCode" pgr) "null" -- EPS:  extra check
    , respMessage = FromKeysOrObj "ResponseMessage" "Error" (getField @"respMessage" pgr) ""     -- EPS:  extra check
    }

stripe'
  :: TxnDetail
  -> PaymentGatewayResponse
  -> EValueMap
  -> MerchantPGRTemp
stripe' txn pgr xmls = defaultMerchantPGRTemp
      { txnId                = Value $ Just $ getField @"txnId" txn
      , rrn                  = XmlKey "id" "null"
      , epgTxnId             = XmlKey "id" "null"
      , authId               = Value justNa
      , respCode             = Value respCode
      , respMessage          = FromKeyOrObj "failureMessage" (getField @"respMessage" pgr) "null"
      , offer                = Default
      , offer_type           = Default
      , offer_availed        = Default
      , discount_amount      = Default
      , offer_failure_reason = Default
      }
  where
        status      = lookupXML xmls "status" "null"
        failureCode = lookupXML xmls "failureCode" "null"
        respCode    = Just $ if status == "failed" then failureCode else status


ipg' :: TxnDetail -> EValueMap -> MerchantPGRTemp
ipg' txn xmls = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "ipgTransactionId" "null"
    , authId      = Value justNa
    , respCode    = Value
        $ ipgRespCodeAndMessage xmls "fail_rc" "approval_code"
        $ lookupXML xmls "ApprovalCode" "null"
    , respMessage = Value
        $ ipgRespCodeAndMessage xmls "fail_reason" "status"
        $ lookupXML xmls "TransactionState" "null"
    }
  where
    ipgRespCodeAndMessage xmls' keys_1 key_2 df = Just $ lookupXMLKeys xmls' keys_1 key_2 df

axisnb' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
axisnb' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "BID" ""
    , authId      = Value justNa
    , respCode    = Value $ getField @"axisRespCode" pgr
    , respMessage = Value $ getField @"axisRespMessage" pgr
    }

zestmoney' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
zestmoney' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = Value justNa
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

ccavenue_v2'
  :: TxnDetail
  -> PaymentGatewayResponse
  -> MerchantPGRTemp
ccavenue_v2' txn pgr = defaultMerchantPGRTemp
    { txnId                = Value $ Just $ getField @"txnId" txn
    , rrn                  = XmlKeys "bank_ref_no" "order_bank_ref_no" "null"
    , epgTxnId             = XmlKeys "tracking_id" "reference_no"      "null"
    , authId               = Value justNa
    , respCode             = Value (getField @"respCode" pgr)
    , respMessage          = FromObjOrkey (getField @"respMessage" pgr) "order_status" "null"
    }

cybersource' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
cybersource' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKeys "requestID" "RequestID" "null"
    , authId      = XmlKeys "ccAuthReply_authorizationCode" "AuthorizationCode" "NA"
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

paypal' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
paypal' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKey "paymentId" ""
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

olapostpaid' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
olapostpaid' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKeys "transactionId" "globalMerchantId" ""
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

blazepay'
  :: TxnDetail
  -> PaymentGatewayResponse
  -> MerchantPGRTemp
blazepay' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKeys "issuerRefNo" "RRN" "null"
    , epgTxnId    = XmlKeys "pgTxnNo" "pgTxnId" "null"
    , authId      = XmlKey "authIdCode" "null"
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = FromObjOrkey (getField @"respMessage" pgr) "respMsg" "null"
    }

simpl' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
simpl' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKey "id" ""
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

gocashfree' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
gocashfree' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKey "referenceId"  ""
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

razorpay' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
razorpay' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKeys "razorpay_payment_id" "id" "null"
    , authId      = Value justNa
    , respCode    = Value $ Just $ fromMaybe "null" (getField @"respCode" pgr)
    , respMessage = Value $ Just $ fromMaybe "null" (getField @"respMessage" pgr)
    }

-- olmoney' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- olmoney' txn pgr = defaultMerchantPGRTemp
--     { txnId       = Value $ Just $ getField @"txnId" txn -- EHS: eps has not Just
--     , rrn         = Value justEmpty
--     , epgTxnId    = XmlKey "transactionId" "NA"
--     , authId      = Value justNa
--     , respCode    = Value (getField @"respCode" pgr)
--     , respMessage = Value (getField @"respMessage" pgr)
--     }

mpesa' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
mpesa' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKeys "mcompgtransid" "mcomPgTransID" "null"
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

sbibuddy' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
sbibuddy' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "transactionId"  "NA"
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

citrus' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
citrus' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "issuerRefNo" "null"
    , epgTxnId    = XmlKey "pgTxnNo" "null"
    , authId      = XmlKey "authIdCode" "null"
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

axis' :: PaymentGatewayResponse -> MerchantPGRTemp
axis' pgr = defaultMerchantPGRTemp
    { txnId       = XmlKey "vpc_MerchTxnRef" "null"
    , rrn         = XmlKey "vpc_ReceiptNo" "null"
    , epgTxnId    = XmlKey "vpc_TransactionNo" "null"
    , authId      = XmlKey "vpc_AuthorizeId" "null"
    , respCode    = FromKeyOrObj "vpc_TxnResponseCode" (getField @"respCode" pgr) "null" -- EPS: extra check
    , respMessage = FromKeyOrObj "vpc_Message" (getField @"respMessage" pgr) "null" -- EPS: extra check
    }

migs' :: PaymentGatewayResponse -> MerchantPGRTemp
migs' pgr = defaultMerchantPGRTemp
    { txnId       = XmlKey "vpc_MerchTxnRef" "null"
    , rrn         = XmlKey "vpc_ReceiptNo" "null"
    , epgTxnId    = XmlKey "vpc_TransactionNo" "null"
    , authId      = XmlKey "vpc_AuthorizeId" "null"
    , respCode    = FromKeyOrObj "vpc_TxnResponseCode" (getField @"respCode" pgr) "null" -- EPS: extra check
    , respMessage = FromKeyOrObj "vpc_Message" (getField @"respMessage" pgr) "null" -- EPS: extra check
    }

hdfc' :: PaymentGatewayResponse -> MerchantPGRTemp
hdfc' pgr = defaultMerchantPGRTemp
    { txnId       = XmlKey "trackid" "null"
    , rrn         = XmlKey "ref" "null"
    , epgTxnId    = XmlKey "paymentid" "null"
    , authId      = XmlKey "auth" "null"
    , respCode    = FromKeyOrObj "result" (getField @"respCode" pgr) "null" -- EPS: extra check
    , respMessage = FromKeyOrObj "result" (getField @"respMessage" pgr) "null" -- EPS: extra check
    }

icici' :: PaymentGatewayResponse -> MerchantPGRTemp
icici' pgr = defaultMerchantPGRTemp
    { txnId       = XmlKey "txnId"        "null"
    , rrn         = XmlKey "RRN"          "null"
    , epgTxnId    = XmlKey "epgTxnId"     "null"
    , authId      = XmlKey "authIdCode"   "null"
    , respCode    = FromKeyOrObj "respCode"    (getField @"respCode" pgr)    "null" -- EPS: extra check
    , respMessage = FromKeyOrObj "respMessage" (getField @"respMessage" pgr) "null" -- EPS: extra check
    }

icicinb' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
icicinb' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "BID" "null"
    , authId      = Value justNa
    , respCode    = FromKeyOrObj "PAID" (getField @"respCode" pgr) "null"
    , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
    }

olamoney'
  :: TxnDetail
  -> PaymentGatewayResponse
  -> EValueMap
  -> MerchantPGRTemp
olamoney' txn pgr xmls = defaultMerchantPGRTemp
    { txnId           = Value $ Just $ getField @"txnId" txn
    , rrn             = Value justEmpty
    , epgTxnId        = XmlKey "transactionId" "NA"
    , authId          = Value justNa
    , respCode        = Value (getField @"respCode" pgr)
    , respMessage     = Value (getField @"respMessage" pgr)
    , offer           = if couponCode /= "null" then Value (Just couponCode) else Default
    , offer_type      = if couponCode /= "null" then Value justNa else Default
    , offer_availed   = if couponCode /= "null" then Value (Just $ show $ strToBool isCashbackSuc) else Default
    , discount_amount = if couponCode /= "null" then Value (Just T.empty) else Default
    }
  where
    couponCode    = lookupXML xmls "couponCode"           "null"
    isCashbackSuc = lookupXML xmls "isCashbackSuccessful" "null"
    strToBool :: Text -> Bool
    strToBool "true" = True
    strToBool _      = False

paytm' :: PaymentGatewayResponse -> EValueMap -> MerchantPGRTemp
paytm' pgr xmls = defaultMerchantPGRTemp
    { txnId           = Value (getField @"txnId" pgr)
    , rrn             = XmlKeys "TXNID" "TxnId" "null"
    , epgTxnId        = XmlKeys "TXNID" "TxnId" "null"
    , authId          = Value justEmpty
    , respCode        = Value (getField @"respCode" pgr)
    , respMessage     = Value (getField @"respMessage" pgr)
    , offer           = if promoCampId /= "null" then Value (Just promoCampId) else Default
    , offer_type      = if promoCampId /= "null" then Value justNa else Default
    , offer_availed   =
      if promoCampId /= "null" then Value (Just $ show $ promoStatus == "PROMO_SUCCESS") else Default
    , discount_amount = if promoCampId /= "null" then Value (Just T.empty) else Default
    }
  where
    promoCampId = lookupXML xmls "PROMO_CAMP_ID" "null"
    promoStatus = lookupXML xmls "PROMO_STATUS"  "null"

payu' :: PaymentGatewayResponse -> EValueMap -> MerchantPGRTemp
payu' pgr xmls = defaultMerchantPGRTemp
    { txnId                = Value (getField @"txnId" pgr)
    , rrn                  = XmlKey "bank_ref_num" "null"
    , epgTxnId             = XmlKey "mihpayid"     "null"
    , authId               = XmlKey "field2"       "NA"
    , respCode             = Value $ whenNothing (getField @"respCode" pgr)    (Just "null")
    , respMessage          = Value $ whenNothing (getField @"respMessage" pgr) (Just "null")
    , offer                = if offer /= "NA" then Value (Just offerVal) else Default
    , offer_type           = if offer /= "NA" then Value (Just offerType) else Default
    , offer_availed        = if offer /= "NA" then Value (Just $ show offerAvailed) else Default
    , discount_amount      = if offer /= "NA" then Value (Just disAmount) else Default
    , offer_failure_reason =
        if (offer /= "NA") && (offerFailure /= "null") then Value (Just offerFailure) else Default
    }
  where
    offer        = lookupXML xmls "offer" "NA"
    offerVal     = lookupXMLKeys xmls "offer_availed" "offer" "null"
    offerType    = lookupXML xmls "offer_type" "null"
    offerFailure = lookupXML xmls "offer_failure_reason" "null"
    discount     = lookupXML xmls "discount" "NA"
    offerAvailed = offerVal /= "null"
    disAmount    = maybe T.empty show (readMaybe $ T.unpack discount :: Maybe Int)

freecharge'
  :: TxnDetail
  -> PaymentGatewayResponse
  -> EValueMap
  -> MerchantPGRTemp
freecharge' txn pgr xmls = defaultMerchantPGRTemp
    { txnId           = Value $ Just $ getField @"txnId" txn
    , rrn             = Value justEmpty
    , epgTxnId        = XmlKey "txnId" "null"
    , authId          = Value justNa
    , respCode        = FromKeysOrObj "status"       "errorCode" (getField @"respCode" pgr)    "null"
    , respMessage     = FromKeysOrObj "errorMessage" "status"    (getField @"respMessage" pgr) "null"
    , offer           = if campaignCode /= "NA" then Value (Just campaignCode) else Default
    , offer_type      = if campaignCode /= "NA" then Value justNa else Default
    , offer_availed   = if campaignCode /= "NA" then Value (Just "NA") else Default
    , discount_amount = if campaignCode /= "NA" then Value (Just T.empty) else Default
    }
  where
    campaignCode = lookupXML xmls "campaignCode" "NA"

billdesk' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
billdesk' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "TxnReferenceNo"  "null"
    , epgTxnId    = XmlKey "TxnReferenceNo"  "null"
    , authId      = XmlKey "BankReferenceNo" "NA"
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

kotak' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
kotak' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "RetRefNo" "NA"
    , epgTxnId    = XmlKey "RetRefNo" "NA"
    , authId      = XmlKey "AuthCode" "NA"
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

jiomoney' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
jiomoney' txn pgr  = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "jm_tran_ref_no" "NA"
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

amazonpay' :: TxnDetail -> PaymentGatewayResponse -> EValueMap -> MerchantPGRTemp
amazonpay' txn pgr xmls = defaultMerchantPGRTemp
    { txnId           = Value $ Just $ getField @"txnId" txn
    , rrn             = Value justNa
    , epgTxnId        = XmlKeys "amazonOrderId" "transactionId" "NA"
    , authId          = Value justNa
    , respCode        = Value (getField @"respCode" pgr)
    , respMessage     = Value (getField @"respMessage" pgr)
    , offer           = if sellerNote /= "null" then Value (Just sellerNote) else Default
    , offer_type      = if sellerNote /= "null" then Value (justNa) else Default
    , offer_availed   = if sellerNote /= "null" then Value (Just T.empty) else Default
    , discount_amount = if sellerNote /= "null" then Value (Just T.empty) else Default
    }
  where
    sellerNote = lookupXML xmls "sellerNote" "null"

airtelmoney' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
airtelmoney' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ onlyAlphaNumeric $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKeys "TRAN_ID" "FDC_TXN_ID" "null"
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

phonepe' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
phonepe' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "providerReferenceId" "null"
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

epaylater' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
epaylater' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKeys "eplOrderId" "id" "null"
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

sodexo' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
sodexo' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "retrievalReferenceNumber" "null"
    , epgTxnId    = XmlKey "transactionId" "null"
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

-- fssatmpinv2' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- fssatmpinv2' txn pgr = defaultMerchantPGRTemp
--     { txnId       = Value $ Just $ getField @"txnId" txn
--     , rrn         = XmlKey "ref"    "null"
--     , epgTxnId    = XmlKey "tranid" "null"
--     , authId      = XmlKey "auth"   "null"
--     , respCode    = Value (getField @"respCode" pgr)
--     , respMessage = Value (getField @"respMessage" pgr)
--     }

fssatmpin' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
fssatmpin' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "ref"    "null"
    , epgTxnId    = XmlKey "tranid" "null"
    , authId      = XmlKey "auth"   "null"
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

linepay' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
linepay' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "transactionId" ""
    , epgTxnId    = XmlKey "transactionId" ""
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

citi'
  :: TxnDetail
  -> EValueMap
  -> MerchantPGRTemp
citi' txn xmls = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = Value justEmpty
    , authId      = Value $ whenNothing (listToMaybe $ drop 6 result) (Just "")
    , respCode    = Value $ Just decisionCode
    , respMessage = Value $ Just $ decisionCodeToMessageMap decisionCode
    }
  where
    result       = T.splitOn "|" (lookupXML xmls "CitiToMall" "null")
    decisionCode = fromMaybe "" (listToMaybe $ drop 5 result)

cash' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
cash' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = Value justNa
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

axisupi' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
axisupi' txn pgr = defaultMerchantPGRTemp
    { txnId       = XmlKeys "merchantRequestId" "transactionRef" (getField @"txnId" txn)
    , rrn         = XmlKey  "custRef" "null"
    , epgTxnId    = XmlKeys "gatewayTransactionId" "upiRequestId" "null"
    , authId      = Value   justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }

fsspay' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
fsspay' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "tranid" ""
    , authId      = XmlKey "auth" ""
    , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
    , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
    }

lazypay' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
lazypay' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKeys "pgTxnNo" "transactionId" ""
    , authId      = XmlKey  "authIdCode" "NA"
    , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
    , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
    }

pinelabs' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
pinelabs' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKey "ppc_UniqueMerchantTxnID" ""
    , authId      = Value justNa
    , respCode    = FromKeyOrObj "ppc_TxnResponseCode" (getField @"respCode" pgr)       "null"
    , respMessage = FromObjOrkey (getField @"respMessage" pgr) "ppc_TxnResponseMessage" "null"
    }

airpay' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
airpay' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "APTRANSACTIONID" ""
    , authId      = Value justNa
    , respCode    = FromKeyOrObj "TRANSACTIONSTATUS" (getField @"respCode" pgr)    ""
    , respMessage = FromKeyOrObj "MESSAGE"           (getField @"respMessage" pgr) ""
    }

freechargev2' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
freechargev2' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "txnId" ""
    , authId      = XmlKey "authCode" "NA"
    , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
    , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
    }

hdfcnb' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
hdfcnb' txn pgr = defaultMerchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "BankRefNo" ""
    , authId      = Value justNa
    , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
    , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
    }

payfort' :: TxnDetail -> PaymentGatewayResponse  -> MerchantPGRTemp
payfort' txn pgr = defaultMerchantPGRTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justEmpty
  , epgTxnId    = XmlKey "fort_id" "null"
  , authId      = XmlKey "authorization_code" "null"
  , respCode    = FromKeyOrObj  "response_code"     (getField @"respCode" pgr)    "null"
  , respMessage = FromKeyOrObj  "response_message"  (getField @"respMessage" pgr) "null"
  }

itzcash' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
itzcash' txn pgr = defaultMerchantPGRTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justEmpty
  , epgTxnId    = XmlKey "transactionId"  "NA"
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

eulerUpiGWs' :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
eulerUpiGWs' txn pgr = defaultMerchantPGRTemp
  { txnId       = XmlKey "transactionRef" (getField @"txnId" txn)
  , rrn         = XmlKey "custRef"        "null"
  , epgTxnId    = XmlKey "upiRequestId"   "null"
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

otherGW' :: PaymentGatewayResponse -> MerchantPGRTemp
otherGW' pgr = defaultMerchantPGRTemp
  { txnId       = Value (getField @"txnId" pgr)
  , rrn         = XmlKeys "rrn" "RRN" "null"
  , epgTxnId    = XmlKeys "epgTxnId" "EpgTxnId" "null"
  , authId      = XmlKeys "authIdCode" "AuthIdCode" "null"
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

otherGateways'
  :: Gateway
  -> TxnDetail
  -> PaymentGatewayResponse
  -> MerchantPGRTemp
otherGateways' gateway txn pgr
  | gateway `elem` eulerUpiGatewaysList = eulerUpiGWs' txn pgr
  | otherwise                           = otherGW' pgr
