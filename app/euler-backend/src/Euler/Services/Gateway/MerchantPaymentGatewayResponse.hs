module Euler.Services.Gateway.MerchantPaymentGatewayResponse
  ( MerchantPGRServiceByGateway
  , mkMerchantPGRServiceTemp
  , transformMpgrByGateway

  )
  where

import           EulerHS.Prelude hiding (id)

import           Data.Generics.Product.Fields

import           Euler.API.MerchantPaymentGatewayResponse (MerchantPaymentGatewayResponse' (..))

import           Euler.Common.Types.Gateway
import           Euler.Common.Types.PaymentGatewayResponseXml

import           Euler.Storage.Types.PaymentGatewayResponse
import           Euler.Product.Domain.TxnDetail

import qualified Data.Char as C
import           Data.Text (Text)
import qualified Data.Text as T

-- Gateway
type PGateway = Text

eulerGatewaysToExclude :: [Text]
eulerGatewaysToExclude =
  [ "AIRPAY"
  , "AIRTELMONEY"
  , "AMAZONPAY"
  , "AMEX"
  , "ATOM"
  , "AXIS"
  , "AXIS_UPI"
  , "AXISNB"
  , "BILLDESK"
  , "BLAZEPAY"
  , "CASH"
  , "CCAVENUE_V2"
  , "CITI"
  , "CITRUS"
  , "CYBERSOURCE"
  , "DUMMY"
  , "EBS"
  , "EBS_V3"
  , "EPAYLATER"
  , "FREECHARGE"
  , "FREECHARGE_V2"
  , "FSS_ATM_PIN"
  , "FSS_ATM_PIN_V2"
  , "FSSPAY"
  , "GOCASHFREE"
  , "HDFC"
  , "HDFC_EBS_VAS"
  , "HDFCNB"
  , "ICICI"
  , "ICICINB"
  , "IPG"
  , "JIOMONEY"
  , "KOTAK"
  , "LAZYPAY"
  , "LINEPAY"
  , "MIGS"
  , "MOBIKWIK"
  , "MPESA"
  , "OLAMONEY"
  , "OLAPOSTPAID"
  , "PAYLATER"
  , "PAYPAL"
  , "PAYTM"
  , "PAYTM_V2"
  , "PAYU"
  , "PHONEPE"
  , "PINELABS"
  , "RAZORPAY"
  , "SBIBUDDY"
  , "SIMPL"
  , "SODEXO"
  , "STRIPE"
  , "TPSL"
  , "ZAAKPAY"
  , "ZESTMONEY"
  ]

eulerUpiGatewaysOtherwise :: [Gateway]
eulerUpiGatewaysOtherwise =
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


transformMpgrByGateway
  :: MerchantPaymentGatewayResponse'
  -> EValueMap
  -> MerchantPGRServiceByGateway
  -> MerchantPaymentGatewayResponse'
transformMpgrByGateway merchPGR xmls MerchantPGRServiceByGateway{..}
  = executePGR merchPGR xmls
  . otherGateways
  . ccavenue_v2
  . blazepay
  . stripe
  . citi
  . ipg
  . fssatmpin
  . hdfc_ebs_vas
  . freechargev2
  . airtelmoney
  . cybersource
  . olapostpaid
  . gocashfree
  . epaylater
  . zestmoney
  . billdesk
  . jiomoney
  . sbibuddy
  . razorpay
  . axisupi
  . pinelabs
  . mobikwik
  . linepay
  . phonepe
  . icicinb
  . zaakpay
  . airpay
  . axisnb
  . sodexo
  . citrus
  . paypal
  . hdfcnb
  . kotak
  . mpesa
  . simpl
  . cash
  . tpsl
  . lazypay
  . fsspay
  . amex
  . atom
  . paytm_v2
  . ebs_v3
  . axis
  . hdfc
  . ebs
  . migs
  . icici
  . paylater
  . dummy
  . freecharge
  . payu
  . paytm
  . olamoney
  . amazonpay

  $ defaultMerchantPGRTemp



data MerchantPGRServiceByGateway = MerchantPGRServiceByGateway
  { otherGateways :: MerchantPGRTemp -> MerchantPGRTemp
  , ccavenue_v2   :: MerchantPGRTemp -> MerchantPGRTemp
  , blazepay      :: MerchantPGRTemp -> MerchantPGRTemp
  , stripe        :: MerchantPGRTemp -> MerchantPGRTemp
  , citi          :: MerchantPGRTemp -> MerchantPGRTemp
  , ipg           :: MerchantPGRTemp -> MerchantPGRTemp
  , fssatmpin     :: MerchantPGRTemp -> MerchantPGRTemp
  , hdfc_ebs_vas  :: MerchantPGRTemp -> MerchantPGRTemp
  , freechargev2  :: MerchantPGRTemp -> MerchantPGRTemp
  , airtelmoney   :: MerchantPGRTemp -> MerchantPGRTemp
  , cybersource   :: MerchantPGRTemp -> MerchantPGRTemp
  , olapostpaid   :: MerchantPGRTemp -> MerchantPGRTemp
  , gocashfree    :: MerchantPGRTemp -> MerchantPGRTemp
  , epaylater     :: MerchantPGRTemp -> MerchantPGRTemp
  , zestmoney     :: MerchantPGRTemp -> MerchantPGRTemp
  , billdesk      :: MerchantPGRTemp -> MerchantPGRTemp
  , jiomoney      :: MerchantPGRTemp -> MerchantPGRTemp
  , sbibuddy      :: MerchantPGRTemp -> MerchantPGRTemp
  , razorpay      :: MerchantPGRTemp -> MerchantPGRTemp
  , axisupi       :: MerchantPGRTemp -> MerchantPGRTemp
  , pinelabs      :: MerchantPGRTemp -> MerchantPGRTemp
  , mobikwik      :: MerchantPGRTemp -> MerchantPGRTemp
  , linepay       :: MerchantPGRTemp -> MerchantPGRTemp
  , phonepe       :: MerchantPGRTemp -> MerchantPGRTemp
  , icicinb       :: MerchantPGRTemp -> MerchantPGRTemp
  , zaakpay       :: MerchantPGRTemp -> MerchantPGRTemp
  , airpay        :: MerchantPGRTemp -> MerchantPGRTemp
  , axisnb        :: MerchantPGRTemp -> MerchantPGRTemp
  , sodexo        :: MerchantPGRTemp -> MerchantPGRTemp
  , citrus        :: MerchantPGRTemp -> MerchantPGRTemp
  , paypal        :: MerchantPGRTemp -> MerchantPGRTemp
  , hdfcnb        :: MerchantPGRTemp -> MerchantPGRTemp
  , kotak         :: MerchantPGRTemp -> MerchantPGRTemp
  , mpesa         :: MerchantPGRTemp -> MerchantPGRTemp
  , simpl         :: MerchantPGRTemp -> MerchantPGRTemp
  , cash          :: MerchantPGRTemp -> MerchantPGRTemp
  , tpsl          :: MerchantPGRTemp -> MerchantPGRTemp
  , lazypay       :: MerchantPGRTemp -> MerchantPGRTemp
  , fsspay        :: MerchantPGRTemp -> MerchantPGRTemp
  , amex          :: MerchantPGRTemp -> MerchantPGRTemp
  , atom          :: MerchantPGRTemp -> MerchantPGRTemp
  , paytm_v2      :: MerchantPGRTemp -> MerchantPGRTemp
  , ebs_v3        :: MerchantPGRTemp -> MerchantPGRTemp
  , axis          :: MerchantPGRTemp -> MerchantPGRTemp
  , hdfc          :: MerchantPGRTemp -> MerchantPGRTemp
  , ebs           :: MerchantPGRTemp -> MerchantPGRTemp
  , migs          :: MerchantPGRTemp -> MerchantPGRTemp
  , icici         :: MerchantPGRTemp -> MerchantPGRTemp
  , paylater      :: MerchantPGRTemp -> MerchantPGRTemp
  , dummy         :: MerchantPGRTemp -> MerchantPGRTemp
  , freecharge    :: MerchantPGRTemp -> MerchantPGRTemp
  , payu          :: MerchantPGRTemp -> MerchantPGRTemp
  , paytm         :: MerchantPGRTemp -> MerchantPGRTemp
  , olamoney      :: MerchantPGRTemp -> MerchantPGRTemp
  , amazonpay     :: MerchantPGRTemp -> MerchantPGRTemp
  }

mkMerchantPGRServiceTemp
  :: PGateway
  -> TxnDetail
  -> PaymentGatewayResponse
  -> EValueMap
  -> MerchantPGRServiceByGateway
mkMerchantPGRServiceTemp gateway txn pgr xmls = MerchantPGRServiceByGateway
  { otherGateways = otherGateways' gateway txn pgr
  , ccavenue_v2   = ccavenue_v2' gateway txn pgr
  , blazepay      = blazepay' gateway txn pgr
  , stripe        = stripe' gateway txn pgr xmls
  , citi          = citi' gateway txn xmls
  , ipg           = ipg' gateway txn xmls
  , fssatmpin     = fssatmpin' gateway txn pgr
  , hdfc_ebs_vas  = hdfc_ebs_vas' gateway txn pgr
  , freechargev2  = freechargev2' gateway txn pgr
  , airtelmoney   = airtelmoney' gateway txn pgr
  , cybersource   = cybersource' gateway txn pgr
  , olapostpaid   = olapostpaid' gateway txn pgr
  , gocashfree    = gocashfree' gateway txn pgr
  , epaylater     = epaylater' gateway txn pgr
  , zestmoney     = zestmoney' gateway txn pgr
  , billdesk      = billdesk' gateway txn pgr
  , jiomoney      = jiomoney' gateway txn pgr
  , sbibuddy      = sbibuddy' gateway txn pgr
  , razorpay      = razorpay' gateway txn pgr
  , axisupi       = axisupi' gateway txn pgr
  , pinelabs      = pinelabs' gateway txn pgr
  , mobikwik      = mobikwik' gateway txn pgr
  , linepay       = linepay' gateway txn pgr
  , phonepe       = phonepe' gateway txn pgr
  , icicinb       = icicinb' gateway txn pgr
  , zaakpay       = zaakpay' gateway txn pgr
  , airpay        = airpay' gateway txn pgr
  , axisnb        = axisnb' gateway txn pgr
  , sodexo        = sodexo' gateway txn pgr
  , citrus        = citrus' gateway txn pgr
  , paypal        = paypal' gateway txn pgr
  , hdfcnb        = hdfcnb' gateway txn pgr
  , kotak         = kotak' gateway txn pgr
  , mpesa         = mpesa' gateway txn pgr
  , simpl         = simpl' gateway txn pgr
  , cash          = cash' gateway txn pgr
  , tpsl          = tpsl' gateway txn pgr
  , lazypay       = lazypay' gateway txn pgr
  , fsspay        = fsspay' gateway txn pgr
  , amex          = amex' gateway txn pgr
  , atom          = atom' gateway txn pgr
  , paytm_v2      = paytm_v2' gateway pgr
  , ebs_v3        = ebs_v3' gateway pgr
  , axis          = axis' gateway pgr
  , hdfc          = hdfc' gateway pgr
  , ebs           = ebs' gateway pgr
  , migs          = migs' gateway pgr
  , icici         = icici' gateway pgr
  , paylater      = paylater' gateway txn
  , dummy         = dummy' gateway
  , freecharge    = freecharge' gateway txn pgr xmls
  , payu          = payu' gateway pgr xmls
  , paytm         = paytm' gateway pgr xmls
  , olamoney      = olamoney' gateway txn pgr xmls
  , amazonpay     = amazonpay' gateway txn pgr xmls
  }


executePGR
  :: MerchantPaymentGatewayResponse'
  -> EValueMap
  -> MerchantPGRTemp
  -> MerchantPaymentGatewayResponse'
executePGR merchPGR xmls merchTemp =
  merchPGR
    { txn_id               = runPGR xmls $ getField @"txnId" merchTemp
    , rrn                  = runPGR xmls $ getField @"rrn" merchTemp
    , epg_txn_id           = runPGR xmls $ getField @"epgTxnId" merchTemp
    , auth_id_code         = runPGR xmls $ getField @"authId" merchTemp
    , resp_code            = runPGR xmls $ getField @"respCode" merchTemp
    , resp_message         = runPGR xmls $ getField @"respMessage" merchTemp
    , offer                = runPGR xmls $ getField @"offer" merchTemp
    , offer_type           = runPGR xmls $ getField @"offer_type" merchTemp
    , offer_availed        = runPGR xmls $ getField @"offer_availed" merchTemp
    , discount_amount      = runPGR xmls $ getField @"discount_amount" merchTemp
    , offer_failure_reason = runPGR xmls $ getField @"offer_failure_reason" merchTemp
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

zaakpay' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
zaakpay' gateway txn pgr merchantPGRTemp
  | gateway == "ZAAKPAY" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKeys "pgTransId" "txnId" "null"
    , authId      = Value justNa
    , respCode    = FromKeyOrObj  "responseCode" (getField @"respCode" pgr) "null"
    , respMessage = FromKeysOrObj "responseMessage" "responseDescription" (getField @"respMessage" pgr)  "null"
    }
  | otherwise = merchantPGRTemp

atom' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
atom' gateway txn pgr merchantPGRTemp
  | gateway == "ATOM" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKeys "bank_txn" "BID" "null"
    , epgTxnId    = XmlKeys "mmp_txn" "atomtxnId" "null"
    , authId      = XmlKey  "auth_code" "null"
    , respCode    = Value $ getField @"respCode" pgr
    , respMessage = Value $ getField @"respMessage" pgr
    }
  | otherwise = merchantPGRTemp


paylater' :: PGateway -> TxnDetail -> MerchantPGRTemp -> MerchantPGRTemp
paylater' gateway txn merchantPGRTemp
  | gateway == "PAYLATER" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = Value justNa
    , authId      = Value justNa
    , respCode    = Value justNa
    , respMessage = Value justNa
    }
  | otherwise = merchantPGRTemp

mobikwik' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
mobikwik' gateway txn pgr merchantPGRTemp
  | gateway == "MOBIKWIK" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKeys "refid" "refId" "null"
    , authId      = Value justNa
    , respCode    = FromKeyOrObj "statuscode" (getField @"respCode" pgr) "null" -- EPS extra check
    , respMessage = FromKeyOrObj "statusmessage" (getField @"respMessage" pgr) "null" -- EPS extra check
    }
  | otherwise = merchantPGRTemp

ebs_v3' :: PGateway -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
ebs_v3' gateway pgr merchantPGRTemp
  | gateway == "EBS_V3" = merchantPGRTemp
    { txnId       = Value $ getField @"txnId" pgr
    , rrn         = XmlKeys "PaymentID" "paymentId" "null"
    , epgTxnId    = XmlKeys "TransactionID" "transactionId" "null"
    , authId      = Value justEmpty
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

ebs' :: PGateway -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
ebs' gateway pgr merchantPGRTemp
  | gateway == "EBS" = merchantPGRTemp
    { txnId       = Value (getField @"txnId" pgr)
    , rrn         = XmlKey "vpc_TransactionId" "null"
    , epgTxnId    = XmlKey "vpc_PaymentId" "null"
    , authId      = Value justEmpty
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

paytm_v2' :: PGateway -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
paytm_v2' gateway pgr merchantPGRTemp
  | gateway == "PAYTM_V2" = merchantPGRTemp
    { txnId       = Value (getField @"txnId" pgr)
    , rrn         = XmlKey  "BANKTXNID" ""
    , epgTxnId    = XmlKeys "TXNID" "TxnId" ""
    , authId      = Value justEmpty
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

tpsl' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
tpsl' gateway txn pgr merchantPGRTemp
  | gateway == "TPSL" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "clnt_txn_ref" ""
    , epgTxnId    = XmlKey "tpsl_txn_id" ""
    , authId      = Value  justEmpty
    , respCode    = FromKeyOrObj "txn_status" (getField @"respCode" pgr) ""
    , respMessage = FromKeyOrObj "txn_msg" (getField @"respMessage" pgr) ""
    }
  | otherwise = merchantPGRTemp

amex' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
amex' gateway txn pgr merchantPGRTemp
  | gateway == "AMEX" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "vpc_ReceiptNo" "null"
    , epgTxnId    = XmlKey "vpc_TransactionNo" "null"
    , authId      = XmlKey "vpc_AuthorizeId" "null"
    , respCode    = FromKeyOrObj "vpc_TxnResponseCode" (getField @"respCode" pgr)    "null" -- EPS: extra check
    , respMessage = FromKeyOrObj "vpc_Message"         (getField @"respMessage" pgr) "null" -- EPS: extra check
    }
  | otherwise = merchantPGRTemp

dummy' :: PGateway -> MerchantPGRTemp -> MerchantPGRTemp
dummy' gateway merchantPGRTemp
  | gateway == "DUMMY" = merchantPGRTemp
    { txnId       = XmlKey "txnId"       "null"
    , rrn         = XmlKeys "RRN"  "rrn" "null"
    , epgTxnId    = XmlKey "epgTxnId"    "null"
    , authId      = XmlKey "authIdCode"  "null"
    , respCode    = XmlKey "respCode"    "null"
    , respMessage = XmlKey "respMessage" "null"
    }
  | otherwise = merchantPGRTemp

hdfc_ebs_vas' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
hdfc_ebs_vas' gateway txn pgr merchantPGRTemp
  | gateway == "HDFC_EBS_VAS" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = Value justNa
    , authId      = Value justNa
    , respCode    = FromKeyOrObj  "ResponseCode" (getField @"respCode" pgr) "null" -- EPS:  extra check
    , respMessage = FromKeysOrObj "ResponseMessage" "Error" (getField @"respMessage" pgr) ""     -- EPS:  extra check
    }
  | otherwise = merchantPGRTemp

stripe'
  :: PGateway
  -> TxnDetail
  -> PaymentGatewayResponse
  -> EValueMap
  -> MerchantPGRTemp
  -> MerchantPGRTemp
stripe' gateway txn pgr xmls merchantPGRTemp
  | gateway == "" = merchantPGRTemp
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
  | otherwise = merchantPGRTemp
  where
        status      = lookupXML xmls "status" "null"
        failureCode = lookupXML xmls "failureCode" "null"
        respCode    = Just $ if status == "failed" then failureCode else status


ipg' :: PGateway -> TxnDetail -> EValueMap -> MerchantPGRTemp -> MerchantPGRTemp
ipg' gateway txn xmls merchantPGRTemp
  | gateway == "IPG" = merchantPGRTemp
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
  | otherwise = merchantPGRTemp
  where
    ipgRespCodeAndMessage xmls' keys_1 key_2 df = Just $ lookupXMLKeys xmls' keys_1 key_2 df

axisnb' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
axisnb' gateway txn pgr merchantPGRTemp
  | gateway == "AXISNB" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "BID" ""
    , authId      = Value justNa
    , respCode    = Value $ getField @"axisRespCode" pgr
    , respMessage = Value $ getField @"axisRespMessage" pgr
    }
  | otherwise = merchantPGRTemp

zestmoney' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
zestmoney' gateway txn pgr merchantPGRTemp
  | gateway == "ZESTMONEY" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = Value justNa
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

ccavenue_v2'
  :: PGateway
  -> TxnDetail
  -> PaymentGatewayResponse
  -> MerchantPGRTemp
  -> MerchantPGRTemp
ccavenue_v2' gateway txn pgr merchantPGRTemp
  | gateway == "CCAVENUE_V2" = merchantPGRTemp
    { txnId                = Value $ Just $ getField @"txnId" txn
    , rrn                  = XmlKeys "bank_ref_no" "order_bank_ref_no" "null"
    , epgTxnId             = XmlKeys "tracking_id" "reference_no"      "null"
    , authId               = Value justNa
    , respCode             = Value (getField @"respCode" pgr)
    , respMessage          = FromObjOrkey (getField @"respMessage" pgr) "order_status" "null"
    }
  | otherwise = merchantPGRTemp

cybersource' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
cybersource' gateway txn pgr merchantPGRTemp
  | gateway == "CYBERSOURCE" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKeys "requestID" "RequestID" "null"
    , authId      = XmlKeys "ccAuthReply_authorizationCode" "AuthorizationCode" "NA"
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

paypal' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
paypal' gateway txn pgr merchantPGRTemp
  | gateway == "PAYPAL" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKey "paymentId" ""
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

olapostpaid' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
olapostpaid' gateway txn pgr merchantPGRTemp
  | gateway == "OLAPOSTPAID" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKeys "transactionId" "globalMerchantId" ""
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

blazepay'
  :: PGateway
  -> TxnDetail
  -> PaymentGatewayResponse
  -> MerchantPGRTemp
  -> MerchantPGRTemp
blazepay' gateway txn pgr merchantPGRTemp
  | gateway == "BLAZEPAY" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKeys "issuerRefNo" "RRN" "null"
    , epgTxnId    = XmlKeys "pgTxnNo" "pgTxnId" "null"
    , authId      = XmlKey "authIdCode" "null"
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = FromObjOrkey (getField @"respMessage" pgr) "respMsg" "null"
    }
  | otherwise = merchantPGRTemp

simpl' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
simpl' gateway txn pgr merchantPGRTemp
  | gateway == "SIMPL" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKey "id" ""
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

gocashfree' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
gocashfree' gateway txn pgr merchantPGRTemp
  | gateway == "GOCASHFREE" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKey "referenceId"  ""
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

razorpay' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
razorpay' gateway txn pgr merchantPGRTemp
  | gateway == "RAZORPAY" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKeys "razorpay_payment_id" "id" "null"
    , authId      = Value justNa
    , respCode    = Value $ Just $ fromMaybe "null" (getField @"respCode" pgr)
    , respMessage = Value $ Just $ fromMaybe "null" (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

-- olmoney' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
-- olmoney' gateway txn pgr merchantPGRTemp
--   | gateway == "" = merchantPGRTemp
--     { txnId       = Value $ Just $ getField @"txnId" txn -- EHS: eps has not Just
--     , rrn         = Value justEmpty
--     , epgTxnId    = XmlKey "transactionId" "NA"
--     , authId      = Value justNa
--     , respCode    = Value (getField @"respCode" pgr)
--     , respMessage = Value (getField @"respMessage" pgr)
--     }
--   | otherwise = merchantPGRTemp

mpesa' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
mpesa' gateway txn pgr merchantPGRTemp
  | gateway == "MPESA" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKeys "mcompgtransid" "mcomPgTransID" "null"
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

sbibuddy' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
sbibuddy' gateway txn pgr merchantPGRTemp
  | gateway == "SBIBUDDY" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "transactionId"  "NA"
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

citrus' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
citrus' gateway txn pgr merchantPGRTemp
  | gateway == "CITRUS" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "issuerRefNo" "null"
    , epgTxnId    = XmlKey "pgTxnNo" "null"
    , authId      = XmlKey "authIdCode" "null"
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

axis' :: PGateway -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
axis' gateway pgr merchantPGRTemp
  | gateway == "AXIS" = merchantPGRTemp
    { txnId       = XmlKey "vpc_MerchTxnRef" "null"
    , rrn         = XmlKey "vpc_ReceiptNo" "null"
    , epgTxnId    = XmlKey "vpc_TransactionNo" "null"
    , authId      = XmlKey "vpc_AuthorizeId" "null"
    , respCode    = FromKeyOrObj "vpc_TxnResponseCode" (getField @"respCode" pgr) "null" -- EPS: extra check
    , respMessage = FromKeyOrObj "vpc_Message" (getField @"respMessage" pgr) "null" -- EPS: extra check
    }
  | otherwise = merchantPGRTemp

migs' :: PGateway -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
migs' gateway pgr merchantPGRTemp
  | gateway == "MIGS" = merchantPGRTemp
    { txnId       = XmlKey "vpc_MerchTxnRef" "null"
    , rrn         = XmlKey "vpc_ReceiptNo" "null"
    , epgTxnId    = XmlKey "vpc_TransactionNo" "null"
    , authId      = XmlKey "vpc_AuthorizeId" "null"
    , respCode    = FromKeyOrObj "vpc_TxnResponseCode" (getField @"respCode" pgr) "null" -- EPS: extra check
    , respMessage = FromKeyOrObj "vpc_Message" (getField @"respMessage" pgr) "null" -- EPS: extra check
    }
  | otherwise = merchantPGRTemp

hdfc' :: PGateway -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
hdfc' gateway pgr merchantPGRTemp
  | gateway == "HDFC" = merchantPGRTemp
    { txnId       = XmlKey "trackid" "null"
    , rrn         = XmlKey "ref" "null"
    , epgTxnId    = XmlKey "paymentid" "null"
    , authId      = XmlKey "auth" "null"
    , respCode    = FromKeyOrObj "result" (getField @"respCode" pgr) "null" -- EPS: extra check
    , respMessage = FromKeyOrObj "result" (getField @"respMessage" pgr) "null" -- EPS: extra check
    }
  | otherwise = merchantPGRTemp

icici' :: PGateway -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
icici' gateway pgr merchantPGRTemp
  | gateway == "ICICI" = merchantPGRTemp
    { txnId       = XmlKey "txnId"        "null"
    , rrn         = XmlKey "RRN"          "null"
    , epgTxnId    = XmlKey "epgTxnId"     "null"
    , authId      = XmlKey "authIdCode"   "null"
    , respCode    = FromKeyOrObj "respCode"    (getField @"respCode" pgr)    "null" -- EPS: extra check
    , respMessage = FromKeyOrObj "respMessage" (getField @"respMessage" pgr) "null" -- EPS: extra check
    }
  | otherwise = merchantPGRTemp

icicinb' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
icicinb' gateway txn pgr merchantPGRTemp
  | gateway == "ICICINB" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "BID" "null"
    , authId      = Value justNa
    , respCode    = FromKeyOrObj "PAID" (getField @"respCode" pgr) "null"
    , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
    }
  | otherwise = merchantPGRTemp

olamoney'
  :: PGateway
  -> TxnDetail
  -> PaymentGatewayResponse
  -> EValueMap
  -> MerchantPGRTemp
  -> MerchantPGRTemp
olamoney' gateway txn pgr xmls merchantPGRTemp
  | gateway == "OLAMONEY" = merchantPGRTemp
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
  | otherwise = merchantPGRTemp
  where
    couponCode    = lookupXML xmls "couponCode"           "null"
    isCashbackSuc = lookupXML xmls "isCashbackSuccessful" "null"
    strToBool :: Text -> Bool
    strToBool "true" = True
    strToBool _      = False

paytm' :: PGateway -> PaymentGatewayResponse -> EValueMap -> MerchantPGRTemp -> MerchantPGRTemp
paytm' gateway pgr xmls merchantPGRTemp
  | gateway == "PAYTM" = merchantPGRTemp
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
  | otherwise = merchantPGRTemp
  where
    promoCampId = lookupXML xmls "PROMO_CAMP_ID" "null"
    promoStatus = lookupXML xmls "PROMO_STATUS"  "null"

payu' :: PGateway -> PaymentGatewayResponse -> EValueMap -> MerchantPGRTemp -> MerchantPGRTemp
payu' gateway pgr xmls merchantPGRTemp
  | gateway == "PAYU" = merchantPGRTemp
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
  | otherwise = merchantPGRTemp
  where
    offer        = lookupXML xmls "offer" "NA"
    offerVal     = lookupXMLKeys xmls "offer_availed" "offer" "null"
    offerType    = lookupXML xmls "offer_type" "null"
    offerFailure = lookupXML xmls "offer_failure_reason" "null"
    discount     = lookupXML xmls "discount" "NA"
    offerAvailed = offerVal /= "null"
    disAmount    = maybe T.empty show (readMaybe $ T.unpack discount :: Maybe Int)

freecharge'
  :: PGateway
  -> TxnDetail
  -> PaymentGatewayResponse
  -> EValueMap
  -> MerchantPGRTemp
  -> MerchantPGRTemp
freecharge' gateway txn pgr xmls merchantPGRTemp
  | gateway == "FREECHARGE" = merchantPGRTemp
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
  | otherwise = merchantPGRTemp
  where
    campaignCode = lookupXML xmls "campaignCode" "NA"

billdesk' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
billdesk' gateway txn pgr merchantPGRTemp
  | gateway == "BILLDESK" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "TxnReferenceNo"  "null"
    , epgTxnId    = XmlKey "TxnReferenceNo"  "null"
    , authId      = XmlKey "BankReferenceNo" "NA"
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

kotak' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
kotak' gateway txn pgr merchantPGRTemp
  | gateway == "KOTAK" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "RetRefNo" "NA"
    , epgTxnId    = XmlKey "RetRefNo" "NA"
    , authId      = XmlKey "AuthCode" "NA"
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

jiomoney' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
jiomoney' gateway txn pgr merchantPGRTemp
  | gateway == "JIOMONEY" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "jm_tran_ref_no" "NA"
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

amazonpay' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> EValueMap -> MerchantPGRTemp -> MerchantPGRTemp
amazonpay' gateway txn pgr xmls merchantPGRTemp
  | gateway == "AMAZONPAY" = merchantPGRTemp
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
  | otherwise = merchantPGRTemp
  where
    sellerNote = lookupXML xmls "sellerNote" "null"

airtelmoney' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
airtelmoney' gateway txn pgr merchantPGRTemp
  | gateway == "AIRTELMONEY" = merchantPGRTemp
    { txnId       = Value $ Just $ onlyAlphaNumeric $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKeys "TRAN_ID" "FDC_TXN_ID" "null"
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

phonepe' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
phonepe' gateway txn pgr merchantPGRTemp
  | gateway == "PHONEPE" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "providerReferenceId" "null"
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

epaylater' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
epaylater' gateway txn pgr merchantPGRTemp
  | gateway == "EPAYLATER" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKeys "eplOrderId" "id" "null"
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

sodexo' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
sodexo' gateway txn pgr merchantPGRTemp
  | gateway == "SODEXO" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "retrievalReferenceNumber" "null"
    , epgTxnId    = XmlKey "transactionId" "null"
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

-- fssatmpinv2' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
-- fssatmpinv2' gateway txn pgr merchantPGRTemp
--   | gateway == "" = merchantPGRTemp
--     { txnId       = Value $ Just $ getField @"txnId" txn
--     , rrn         = XmlKey "ref"    "null"
--     , epgTxnId    = XmlKey "tranid" "null"
--     , authId      = XmlKey "auth"   "null"
--     , respCode    = Value (getField @"respCode" pgr)
--     , respMessage = Value (getField @"respMessage" pgr)
--     }
--   | otherwise = merchantPGRTemp

fssatmpin' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
fssatmpin' gateway txn pgr merchantPGRTemp
  | gateway == "FSS_ATM_PIN_V2" || gateway == "FSS_ATM_PIN" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "ref"    "null"
    , epgTxnId    = XmlKey "tranid" "null"
    , authId      = XmlKey "auth"   "null"
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

linepay' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
linepay' gateway txn pgr merchantPGRTemp
  | gateway == "LINEPAY" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKey "transactionId" ""
    , epgTxnId    = XmlKey "transactionId" ""
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

citi'
  :: PGateway
  -> TxnDetail
  -> EValueMap
  -> MerchantPGRTemp
  -> MerchantPGRTemp
citi' gateway txn xmls merchantPGRTemp
  | gateway == "CITI" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = Value justEmpty
    , authId      = Value $ whenNothing (listToMaybe $ drop 6 result) (Just "")
    , respCode    = Value $ Just decisionCode
    , respMessage = Value $ Just $ decisionCodeToMessageMap decisionCode
    }
  | otherwise = merchantPGRTemp
  where
    result       = T.splitOn "|" (lookupXML xmls "CitiToMall" "null")
    decisionCode = fromMaybe "" (listToMaybe $ drop 5 result)

cash' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
cash' gateway txn pgr merchantPGRTemp
  | gateway == "CASH" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = Value justNa
    , authId      = Value justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

axisupi' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
axisupi' gateway txn pgr merchantPGRTemp
  | gateway == "AXIS_UPI" = merchantPGRTemp
    { txnId       = XmlKeys "merchantRequestId" "transactionRef" (getField @"txnId" txn)
    , rrn         = XmlKey  "custRef" "null"
    , epgTxnId    = XmlKeys "gatewayTransactionId" "upiRequestId" "null"
    , authId      = Value   justNa
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = Value (getField @"respMessage" pgr)
    }
  | otherwise = merchantPGRTemp

fsspay' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
fsspay' gateway txn pgr merchantPGRTemp
  | gateway == "FSSPAY" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "tranid" ""
    , authId      = XmlKey "auth" ""
    , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
    , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
    }
  | otherwise = merchantPGRTemp

lazypay' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
lazypay' gateway txn pgr merchantPGRTemp
  | gateway == "LAZYPAY" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKeys "pgTxnNo" "transactionId" ""
    , authId      = XmlKey  "authIdCode" "NA"
    , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
    , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
    }
  | otherwise = merchantPGRTemp

pinelabs' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
pinelabs' gateway txn pgr merchantPGRTemp
  | gateway == "PINELABS" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justEmpty
    , epgTxnId    = XmlKey "ppc_UniqueMerchantTxnID" ""
    , authId      = Value justNa
    , respCode    = FromKeyOrObj "ppc_TxnResponseCode" (getField @"respCode" pgr)       "null"
    , respMessage = FromObjOrkey (getField @"respMessage" pgr) "ppc_TxnResponseMessage" "null"
    }
  | otherwise = merchantPGRTemp

airpay' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
airpay' gateway txn pgr merchantPGRTemp
  | gateway == "AIRPAY" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "APTRANSACTIONID" ""
    , authId      = Value justNa
    , respCode    = FromKeyOrObj "TRANSACTIONSTATUS" (getField @"respCode" pgr)    ""
    , respMessage = FromKeyOrObj "MESSAGE"           (getField @"respMessage" pgr) ""
    }
  | otherwise = merchantPGRTemp

freechargev2' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
freechargev2' gateway txn pgr merchantPGRTemp
  | gateway == "FREECHARGE_V2" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "txnId" ""
    , authId      = XmlKey "authCode" "NA"
    , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
    , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
    }
  | otherwise = merchantPGRTemp

hdfcnb' :: PGateway -> TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp -> MerchantPGRTemp
hdfcnb' gateway txn pgr merchantPGRTemp
  | gateway == "HDFCNB" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = Value justNa
    , epgTxnId    = XmlKey "BankRefNo" ""
    , authId      = Value justNa
    , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
    , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
    }
  | otherwise = merchantPGRTemp

otherGateways'
  :: PGateway
  -> TxnDetail
  -> PaymentGatewayResponse
  -> MerchantPGRTemp
  -> MerchantPGRTemp
otherGateways' gateway txn pgr merchantPGRTemp
  | gateway `notElem` eulerGatewaysToExclude && gateway `elem` includeGateways =
      merchantPGRTemp
        { txnId       = Value (getField @"txnId" pgr)
        , rrn         = XmlKeys "rrn" "RRN" "null"
        , epgTxnId    = XmlKeys "epgTxnId" "EpgTxnId" "null"
        , authId      = XmlKeys "authIdCode" "AuthIdCode" "null"
        , respCode    = Value (getField @"respCode" pgr)
        , respMessage = Value (getField @"respMessage" pgr)
        }
  | gateway `notElem` eulerGatewaysToExclude =
    merchantPGRTemp
      { txnId       = XmlKey "transactionRef" (getField @"txnId" txn)
      , rrn         = XmlKey "custRef"        "null"
      , epgTxnId    = XmlKey "upiRequestId"   "null"
      , authId      = Value justNa
      , respCode    = Value (getField @"respCode" pgr)
      , respMessage = Value (getField @"respMessage" pgr)
      }
  | otherwise = merchantPGRTemp
  where
    includeGateways = map show eulerUpiGatewaysOtherwise
