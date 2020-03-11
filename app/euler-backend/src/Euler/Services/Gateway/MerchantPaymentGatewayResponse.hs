module Euler.Services.Gateway.MerchantPaymentGatewayResponse
  ( MerchantPGRServiceByGateway
  , mkMerchantPGRServiceTemp
  , transformMpgrByGateway

  )
  where

import           EulerHS.Prelude as P

import           Data.Generics.Product.Fields

import           Euler.API.MerchantPaymentGatewayResponse (MerchantPaymentGatewayResponse (..),
                                                           MerchantPaymentGatewayResponse' (..))

import           Euler.Common.Types.Gateway
import           Euler.Common.Types.PaymentGatewayResponseXml

import           Euler.Storage.Types.PaymentGatewayResponse
import           Euler.Storage.Types.TxnDetail

import qualified Data.Char as C
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- Gateway
type PGateway = Text

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
  :: MerchantPGRServiceByGateway
  -> EValueMap
  -> MerchantPaymentGatewayResponse'
  -> MerchantPaymentGatewayResponse'
transformMpgrByGateway MerchantPGRServiceByGateway{..} xmls merchPGR
  = executePGR merchPGR xmls
  . ccavenue_v2
  . blazepay
  . stripe

  $ defaultMerchantPGRTemp



data MerchantPGRServiceByGateway = MerchantPGRServiceByGateway
  { ccavenue_v2 :: MerchantPGRTemp -> MerchantPGRTemp
  , blazepay    :: MerchantPGRTemp -> MerchantPGRTemp
  , stripe    :: MerchantPGRTemp -> MerchantPGRTemp
  }

mkMerchantPGRServiceTemp
  :: PGateway
  -> TxnDetail
  -> PaymentGatewayResponse
  -> EValueMap
  -> MerchantPGRServiceByGateway
mkMerchantPGRServiceTemp gateway txn pgr xmls = MerchantPGRServiceByGateway
  { ccavenue_v2 = ccavenue_v2' gateway txn pgr xmls
  , blazepay    = blazepay' gateway txn pgr xmls
  , stripe      = stripe' gateway txn pgr xmls
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

-- zaakpay :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- zaakpay txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justEmpty
--   , epgTxnId    = XmlKeys "pgTransId" "txnId" "null"
--   , authId      = Value justNa
--   , respCode    = FromKeyOrObj  "responseCode" (getField @"respCode" pgr) "null" -- EPS: extra check
--   , respMessage = FromKeysOrObj "responseMessage" "responseDescription" (getField @"respMessage" pgr)  "null" -- EPS: extra check
--   }

-- atom :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- atom txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = XmlKeys "bank_txn" "BID" "null"
--   , epgTxnId    = XmlKeys "mmp_txn" "atomtxnId" "null"
--   , authId      = XmlKey  "auth_code" "null"
--   , respCode    = Value $ getField @"respCode" pgr
--   , respMessage = Value $ getField @"respMessage" pgr
--   }


-- paylater :: TxnDetail -> MerchantPGRTemp
-- paylater txn = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justEmpty
--   , epgTxnId    = Value justNa
--   , authId      = Value justNa
--   , respCode    = Value justNa
--   , respMessage = Value justNa
--   }

-- mobikwik :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- mobikwik txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justEmpty
--   , epgTxnId    = XmlKeys "refid" "refId" "null"
--   , authId      = Value justNa
--   , respCode    = FromKeyOrObj "statuscode" (getField @"respCode" pgr) "null" -- EPS extra check
--   , respMessage = FromKeyOrObj "statusmessage" (getField @"respMessage" pgr) "null" -- EPS extra check
--   }

-- ebs_v3 :: PaymentGatewayResponse -> MerchantPGRTemp
-- ebs_v3 pgr = MerchantPGRTemp
--   { txnId       = Value $ getField @"txnId" pgr
--   , rrn         = XmlKeys "PaymentID" "paymentId" "null"
--   , epgTxnId    = XmlKeys "TransactionID" "transactionId" "null"
--   , authId      = Value justEmpty
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- ebs :: PaymentGatewayResponse -> MerchantPGRTemp
-- ebs pgr = MerchantPGRTemp
--   { txnId       = Value (getField @"txnId" pgr)
--   , rrn         = XmlKey "vpc_TransactionId" "null"
--   , epgTxnId    = XmlKey "vpc_PaymentId" "null"
--   , authId      = Value justEmpty
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- paytm_v2 :: PaymentGatewayResponse -> MerchantPGRTemp
-- paytm_v2 pgr = MerchantPGRTemp
--   { txnId       = Value (getField @"txnId" pgr)
--   , rrn         = XmlKey  "BANKTXNID" ""
--   , epgTxnId    = XmlKeys "TXNID" "TxnId" ""
--   , authId      = Value justEmpty
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- tpsl :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- tpsl txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = XmlKey "clnt_txn_ref" ""
--   , epgTxnId    = XmlKey "tpsl_txn_id" ""
--   , authId      = Value  justEmpty
--   , respCode    = FromKeyOrObj "txn_status" (getField @"respCode" pgr) ""
--   , respMessage = FromKeyOrObj "txn_msg" (getField @"respMessage" pgr) ""
--   }

-- amex :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- amex txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = XmlKey "vpc_ReceiptNo" "null"
--   , epgTxnId    = XmlKey "vpc_TransactionNo" "null"
--   , authId      = XmlKey "vpc_AuthorizeId" "null"
--   , respCode    = FromKeyOrObj "vpc_TxnResponseCode" (getField @"respCode" pgr)    "null" -- EPS: extra check
--   , respMessage = FromKeyOrObj "vpc_Message"         (getField @"respMessage" pgr) "null" -- EPS: extra check
--   }

-- dummy :: MerchantPGRTemp
-- dummy = MerchantPGRTemp
--   { txnId       = XmlKey "txnId"       "null"
--   , rrn         = XmlKeys "RRN"  "rrn" "null"
--   , epgTxnId    = XmlKey "epgTxnId"    "null"
--   , authId      = XmlKey "authIdCode"  "null"
--   , respCode    = XmlKey "respCode"    "null"
--   , respMessage = XmlKey "respMessage" "null"
--   }

-- hdfc_ebs_vas :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- hdfc_ebs_vas txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justEmpty
--   , epgTxnId    = Value justNa
--   , authId      = Value justNa
--   , respCode    = FromKeyOrObj  "ResponseCode" (getField @"respCode" pgr) "null" -- EPS:  extra check
--   , respMessage = FromKeysOrObj "ResponseMessage" "Error" (getField @"respMessage" pgr) ""     -- EPS:  extra check
--   }

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


-- ipg :: TxnDetail -> EValueMap -> MerchantPGRTemp
-- ipg txn xmls = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKey "ipgTransactionId" "null"
--   , authId      = Value justNa
--   , respCode    = Value
--       $ ipgRespCodeAndMessage xmls "fail_rc" "approval_code"
--       $ lookupXML xmls "ApprovalCode" "null"
--   , respMessage = Value
--       $ ipgRespCodeAndMessage xmls "fail_reason" "status"
--       $ lookupXML xmls "TransactionState" "null"
--   }
--   where
--     ipgRespCodeAndMessage xmls keys_1 key_2 df = Just $ lookupXMLKeys xmls keys_1 key_2 df

-- axisnb :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- axisnb txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKey "BID" ""
--   , authId      = Value justNa
--   , respCode    = Value $ getField @"axisRespCode" pgr
--   , respMessage = Value $ getField @"axisRespMessage" pgr
--   }

-- zestmoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- zestmoney txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = Value justNa
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

ccavenue_v2'
  :: PGateway
  -> TxnDetail
  -> PaymentGatewayResponse
  -> EValueMap
  -> MerchantPGRTemp
  -> MerchantPGRTemp
ccavenue_v2' gateway txn pgr xmls merchantPGRTemp
  | gateway == "CCAVENUE_V2" = merchantPGRTemp
    { txnId                = Value $ Just $ getField @"txnId" txn
    , rrn                  = XmlKeys "bank_ref_no" "order_bank_ref_no" "null"
    , epgTxnId             = XmlKeys "tracking_id" "reference_no"      "null"
    , authId               = Value justNa
    , respCode             = Value (getField @"respCode" pgr)
    , respMessage          = FromObjOrkey (getField @"respMessage" pgr) "order_status" "null"
    , offer                = Default
    , offer_type           = Default
    , offer_availed        = Default
    , discount_amount      = Default
    , offer_failure_reason = Default
    }
  | otherwise = merchantPGRTemp

-- cybersource :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- cybersource txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKeys "requestID" "RequestID" "null"
--   , authId      = XmlKeys "ccAuthReply_authorizationCode" "AuthorizationCode" "NA"
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- paypal :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- paypal txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justEmpty
--   , epgTxnId    = XmlKey "paymentId" ""
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- olapostpaid :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- olapostpaid txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justEmpty
--   , epgTxnId    = XmlKeys "transactionId" "globalMerchantId" ""
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

blazepay'
  :: PGateway
  -> TxnDetail
  -> PaymentGatewayResponse
  -> EValueMap
  -> MerchantPGRTemp
  -> MerchantPGRTemp
blazepay' gateway txn pgr xmls merchantPGRTemp
  | gateway == "BLAZEPAY" = merchantPGRTemp
    { txnId       = Value $ Just $ getField @"txnId" txn
    , rrn         = XmlKeys "issuerRefNo" "RRN" "null"
    , epgTxnId    = XmlKeys "pgTxnNo" "pgTxnId" "null"
    , authId      = XmlKey "authIdCode" "null"
    , respCode    = Value (getField @"respCode" pgr)
    , respMessage = FromObjOrkey (getField @"respMessage" pgr) "respMsg" "null"
    , offer                = Default
    , offer_type           = Default
    , offer_availed        = Default
    , discount_amount      = Default
    , offer_failure_reason = Default
    }
  | otherwise = merchantPGRTemp

-- simpl :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- simpl txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justEmpty
--   , epgTxnId    = XmlKey "id" ""
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- gocashfree :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- gocashfree txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justEmpty
--   , epgTxnId    = XmlKey "referenceId"  ""
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }


-- razorpay :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- razorpay txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justEmpty
--   , epgTxnId    = XmlKeys "razorpay_payment_id" "id" "null"
--   , authId      = Value justNa
--   , respCode    = Value $ Just $ fromMaybe "null" (getField @"respCode" pgr)
--   , respMessage = Value $ Just $ fromMaybe "null" (getField @"respMessage" pgr)
--   }

-- olmoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- olmoney txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn -- EHS: eps has not Just
--   , rrn         = Value justEmpty
--   , epgTxnId    = XmlKey "transactionId" "NA"
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- mpesa :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- mpesa txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKeys "mcompgtransid" "mcomPgTransID" "null"
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- sbibuddy :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- sbibuddy txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKey "transactionId"  "NA"
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- citrus :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- citrus txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = XmlKey "issuerRefNo" "null"
--   , epgTxnId    = XmlKey "pgTxnNo" "null"
--   , authId      = XmlKey "authIdCode" "null"
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- axis :: PaymentGatewayResponse -> MerchantPGRTemp
-- axis pgr = MerchantPGRTemp
--   { txnId       = XmlKey "vpc_MerchTxnRef" "null"
--   , rrn         = XmlKey "vpc_ReceiptNo" "null"
--   , epgTxnId    = XmlKey "vpc_TransactionNo" "null"
--   , authId      = XmlKey "vpc_AuthorizeId" "null"
--   , respCode    = FromKeyOrObj "vpc_TxnResponseCode" (getField @"respCode" pgr) "null" -- EPS: extra check
--   , respMessage = FromKeyOrObj "vpc_Message" (getField @"respMessage" pgr) "null" -- EPS: extra check
--   }

-- migs :: PaymentGatewayResponse -> MerchantPGRTemp
-- migs pgr = MerchantPGRTemp
--   { txnId       = XmlKey "vpc_MerchTxnRef" "null"
--   , rrn         = XmlKey "vpc_ReceiptNo" "null"
--   , epgTxnId    = XmlKey "vpc_TransactionNo" "null"
--   , authId      = XmlKey "vpc_AuthorizeId" "null"
--   , respCode    = FromKeyOrObj "vpc_TxnResponseCode" (getField @"respCode" pgr) "null" -- EPS: extra check
--   , respMessage = FromKeyOrObj "vpc_Message" (getField @"respMessage" pgr) "null" -- EPS: extra check
--   }

-- hdfc :: PaymentGatewayResponse -> MerchantPGRTemp
-- hdfc pgr = MerchantPGRTemp
--   { txnId       = XmlKey "trackid" "null"
--   , rrn         = XmlKey "ref" "null"
--   , epgTxnId    = XmlKey "paymentid" "null"
--   , authId      = XmlKey "auth" "null"
--   , respCode    = FromKeyOrObj "result" (getField @"respCode" pgr) "null" -- EPS: extra check
--   , respMessage = FromKeyOrObj "result" (getField @"respMessage" pgr) "null" -- EPS: extra check
--   }

-- icici :: PaymentGatewayResponse -> MerchantPGRTemp
-- icici pgr = MerchantPGRTemp
--   { txnId       = XmlKey "txnId"        "null"
--   , rrn         = XmlKey "RRN"          "null"
--   , epgTxnId    = XmlKey "epgTxnId"     "null"
--   , authId      = XmlKey "authIdCode"   "null"
--   , respCode    = FromKeyOrObj "respCode"    (getField @"respCode" pgr)    "null" -- EPS: extra check
--   , respMessage = FromKeyOrObj "respMessage" (getField @"respMessage" pgr) "null" -- EPS: extra check
--   }

-- icicinb :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- icicinb txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKey "BID" "null"
--   , authId      = Value justNa
--   , respCode    = FromKeyOrObj "PAID" (getField @"respCode" pgr) "null"
--   , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
--   }

-- olamoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- olamoney txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justEmpty
--   , epgTxnId    = XmlKey "transactionId" "NA"
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- paytm :: PaymentGatewayResponse -> MerchantPGRTemp
-- paytm pgr = MerchantPGRTemp
--   { txnId       = Value (getField @"txnId" pgr)
--   , rrn         = XmlKeys "TXNID" "TxnId" "null"
--   , epgTxnId    = XmlKeys "TXNID" "TxnId" "null"
--   , authId      = Value justEmpty
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- payu :: PaymentGatewayResponse -> MerchantPGRTemp
-- payu pgr = MerchantPGRTemp
--   { txnId       = Value (getField @"txnId" pgr)
--   , rrn         = XmlKey "bank_ref_num" "null"
--   , epgTxnId    = XmlKey "mihpayid"     "null"
--   , authId      = XmlKey "field2"       "NA"
--   , respCode    = Value $ whenNothing (getField @"respCode" pgr)    (Just "null")
--   , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "null")
--   }

-- freecharge :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- freecharge txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justEmpty
--   , epgTxnId    = XmlKey "txnId" "null"
--   , authId      = Value justNa
--   , respCode    = FromKeysOrObj "status"       "errorCode" (getField @"respCode" pgr)    "null" -- EPS: extra check
--   , respMessage = FromKeysOrObj "errorMessage" "status"    (getField @"respMessage" pgr) "null" -- EPS: extra check
--   }

-- billdesk :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- billdesk txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = XmlKey "TxnReferenceNo"  "null"
--   , epgTxnId    = XmlKey "TxnReferenceNo"  "null"
--   , authId      = XmlKey "BankReferenceNo" "NA"
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- kotak :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- kotak txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = XmlKey "RetRefNo" "NA"
--   , epgTxnId    = XmlKey "RetRefNo" "NA"
--   , authId      = XmlKey "AuthCode" "NA"
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- jiomoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- jiomoney txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKey "jm_tran_ref_no" "NA"
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- amazonpay :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- amazonpay txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKeys "amazonOrderId" "transactionId" "NA"
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- airtelmoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- airtelmoney txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ onlyAlphaNumeric $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKeys "TRAN_ID" "FDC_TXN_ID" "null"
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- phonepe :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- phonepe txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKey "providerReferenceId" "null"
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- epaylater :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- epaylater txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKeys "eplOrderId" "id" "null"
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- sodexo :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- sodexo txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = XmlKey "retrievalReferenceNumber" "null"
--   , epgTxnId    = XmlKey "transactionId" "null"
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- fssatmpinv2 :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- fssatmpinv2 txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = XmlKey "ref"    "null"
--   , epgTxnId    = XmlKey "tranid" "null"
--   , authId      = XmlKey "auth"   "null"
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- fssatmpin :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- fssatmpin txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = XmlKey "ref"    "null"
--   , epgTxnId    = XmlKey "tranid" "null"
--   , authId      = XmlKey "auth"   "null"
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- linepay :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- linepay txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = XmlKey "transactionId" ""
--   , epgTxnId    = XmlKey "transactionId" ""
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- citi
--   :: TxnDetail
--   -> EValueMap
--   -> MerchantPGRTemp
-- citi txn xmls = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justEmpty
--   , epgTxnId    = Value justEmpty
--   , authId      = Value $ whenNothing (listToMaybe $ drop 6 result) (Just "")
--   , respCode    = Value $ Just decisionCode
--   , respMessage = Value $ Just $ decisionCodeToMessageMap decisionCode
--   }
--   where
--     result       = T.splitOn "|" (lookupXML xmls "CitiToMall" "null")
--     decisionCode = fromMaybe "" (listToMaybe $ drop 5 result)

-- cash :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- cash txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = Value justNa
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- axisupi :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- axisupi txn pgr = MerchantPGRTemp
--   { txnId       = XmlKeys "merchantRequestId" "transactionRef" (getField @"txnId" txn)
--   , rrn         = XmlKey  "custRef" "null"
--   , epgTxnId    = XmlKeys "gatewayTransactionId" "upiRequestId" "null"
--   , authId      = Value   justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- otherGateways :: PaymentGatewayResponse -> MerchantPGRTemp
-- otherGateways pgr = MerchantPGRTemp
--   { txnId       = Value (getField @"txnId" pgr)
--   , rrn         = XmlKeys "rrn"        "RRN"        "null"
--   , epgTxnId    = XmlKeys "epgTxnId"   "EpgTxnId"   "null"
--   , authId      = XmlKeys "authIdCode" "AuthIdCode" "null"
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- eulerUpiGWs :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- eulerUpiGWs txn pgr = MerchantPGRTemp
--   { txnId       = XmlKey "transactionRef" (getField @"txnId" txn)
--   , rrn         = XmlKey "custRef"        "null"
--   , epgTxnId    = XmlKey "upiRequestId"   "null"
--   , authId      = Value justNa
--   , respCode    = Value (getField @"respCode" pgr)
--   , respMessage = Value (getField @"respMessage" pgr)
--   }

-- fsspay :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- fsspay txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKey "tranid" ""
--   , authId      = XmlKey "auth" ""
--   , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
--   , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
--   }

-- lazypay :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- lazypay txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKeys "pgTxnNo" "transactionId" ""
--   , authId      = XmlKey  "authIdCode" "NA"
--   , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
--   , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
--   }

-- pinelabs :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- pinelabs txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justEmpty
--   , epgTxnId    = XmlKey "ppc_UniqueMerchantTxnID" ""
--   , authId      = Value justNa
--   , respCode    = FromKeyOrObj "ppc_TxnResponseCode" (getField @"respCode" pgr)       "null"
--   , respMessage = FromObjOrkey (getField @"respMessage" pgr) "ppc_TxnResponseMessage" "null"
--   }

-- airpay :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- airpay txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKey "APTRANSACTIONID" ""
--   , authId      = Value justNa
--   , respCode    = FromKeyOrObj "TRANSACTIONSTATUS" (getField @"respCode" pgr)    ""
--   , respMessage = FromKeyOrObj "MESSAGE"           (getField @"respMessage" pgr) ""
--   }

-- freechargev2 :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- freechargev2 txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKey "txnId" ""
--   , authId      = XmlKey "authCode" "NA"
--   , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
--   , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
--   }

-- hdfcnb :: TxnDetail -> PaymentGatewayResponse -> MerchantPGRTemp
-- hdfcnb txn pgr = MerchantPGRTemp
--   { txnId       = Value $ Just $ getField @"txnId" txn
--   , rrn         = Value justNa
--   , epgTxnId    = XmlKey "BankRefNo" ""
--   , authId      = Value justNa
--   , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
--   , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
--   }

