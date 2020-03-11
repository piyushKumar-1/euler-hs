{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Euler.API.MerchantPaymentGatewayResponse where

import           EulerHS.Prelude as P

import           Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as LMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import           Data.Generics.Product.Fields

import           Euler.API.Types
import           Euler.Common.Types.PaymentGatewayResponseXml
import           Euler.Product.Domain as D
import           Euler.Storage.Types.PaymentGatewayResponse
import           Euler.Storage.Types.TxnDetail



data MerchantPaymentGatewayResponse' = MerchantPaymentGatewayResponse'
  { resp_code            :: Maybe Text
  , rrn                  :: Maybe Text
  , created              :: Maybe Text
  , epg_txn_id           :: Maybe Text
  , resp_message         :: Maybe Text
  , auth_id_code         :: Maybe Text
  , txn_id               :: Maybe Text
  , offer                :: Maybe Text
  , offer_type           :: Maybe Text
  , offer_availed        :: Maybe Text -- Foreign
  , discount_amount      :: Maybe Text -- Foreign
  , offer_failure_reason :: Maybe Text
  , gateway_response     :: Maybe Text -- Foreign
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
  { resp_code            :: Maybe Text -- Foreign
  , rrn                  :: Maybe Text -- Foreign
  , created              :: Maybe Text -- Foreign
  , epg_txn_id           :: Maybe Text -- Foreign
  , resp_message         :: Maybe Text -- Foreign
  , auth_id_code         :: Maybe Text -- Foreign
  , txn_id               :: Maybe Text -- Foreign
  , offer                :: Maybe Text
  , offer_type           :: Maybe Text
  , offer_availed        :: Maybe Text -- Foreign
  , discount_amount      :: Maybe Text -- Foreign
  , offer_failure_reason :: Maybe Text
  , gateway_response     :: Maybe Text -- Foreign
  }  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

makeMerchantPaymentGatewayResponse
  :: Maybe Text
  -> MerchantPaymentGatewayResponse'
  -> MerchantPaymentGatewayResponse
makeMerchantPaymentGatewayResponse gatewayResponse pgr' = MerchantPaymentGatewayResponse
  { resp_code = Just $ checkNull $ getField @"resp_code" pgr'
  , rrn = Just $ checkNull $ getField @"resp_code" pgr'
  , created = Just $ checkNull $ getField @"resp_code" pgr'
  , epg_txn_id = Just $ checkNull $ getField @"resp_code" pgr'
  , resp_message = Just $ checkNull $ getField @"resp_code" pgr'
  , auth_id_code = Just $ checkNull $ getField @"resp_code" pgr'
  , txn_id = Just $ checkNull $ getField @"resp_code" pgr'
  , offer = getField @"offer" pgr'
  , offer_type = getField @"offer_type" pgr'
  , offer_availed = getField @"offer_availed" pgr'
  , discount_amount = getField @"discount_amount" pgr'
  , offer_failure_reason = getField @"offer_failure_reason" pgr'
  , gateway_response = gatewayResponse
  }
    -- EHS: TODO move to common utils or sth to that effect
  where
    checkNull :: Maybe Text -> Text
    checkNull Nothing = mempty
    checkNull (Just resp)
      | resp == "null" = mempty
      | otherwise      = resp

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
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

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
  -> Map.Map TL.Text EValue
  -> MerchantPaymentGatewayResponseTemp
  -> MerchantPaymentGatewayResponse'
executePGR merchPGR xmls merchTemp =
  merchPGR
    { txn_id       = runPGR xmls $ getField @"txnId" merchTemp
    , rrn          = runPGR xmls $ getField @"rrn" merchTemp
    , epg_txn_id   = runPGR xmls $ getField @"epgTxnId" merchTemp
    , auth_id_code = runPGR xmls $ getField @"authId" merchTemp
    , resp_code    = runPGR xmls $ getField @"respCode" merchTemp
    , resp_message = runPGR xmls $ getField @"respMessage" merchTemp
    }

runPGR
  :: Map.Map TL.Text EValue
  -> PGRField
  -> Maybe Text
-- runPGR _ _= undefined
-- runPGR xmls (Fn f a) = f a
runPGR xmls (FromObjOrkeys ndf key_1 key_2 df) = Just $ fromMaybe (lookupXMLKeys xmls key_1 key_2 df) ndf
runPGR xmls (FromKeysOrObj key_1 key_2 ndf df) = Just $ lookupXMLKeys xmls key_1 key_2 $ fromMaybe df ndf
runPGR xmls (FromObjOrkey ndf key df) = Just $ fromMaybe (lookupXML xmls key df) ndf
runPGR xmls (FromKeyOrObj key ndf df) = Just $ lookupXML xmls key $ fromMaybe df ndf
runPGR xmls (XmlKeys key_1 key_2 df)  = Just $ lookupXMLKeys xmls key_1 key_2 df
runPGR xmls (XmlKey key df)           = Just $ lookupXML xmls key df
runPGR xmls (Value b) = b


lookupXML
  :: Map.Map TL.Text EValue
  -> Text
  -> Text
  -> Text
lookupXML xml key defaultValue = maybe defaultValue P.id $
  (<|>)
  (previewValue $ matchKeyfn_1 xml key)
  (previewValue $ listToMaybe $ getRecordValues $ matchKeyfn_2 xml key) -- EHS: check if 'preview _1' == 'listToMaybe'


-- previewValue
--   :: ∀ a r.
--       (Maybe { string :: Array a | r }) → Maybe a
-- previewValue =
--   preview _XMLValue >=> \v → isNotString v # if _ then Nothing else Just v
previewValue :: Maybe EValue -> Maybe Text
previewValue Nothing = Nothing
previewValue (Just (EText val)) = Just (TL.toStrict val)
previewValue (Just _) = Nothing

matchKeyfn_1
  :: Map.Map TL.Text EValue
  -> Text
  -> Maybe EValue
matchKeyfn_1 xml key = LMap.lookup (TL.fromStrict key) xml
  -- find (preview _XMLKey >>> eq (Just key)) xml

-- matchKeyfn_2
--   :: ∀ a r.
--       (Array { string :: String | r})
--       → String
--       → Maybe { string :: String | r }
-- matchKeyfn_2 xml key =
--   find (\val → val.string == key) xml
matchKeyfn_2
  :: Map.Map TL.Text EValue
  -> Text
  -> Maybe EValue
matchKeyfn_2 xml key = LMap.lookup (TL.fromStrict key) xml
  -- find (\val → val.string == key) xml


getRecordValues :: Maybe EValue -> [EValue]
getRecordValues (Just (EGroovyHM (GroovyHM m))) = Map.elems m
getRecordValues _ = []

-- exports.getRecordValues = function(rec) {
--   var i = [];
--   if (typeof rec == "object") {
--     for (var k in rec) {
--       i.push(rec[k]);
--     }
--   }
--   return i;
-- }

-- var obj = {
--     "a": 1,
--     "b": 2,
--     "c": 3
-- };

-- console.log(getRecordValues(obj))
-- [1,2,3]

lookupXMLKeys
  :: Map.Map TL.Text EValue
  -> Text
  -> Text
  -> Text
  -> Text
lookupXMLKeys xmls key_1 key_2 defaultValue =
  on chooseOne (\key -> lookupXML xmls key defaultValue) key_1 key_2
  where
        chooseOne a b = if a == defaultValue then b else a

justEmpty, justNa :: Maybe Text
justEmpty = Just T.empty
justNa = Just "NA"

onlyAlphaNumeric :: Text -> Text
onlyAlphaNumeric = undefined
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

zaakpay :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
zaakpay txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justEmpty
  , epgTxnId    = XmlKeys "pgTransId" "txnId" "null"
  , authId      = Value justNa
  , respCode    = FromKeyOrObj  "responseCode" (getField @"respCode" pgr) "null" -- EPS: extra check
  , respMessage = FromKeysOrObj "responseMessage" "responseDescription" (getField @"respMessage" pgr)  "null" -- EPS: extra check
  }

atom :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
atom txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = XmlKeys "bank_txn" "BID" "null"
  , epgTxnId    = XmlKeys "mmp_txn" "atomtxnId" "null"
  , authId      = XmlKey  "auth_code" "null"
  , respCode    = Value $ getField @"respCode" pgr
  , respMessage = Value $ getField @"respMessage" pgr
  }


paylater :: TxnDetail -> MerchantPaymentGatewayResponseTemp
paylater txn = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justEmpty
  , epgTxnId    = Value justNa
  , authId      = Value justNa
  , respCode    = Value justNa
  , respMessage = Value justNa
  }

mobikwik :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
mobikwik txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justEmpty
  , epgTxnId    = XmlKeys "refid" "refId" "null"
  , authId      = Value justNa
  , respCode    = FromKeyOrObj "statuscode" (getField @"respCode" pgr) "null" -- EPS extra check
  , respMessage = FromKeyOrObj "statusmessage" (getField @"respMessage" pgr) "null" -- EPS extra check
  }

ebs_v3 :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
ebs_v3 pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ getField @"txnId" pgr
  , rrn         = XmlKeys "PaymentID" "paymentId" "null"
  , epgTxnId    = XmlKeys "TransactionID" "transactionId" "null"
  , authId      = Value justEmpty
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

ebs :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
ebs pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value (getField @"txnId" pgr)
  , rrn         = XmlKey "vpc_TransactionId" "null"
  , epgTxnId    = XmlKey "vpc_PaymentId" "null"
  , authId      = Value justEmpty
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

paytm_v2 :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
paytm_v2 pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value (getField @"txnId" pgr)
  , rrn         = XmlKey  "BANKTXNID" ""
  , epgTxnId    = XmlKeys "TXNID" "TxnId" ""
  , authId      = Value justEmpty
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

tpsl :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
tpsl txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = XmlKey "clnt_txn_ref" ""
  , epgTxnId    = XmlKey "tpsl_txn_id" ""
  , authId      = Value  justEmpty
  , respCode    = FromKeyOrObj "txn_status" (getField @"respCode" pgr) ""
  , respMessage = FromKeyOrObj "txn_msg" (getField @"respMessage" pgr) ""
  }

amex :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
amex txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = XmlKey "vpc_ReceiptNo" "null"
  , epgTxnId    = XmlKey "vpc_TransactionNo" "null"
  , authId      = XmlKey "vpc_AuthorizeId" "null"
  , respCode    = FromKeyOrObj "vpc_TxnResponseCode" (getField @"respCode" pgr)    "null" -- EPS: extra check
  , respMessage = FromKeyOrObj "vpc_Message"         (getField @"respMessage" pgr) "null" -- EPS: extra check
  }

dummy :: MerchantPaymentGatewayResponseTemp
dummy = MerchantPaymentGatewayResponseTemp
  { txnId       = XmlKey "txnId"       "null"
  , rrn         = XmlKeys "RRN"  "rrn" "null"
  , epgTxnId    = XmlKey "epgTxnId"    "null"
  , authId      = XmlKey "authIdCode"  "null"
  , respCode    = XmlKey "respCode"    "null"
  , respMessage = XmlKey "respMessage" "null"
  }

hdfc_ebs_vas :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
hdfc_ebs_vas txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justEmpty
  , epgTxnId    = Value justNa
  , authId      = Value justNa
  , respCode    = FromKeyOrObj  "ResponseCode" (getField @"respCode" pgr) "null" -- EPS:  extra check
  , respMessage = FromKeysOrObj "ResponseMessage" "Error" (getField @"respMessage" pgr) ""     -- EPS:  extra check
  }

stripe
  :: TxnDetail
  -> PaymentGatewayResponse
  -> Map.Map TL.Text EValue
  -> MerchantPaymentGatewayResponseTemp
stripe txn pgr xmls = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = XmlKey "id" "null"
  , epgTxnId    = XmlKey "id" "null"
  , authId      = Value justNa
  , respCode    = Value respCode
  , respMessage = FromKeyOrObj "failureMessage" (getField @"respMessage" pgr) "null" -- EPS: extra check
  }
  where
        status      = lookupXML xmls "status" "null"
        failureCode = lookupXML xmls "failureCode" "null"
        respCode    = Just $ if status == "failed" then failureCode else status


ipg :: TxnDetail -> Map.Map TL.Text EValue -> MerchantPaymentGatewayResponseTemp
ipg txn xmls = MerchantPaymentGatewayResponseTemp
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
    ipgRespCodeAndMessage xmls keys_1 key_2 df = Just $ lookupXMLKeys xmls keys_1 key_2 df

axisnb :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
axisnb txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKey "BID" ""
  , authId      = Value justNa
  , respCode    = Value $ getField @"axisRespCode" pgr
  , respMessage = Value $ getField @"axisRespMessage" pgr
  }

zestmoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
zestmoney txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = Value justNa
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

ccavenue_v2
  :: TxnDetail
  -> PaymentGatewayResponse
  -> Map.Map TL.Text EValue
  -> MerchantPaymentGatewayResponseTemp
ccavenue_v2 txn pgr xmls = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = XmlKeys "bank_ref_no" "order_bank_ref_no" "null"
  , epgTxnId    = XmlKeys "tracking_id" "reference_no"      "null"
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = FromObjOrkey (getField @"respMessage" pgr) "order_status" "null" -- extra check
  }

cybersource :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
cybersource txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKeys "requestID" "RequestID" "null"
  , authId      = XmlKeys "ccAuthReply_authorizationCode" "AuthorizationCode" "NA"
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

paypal :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
paypal txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justEmpty
  , epgTxnId    = XmlKey "paymentId" ""
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

olapostpaid :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
olapostpaid txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justEmpty
  , epgTxnId    = XmlKeys "transactionId" "globalMerchantId" ""
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

blazepay
  :: TxnDetail
  -> PaymentGatewayResponse
  -> Map.Map TL.Text EValue
  -> MerchantPaymentGatewayResponseTemp
blazepay txn pgr xmls = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = XmlKeys "issuerRefNo" "RRN" "null"
  , epgTxnId    = XmlKeys "pgTxnNo" "pgTxnId" "null"
  , authId      = XmlKey "authIdCode" "null"
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = FromObjOrkey (getField @"respMessage" pgr) "respMsg" "null" -- EPS: extra check
  }

simpl :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
simpl txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justEmpty
  , epgTxnId    = XmlKey "id" ""
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

gocashfree :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
gocashfree txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justEmpty
  , epgTxnId    = XmlKey "referenceId"  ""
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }


razorpay :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
razorpay txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justEmpty
  , epgTxnId    = XmlKeys "razorpay_payment_id" "id" "null"
  , authId      = Value justNa
  , respCode    = Value $ Just $ fromMaybe "null" (getField @"respCode" pgr)
  , respMessage = Value $ Just $ fromMaybe "null" (getField @"respMessage" pgr)
  }

olmoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
olmoney txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn -- EHS: eps has not Just
  , rrn         = Value justEmpty
  , epgTxnId    = XmlKey "transactionId" "NA"
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

mpesa :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
mpesa txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKeys "mcompgtransid" "mcomPgTransID" "null"
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

sbibuddy :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
sbibuddy txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKey "transactionId"  "NA"
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

citrus :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
citrus txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = XmlKey "issuerRefNo" "null"
  , epgTxnId    = XmlKey "pgTxnNo" "null"
  , authId      = XmlKey "authIdCode" "null"
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

axis :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
axis pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = XmlKey "vpc_MerchTxnRef" "null"
  , rrn         = XmlKey "vpc_ReceiptNo" "null"
  , epgTxnId    = XmlKey "vpc_TransactionNo" "null"
  , authId      = XmlKey "vpc_AuthorizeId" "null"
  , respCode    = FromKeyOrObj "vpc_TxnResponseCode" (getField @"respCode" pgr) "null" -- EPS: extra check
  , respMessage = FromKeyOrObj "vpc_Message" (getField @"respMessage" pgr) "null" -- EPS: extra check
  }

migs :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
migs pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = XmlKey "vpc_MerchTxnRef" "null"
  , rrn         = XmlKey "vpc_ReceiptNo" "null"
  , epgTxnId    = XmlKey "vpc_TransactionNo" "null"
  , authId      = XmlKey "vpc_AuthorizeId" "null"
  , respCode    = FromKeyOrObj "vpc_TxnResponseCode" (getField @"respCode" pgr) "null" -- EPS: extra check
  , respMessage = FromKeyOrObj "vpc_Message" (getField @"respMessage" pgr) "null" -- EPS: extra check
  }

hdfc :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
hdfc pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = XmlKey "trackid" "null"
  , rrn         = XmlKey "ref" "null"
  , epgTxnId    = XmlKey "paymentid" "null"
  , authId      = XmlKey "auth" "null"
  , respCode    = FromKeyOrObj "result" (getField @"respCode" pgr) "null" -- EPS: extra check
  , respMessage = FromKeyOrObj "result" (getField @"respMessage" pgr) "null" -- EPS: extra check
  }

icici :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
icici pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = XmlKey "txnId"        "null"
  , rrn         = XmlKey "RRN"          "null"
  , epgTxnId    = XmlKey "epgTxnId"     "null"
  , authId      = XmlKey "authIdCode"   "null"
  , respCode    = FromKeyOrObj "respCode"    (getField @"respCode" pgr)    "null" -- EPS: extra check
  , respMessage = FromKeyOrObj "respMessage" (getField @"respMessage" pgr) "null" -- EPS: extra check
  }

icicinb :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
icicinb txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKey "BID" "null"
  , authId      = Value justNa
  , respCode    = FromKeyOrObj "PAID" (getField @"respCode" pgr) "null"
  , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
  }

olamoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
olamoney txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justEmpty
  , epgTxnId    = XmlKey "transactionId" "NA"
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

paytm :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
paytm pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value (getField @"txnId" pgr)
  , rrn         = XmlKeys "TXNID" "TxnId" "null"
  , epgTxnId    = XmlKeys "TXNID" "TxnId" "null"
  , authId      = Value justEmpty
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

payu :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
payu pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value (getField @"txnId" pgr)
  , rrn         = XmlKey "bank_ref_num" "null"
  , epgTxnId    = XmlKey "mihpayid"     "null"
  , authId      = XmlKey "field2"       "NA"
  , respCode    = Value $ whenNothing (getField @"respCode" pgr)    (Just "null")
  , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "null")
  }

freecharge :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
freecharge txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justEmpty
  , epgTxnId    = XmlKey "txnId" "null"
  , authId      = Value justNa
  , respCode    = FromKeysOrObj "status"       "errorCode" (getField @"respCode" pgr)    "null" -- EPS: extra check
  , respMessage = FromKeysOrObj "errorMessage" "status"    (getField @"respMessage" pgr) "null" -- EPS: extra check
  }

billdesk :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
billdesk txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = XmlKey "TxnReferenceNo"  "null"
  , epgTxnId    = XmlKey "TxnReferenceNo"  "null"
  , authId      = XmlKey "BankReferenceNo" "NA"
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

kotak :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
kotak txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = XmlKey "RetRefNo" "NA"
  , epgTxnId    = XmlKey "RetRefNo" "NA"
  , authId      = XmlKey "AuthCode" "NA"
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

jiomoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
jiomoney txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKey "jm_tran_ref_no" "NA"
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

amazonpay :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
amazonpay txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKeys "amazonOrderId" "transactionId" "NA"
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

airtelmoney :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
airtelmoney txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ onlyAlphaNumeric $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKeys "TRAN_ID" "FDC_TXN_ID" "null"
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

phonepe :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
phonepe txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKey "providerReferenceId" "null"
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

epaylater :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
epaylater txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKeys "eplOrderId" "id" "null"
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

sodexo :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
sodexo txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = XmlKey "retrievalReferenceNumber" "null"
  , epgTxnId    = XmlKey "transactionId" "null"
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

fssatmpinv2 :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
fssatmpinv2 txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = XmlKey "ref"    "null"
  , epgTxnId    = XmlKey "tranid" "null"
  , authId      = XmlKey "auth"   "null"
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

fssatmpin :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
fssatmpin txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = XmlKey "ref"    "null"
  , epgTxnId    = XmlKey "tranid" "null"
  , authId      = XmlKey "auth"   "null"
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

linepay :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
linepay txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = XmlKey "transactionId" ""
  , epgTxnId    = XmlKey "transactionId" ""
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

citi
  :: TxnDetail
  -> Map.Map TL.Text EValue
  -> MerchantPaymentGatewayResponseTemp
citi txn xmls = MerchantPaymentGatewayResponseTemp
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

cash :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
cash txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = Value justNa
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

axisupi :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
axisupi txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = XmlKeys "merchantRequestId" "transactionRef" (getField @"txnId" txn)
  , rrn         = XmlKey  "custRef" "null"
  , epgTxnId    = XmlKeys "gatewayTransactionId" "upiRequestId" "null"
  , authId      = Value   justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

otherGateways :: PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
otherGateways pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value (getField @"txnId" pgr)
  , rrn         = XmlKeys "rrn"        "RRN"        "null"
  , epgTxnId    = XmlKeys "epgTxnId"   "EpgTxnId"   "null"
  , authId      = XmlKeys "authIdCode" "AuthIdCode" "null"
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

eulerUpiGWs :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
eulerUpiGWs txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = XmlKey "transactionRef" (getField @"txnId" txn)
  , rrn         = XmlKey "custRef"        "null"
  , epgTxnId    = XmlKey "upiRequestId"   "null"
  , authId      = Value justNa
  , respCode    = Value (getField @"respCode" pgr)
  , respMessage = Value (getField @"respMessage" pgr)
  }

fsspay :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
fsspay txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKey "tranid" ""
  , authId      = XmlKey "auth" ""
  , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
  , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
  }

lazypay :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
lazypay txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKeys "pgTxnNo" "transactionId" ""
  , authId      = XmlKey  "authIdCode" "NA"
  , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
  , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
  }

pinelabs :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
pinelabs txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justEmpty
  , epgTxnId    = XmlKey "ppc_UniqueMerchantTxnID" ""
  , authId      = Value justNa
  , respCode    = FromKeyOrObj "ppc_TxnResponseCode" (getField @"respCode" pgr)       "null"
  , respMessage = FromObjOrkey (getField @"respMessage" pgr) "ppc_TxnResponseMessage" "null"
  }

airpay :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
airpay txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKey "APTRANSACTIONID" ""
  , authId      = Value justNa
  , respCode    = FromKeyOrObj "TRANSACTIONSTATUS" (getField @"respCode" pgr)    ""
  , respMessage = FromKeyOrObj "MESSAGE"           (getField @"respMessage" pgr) ""
  }

freechargev2 :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
freechargev2 txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKey "txnId" ""
  , authId      = XmlKey "authCode" "NA"
  , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
  , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
  }

hdfcnb :: TxnDetail -> PaymentGatewayResponse -> MerchantPaymentGatewayResponseTemp
hdfcnb txn pgr = MerchantPaymentGatewayResponseTemp
  { txnId       = Value $ Just $ getField @"txnId" txn
  , rrn         = Value justNa
  , epgTxnId    = XmlKey "BankRefNo" ""
  , authId      = Value justNa
  , respCode    = Value $ whenNothing (getField @"respCode" pgr) (Just "")
  , respMessage = Value $ whenNothing (getField @"respMessage" pgr) (Just "")
  }



