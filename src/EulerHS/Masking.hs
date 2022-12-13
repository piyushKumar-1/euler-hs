{-# LANGUAGE RecordWildCards #-}

module EulerHS.Masking where

import qualified Data.Aeson as Aeson
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (member)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified EulerHS.Logger.Types as Log
import           EulerHS.Prelude
import qualified Network.HTTP.Types as HTTP
import qualified EulerHS.Extra.Regex as Regex
import qualified Data.HashSet as HS
import Data.String.Conversions hiding ((<>))

shouldMaskKey :: Maybe Log.LogMaskingConfig -> Text -> Bool
shouldMaskKey Nothing _ = False
shouldMaskKey (Just Log.LogMaskingConfig{..}) key =
  case _keyType of
    Log.WhiteListKey -> not $ member key _maskKeys
    Log.BlackListKey -> member key _maskKeys

defaultMaskText :: Text
defaultMaskText = "***"

maskHTTPHeaders :: (Text -> Bool) -> Text -> Map.Map Text Text ->  Map.Map Text Text
maskHTTPHeaders shouldMask maskText = Map.mapWithKey maskHeader
  where
    maskHeader :: Text -> Text -> Text
    maskHeader key value  = if shouldMask key then maskText else value

maskServantHeaders :: (Text -> Bool) -> Text -> Seq HTTP.Header -> Seq HTTP.Header
maskServantHeaders shouldMask maskText headers = maskHeader <$> headers
  where
    maskHeader :: HTTP.Header -> HTTP.Header
    maskHeader (headerName,headerValue) =
      if shouldMask (decodeUtf8 $ CI.original headerName)
        then (headerName,encodeUtf8 maskText)
        else (headerName,headerValue)

maskQueryStrings :: (Text -> Bool) -> Text -> Seq HTTP.QueryItem -> Seq HTTP.QueryItem
maskQueryStrings shouldMask maskText queryStrings = maskQueryString <$> queryStrings
  where
    maskQueryString :: HTTP.QueryItem -> HTTP.QueryItem
    maskQueryString (key,value) =
      if shouldMask (decodeUtf8 key)
        then (key,Just $ encodeUtf8 maskText)
        else (key,value)

parseRequestResponseBody :: (Text -> Bool) -> Text -> Maybe ByteString -> ByteString -> Aeson.Value
parseRequestResponseBody shouldMask maskText mbContentType req
  | isContentTypeBlockedForLogging mbContentType = notSupportedPlaceHolder mbContentType
  | otherwise =
      case Aeson.eitherDecodeStrict req of
        Right value -> maskJSON shouldMask maskText value
        Left _ -> maskJSON shouldMask maskText $ handleQueryString req

maskJSON :: (Text -> Bool) -> Text -> Aeson.Value -> Aeson.Value
maskJSON shouldMask maskText (Aeson.Object r) = Aeson.Object $ handleObject shouldMask maskText r
maskJSON shouldMask maskText (Aeson.Array r) =  Aeson.Array $ maskJSON shouldMask maskText <$> r
maskJSON _ _ value = value

handleObject :: (Text -> Bool) -> Text -> Aeson.Object -> Aeson.Object
handleObject shouldMask maskText = HashMap.mapWithKey maskingFn
  where
    maskingFn key value = maskJSON shouldMask maskText $ updatedValue key value
    updatedValue key fn = if shouldMask key then Aeson.String maskText else fn

handleQueryString :: ByteString -> Aeson.Value
handleQueryString strg = Aeson.Object . fmap (Aeson.String . fromMaybe "") . HashMap.fromList $ HTTP.parseQueryText strg

notSupportedPlaceHolder :: Maybe ByteString -> Aeson.Value
notSupportedPlaceHolder (Just bs) = Aeson.String $ "Logging Not Support For this content " <> decodeUtf8 bs
notSupportedPlaceHolder Nothing = Aeson.String "Logging Not Support For this content "

isContentTypeBlockedForLogging :: Maybe ByteString -> Bool
isContentTypeBlockedForLogging Nothing = False
isContentTypeBlockedForLogging (Just contentType) =
       Text.isInfixOf "html" (Text.toLower $ decodeUtf8 contentType)
    || Text.isInfixOf "xml" (Text.toLower $ decodeUtf8 contentType)

getContentTypeForServant :: HTTP.ResponseHeaders -> Maybe ByteString
getContentTypeForServant = List.lookup HTTP.hContentType

getContentTypeForHTTP :: Map.Map Text Text -> Maybe ByteString
getContentTypeForHTTP header = getContentTypeForServant getTupleList
  where
    getTupleList = makeHeaderLableCI <$> Map.assocs header
    makeHeaderLableCI (headerName,headerValue) = (CI.mk $ encodeUtf8 headerName, encodeUtf8 headerValue)

-- PS Implemention for masking XML [blacklisting]
-- TODO: move away from regex

maskXMLText :: HS.HashSet Text -> Text.Text -> Text.Text
maskXMLText customMaskingKeys xml =
  let keysToMask = HS.union defaultMaskingKeys customMaskingKeys
  in foldl' (\acc x -> maskXMLForTAG x $ maskXMLForAttribute x acc) xml keysToMask
  where

    maskXMLForAttribute :: Text.Text -> Text.Text -> Text.Text
    maskXMLForAttribute key xmlToMask =
      case (Regex.regex ("(" <> key <> ")=\"[^>]*(\")" :: Text.Text)) of
        Left _ -> "[HIDDEN]" -- "ISSUE WITH REGEX"
        Right cRegex -> Regex.replace cRegex (((toSBSFromText key) <>  "=\"FILTERED\"") :: SBS) xmlToMask 

    maskXMLForTAG :: Text.Text -> Text.Text -> Text.Text
    maskXMLForTAG key xmlToMask = 
      case (Regex.regex ("<(" <> key <> ")>[^</]*</(" <> key <> ")>" :: Text.Text)) of
        Left _ -> "[HIDDEN]" -- "ISSUE WITH REGEX"
        Right cRegex -> Regex.replace cRegex (("<"  <> (toSBSFromText key) <>  ">FILTERED</" <> (toSBSFromText key) <> ">") :: SBS) xmlToMask 
    
    toSBSFromText :: Text.Text -> ByteString
    toSBSFromText str = encodeUtf8 $ str

    -- This is taken from euler-ps 
    defaultMaskingKeys :: HS.HashSet Text 
    defaultMaskingKeys = HS.fromList ["cardNumber", "card_number", "card_exp_month", "cardExpMonth", "card_exp_year", "cardExpYear", "card_security_code",
        "cardSecurityCode", "secretKey", "appId", "vpc_CardExp", "vpc_CardNum", "vpc_CardSecurityCode", "vpc_Card", "vpc_AccessCode", "vpc_User",
        "vpc_Password", "vpc_Merchant", "vpc_key", "paydata", "billDeskMerchantId", "card_expiryMonth", "card_expiryYear", "card_cvv", "card_holder",
        "accessToken", "olaPublicKey", "merchantPrivateKey", "xTenantKey", "xAuthKey", "access_token", "txnToken", "channelId", "mid", "cardInfo",
        "clientId", "ccnum", "ccname", "ccvv", "ccexpmon", "ccexpyr", "zero_click_token", "card[number]", "card[name]", "card[expiry_month]",
        "card[expiry_year]", "card[cvv]", "token", "expyear", "expmonth", "card", "password", "cvv2", "pass", "login", "mdd", "signature",
        "cnumber", "expmon", "expyr", "tranportalId", "PaymentID", "merchanttypekey", "vpc_SecureHash", "hash", "userAccessToken", "couponCode",
        "MID", "PAYMENT_DETAILS", "SSOToken", "CHECKSUMHASH", "CHANNEL_ID", "offer_key", "key", "access_code", "X-API-KEY", "ifsc", "bankIFSC",
        "accountNumber", "IFSC", "payerIfsc", "ifscCode", "encryptionKey", "encryptionIV", "iFSC", "accNo", "beneficiaryAccountNumber",
        "bankAccountNumber", "merchantGatewayAccount", "merchantAccount", "accountDetails", "cardData", "txnCardInfo", "shippingAddress",
        "billingAddress", "walletAccount", "object_reference_id", "email", "mobile_number", "first_name", "last_name", "customer_id",
        "customer_email", "customer_phone", "billing_address", "metadata", "customerPhone", "customerId", "customerEmail", "objectReferenceId",
        "mobileNumber", "firstName", "lastName", "shipping_address", "api_key", "apiKey", "account_details", "bank_account[account_number]",
        "bank_account[ifsc]", "expirationMonth", "expirationYear", "cvNumber", "name_on_card", "card_data", "c:proxyPAN", "expiry_date",
        "phone_number", "phone", "mobile", "contact", "phoneNumber", "contact_number", "contactNumber", "email_id", "customer_ip", "cardIsin",
        "cardEpYear", "cardReference", "cardFingerprint", "card_holder_name", "card_expiry_date", "number", "enc_card_number", "encrypted_pan",
        "month", "enc_expiry_month", "expiry_month", "expiry", "card_expiry_month", "encrypted_expiry_month", "year", "enc_expiry_year",
        "expiry_year", "card_expiry_year", "encrypted_expiry_year", "securityCode", "cvv_number", "cvv", "encryptedcvv", "nameOnCard", "udf1",
        "cname2", "name", "card_name", "member", "nameoncard", "CUST_MOBILE", "udf3", "customerMobileNumber", "billing_tel", "delivery_tel",
        "MOBILE_NO", "mobile_no", "mobileNo", "mobileNUmber", "CUST_EMAIL", "udf2", "billing_email", "delivery_email", "EMAIL", "email_address",
        "Authorization", "authorization", "Cookie", "cookie", "Proxy-Authorization", "proxy-authorization", "expday", "payerVpa"
      ]