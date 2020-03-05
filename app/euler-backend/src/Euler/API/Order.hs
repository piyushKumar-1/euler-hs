{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Euler.API.Order where

import           EulerHS.Prelude
import           Web.FormUrlEncoded

import qualified Data.ByteString.Lazy         as BSL
import qualified Data.HashMap.Strict          as HM
import qualified Data.Map.Strict              as Map
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Generics.Product.Fields
import           Data.Time
import           Web.FormUrlEncoded
import           Data.Semigroup
import           Generics.Deriving.Semigroup (gsappenddefault)


import           Euler.Common.Types.External.Mandate (MandateFeature)
import           Euler.Common.Types.Order (OrderId)
import           Euler.Common.Types.Promotion
import           Euler.Common.Types.Refund
import           Euler.API.Types

-- import           Euler.Storage.Types.Mandate

import           Euler.Product.Domain as D
-- import qualified Prelude as P

import           Euler.Common.Types.Currency (Currency)
-- import           Euler.Common.Types.Customer (CustomerId)
-- import           Euler.Common.Types.External.Mandate (MandateFeature (..))
import           Euler.Common.Types.External.Order (OrderStatus (..))
import           Euler.Common.Types.Money
-- import           Euler.Common.Types.Promotion
import           Euler.Common.Types.Refund as Refund



import           Euler.Storage.Types.Mandate

import           Euler.Product.Domain.Chargeback as D
import           Euler.Product.Domain.Money
import           Euler.Product.Domain.Refund as D
import           Euler.Product.Domain.Mandate as D


-- Previously: OrderCreateReq
data OrderCreateRequest = OrderCreateRequest
  { order_id                          :: Text
  , amount                            :: Double
  , currency                          :: Maybe Currency -- Text -- EUR, USD, GBP,
--  Default value: MerchantIframePreferences defaultCurrency or INR
  , customer_id                       :: Maybe Text
  , customer_email                    :: Maybe Text
  , customer_phone                    :: Maybe Text
  , description                       :: Maybe Text
  , return_url                        :: Maybe Text
  , product_id                        :: Maybe Text

  , billing_address_first_name        :: Maybe Text
  , billing_address_last_name         :: Maybe Text
  , billing_address_line1             :: Maybe Text
  , billing_address_line2             :: Maybe Text
  , billing_address_line3             :: Maybe Text
  , billing_address_city              :: Maybe Text
  , billing_address_state             :: Maybe Text
  , billing_address_country           :: Maybe Text
  , billing_address_postal_code       :: Maybe Text
  , billing_address_phone             :: Maybe Text
  , billing_address_country_code_iso  :: Maybe Text -- Default value: IND

  , shipping_address_first_name       :: Maybe Text
  , shipping_address_last_name        :: Maybe Text
  , shipping_address_line1            :: Maybe Text
  , shipping_address_line2            :: Maybe Text
  , shipping_address_line3            :: Maybe Text
  , shipping_address_city             :: Maybe Text
  , shipping_address_state            :: Maybe Text
  , shipping_address_country          :: Maybe Text
  , shipping_address_postal_code      :: Maybe Text
  , shipping_address_phone            :: Maybe Text
  , shipping_address_country_code_iso :: Maybe Text -- Default value: IND
  , udf1                              :: Maybe Text
  , udf2                              :: Maybe Text
  , udf3                              :: Maybe Text
  , udf4                              :: Maybe Text
  , udf5                              :: Maybe Text
  , udf6                              :: Maybe Text
  , udf7                              :: Maybe Text
  , udf8                              :: Maybe Text
  , udf9                              :: Maybe Text
  , udf10                             :: Maybe Text
  , metaData                          :: Maybe Text
  , gateway_id                        :: Maybe Text -- EHS: converted to Int, why Text?
  , options_create_mandate            :: Maybe MandateFeature
  , mandate_max_amount                :: Maybe Text
  , auto_refund                       :: Maybe Bool
  , options_get_client_auth_token     :: Maybe Bool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, ToForm)

instance FromForm OrderCreateRequest where
  fromForm f = do
    let metaData' = foldl appendOnlyJust []
          $ map (\(k,v) -> (T.stripPrefix "metadata." k, listToMaybe v))
          $ HM.toList
          $ HM.filterWithKey (\k _ -> T.isPrefixOf "metadata." k) $ unForm f
    metaData <- case metaData' of
      [] -> pure Nothing
      l  -> pure $ Just $ T.decodeUtf8 $ BSL.toStrict $ encode $ Map.fromList l
    order_id <- parseUnique "order_id" f
    amount <- parseUnique "amount" f
    currency <- parseMaybe "currency" f
    customer_id <- parseMaybe "customer_id" f
    customer_email <- parseMaybe "customer_email" f
    customer_phone <- parseMaybe "customer_phone" f
    description <- parseMaybe "description" f
    return_url <- parseMaybe "return_url" f
    product_id <- parseMaybe "product_id" f
    billing_address_first_name <- parseMaybe "billing_address_first_name" f
    billing_address_last_name <- parseMaybe "billing_address_last_name" f
    billing_address_line1 <- parseMaybe "billing_address_line1" f
    billing_address_line2 <- parseMaybe "billing_address_line2" f
    billing_address_line3 <- parseMaybe "billing_address_line3" f
    billing_address_city <- parseMaybe "billing_address_city" f
    billing_address_state <- parseMaybe "billing_address_state" f
    billing_address_country <- parseMaybe "billing_address_country" f
    billing_address_postal_code <- parseMaybe "billing_address_postal_code" f
    billing_address_phone <- parseMaybe "billing_address_phone" f
    billing_address_country_code_iso <- parseMaybe "billing_address_country_code_iso" f
    shipping_address_first_name <- parseMaybe "shipping_address_first_name" f
    shipping_address_last_name <- parseMaybe "shipping_address_last_name" f
    shipping_address_line1 <- parseMaybe "shipping_address_line1" f
    shipping_address_line2 <- parseMaybe "shipping_address_line2" f
    shipping_address_line3 <- parseMaybe "shipping_address_line3" f
    shipping_address_city <- parseMaybe "shipping_address_city" f
    shipping_address_state <- parseMaybe "shipping_address_state" f
    shipping_address_country <- parseMaybe "shipping_address_country" f
    shipping_address_postal_code <- parseMaybe "shipping_address_postal_code" f
    shipping_address_phone <- parseMaybe "shipping_address_phone" f
    shipping_address_country_code_iso <- parseMaybe "shipping_address_country_code_iso" f
    udf1 <- parseMaybe "udf1" f
    udf2 <- parseMaybe "udf2" f
    udf3 <- parseMaybe "udf3" f
    udf4 <- parseMaybe "udf4" f
    udf5 <- parseMaybe "udf5" f
    udf6 <- parseMaybe "udf6" f
    udf7 <- parseMaybe "udf7" f
    udf8 <- parseMaybe "udf8" f
    udf9 <- parseMaybe "udf9" f
    udf10 <- parseMaybe "udf10" f
    gateway_id <- parseMaybe "gateway_id" f
    mandate_max_amount <- parseMaybe "mandate_max_amount" f
    auto_refund <- parseMaybe "auto_refund" f
    options_create_mandate <- liftA2 (<|>) (parseMaybe "options.create_mandate" f) (parseMaybe "options_create_mandate" f)
    options_get_client_auth_token <- liftA2 (<|>) (parseMaybe "options.get_client_auth_token" f) (parseMaybe "options_get_client_auth_token" f)
    pure OrderCreateRequest{..}

instance FromJSON OrderCreateRequest where
  parseJSON = withObject "OrderCreateRequest" $ \o -> do
    let metaData' = foldl appendOnlyJust []
          $ map (\(k,v) -> (T.stripPrefix "metadata." k, fromStrValue v))
          $ HM.toList
          $ HM.filterWithKey (\k _ -> T.isPrefixOf "metadata." k) o
    metaData <- case metaData' of
      [] -> pure Nothing
      l  -> pure $ Just $ T.decodeUtf8 $ BSL.toStrict $ encode $ Map.fromList l
    order_id <- o .: "order_id"
    amount <- o .: "amount"
    currency <- o .: "currency"
    customer_id <- o .: "customer_id"
    customer_email <- o .: "customer_email"
    customer_phone <- o .: "customer_phone"
    description <- o .: "description"
    return_url <- o .: "return_url"
    product_id <- o .: "product_id"
    billing_address_first_name <- o .: "billing_address_first_name"
    billing_address_last_name <- o .: "billing_address_last_name"
    billing_address_line1 <- o .: "billing_address_line1"
    billing_address_line2 <- o .: "billing_address_line2"
    billing_address_line3 <- o .: "billing_address_line3"
    billing_address_city <- o .: "billing_address_city"
    billing_address_state <- o .: "billing_address_state"
    billing_address_country <- o .: "billing_address_country"
    billing_address_postal_code <- o .: "billing_address_postal_code"
    billing_address_phone <- o .: "billing_address_phone"
    billing_address_country_code_iso <- o .: "billing_address_country_code_iso"
    shipping_address_first_name <- o .: "shipping_address_first_name"
    shipping_address_last_name <- o .: "shipping_address_last_name"
    shipping_address_line1 <- o .: "shipping_address_line1"
    shipping_address_line2 <- o .: "shipping_address_line2"
    shipping_address_line3 <- o .: "shipping_address_line3"
    shipping_address_city <- o .: "shipping_address_city"
    shipping_address_state <- o .: "shipping_address_state"
    shipping_address_country <- o .: "shipping_address_country"
    shipping_address_postal_code <- o .: "shipping_address_postal_code"
    shipping_address_phone <- o .: "shipping_address_phone"
    shipping_address_country_code_iso <- o .: "shipping_address_country_code_iso"
    udf1 <- o .: "udf1"
    udf2 <- o .: "udf2"
    udf3 <- o .: "udf3"
    udf4 <- o .: "udf4"
    udf5 <- o .: "udf5"
    udf6 <- o .: "udf6"
    udf7 <- o .: "udf7"
    udf8 <- o .: "udf8"
    udf9 <- o .: "udf9"
    udf10 <- o .: "udf10"
    gateway_id <- o .: "gateway_id"
    mandate_max_amount <- o .: "mandate_max_amount"
    auto_refund <- o .: "auto_refund"
    options_create_mandate <- o .: "options.create_mandate" <|> o .: "options_create_mandate"
    options_get_client_auth_token <- o .: "options.get_client_auth_token" <|> o .: "options_get_client_auth_token"
    pure OrderCreateRequest{..}

fromStrValue :: Value -> Maybe Text
fromStrValue s = case s of
  String x -> Just x
  _        -> Nothing

appendOnlyJust :: [(a, b)] -> (Maybe a, Maybe b) -> [(a, b)]
appendOnlyJust xs (Just k, Just v) = (k,v) : xs
appendOnlyJust xs _                = xs

fromStrValue :: Value -> Maybe Text
fromStrValue s = case s of
  String x -> Just x
  _        -> Nothing

appendOnlyJust :: [(a, b)] -> (Maybe a, Maybe b) -> [(a, b)]
appendOnlyJust xs (Just k, Just v) = (k,v) : xs
appendOnlyJust xs _                = xs

-- instance FromFormUrlEncoded Order where
--   fromFormUrlEncoded inputs =
--     User <$> lkp "email" <*> lkp "password"
--
--     where lkp input_label = case lookup input_label inputs of
--                  Nothing -> Left $ "label " <> input_label <> " not found"
--                  Just v    -> Right v

{- *
how to handle metadata for payment aggregators? e.g.

metadata.CCAVENUE_V2:promo_code
metadata.BILLDESK:AdditionalInfo3
metadata.BILLDESK:AdditionalInfo4
metadata.FREECHARGE:campaignCode
metadata.HSBC_UPI:addInfo
metadata.MIGS:vpc_AddendumData
metadata.MIGS:vpc_OrderInfo
metadata.OLAMONEY:couponCode

curl -X POST https://api.juspay.in/orders \
-u your_api_key: \
-d "order_id=1418394476" \
-d "amount=100.00" \
...
-d "metadata.PAYTM:PROMO_CAMP_ID=xyz" \
-d "metadata.PAYTM:CUST_ID=abcd" \
-d "metadata.PAYU:offer_key=qwerty" \
-d "PAYU.gateway_reference_id:reference_id”

in euler-ps it handles in ECR.js with modifyRequestBodyMiddleware
and in instance  Decode OrderCreateReq with modifyRequestBody from
src/Types/Communication/OLTP/Order.js
-}

-- Looks like domain type to me?
data OrderStatusQuery = OrderStatusQuery
  { orderId                 :: OrderId
  , merchantId              :: Text       -- ^
  , resellerId              :: Maybe Text -- ^
  , isAuthenticated         :: Bool
  , sendCardIsin            :: Bool
  , txnId                   :: Maybe Text -- ^ optional txn (seems to be always CHARGED?)
  , sendFullGatewayResponse :: Bool
  -- add info to handle case for orderCreate response (see execOrderStatusQuery function)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- we can live without it completely
data OrderStatusRequest = OrderStatusRequest
  { txn_uuid    :: Maybe Text
  , merchant_id :: Maybe Text
  , order_id    :: Maybe Text
  , txnUuid     :: Maybe Text
  , merchantId  :: Maybe Text
  , orderId     :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

defaultOrderStatusRequest :: OrderStatusRequest
defaultOrderStatusRequest  = OrderStatusRequest
  { txn_uuid     = Nothing -- :: Maybe Text
  , merchant_id  = Nothing -- :: Maybe Text
  , order_id     = Nothing -- :: Maybe Text
  , txnUuid      = Nothing -- :: Maybe Text
  , merchantId   = Nothing -- :: Maybe Text
  , orderId      = Nothing -- :: Maybe Text
  }

-- from src/Types/Communication/OLTP/OrderStatus.purs
-- TODO better naming, probably - mkStatusRequest :: Text -> OrderStatusRequest
getOrderStatusRequest :: Text -> OrderStatusRequest
getOrderStatusRequest ordId = OrderStatusRequest {  txn_uuid    = Nothing
                                                  , merchant_id = Nothing
                                                  , order_id    = Just ordId
                                                  , txnUuid     = Nothing
                                                  , merchantId  = Nothing
                                                  , orderId     = Nothing
                                                 -- , "options.add_full_gateway_response" : NullOrUndefined Nothing
                                                  }

mkStatusRequest :: Text -> OrderStatusRequest
mkStatusRequest orderId =
  OrderStatusRequest
  { txn_uuid    = Nothing
  , merchant_id = Nothing
  , order_id    = Just orderId
  , txnUuid     = Nothing
  , merchantId  = Nothing
  , orderId     = Nothing
  -- , "options.add_full_gateway_response" : NullOrUndefined Nothing
  }




--  Previously OrderAPIResponse
data OrderCreateResponse = OrderCreateResponse
  { status          :: OrderStatus
  , status_id       :: Int
  , id              :: Text
  , order_id        :: Text
  , payment_links   :: Paymentlinks
  , udf9            :: Maybe Text
  , udf8            :: Maybe Text
  , udf7            :: Maybe Text
  , udf6            :: Maybe Text
  , udf5            :: Maybe Text
  , udf4            :: Maybe Text
  , udf3            :: Maybe Text
  , udf2            :: Maybe Text
  , udf10           :: Maybe Text
  , udf1            :: Maybe Text
  , return_url      :: Maybe Text
  , refunded        :: Maybe Bool
  , product_id      :: Maybe Text
  , merchant_id     :: Maybe Text
  , date_created    :: Maybe LocalTime
  , customer_phone  :: Maybe Text
  , customer_id     :: Maybe Text
  , customer_email  :: Maybe Text
  , currency        :: Maybe Text
  , amount_refunded :: Maybe Double
  , amount          :: Maybe Double
  , juspay          :: Maybe OrderTokenResp
  }
  deriving (Show, Eq, Ord, Generic)

orderCreateResponseOptions :: Options
orderCreateResponseOptions = defaultOptions
  { omitNothingFields = True
  }

instance ToJSON OrderCreateResponse where
  toJSON     = genericToJSON orderCreateResponseOptions
  toEncoding = genericToEncoding orderCreateResponseOptions

instance FromJSON OrderCreateResponse where
  parseJSON = genericParseJSON orderCreateResponseOptions

defaultOrderCreateResponse :: OrderCreateResponse
defaultOrderCreateResponse = OrderCreateResponse
  { status          = NEW -- :: OrderStatus
  , status_id       = -1 -- :: Int
  , id              = "" -- :: Text
  , order_id        = "" -- :: Text
  , payment_links   = defaultPaymentlinks -- :: Paymentlinks
  , udf9            = Nothing -- :: Maybe Text
  , udf8            = Nothing -- :: Maybe Text
  , udf7            = Nothing -- :: Maybe Text
  , udf6            = Nothing -- :: Maybe Text
  , udf5            = Nothing -- :: Maybe Text
  , udf4            = Nothing -- :: Maybe Text
  , udf3            = Nothing -- :: Maybe Text
  , udf2            = Nothing -- :: Maybe Text
  , udf10           = Nothing -- :: Maybe Text
  , udf1            = Nothing -- :: Maybe Text
  , return_url      = Nothing -- :: Maybe Text
  , refunded        = Nothing -- :: Maybe Bool
  , product_id      = Nothing -- :: Maybe Text
  , merchant_id     = Nothing -- :: Maybe Text
  , date_created    = Nothing -- :: Maybe LocalTime
  , customer_phone  = Nothing -- :: Maybe Text
  , customer_id     = Nothing -- :: Maybe Text
  , customer_email  = Nothing -- :: Maybe Text
  , currency        = Nothing -- :: Maybe Text
  , amount_refunded = Nothing -- :: Maybe Double
  , amount          = Nothing -- :: Maybe Double
  , juspay          = Nothing -- :: Maybe OrderTokenResp
  }

data Paymentlinks = Paymentlinks
  { iframe :: Maybe Text
  , web    :: Maybe Text
  , mobile :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

defaultPaymentlinks :: Paymentlinks
defaultPaymentlinks = Paymentlinks
  { iframe = Nothing
  , web    = Nothing
  , mobile = Nothing
  }

-- EHS: why fields are maybe??
data OrderTokenResp = OrderTokenResp
  { client_auth_token        :: Maybe Text
  , client_auth_token_expiry :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

defaultOrderTokenResp :: OrderTokenResp
defaultOrderTokenResp = OrderTokenResp
  { client_auth_token        = Nothing -- :: Maybe Text
  , client_auth_token_expiry = Nothing -- :: Maybe Text
  }
-- from src/Types/Communication/OLTP/OrderStatus.purs
data OrderStatusResponse = OrderStatusResponse
  {  id                        :: Text
  ,  merchant_id               :: Maybe Text
  ,  amount                    :: Maybe Double
  ,  currency                  :: Maybe Text
  ,  order_id                  :: Maybe Text
  ,  date_created              :: Text
  ,  return_url                :: Maybe Text
  ,  product_id                :: Text
  ,  customer_email            :: Maybe Text -- Foreign
  ,  customer_phone            :: Maybe Text -- Foreign
  ,  customer_id               :: Maybe Text -- Foreign
  ,  payment_links             :: Paymentlinks
  ,  udf1                      :: Text
  ,  udf2                      :: Text
  ,  udf3                      :: Text
  ,  udf4                      :: Text
  ,  udf5                      :: Text
  ,  udf6                      :: Text
  ,  udf7                      :: Text
  ,  udf8                      :: Text
  ,  udf9                      :: Text
  ,  udf10                     :: Text
  ,  txn_id                    :: Maybe Text
  ,  status_id                 :: Int
  ,  status                    :: Text
  ,  payment_method_type       :: Maybe Text
  ,  auth_type                 :: Maybe Text
  ,  card                      :: Maybe Card
  ,  payment_method            :: Maybe Text
  ,  refunded                  :: Maybe Bool
  ,  amount_refunded           :: Maybe Double
  ,  chargebacks               :: Maybe [Chargeback']
  ,  refunds                   :: Maybe [Refund']
  ,  mandate                   :: Maybe Mandate'
  ,  promotion                 :: Maybe Promotion'
  ,  risk                      :: Maybe Risk
  ,  bank_error_code           :: Maybe Text
  ,  bank_error_message        :: Maybe Text
  ,  txn_uuid                  :: Maybe Text
  ,  gateway_payload           :: Maybe Text
  ,  txn_detail                :: Maybe TxnDetail'
  ,  payment_gateway_response' :: Maybe MerchantPaymentGatewayResponse'
  ,  payment_gateway_response  :: Maybe MerchantPaymentGatewayResponse
  ,  gateway_id                :: Maybe Int
  ,  emi_bank                  :: Maybe Text
  ,  emi_tenure                :: Maybe Int
  ,  gateway_reference_id      :: Maybe Text -- Foreign
  ,  payer_vpa                 :: Maybe Text -- Foreign
  ,  payer_app_name            :: Maybe Text -- Foreign
  ,  juspay                    :: Maybe OrderTokenResp
  ,  second_factor_response    :: Maybe MerchantSecondFactorResponse
  ,  txn_flow_info             :: Maybe TxnFlowInfo
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

defaultOrderStatusResponse :: OrderStatusResponse
defaultOrderStatusResponse = OrderStatusResponse
  {  id                        = "" -- :: Text
  ,  merchant_id               = Nothing -- :: Maybe Text
  ,  amount                    = Just 0 -- :: Maybe Double
  ,  currency                  = Nothing -- :: Maybe Text
  ,  order_id                  = Nothing -- :: Maybe Text
  ,  date_created              = "" -- :: Text
  ,  return_url                = Nothing -- :: Maybe Text
  ,  product_id                = "" -- :: Text
  ,  customer_email            = Nothing -- :: Maybe Text -- Foreign
  ,  customer_phone            = Nothing -- :: Maybe Text -- Foreign
  ,  customer_id               = Nothing -- :: Maybe Text -- Foreign
  ,  payment_links             = defaultPaymentlinks -- :: Paymentlinks
  ,  udf1                      = "udf1" -- :: Text
  ,  udf2                      = "udf2" -- :: Text
  ,  udf3                      = "udf3" -- :: Text
  ,  udf4                      = "udf4" -- :: Text
  ,  udf5                      = "udf5" -- :: Text
  ,  udf6                      = "udf6" -- :: Text
  ,  udf7                      = "udf7" -- :: Text
  ,  udf8                      = "udf8" -- :: Text
  ,  udf9                      = "udf9" -- :: Text
  ,  udf10                     = "udf10" -- :: Text
  ,  txn_id                    = Nothing -- :: Maybe Text
  ,  status_id                 = 0       -- :: Int
  ,  status                    = "DEFAULT" -- :: Text
  ,  payment_method_type       = Nothing -- :: Maybe Text
  ,  auth_type                 = Nothing -- :: Maybe Text
  ,  card                      = Nothing -- :: Maybe Card
  ,  payment_method            = Nothing -- :: Maybe Text
  ,  refunded                  = Nothing -- :: Maybe Bool
  ,  amount_refunded           = Nothing -- :: Maybe Double
  ,  chargebacks               = Nothing -- :: Maybe [Chargeback']
  ,  refunds                   = Nothing -- :: Maybe [Refund']
  ,  mandate                   = Nothing -- :: Maybe Mandate'
  ,  promotion                 = Nothing -- :: Maybe Promotion'
  ,  risk                      = Nothing -- :: Maybe Risk
  ,  bank_error_code           = Nothing -- :: Maybe Text
  ,  bank_error_message        = Nothing -- :: Maybe Text
  ,  txn_uuid                  = Nothing -- :: Maybe Text
  ,  gateway_payload           = Nothing -- :: Maybe Text
  ,  txn_detail                = Nothing -- :: Maybe TxnDetail'
  ,  payment_gateway_response' = Nothing -- :: Maybe MerchantPaymentGatewayResponse'
  ,  payment_gateway_response  = Nothing -- :: Maybe MerchantPaymentGatewayResponse
  ,  gateway_id                = Nothing -- :: Maybe Int
  ,  emi_bank                  = Nothing -- :: Maybe Text
  ,  emi_tenure                = Nothing -- :: Maybe Int
  ,  gateway_reference_id      = Nothing -- :: Maybe Text -- Foreign
  ,  payer_vpa                 = Nothing -- :: Maybe Text -- Foreign
  ,  payer_app_name            = Nothing -- :: Maybe Text -- Foreign
  ,  juspay                    = Nothing -- :: Maybe OrderTokenResp
  ,  second_factor_response    = Nothing -- :: Maybe MerchantSecondFactorResponse
  ,  txn_flow_info             = Nothing -- :: Maybe TxnFlowInfo
  }

data OrderStatusResponseTemp = OrderStatusResponseTemp
  {  idT                        :: Maybe (First Text)
  ,  merchant_idT               :: Maybe (First Text)
  ,  amountT                    :: Maybe (Last Double)
  ,  currencyT                  :: Maybe (Last Text)
  ,  order_idT                  :: Maybe (First Text)
  ,  date_createdT              :: Maybe (Last Text)
  ,  return_urlT                :: Maybe (Last Text)
  ,  product_idT                :: Maybe (Last Text)
  ,  customer_emailT            :: Maybe (Last Text)
  ,  customer_phoneT            :: Maybe (Last Text)
  ,  customer_idT               :: Maybe (Last Text)
  ,  payment_linksT             :: Maybe (Last Paymentlinks)
  ,  udf1T                      :: Maybe (Last Text)
  ,  udf2T                      :: Maybe (Last Text)
  ,  udf3T                      :: Maybe (Last Text)
  ,  udf4T                      :: Maybe (Last Text)
  ,  udf5T                      :: Maybe (Last Text)
  ,  udf6T                      :: Maybe (Last Text)
  ,  udf7T                      :: Maybe (Last Text)
  ,  udf8T                      :: Maybe (Last Text)
  ,  udf9T                      :: Maybe (Last Text)
  ,  udf10T                     :: Maybe (Last Text)
  ,  txn_idT                    :: Maybe (Last Text)
  ,  status_idT                 :: Maybe (Last Int)
  ,  statusT                    :: Maybe (Last Text)
  ,  payment_method_typeT       :: Maybe (Last Text)
  ,  auth_typeT                 :: Maybe (Last Text)
  ,  cardT                      :: Maybe (Last Card)
  ,  payment_methodT            :: Maybe (Last Text)
  ,  refundedT                  :: Maybe (Last Bool)
  ,  amount_refundedT           :: Maybe (Last Double)
  ,  chargebacksT               :: Maybe (Last [Chargeback'])
  ,  refundsT                   :: Maybe (Last [Refund'])
  ,  mandateT                   :: Maybe (Last Mandate')
  ,  promotionT                 :: Maybe (Last Promotion')
  ,  riskT                      :: Maybe (Last Risk)
  ,  bank_error_codeT           :: Maybe (Last Text)
  ,  bank_error_messageT        :: Maybe (Last Text)
  ,  txn_uuidT                  :: Maybe (Last Text)
  ,  gateway_payloadT           :: Maybe (Last Text)
  ,  txn_detailT                :: Maybe (Last TxnDetail')
  ,  payment_gateway_responseT' :: Maybe (Last MerchantPaymentGatewayResponse')
  ,  payment_gateway_responseT  :: Maybe (Last MerchantPaymentGatewayResponse)
  ,  gateway_idT                :: Maybe (Last Int)
  ,  emi_bankT                  :: Maybe (Last Text)
  ,  emi_tenureT                :: Maybe (Last Int)
  ,  gateway_reference_idT      :: Maybe (Last Text)
  ,  payer_vpaT                 :: Maybe (Last Text)
  ,  payer_app_nameT            :: Maybe (Last Text)
  ,  juspayT                    :: Maybe (Last OrderTokenResp)
  ,  second_factor_responseT    :: Maybe (Last MerchantSecondFactorResponse)
  ,  txn_flow_infoT             :: Maybe (Last TxnFlowInfo)
  }
  deriving (Show, Eq, Ord, Generic)

defaultOrderStatusResponseTemp :: OrderStatusResponseTemp
defaultOrderStatusResponseTemp = OrderStatusResponseTemp
  {  idT                        = mempty
  ,  merchant_idT               = mempty
  ,  amountT                    = mempty
  ,  currencyT                  = mempty
  ,  order_idT                  = mempty
  ,  date_createdT              = mempty
  ,  return_urlT                = mempty
  ,  product_idT                = mempty
  ,  customer_emailT            = mempty
  ,  customer_phoneT            = mempty
  ,  customer_idT               = mempty
  ,  payment_linksT             = mempty
  ,  udf1T                      = mempty
  ,  udf2T                      = mempty
  ,  udf3T                      = mempty
  ,  udf4T                      = mempty
  ,  udf5T                      = mempty
  ,  udf6T                      = mempty
  ,  udf7T                      = mempty
  ,  udf8T                      = mempty
  ,  udf9T                      = mempty
  ,  udf10T                     = mempty
  ,  txn_idT                    = mempty
  ,  status_idT                 = mempty
  ,  statusT                    = mempty
  ,  payment_method_typeT       = mempty
  ,  auth_typeT                 = mempty
  ,  cardT                      = mempty
  ,  payment_methodT            = mempty
  ,  refundedT                  = mempty
  ,  amount_refundedT           = mempty
  ,  chargebacksT               = mempty
  ,  refundsT                   = mempty
  ,  mandateT                   = mempty
  ,  promotionT                 = mempty
  ,  riskT                      = mempty
  ,  bank_error_codeT           = mempty
  ,  bank_error_messageT        = mempty
  ,  txn_uuidT                  = mempty
  ,  gateway_payloadT           = mempty
  ,  txn_detailT                = mempty
  ,  payment_gateway_responseT' = mempty
  ,  payment_gateway_responseT  = mempty
  ,  gateway_idT                = mempty
  ,  emi_bankT                  = mempty
  ,  emi_tenureT                = mempty
  ,  gateway_reference_idT      = mempty
  ,  payer_vpaT                 = mempty
  ,  payer_app_nameT            = mempty
  ,  juspayT                    = mempty
  ,  second_factor_responseT    = mempty
  ,  txn_flow_infoT             = mempty
  }

instance Semigroup OrderStatusResponseTemp where
  (<>) = gsappenddefault

instance Monoid OrderStatusResponseTemp where
  mempty = defaultOrderStatusResponseTemp
  mappend = (<>)


-- from src/Externals/EC/Common.purs
data Card = Card
  {  expiry_year      :: Maybe Text
  ,  card_reference   :: Maybe Text
  ,  saved_to_locker  :: Maybe Bool
  ,  expiry_month     :: Maybe Text
  ,  name_on_card     :: Maybe Text
  ,  card_issuer      :: Maybe Text
  ,  last_four_digits :: Maybe Text
  ,  using_saved_card :: Maybe Bool
  ,  card_fingerprint :: Maybe Text
  ,  card_isin        :: Maybe Text
  ,  card_type        :: Maybe Text
  ,  card_brand       :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- from src/Externals/EC/Common.purs
data PaymentInfo = PaymentInfo
  {  payment_method_type :: Maybe Text
  ,  payment_method      :: Maybe Text
  ,  card                :: Maybe Card
  ,  auth_type           :: Maybe Text
  ,  authentication      :: Maybe Authentication
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- from src/Externals/EC/Common.purs
data Authentication = Authentication
  { second_factore_response :: Maybe SecondFactorResponse' }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- from src/Externals/EC/Common.purs
-- Prime added to differ from storage type
data SecondFactorResponse' = SecondFactorResponse'
  { cavv           :: Maybe Text
  , eci            :: Maybe Text
  , xid            :: Maybe Text
  , status         :: Maybe Text
  , currency       :: Maybe Text
  , response_id    :: Maybe Text
  , mpi_error_code :: Maybe Text
  , date_created   :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- from src/Types/Communication/OLTP/OrderStatus.purs
data Chargeback' = Chargeback'
  {  id                  :: Text
  ,  amount              :: Double
  ,  object_reference_id :: Text
  ,  txn                 :: TxnDetail'
  ,  date_resolved       :: Maybe LocalTime -- TODO: remove maybe?
  ,  date_created        :: LocalTime
  ,  last_updated        :: LocalTime
  ,  object              :: Text
  ,  dispute_status      :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mapChargeback :: TxnDetail' -> D.Chargeback -> Chargeback'
mapChargeback txn chargeback =
  Chargeback'
  {  id = chargebackId $ getField @"id" chargeback
  ,  amount = fromMoney $ getField @"amount" chargeback
  ,  object_reference_id = getField @"objectReferenceId" chargeback
  ,  txn = txn
  ,  date_resolved = getField @"dateResolved" chargeback
  ,  date_created = getField @"dateCreated" chargeback
  ,  last_updated = getField @"lastUpdated" chargeback
  ,  object = "chargeback"
  ,  dispute_status = getField @"disputeStatus" chargeback
  }


-- from src/Types/Communication/OLTP/OrderStatus.purs
data Refund' = Refund'
  {  id                    :: Text -- Foreign
  ,  amount                :: Double
  ,  unique_request_id     :: Text
  ,  ref                   :: Text -- Foreign
  ,  created               :: Text
  ,  status                :: RefundStatus -- Refund.RefundStatus
  ,  error_message         :: Text
  ,  sent_to_gateway       :: Bool
  ,  arn                   :: Text
  ,  initiated_by          :: Text
  ,  internal_reference_id :: Text
  ,  refund_source         :: Text -- Foreign
  ,  refund_type           :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


mapRefund :: D.Refund -> Refund'
mapRefund refund = Refund'
  {  id = blanked $ getField @"referenceId" refund
  ,  amount = fromMoney $ getField @"amount" refund
  ,  unique_request_id = blanked $ getField @"uniqueRequestId" refund
  ,  ref = blanked $ getField @"epgTxnId" refund
  ,  created = show $ getField @"dateCreated" refund -- TODO date format
  ,  status = getField @"status" refund --"" ORIG TODO // transform this
  ,  error_message = blanked $ getField @"errorMessage" refund
  ,  sent_to_gateway = D.getStatus refund
  ,  arn = blanked $ getField @"refundArn" refund
  ,  initiated_by = blanked $ getField @"initiatedBy" refund
  ,  internal_reference_id = D.getRefId refund
  ,  refund_source = blanked $ getField @"refundSource" refund
  ,  refund_type = blanked $ getField @"refundType" refund
  }
  where
    blanked = fromMaybe T.empty


-- from src/Types/Storage/EC/Mandate/Types.purs
data Mandate' = Mandate'
  { mandate_token  :: Text
  , mandate_status :: Maybe Text
  , mandate_id     :: Text
  , bank_details   :: Maybe EmandateDetail
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data EmandateDetail = EmandateDetail
  { id :: Maybe Text
  , bank_name :: Maybe Text
  , ifsc :: Text
  , account_number :: Text
  , beneficiary_name :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mapMandate :: D.Mandate -> Mandate'
mapMandate D.Mandate {..} = Mandate'
  { mandate_token = token
  , mandate_status = Just $ show $ status
  , mandate_id = mandateId
  }

-- from src/Types/Communication/OLTP/OrderStatus.purs
data Risk' = Risk'
  { provider            :: Maybe Text
  , status              :: Maybe Text
  , message             :: Maybe Text
  , flagged             :: Maybe Bool
  , recommended_action  :: Maybe Text
  , ebs_risk_level      :: Maybe Text
  , ebs_payment_status  :: Maybe Text
  , ebs_bin_country     :: Maybe Text
  , ebs_risk_percentage :: Maybe Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- from src/Types/Communication/OLTP/OrderStatus.purs
data Risk = Risk
  { provider            :: Maybe Text -- Foreign
  , status              :: Maybe Text -- Foreign
  , message             :: Maybe Text -- Foreign
  , flagged             :: Maybe Text -- Foreign
  , recommended_action  :: Maybe Text -- Foreign
  , ebs_risk_level      :: Maybe Text -- Foreign
  , ebs_payment_status  :: Maybe Text -- Foreign
  , ebs_bin_country     :: Maybe Text -- Foreign
  , ebs_risk_percentage :: Maybe Text -- Foreign
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- from src/Types/Communication/OLTP/OrderStatus.purs
data TxnDetail' = TxnDetail'
  { txn_id           :: Text
  , order_id         :: Text
  , txn_uuid         :: Maybe Text
  , gateway_id       :: Maybe Int
  , status           :: Text
  , gateway          :: Maybe Text
  , express_checkout :: Maybe Bool
  , redirect         :: Maybe Bool
  , net_amount       :: Maybe Text -- Foreign
  , surcharge_amount :: Maybe Text -- Foreign
  , tax_amount       :: Maybe Text -- Foreign
  , txn_amount       :: Maybe Text -- Foreign
  , currency         :: Maybe Text
  , error_message    :: Maybe Text
  , error_code       :: Maybe Text -- Foreign
  , txn_object_type  :: Maybe Text
  , source_object    :: Maybe Text
  , source_object_id :: Maybe Text
  , created          :: Maybe LocalTime
}
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
-- from src/Types/Communication/OLTP/OrderStatus.purs
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

data MerchantSecondFactorResponse = MerchantSecondFactorResponse
  {  cavv         :: Text -- Foreign with comment (nullable, so keeping it as Foreign to send it as null with key)
  ,  eci          :: Text
  ,  xid          :: Text
  ,  pares_status :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkMerchantSecondFactorResponse :: D.SecondFactorResponse -> MerchantSecondFactorResponse
mkMerchantSecondFactorResponse sfr = MerchantSecondFactorResponse
  { cavv = fromMaybe T.empty $ getField @"cavv" sfr
  , eci = getField @"eci" sfr
  , xid = getField @"xid" sfr
  , pares_status = getField @"status" sfr
  }

-- from src/Types/Storage/EC/Feature.purs
{-
newtype FeatureFE = FeatureFE (FeatureAll (id :: Int))
newtype Feature = Feature (FeatureAll (id :: NullOrUndefined Int))

type FeatureAll a =
  { version :: Int
  , enabled :: Boolean
  , name :: String
  , merchantId :: NullOrUndefined String
  , disabledUntil :: NullOrUndefined Date
  | a
  }
-}


mkTxnFlowInfo :: ViesGatewayAuthReqParams -> TxnFlowInfo
mkTxnFlowInfo params = TxnFlowInfo
  {  flow_type = maybe T.empty show $ getField @"flow" params
  ,  status = fromMaybe T.empty $ getField @"flowStatus" params
  ,  error_code = fromMaybe T.empty $ getField @"errorCode" params
  ,  error_message = fromMaybe T.empty $ getField @"errorMessage" params
  }

-- The only amount, address and UDF fields can be updated.
data OrderUpdateRequest = OrderUpdateRequest
  { amount                            :: Maybe Double

  , billing_address_first_name        :: Maybe Text
  , billing_address_last_name         :: Maybe Text
  , billing_address_line1             :: Maybe Text
  , billing_address_line2             :: Maybe Text
  , billing_address_line3             :: Maybe Text
  , billing_address_city              :: Maybe Text
  , billing_address_state             :: Maybe Text
  , billing_address_country           :: Maybe Text
  , billing_address_postal_code       :: Maybe Text
  , billing_address_phone             :: Maybe Text
  , billing_address_country_code_iso  :: Maybe Text

  , shipping_address_first_name       :: Maybe Text
  , shipping_address_last_name        :: Maybe Text
  , shipping_address_line1            :: Maybe Text
  , shipping_address_line2            :: Maybe Text
  , shipping_address_line3            :: Maybe Text
  , shipping_address_city             :: Maybe Text
  , shipping_address_state            :: Maybe Text
  , shipping_address_country          :: Maybe Text
  , shipping_address_postal_code      :: Maybe Text
  , shipping_address_phone            :: Maybe Text
  , shipping_address_country_code_iso :: Maybe Text

  , udf1                              :: Maybe Text
  , udf2                              :: Maybe Text
  , udf3                              :: Maybe Text
  , udf4                              :: Maybe Text
  , udf5                              :: Maybe Text
  , udf6                              :: Maybe Text
  , udf7                              :: Maybe Text
  , udf8                              :: Maybe Text
  , udf9                              :: Maybe Text
  , udf10                             :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)
-- from src/Types/Communication/OLTP/OrderStatus.purs
data TxnFlowInfo = TxnFlowInfo
  {  flow_type     :: Text -- Foreign
  ,  status        :: Text -- Foreign
  ,  error_code    :: Text -- Foreign
  ,  error_message :: Text -- Foreign
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data ViesFlow
  = VIES_ENROLLMENT
  | VIES_REPEAT
  | INVALID
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data ViesGatewayAuthReqParams = ViesGatewayAuthReqParams
  { viesReferenceId :: Maybe Text
  , viesInitError :: Maybe Text
  , errorCode :: Maybe Text
  , errorMessage :: Maybe Text
  , flowStatus :: Maybe Text
  , flow :: Maybe ViesFlow
  , crid :: Maybe Text
  , authFlow :: Maybe Text
  -- , errorDump :: Maybe Foreign
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
