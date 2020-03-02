module Euler.Product.OLTP.Order.OrderVersioningService
  ( OrderVersioningService (..)
  , mkOrderVersioningService
  )
  where

import EulerHS.Prelude
import EulerHS.Language
import qualified EulerHS.Prelude as EHP

-- EHS: rework imports. Use top level modules.
import qualified Euler.API.Order                   as API
import qualified Euler.Common.Types                as D
import qualified Euler.Common.Types.External.Mandate as MEx
import qualified Euler.Common.Types.External.Order as OEx
import qualified Euler.Common.Metric               as Metric
import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Product.Domain.Order        as D
import qualified Euler.Product.Domain              as D
import           Euler.Product.Domain.MerchantAccount
import           Euler.Product.OLTP.Services.RedisService
import qualified Euler.Product.Domain.Templates    as Ts
import qualified Euler.Config.Config               as Config
import qualified Euler.Config.ServiceConfiguration as SC
import           Euler.Lens


data OrderVersioningService = OrderVersioningService
  { makeOrderResponse :: Config.Config
                      -> D.Order
                      -> MerchantAccount
                      -> Maybe D.ResellerAccount
                      -> Flow API.OrderCreateResponse
  }

type Version = Text

mkOrderVersioningService :: Maybe Version -> Maybe Bool -> OrderVersioningService
mkOrderVersioningService (Just version) (Just True)
  | version >= "2018-07-01"  = OrderVersioningService mkTokenizedOrderResponse
mkOrderVersioningService _ _ = OrderVersioningService mkOrderResponse

mkOrderResponse
  :: Config.Config
  -> D.Order
  -> MerchantAccount
  -> Maybe D.ResellerAccount
  -> Flow API.OrderCreateResponse
mkOrderResponse cfg order@(D.Order {..}) _ mbResellerAcc = do
  let (r :: API.OrderCreateResponse) = API.defaultOrderCreateResponse
        { API.status        = OEx.CREATED
        , API.status_id     = OEx.orderStatusToInt OEx.CREATED
        , API.id            = orderUuid
        , API.order_id      = orderId
        , API.payment_links = API.Paymentlinks
            -- EHS: magic constants
            { API.web    = Just url'
            , API.mobile = Just $ url' <> "?mobile=true"
            , API.iframe = Just url'
            }
        , API.return_url    = returnUrl
        , API.refunded      = Just refundedEntirely
        , API.product_id    = productId
        , API.merchant_id   = Just merchantId
        , API.date_created  = Just dateCreated
        , API.customer_phone = customerPhone
        , API.customer_id   = customerId
        , API.customer_email = customerEmail
        , API.currency      = Just $ show currency
        , API.amount_refunded = amountRefunded
        , API.amount = Just $ D.fromMoney amount
        , API.juspay = Nothing
        }
  pure r
  where
  -- EHS: magic constants
    mbResellerEndpoint = mbResellerAcc >>= (^. _resellerApiEndpoint)
    url = maybe (cfg ^. _protocol <> "://" <>  cfg ^. _host) EHP.id mbResellerEndpoint
    url' = url <> "/merchant/pay/" <> orderUuid

mkTokenizedOrderResponse
  :: Config.Config
  -> D.Order
  -> MerchantAccount
  -> Maybe D.ResellerAccount
  -> Flow API.OrderCreateResponse
mkTokenizedOrderResponse cfg order@(D.Order {..}) mAcc mbResellerAcc = do
  apiResp    <- mkOrderResponse cfg order mAcc mbResellerAcc
  orderToken <- acquireOrderToken (order ^. _id) merchantId
  let (r :: API.OrderCreateResponse) = apiResp
        { API.status          = OEx.NEW
        , API.status_id       = OEx.orderStatusToInt OEx.NEW      -- EHS: this logic should be tested.
        , API.juspay          = Just orderToken
        , API.udf1            = D.udf1  udf <|> Just ""   -- If UDF field not set, it will be Just ""
        , API.udf2            = D.udf2  udf <|> Just ""   -- If UDF field not set, it will be Just ""
        , API.udf3            = D.udf3  udf <|> Just ""   -- If UDF field not set, it will be Just ""
        , API.udf4            = D.udf4  udf <|> Just ""   -- If UDF field not set, it will be Just ""
        , API.udf5            = D.udf5  udf <|> Just ""   -- If UDF field not set, it will be Just ""
        , API.udf6            = D.udf6  udf <|> Just ""   -- If UDF field not set, it will be Just ""
        , API.udf7            = D.udf7  udf <|> Just ""   -- If UDF field not set, it will be Just ""
        , API.udf8            = D.udf8  udf <|> Just ""   -- If UDF field not set, it will be Just ""
        , API.udf9            = D.udf9  udf <|> Just ""   -- If UDF field not set, it will be Just ""
        , API.udf10           = D.udf10 udf <|> Just ""   -- If UDF field not set, it will be Just ""
        , API.return_url      = returnUrl <|> Just ""       -- If return_url not set, it will be Just ""
        , API.refunded        = Just refundedEntirely
        , API.product_id      = productId <|> Just ""       -- If productId not set, it will be Just ""
        , API.merchant_id     = Just merchantId
        , API.date_created    = Just dateCreated
        , API.customer_phone  = customerPhone <|> Just ""   -- If customerPhone not set, it will be Just ""
        , API.customer_id     = customerId <|> Just ""      -- If customerId not set, it will be Just ""
        , API.customer_email  = customerEmail <|> Just ""   -- If customerEmail not set, it will be Just ""
        , API.currency        = (Just $ show currency)
       -- , API.amount_refunded = amountRefunded <|> Just 0.0 -- EHS: where this shoud be taken from on order create??
        , API.amount_refunded = Just 0.0
        , API.amount          = Just (D.fromMoney amount)
        }
  pure r


acquireOrderToken :: D.OrderPId -> D.MerchantId -> Flow API.OrderTokenResp
acquireOrderToken orderPId merchantId = do
  -- EHS: magic constant
  TokenizedResource {token, expiry} <- tokenizeResource (SC.ResourceInt orderPId) "ORDER" merchantId

  -- EHS: check this
  runIO $ Metric.incrementClientAuthTokenGeneratedCount merchantId

  pure $ API.OrderTokenResp
    { API.client_auth_token        = Just token
    , API.client_auth_token_expiry = Just expiry
    }
