module Euler.Version.Services.OrderCreateResponse
  ( OrderCreateResponseService
  , mkOrderCreateResponseService
  , tokenizeOrderCreateResponse
  )
  where

import EulerHS.Prelude

import           EulerHS.Language

import           Euler.Common.Types.Order
import           Euler.Version.Services.OrderTokenResp

import qualified Euler.API.Order                as API
import qualified Euler.Product.Domain.Templates as Ts

tokenizeOrderCreateResponse
  :: OrderCreateResponseService
  -> OrderPId
  -> MerchantId
  -> Ts.OrderCreateTemplate
  -> API.OrderCreateResponse
  -> Flow API.OrderCreateResponse
tokenizeOrderCreateResponse
  OrderCreateResponseService{..}
  orderPId
  merchantId
  orderCreateTemplate
  oResp = do
    token <- getToken tokenService orderPId merchantId
    pure $ updateResponse orderCreateTemplate oResp token

data OrderCreateResponseService = OrderCreateResponseService
  { tokenService :: OrderTokenRespService
  , updateResponse :: Ts.OrderCreateTemplate
                   -> API.OrderCreateResponse
                   -> OrderTokenResp
                   -> API.OrderCreateResponse
  }

mkOrderCreateResponseService :: Version -> OrderCreateResponseService
mkOrderCreateResponseService version = OrderCreateResponseService
  { tokenService = mkOrderTokenRespService version
  , updateResponse = updateResponse' version
  }

updateResponse' version
  | version >= "2018-07-01" = mkTokenizedOrderResponse
  | otherwise = \ _ orderCreateResp _ -> orderCreateResp

mkTokenizedOrderResponse
  :: Ts.OrderCreateTemplate
  -> API.OrderCreateResponse
  -> OrderTokenResp
  -> API.OrderCreateResponse
mkTokenizedOrderResponse Ts.OrderCreateTemplate{..} apiResp orderTokenResp = apiResp
    { API.status          = NEW
    , API.status_id       = orderStatusToInt NEW      -- EHS: this logic should be tested.
    , API.juspay          = Just orderTokenResp
    , API.udf1            = udf1 <|> Just ""
    , API.udf2            = udf2 <|> Just ""
    , API.udf3            = udf3 <|> Just ""
    , API.udf4            = udf4 <|> Just ""
    , API.udf5            = udf5 <|> Just ""
    , API.udf6            = udf6 <|> Just ""
    , API.udf7            = udf7 <|> Just ""
    , API.udf8            = udf8 <|> Just ""
    , API.udf9            = udf9 <|> Just ""
    , API.udf10           = udf10 <|> Just ""
    , API.return_url      = returnUrl <|> Just ""
    , API.refunded        = refundedEntirely <|> Just False
    , API.product_id      = productId <|> Just ""
    , API.merchant_id     = merchantId <|> Just ""
    , API.date_created    = Just $ dateCreated
    , API.customer_phone  = customerPhone <|> Just ""
    , API.customer_id     = customerId <|> Just ""
    , API.customer_email  = customerEmail <|> Just ""
    , API.currency        = currency <|> Just ""
    , API.amount_refunded = amountRefunded <|> Just 0.0
    , API.amount          = amount <|> Just 0.0
    }
