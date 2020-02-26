module Euler.Services.Version.OrderCreateResponse
  ( OrderCreateResponseService
  , mkOrderCreateResponseService
  , tokenizeOrderCreateResponse
  )
  where
--
import EulerHS.Prelude
--
import           EulerHS.Language
--
import           Euler.Common.Types.Order
import           Euler.Services.Version.OrderTokenResp
--
import qualified Euler.API.Order                as API
import qualified Euler.Common.Types as C
import qualified Euler.Common.Types.External.Order as OEx
import qualified Euler.Product.Domain as D
import qualified Euler.Product.Domain.Templates as Ts
--

type Version = Text

tokenizeOrderCreateResponse
  :: OrderCreateResponseService
  -> C.OrderPId
  -> C.MerchantId
  -> D.Order
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
--
data OrderCreateResponseService = OrderCreateResponseService
  { tokenService :: OrderTokenRespService
  , updateResponse :: D.Order --Ts.OrderCreateTemplate
                   -> API.OrderCreateResponse
                   -> Maybe API.OrderTokenResp
                   -> API.OrderCreateResponse
  }
--
mkOrderCreateResponseService :: Version -> OrderCreateResponseService
mkOrderCreateResponseService version = OrderCreateResponseService
  { tokenService = mkOrderTokenRespService version
  , updateResponse = updateResponse' version
  }
--
updateResponse' version
  | version >= "2018-07-01" = mkTokenizedOrderResponse
  | otherwise = \ _ orderCreateResp _ -> orderCreateResp
--
mkTokenizedOrderResponse
  :: D.Order --Ts.OrderCreateTemplate
  -> API.OrderCreateResponse
  -> Maybe API.OrderTokenResp
  -> API.OrderCreateResponse
mkTokenizedOrderResponse D.Order{..} apiResp orderTokenResp = apiResp
    { API.status          = OEx.NEW
    , API.status_id       = OEx.orderStatusToInt OEx.NEW      -- EHS: this logic should be tested.
    , API.juspay          = orderTokenResp
    , API.udf1            = (C.udf1  udf) <|> Just ""
    , API.udf2            = (C.udf2  udf) <|> Just ""
    , API.udf3            = (C.udf3  udf) <|> Just ""
    , API.udf4            = (C.udf4  udf) <|> Just ""
    , API.udf5            = (C.udf5  udf) <|> Just ""
    , API.udf6            = (C.udf6  udf) <|> Just ""
    , API.udf7            = (C.udf7  udf) <|> Just ""
    , API.udf8            = (C.udf8  udf) <|> Just ""
    , API.udf9            = (C.udf9  udf) <|> Just ""
    , API.udf10           = (C.udf10 udf) <|> Just ""
    , API.return_url      = returnUrl <|> Just ""
    , API.refunded        = Just refundedEntirely -- refundedEntirely <|> Just False
    , API.product_id      = productId <|> Just ""
    , API.merchant_id     = Just merchantId -- merchantId <|> Just ""
    , API.date_created    = Just $ dateCreated
    , API.customer_phone  = customerPhone <|> Just ""
    , API.customer_id     = customerId <|> Just ""
    , API.customer_email  = customerEmail <|> Just ""
    , API.currency        = (Just $ show currency) -- currency <|> Just ""
    , API.amount_refunded = amountRefunded <|> Just 0.0
    , API.amount          = Just (C.fromMoney amount) -- amount <|> Just 0.0
    }
