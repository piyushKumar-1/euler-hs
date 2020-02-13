module Euler.Version.Services.OrderTokenResp
  ( OrderTokenRespService
  , mkOrderTokenRespService
  )
  where

import EulerHS.Prelude

import           EulerHS.Language
import           Euler.Product.OLTP.Services.RedisService

import qualified Euler.API.Order     as API
import qualified Euler.Common.Metric as Metric


-- Euler.Product.Domain.Order
type OrderPId = Int

-- Euler.Common.Types.Merchant
type MerchantId = Text


data OrderTokenRespService = OrderTokenRespService
  { getToken :: OrderPId -> MerchantId -> Flow (Maybe API.OrderTokenResp)
  }

mkOrderTokenRespService :: Version -> OrderTokenRespService
mkOrderTokenRespService version = OrderTokenRespService
  { getToken = getToken' version}

getToken' :: Version -> OrderPId -> MerchantId -> Flow (Maybe API.OrderTokenResp)
getToken' version orderPId merchantId
  | version >= "2018-07-01" = Just <$> acquireOrderToken orderPId merchantId
  | otherwise = \ _ _ -> pure Nothing

acquireOrderToken :: OrderPId -> MerchantId -> Flow API.OrderTokenResp
acquireOrderToken orderPId merchantId = do
  -- EHS: magic constant
  TokenizedResource {token, expiry} <- tokenizeResource (SC.ResourceInt orderPId) "ORDER" merchantId

  -- EHS: check this
  runIO $ Metric.incrementClientAuthTokenGeneratedCount merchantId

  pure $ API.OrderTokenResp
    { API.client_auth_token        = Just token
    , API.client_auth_token_expiry = Just expiry
    }
