module Euler.Services.Version.OrderTokenResp
  ( OrderTokenRespService(..)
  , mkOrderTokenRespService
  )
  where

import EulerHS.Prelude

import           EulerHS.Language
import           Euler.Product.OLTP.Services.TokenService

import qualified Euler.API.Order     as API
import qualified Euler.Common.Metric as Metric
import qualified Euler.Config.ServiceConfiguration as SC

type Version = Text
--
-- -- Euler.Product.Domain.Order
type OrderPId = Int
--
-- -- Euler.Common.Types.Merchant
type MerchantId = Text
--
--
data OrderTokenRespService = OrderTokenRespService
  { getToken :: OrderPId -> MerchantId -> Flow (Maybe API.OrderTokenResp)
  }
--
mkOrderTokenRespService :: Version -> OrderTokenRespService
mkOrderTokenRespService version = OrderTokenRespService
  { getToken = getToken' version}
--
getToken' :: Version -> OrderPId -> MerchantId -> Flow (Maybe API.OrderTokenResp)
getToken' version orderPId merchantId
  | version >= "2018-07-01" = Just <$> acquireOrderToken orderPId merchantId
  | otherwise =  pure Nothing
--