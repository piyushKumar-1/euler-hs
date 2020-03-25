
module Euler.Product.OLTP.Services.TokenService where

import           EulerHS.Prelude hiding (id)
import           EulerHS.Language

-- EHS: rework imports. Use top level modules.
import qualified Euler.API.Order                   as API
import qualified Euler.Common.Types                as D
--import qualified Euler.Common.Types.External.Order as OEx
import qualified Euler.Common.Metric               as Metric
import qualified Euler.Config.Config               as Config
import qualified Euler.Config.ServiceConfiguration as SC
import           Euler.Lens
import qualified Euler.Product.Domain              as D
--import qualified Euler.Product.Domain.Order        as D
--import           Euler.Product.Domain.MerchantAccount
import           Euler.Product.OLTP.Services.RedisService

-- | Possible auth token resource types
data AuthTokenResourceType
  = ORDER
  | CUSTOMER
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

acquireOrderToken :: D.OrderPId -> D.MerchantId -> Flow API.OrderTokenResp
acquireOrderToken orderId merchantId = do
    tokenized <- tokenizeResource (SC.ResourceInt orderId) tag merchantId
    -- EHS: check this
    runIO $ Metric.incrementClientAuthTokenGeneratedCount merchantId
    pure $ conv tokenized
  where
    tag = show ORDER
    conv TokenizedResource {token, expiry} =
      API.OrderTokenResp
        { API.client_auth_token        = Just token
        , API.client_auth_token_expiry = Just expiry
        }

