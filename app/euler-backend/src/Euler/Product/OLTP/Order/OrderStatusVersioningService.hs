module Euler.Product.OLTP.Order.OrderStatusVersioningService
  ( SHandle(..)
  , mkService
  ) where

import EulerHS.Prelude hiding (id)
import EulerHS.Language

-- EHS: rework imports. Use top level modules.
import qualified Euler.API.Order                   as API
import qualified Euler.Common.Types                as D
import qualified Euler.Common.Types.External.Order as OEx
import qualified Euler.Product.Domain.Order        as D
import qualified Euler.Product.Domain              as D
import           Euler.Product.Domain.MerchantAccount
import           Euler.Product.OLTP.Services.TokenService
import qualified Euler.Config.Config               as Config
import           Euler.Lens


newtype SHandle = SHandle
  { makeResponse :: API.OrderStatusResponse -> Flow API.OrderStatusResponse
  }


mkService :: (Maybe Version) -> TokenNeeded -> SHandle
mkService = undefined


-- EHS: common type/enumeration?
type Version = Text

type TokenNeeded = Bool