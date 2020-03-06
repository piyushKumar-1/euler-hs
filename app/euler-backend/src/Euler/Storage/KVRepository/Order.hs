module Euler.Storage.KVRepository.Order
  ( updateOrderCache
  )
  where

import EulerHS.Prelude

import           EulerHS.Language
import qualified Euler.Common.Types  as C
import qualified Euler.Config.Config as Config
import qualified Euler.Storage.Types as DB


-- For compatibility with other backends, we should save types that we use together through Redis
-- EHS: There is no code reading for order from cache (lookup by "_orderid_" gives nothing).
--      Why this cache exist? What it does?
updateOrderCache :: C.OrderId -> C.MerchantId -> DB.OrderReference -> Flow ()
updateOrderCache orderId merchantId dbOrder = do
  -- EHS: magic constant.
  void $ rSetex (merchantId <> "_orderid_" <> orderId) dbOrder Config.orderTtl