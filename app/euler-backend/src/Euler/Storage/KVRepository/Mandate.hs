module Euler.Storage.KVRepository.Mandate
  ( updateMandateCache
  , createMandate
  )
  where

import EulerHS.Prelude

import           EulerHS.Language

import qualified Euler.Config.Config                 as Config
import qualified Euler.Constants                     as Constants
import qualified Euler.Common.Types                  as D
import qualified Euler.Common.Types.External.Mandate as MEx
import qualified Euler.Product.Domain                as D
import qualified Euler.Storage.Types                 as DB

import           WebService.Language

import Euler.Lens

-- EHS: previously setMandateInCache
-- EHS: Seems mandate_max_amount should always be set irrespective the option mandate.
--      In the previous code, we're expecting max amount to be set even if mandate is DISABLED.
--      It's not clear whether this a valid behaviour (seems not).
-- EHS: There is no code reading for mandate from cache (lookup by "_mandate_data_" gives nothing).
--      Why this cache exist? What it does?
updateMandateCache :: D.Order -> D.OrderMandate -> Flow ()
updateMandateCache order mandate = case mandate of
  D.MandateRequired maxAmount        -> updateMandateCache' maxAmount
  D.MandateOptional (Just maxAmount) -> updateMandateCache' maxAmount
  _                                  -> pure ()

  where
    updateMandateCache' ma = do
      let merchantId = order ^. _merchantId
      let orderId    = order ^. _orderId
      mandate' <- createMandate order ma
      -- EHS: magic constant
      -- EHS: mandate in cache can be used in another backends (ps, groovy)
      void $ rSetex Constants.ecRedis (merchantId <> "_mandate_data_" <> orderId) mandate' Config.mandateTtl

createMandate :: D.Order -> Double -> Flow DB.Mandate
createMandate order' maxAmount = do
  mandateId   <- getShortUUID
  currentDate <- getCurrentTimeUTC
  token       <- getUUID32
  pure $ DB.Mandate
    { DB.id = Nothing
    , DB.merchantId = order' ^. _merchantId
    , DB.currency = Just $ order' ^. _currency
    , DB.endDate = Nothing
    , DB.startDate = Nothing
    , DB.maxAmount = Just maxAmount
    , DB.merchantCustomerId = order' ^. _customerId
    , DB.paymentMethod = Nothing
    , DB.paymentMethodType = Nothing
    , DB.paymentMethodId = Nothing
    , DB.gateway = Nothing
    , DB.gatewayParams = Nothing
    , DB.token = token
    , DB.mandateId = mandateId
    , DB.status = MEx.CREATED
    , DB.authOrderId = Just $ order' ^. _id
    , DB.activatedAt = Nothing
    , DB.dateCreated = currentDate
    , DB.lastModified = currentDate
    , DB.authTxnCardInfo = Nothing
    , DB.merchantGatewayAccountId = Nothing
    , DB.metadata = Nothing
    , DB.mandateType = Nothing
    }
