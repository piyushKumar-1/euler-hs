module Euler.Storage.Repository.MerchantIframePreferences
  ( loadMerchantPrefs
  )
  where

import EulerHS.Prelude

import           EulerHS.Extra.Validation
import           EulerHS.Language

import           Euler.Storage.DBConfig

import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Common.Types                   as C
import qualified Euler.Storage.Types                  as DB

import           Database.Beam ((==.))
import qualified Database.Beam as B
import           Euler.Lens

-- EHS: should be MerchantIframePreferences converted to domain type?
-- EHS: should we validate MerchantIframePreferences?
-- EHS: rework this function.
loadMerchantPrefs :: C.MerchantId -> Flow DB.MerchantIframePreferences
loadMerchantPrefs merchantId' = do
  res <- withDB eulerDB $ do
    let predicate DB.MerchantIframePreferences {merchantId} = merchantId ==. (B.val_ merchantId')
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.merchant_iframe_preferences DB.eulerDBSchema)

  case res of
    Just mIPrefs -> pure mIPrefs
    Nothing -> do
      logError "merchant_iframe_preferences" $ "Not found for merchant " <> merchantId'
      throwException Errs.internalError    -- EHS: error should be specified.