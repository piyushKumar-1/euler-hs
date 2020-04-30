module Euler.Storage.Repository.MerchantAccount
  ( loadMerchantById
  , loadMerchantByMerchantId
  )
  where

import           EulerHS.Prelude hiding (id)

import qualified EulerHS.Extra.Validation as V hiding (transform)
import           EulerHS.Language
import           WebService.Language

import qualified Euler.Common.Errors.PredefinedErrors as Errs
import           Euler.Common.Types.Merchant (MerchantId)
import qualified Euler.Product.Domain.MerchantAccount as D
import           Euler.Storage.Repository.EulerDB
import qualified Euler.Storage.Types as DB
import qualified Euler.Storage.Validators.MerchantAccount as SV

import           Database.Beam ((==.))
import qualified Database.Beam as B
import           Euler.Lens


-- | Load an order by a surrogate ID value
loadMerchantById :: Int -> Flow (Maybe D.MerchantAccount)
loadMerchantById id' = do
  mbMerchantAccount <- withEulerDB $ do
    let predicate DB.MerchantAccount {id} =
          (id ==. B.just_ (B.val_ id'))
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.merchant_account DB.eulerDBSchema)
  transform mbMerchantAccount


loadMerchantByMerchantId :: MerchantId -> Flow (Maybe D.MerchantAccount)
loadMerchantByMerchantId merchId = do
  mbMerchantAccount <- withEulerDB $ do
    let predicate DB.MerchantAccount {merchantId} = merchantId ==. B.just_ (B.val_ merchId)
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.merchant_account DB.eulerDBSchema)
  transform mbMerchantAccount


transform :: Maybe DB.MerchantAccount -> Flow (Maybe D.MerchantAccount)
transform mbMerchantAccount = do
  case mbMerchantAccount of
    Nothing     -> pure Nothing
    Just merchantAccount -> do
      case SV.transSMaccToDomMacc merchantAccount of
        V.Success res -> pure $ Just res
        V.Failure e     -> do
          logErrorT "Incorrect merchant account in DB"
            $  " id: " <> (maybe "no data" show $ merchantAccount ^. _id) <> ", errors: " <> show e
          throwException Errs.internalError


