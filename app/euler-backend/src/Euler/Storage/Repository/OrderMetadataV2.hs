module Euler.Storage.Repository.OrderMetadataV2
  ( saveOrderMetadataV2
  , loadOrderMetadataV2
  )
  where

import EulerHS.Prelude

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           WebService.Language

import           Euler.Storage.DBConfig
import           Euler.Storage.Repository.EulerDB

import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Storage.Types                  as DB
import           Euler.Storage.Validators.OrderMetadataV2
import qualified Euler.Product.Domain as D
import           Euler.Lens

import qualified Database.Beam as B
import           Database.Beam ((==.))


-- create/save OrderMetadataV2
saveOrderMetadataV2 :: DB.OrderMetadataV2 -> Flow DB.OrderMetadataV2
saveOrderMetadataV2 orderMetadataDBVal =
  unsafeInsertRowEulerDB (Errs.mkDBError "Inserting order metadata v2 failed.")
    $ B.insert (DB.order_metadata_v2 DB.eulerDBSchema)
    $ B.insertExpressions [ (B.val_ orderMetadataDBVal) & _id .~ B.default_ ]


loadOrderMetadataV2 :: Int -> Flow (Maybe D.OrderMetadataV2)
loadOrderMetadataV2 ordRefId = do
  meta <- withDB eulerDB $ do
    let predicate DB.OrderMetadataV2 {orderReferenceId} =
          orderReferenceId ==. B.val_ ordRefId
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.order_metadata_v2 DB.eulerDBSchema)
  case traverse transformOrderMetadataV2 meta of
    Success pr -> pure pr
    Failure e -> do
      logErrorT "Incorrect OrderMetadataV2 in DB"
        $  "orderReference Id: " <> show ordRefId
        <> " error: " <> show e
      throwException Errs.internalError