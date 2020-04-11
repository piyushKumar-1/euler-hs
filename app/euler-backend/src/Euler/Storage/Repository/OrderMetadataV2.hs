module Euler.Storage.Repository.OrderMetadataV2
  ( saveOrderMetadataV2
  , loadOrderMetadataV2
  )
  where

import EulerHS.Prelude

import           EulerHS.Language
import           WebService.Language
import           EulerHS.Extra.Validation

import           Euler.Storage.DBConfig
import           Euler.Storage.Repository.EulerDB

import           Euler.Common.Validators (notNegative)
import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Storage.Types                  as DB
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
      logError @Text "Incorrect OrderMetadataV2 in DB"
        $  "orderReference Id: " <> show ordRefId
        <> " error: " <> show e
      throwException Errs.internalError


transformOrderMetadataV2 :: DB.OrderMetadataV2 -> V D.OrderMetadataV2
transformOrderMetadataV2 r = D.OrderMetadataV2
  <$> (D.OrderMetadataV2PId <$> withField @"id" r (extractJust >=> notNegative))
  <*> withField @"browser" r pure
  <*> withField @"browserVersion" r pure
  <*> withField @"dateCreated" r pure
  <*> withField @"device" r pure
  <*> withField @"lastUpdated" r pure
  <*> withField @"metadata" r pure
  <*> withField @"mobile" r pure
  <*> withField @"operatingSystem" r pure
  <*> withField @"orderReferenceId" r notNegative
  <*> withField @"ipAddress" r pure
  <*> withField @"referer" r pure
  <*> withField @"userAgent" r pure
