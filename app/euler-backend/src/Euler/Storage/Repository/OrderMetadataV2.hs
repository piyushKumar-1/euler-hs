module Euler.Storage.Repository.OrderMetadataV2
  (saveOrderMetadataV2
  )
  where

import EulerHS.Prelude

import           EulerHS.Language
import           WebService.Language

import           Euler.Storage.DBConfig
import           Euler.Storage.Repository.EulerDB

import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Storage.Types                  as DB

import qualified Database.Beam as B
import           Euler.Lens


-- create/save OrderMetadataV2
saveOrderMetadataV2 :: DB.OrderMetadataV2 -> Flow DB.OrderMetadataV2
saveOrderMetadataV2 orderMetadataDBVal =
  unsafeInsertRowEulerDB (Errs.mkDBError "Inserting order metadata v2 failed.")
    $ B.insert (DB.order_metadata_v2 DB.eulerDBSchema)
    $ B.insertExpressions [ (B.val_ orderMetadataDBVal) & _id .~ B.default_ ]