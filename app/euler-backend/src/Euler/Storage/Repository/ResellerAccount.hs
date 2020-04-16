module Euler.Storage.Repository.ResellerAccount
  (loadReseller
  )
  where

import EulerHS.Prelude

import           EulerHS.Extra.Validation as V
import           EulerHS.Language
import           WebService.Language

import           Euler.Storage.DBConfig
import           Euler.Storage.Repository.EulerDB
import           Euler.Storage.Validators.ResellerAccount

import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Product.Domain.ResellerAccount as D
import qualified Euler.Storage.Types                  as DB

import           Database.Beam ((==.))
import qualified Database.Beam as B




-- EHS: previously handleReseller
-- EHS: return domain type for Reseller instead of DB type
loadReseller :: Maybe Text -> Flow (Maybe D.ResellerAccount)
loadReseller Nothing = pure Nothing
loadReseller (Just resellerId') = do
  mbResAcc <- withEulerDB $ do
    -- EHS: DB types should be qualified or explicitly named.
    let predicate DB.ResellerAccount {resellerId} = resellerId ==. B.val_ resellerId'
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.reseller_account DB.eulerDBSchema)
  case mbResAcc of
    Nothing -> pure Nothing
    Just racc -> case toDomResAcc racc of
      V.Success v -> pure $ Just v
      V.Failure e -> do
        logError @String "Incorrect reseller account in DB"
          $  " resellerId: " <> resellerId'
          <> " error: " <> show e
        throwException Errs.internalError
