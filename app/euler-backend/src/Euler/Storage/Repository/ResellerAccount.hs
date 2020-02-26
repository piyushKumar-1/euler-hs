module Euler.Storage.Repository.ResellerAccount
  (loadReseller
  )
  where

import EulerHS.Prelude

import           EulerHS.Extra.Validation
import           EulerHS.Language

import           Euler.Storage.DBConfig

import qualified Euler.Storage.Types                  as DB

import           Database.Beam ((==.))
import qualified Database.Beam as B
import           Euler.Lens




-- EHS: previously handleReseller
-- EHS: return domain type for Reseller instead of DB type
loadReseller :: Maybe Text -> Flow (Maybe DB.ResellerAccount)
loadReseller Nothing = pure Nothing
loadReseller (Just resellerId') = withDB eulerDB $ do
    -- EHS: DB types should be qualified or explicitly named.
    let predicate DB.ResellerAccount {resellerId} = resellerId ==. B.val_ resellerId'
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.reseller_account DB.eulerDBSchema)