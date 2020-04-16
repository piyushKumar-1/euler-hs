module Euler.Storage.Repository.PaymentGatewayResponse
       ( loadPGR
       ) where

import           EulerHS.Prelude hiding (id)

import qualified Prelude as P (show)

import           Euler.Storage.DBConfig
import           EulerHS.Language

import qualified Euler.Storage.Types                  as DB

import           Database.Beam ((==.))
import qualified Database.Beam as B



-- EHS: return domain type for PaymentGatewayResponse after it will be created and add validations
loadPGR :: Maybe Int -> Flow (Maybe DB.PaymentGatewayResponse)
loadPGR Nothing = pure Nothing
loadPGR (Just respId) =
  withDB eulerDB $ do
    let predicate DB.PaymentGatewayResponse {id} =
          id ==. B.just_ (B.val_ $ respId)
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.payment_gateway_response DB.eulerDBSchema)

