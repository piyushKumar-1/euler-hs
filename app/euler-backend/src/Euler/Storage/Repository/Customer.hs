module Euler.Storage.Repository.Customer
  ( loadCustomer
  , findCustomerById
  )
  where


import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           Euler.Lens

import           Euler.Storage.DBConfig
import           Euler.Storage.Repository.EulerDB

import           Euler.Common.Errors.PredefinedErrors
import qualified Euler.Common.Types                   as C
import           Euler.Common.Validators
import qualified Euler.Product.Domain                 as D
import qualified Euler.Product.Domain.Templates       as Ts
import qualified Euler.Storage.Types                  as DB

import           Database.Beam ((==.), (||.), (&&.))
import qualified Database.Beam as B


loadCustomer :: Maybe C.CustomerId -> D.MerchantAccountId -> Flow (Maybe Ts.CustomerTemplate)
loadCustomer Nothing _ = pure Nothing
loadCustomer (Just customerId) mAccntId = do

    -- EHS: after refactoring, we query DB every time when customerId is available.
    -- EHS: DB type Customer should be explicit or qualified
    mbCustomer :: Maybe DB.Customer <- withEulerDB $ do
      let predicate DB.Customer {merchantAccountId, id, objectReferenceId}
            = (   id ==.  (B.val_ (Just customerId))
              ||. objectReferenceId ==. (B.val_ (Just customerId))  -- EHS: objectReferenceId is customerId ?
                                                             -- A: in customer DB table "objectReferenceId" - id from merchant
                                                             -- (how merchant identifies customer in their DB eg. by email or mobile number )
                                                             -- and "id" - our id. So merchant can use both
              )
              &&. (merchantAccountId ==. (B.val_ mAccntId))

      findRow
        $ B.select
        $ B.filter_ predicate
        $ B.all_ (DB.customer DB.eulerDBSchema)
-- EHS: create validation?
    pure $ case mbCustomer of
      Just (DB.Customer {..}) -> Just $ Ts.CustomerTemplate
        customerId
        firstName
        lastName
        emailAddress
        -- mobileCountryCode
        mobileNumber
      Nothing              -> Nothing

findCustomerById :: C.CustomerId -> Flow (Maybe D.Customer)
findCustomerById cId = do
  mbCustomer :: Maybe DB.Customer <- withEulerDB $ do
        let predicate DB.Customer {id}
              = id ==.  B.just_ (B.val_ cId)
        findRow
          $ B.select
          $ B.filter_ predicate
          $ B.all_ (DB.customer DB.eulerDBSchema)
  case (traverse validator mbCustomer) of
    Success v -> pure v
    Failure e -> do
      logError @Text "findById"
        $ "Incorrect Customer in DB, id: " <> show cId <> " error: " <> show e
      throwException internalError

validator :: DB.Customer -> V D.Customer
validator v = D.Customer
  <$> withField @"id" v (extractJust >=> textNotEmpty)
  <*> withField @"version" v pure
  <*> withField @"dateCreated" v pure
  <*> withField @"emailAddress" v (extractJust >=> textNotEmpty)
  <*> withField @"firstName" v (extractJust >=> textNotEmpty)
  <*> withField @"lastName" v (extractJust >=> textNotEmpty)
  <*> withField @"lastUpdated" v pure
  <*> withField @"merchantAccountId" v pure
  <*> withField @"mobileCountryCode" v pure
  <*> withField @"mobileNumber" v pure
  <*> withField @"objectReferenceId" v (extractJust >=> textNotEmpty)
