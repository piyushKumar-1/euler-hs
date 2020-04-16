-----------------------------------------------------------------------------
-- |
-- Authentication service interface.

module Euler.Product.OLTP.Services.Auth.AuthService
  ( -- * Abstract handles
    SHandle(..)
    -- * Derived functions
  , withAuth
  ) where

import           EulerHS.Prelude                      hiding (id)

import           EulerHS.Language                     as L
import           WebService.Language

import qualified Euler.API.RouteParameters            as ARP
import qualified Euler.Common.Errors.PredefinedErrors as Errs
import           Euler.Lens
import qualified Euler.Product.Domain.MerchantAccount as DMA


newtype SHandle = SHandle
  { authenticate :: ARP.RouteParameters -> Flow (Either Text DMA.MerchantAccount)
  }

withAuth :: forall req resp .
  SHandle
  -> (ARP.RouteParameters -> req -> DMA.MerchantAccount -> Flow  resp)
  -> ARP.RouteParameters
  -> req
  -> Flow resp
withAuth handle action rps req = do
  res <- authenticate handle rps
  case res of
    Left err -> do
      logErrorT "AuthService" $ "authentication failed with error: " <> err
      -- EHS: refine error
      throwException Errs.internalError
    Right ma -> do
      logInfoT "AuthService" $ "authentication completed for merchant account id: " <> show (ma ^. _id)
      action rps req ma
