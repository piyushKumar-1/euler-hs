-----------------------------------------------------------------------------
-- |
-- Combined API key + token authentication service implementation.
-- Key has higher priority than token.

module Euler.Product.OLTP.Services.Auth.AuthKeyTokenService
  ( newHandle
  ) where

import           EulerHS.Prelude                                   hiding (id)

import           EulerHS.Language                                  as L

import qualified Euler.API.RouteParameters                         as RP
import qualified Euler.Product.Domain.MerchantAccount              as DM
import qualified Euler.Product.OLTP.Services.Auth.AuthService      as X

newHandle :: X.SHandle -> X.SHandle -> X.SHandle
newHandle keyHandle tokenHandle = X.SHandle
  {X.authenticate = authenticate keyHandle tokenHandle
  }

authenticate :: X.SHandle -> X.SHandle -> RP.RouteParameters -> Flow (Either Text DM.MerchantAccount)
authenticate keyH tokenH rps = do
  keyAuth <- X.authenticate keyH rps
  case keyAuth of
    Right _ -> return keyAuth
    Left e -> do
      logInfo @Text "AuthKeyTokenService" e
      tokenAuth <- X.authenticate tokenH rps
      return tokenAuth
