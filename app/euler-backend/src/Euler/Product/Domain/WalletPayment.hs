{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.WalletPayment where

import EulerHS.Prelude

import qualified Euler.Product.Domain.PaymentMethod.Wallet as WPM
import qualified Euler.Product.Domain.PaymentMethod.WalletDirect as WDPM

data WalletPayment
  = WalletPayment
  { payment_method         :: WPM.WalletPaymentMethod

  -- , payment_method_type    :: PaymentMethodType   -- ^ it's CARD here
  -- , redirect_after_payment :: Bool                -- ^ These types are not about domain
  -- , format                 :: Text                -- ^ These types are not about domain
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data DirectWalletPayment
  = DirectWalletPayment
  { payment_method :: WDPM.WalletDirectPaymentMethod
  , direct_wallet_token :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)