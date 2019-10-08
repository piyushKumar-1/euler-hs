{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Transaction where

import EulerHS.Prelude

import qualified Euler.Product.Domain.CardPayment as CP
-- import qualified Euler.Product.Domain.NBPayment as NB
import qualified Euler.Product.Domain.NBPayment as NB
import qualified Euler.Product.Domain.WalletPayment as W
import qualified Euler.Product.Domain.UPIPayment as UPI

import Euler.Product.Domain.Types (PaymentMethodType, OrderId, MerchantId)

-- TODO: rework PaymentMethod / PaymentMethodType approach

-- Transaction
data TransactionType
  = CardTransaction CP.CardPayment
  | ATMSeamlessTransaction CP.ATMSeamlessPayment
  | ATMRedirectionTransaction CP.ATMRedirectionPayment
  | NBTransaction  NB.NBPayment
  | WalletTransaction W.WalletPayment
  | WalletDirectDebitTransaction W.DirectWalletPayment
  | UPITransaction UPI.UPIPayment
  -- TODO
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Transaction = Transaction
  { _order_id               :: OrderId                -- ^
  , _merchant_id            :: MerchantId                -- ^
  , _transaction_type       :: TransactionType

  -- , _payment_method_type    :: PaymentMethodType  -- Depends on transaction type

  , _redirect_after_payment :: Bool
  , _format                 :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


-- TODO: rework PaymentMethod / PaymentMethodType approach

class GetPaymentMethod a where
  getPaymentMethod :: Proxy a -> PaymentMethodType
