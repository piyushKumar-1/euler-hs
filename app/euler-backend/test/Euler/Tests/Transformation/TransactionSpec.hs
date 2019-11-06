module Euler.Tests.Transformation.TransactionSpec where

import           EulerHS.Prelude
import           Test.Hspec
import           EulerHS.Extra.Validation

import qualified Euler.API.Transaction as AT
import qualified Euler.API.Types as AT
import Euler.Product.Domain.CardPayment
import Euler.Product.Domain.UPIPayment
import Euler.Product.Domain.WalletPayment
import Euler.Product.Domain.NBPayment
import qualified Euler.Product.Domain.Transaction as DT
import qualified Euler.Product.Domain.Types as DT
import Euler.Product.Domain.PaymentMethod.Card
import Euler.Product.Domain.PaymentMethod.ATMCard
import Euler.Product.Domain.PaymentMethod.NB
import qualified Euler.Product.Domain.PaymentMethod.Wallet as W
import qualified Euler.Product.Domain.PaymentMethod.WalletDirect as WD
import Euler.Product.Domain.PaymentMethod.UPI
import Euler.API.Validators.Transaction
import Euler.Common.Types.Transaction (AuthType(..))
import Data.Validation


-- regular new CARD

apiRegularNewCardTxn = AT.Transaction
  { order_id               = "some_order_id"    -- :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            = "merchant_id" -- :: Text                -- ^
  , payment_method_type    = AT.CARD       -- :: PaymentMethodType   -- ^
  , payment_method         = Just "VISA"   -- :: Maybe Text
  , redirect_after_payment = True          -- :: Bool                -- ^
  , format                 = "format"      -- :: Text                -- ^

  , auth_type              = Just VISA_CHECKOUT       --Just "auth_type" -- :: Maybe Text
  , card_token             = Nothing       -- Just "card_token" -- :: Maybe Text
  , card_security_code     = Just "card_security_code" -- :: Maybe Text
  , card_number            = Just "1234567890"      -- :: Maybe Text
  , card_exp_month         = Just "card_exp_month"   -- :: Maybe Text
  , card_exp_year          = Just "card_exp_year"    -- :: Maybe Text
  , name_on_card           = Just "name_on_card" -- :: Maybe Text
  , save_to_locker         = Just True           -- :: Maybe Bool
  , is_emi                 = Just True           -- :: Maybe Bool
  , emi_bank               = Just "emi_bank"     -- :: Maybe Text
  , emi_tenure             = Just 5              -- :: Maybe Int
  , upi_vpa                = Nothing      -- :: Maybe Text
  , txn_type               = Nothing     -- :: Maybe Text
  , direct_wallet_token    = Nothing -- :: Maybe Text
  }

domainRegularNewCardTxn = DT.Transaction
  { _order_id = DT.OrderId "some_order_id"
  , _merchant_id = DT.MerchantId "merchant_id"
  , _transaction_type =
    DT.CardTransaction (CardPayment
      {payment_method = VISA
      , card_payment_type =
          NewCardPayment
            { card_number = "1234567890"
            , name_on_card = "name_on_card"
            , card_exp_month = "card_exp_month"
            , card_exp_year = "card_exp_year"
            , card_security_code = "card_security_code"
            , save_to_locker = True
            }
      , is_emi = Just True
      , emi_bank = Just "emi_bank"
      , emi_tenure = Just 5
      , auth_type = Just VISA_CHECKOUT
      })
  , _redirect_after_payment = True
  , _format = "format"}

-- regular saved CARD

apiRegularSavedCardTxn = AT.Transaction
  { order_id               = "some_order_id"    -- :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            = "merchant_id" -- :: Text                -- ^
  , payment_method_type    = AT.CARD       -- :: PaymentMethodType   -- ^
  , payment_method         = Just "VISA"   -- :: Maybe Text
  , redirect_after_payment = True          -- :: Bool                -- ^
  , format                 = "format"      -- :: Text                -- ^

  , auth_type              = Nothing       --Just "auth_type" -- :: Maybe Text
  , card_token             = Just "card_token"       -- Just "card_token" -- :: Maybe Text
  , card_security_code     = Just "card_security_code" -- :: Maybe Text
  , card_number            = Nothing      -- :: Maybe Text
  , card_exp_month         = Nothing   -- :: Maybe Text
  , card_exp_year          = Nothing    -- :: Maybe Text
  , name_on_card           = Nothing -- :: Maybe Text
  , save_to_locker         = Just True           -- :: Maybe Bool
  , is_emi                 = Nothing           -- :: Maybe Bool
  , emi_bank               = Nothing     -- :: Maybe Text
  , emi_tenure             = Nothing              -- :: Maybe Int
  , upi_vpa                = Nothing      -- :: Maybe Text
  , txn_type               = Nothing     -- :: Maybe Text
  , direct_wallet_token    = Nothing -- :: Maybe Text
  }

domainRegularSavedCardTxn = DT.Transaction
  { _order_id = DT.OrderId "some_order_id"
  , _merchant_id = DT.MerchantId "merchant_id"
  , _transaction_type = DT.CardTransaction (CardPayment
    { payment_method = VISA
    , card_payment_type = SavedCardPayment
      { card_token = "card_token"
      , card_security_code = "card_security_code"
      }
    , is_emi = Nothing
    , emi_bank = Nothing
    , emi_tenure = Nothing
    , auth_type = Nothing
    })
  , _redirect_after_payment = True
  , _format = "format"}


-- Seamless new CARD

apiSeamlessNewCardTxn = AT.Transaction
  { order_id               = "some_order_id"    -- :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            = "merchant_id" -- :: Text                -- ^
  , payment_method_type    = AT.CARD       -- :: PaymentMethodType   -- ^
  , payment_method         = Just "VISA"   -- :: Maybe Text
  , redirect_after_payment = True          -- :: Bool                -- ^
  , format                 = "format"      -- :: Text                -- ^

  , auth_type              = Just ATMPIN       --Just "auth_type" -- :: Maybe Text
  , card_token             = Nothing       -- Just "card_token" -- :: Maybe Text
  , card_security_code     = Just "card_security_code" -- :: Maybe Text
  , card_number            = Just "1234567890"      -- :: Maybe Text
  , card_exp_month         = Just "card_exp_month"   -- :: Maybe Text
  , card_exp_year          = Just "card_exp_year"    -- :: Maybe Text
  , name_on_card           = Just "name_on_card" -- :: Maybe Text
  , save_to_locker         = Just True           -- :: Maybe Bool
  , is_emi                 = Nothing           -- :: Maybe Bool
  , emi_bank               = Nothing     -- :: Maybe Text
  , emi_tenure             = Nothing              -- :: Maybe Int
  , upi_vpa                = Nothing      -- :: Maybe Text
  , txn_type               = Nothing     -- :: Maybe Text
  , direct_wallet_token    = Nothing -- :: Maybe Text
  }

domainSeamlessNewCardTxn = DT.Transaction
  { _order_id = DT.OrderId "some_order_id"
  , _merchant_id = DT.MerchantId "merchant_id"
  , _transaction_type =
    DT.ATMSeamlessTransaction (ATMSeamlessPayment
      { payment_method = VISA
      , card_payment_type =
          NewCardPayment
            { card_number = "1234567890"
            , name_on_card = "name_on_card"
            , card_exp_month = "card_exp_month"
            , card_exp_year = "card_exp_year"
            , card_security_code = "card_security_code"
            , save_to_locker = True
            }
      , auth_type = ATMPIN
      })
  , _redirect_after_payment = True
  , _format = "format"}

-- Seamless saved CARD

apiSeamlessSavedCardTxn = AT.Transaction
  { order_id               = "some_order_id"    -- :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            = "merchant_id" -- :: Text                -- ^
  , payment_method_type    = AT.CARD       -- :: PaymentMethodType   -- ^
  , payment_method         = Just "VISA"   -- :: Maybe Text
  , redirect_after_payment = True          -- :: Bool                -- ^
  , format                 = "format"      -- :: Text                -- ^

  , auth_type              = Just ATMPIN      --Just "auth_type" -- :: Maybe Text
  , card_token             = Just "card_token"       -- Just "card_token" -- :: Maybe Text
  , card_security_code     = Just "card_security_code" -- :: Maybe Text
  , card_number            = Nothing      -- :: Maybe Text
  , card_exp_month         = Nothing   -- :: Maybe Text
  , card_exp_year          = Nothing    -- :: Maybe Text
  , name_on_card           = Nothing -- :: Maybe Text
  , save_to_locker         = Just True           -- :: Maybe Bool
  , is_emi                 = Nothing           -- :: Maybe Bool
  , emi_bank               = Nothing     -- :: Maybe Text
  , emi_tenure             = Nothing              -- :: Maybe Int
  , upi_vpa                = Nothing      -- :: Maybe Text
  , txn_type               = Nothing     -- :: Maybe Text
  , direct_wallet_token    = Nothing -- :: Maybe Text
  }

domainSeamlessSavedCardTxn = DT.Transaction
  { _order_id = DT.OrderId "some_order_id"
  , _merchant_id = DT.MerchantId "merchant_id"
  , _transaction_type = DT.ATMSeamlessTransaction (ATMSeamlessPayment
    { payment_method = VISA
    , card_payment_type = SavedCardPayment
      { card_token = "card_token"
      , card_security_code = "card_security_code"
      }
    , auth_type = ATMPIN
    })
  , _redirect_after_payment = True
  , _format = "format"}


-- ATMRedirect CARD

apiATMRedirectCardTxn = AT.Transaction
  { order_id               = "some_order_id"    -- :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            = "merchant_id" -- :: Text                -- ^
  , payment_method_type    = AT.CARD       -- :: PaymentMethodType   -- ^
  , payment_method         = Just "ATM_CARD_BOB"   -- :: Maybe Text
  , redirect_after_payment = True          -- :: Bool                -- ^
  , format                 = "format"      -- :: Text                -- ^

  , auth_type              = Just ATMPIN      --Just "auth_type" -- :: Maybe Text
  , card_token             = Nothing       -- Just "card_token" -- :: Maybe Text
  , card_security_code     = Nothing -- :: Maybe Text
  , card_number            = Nothing      -- :: Maybe Text
  , card_exp_month         = Nothing   -- :: Maybe Text
  , card_exp_year          = Nothing    -- :: Maybe Text
  , name_on_card           = Nothing -- :: Maybe Text
  , save_to_locker         = Nothing           -- :: Maybe Bool
  , is_emi                 = Nothing           -- :: Maybe Bool
  , emi_bank               = Nothing     -- :: Maybe Text
  , emi_tenure             = Nothing              -- :: Maybe Int
  , upi_vpa                = Nothing      -- :: Maybe Text
  , txn_type               = Nothing     -- :: Maybe Text
  , direct_wallet_token    = Nothing -- :: Maybe Text
  }

domainATMRedirectCardTxn = DT.Transaction
  { _order_id = DT.OrderId "some_order_id"
  , _merchant_id = DT.MerchantId "merchant_id"
  , _transaction_type = DT.ATMRedirectionTransaction (ATMRedirectionPayment
    { payment_method = ATM_CARD_BOB
    , auth_type = ATMPIN
    })
  , _redirect_after_payment = True
  , _format = "format"}

--- UPI Collect

apiUpiCollectTxn = AT.Transaction
  { order_id               = "some_order_id"    -- :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            = "merchant_id" -- :: Text                -- ^
  , payment_method_type    = AT.UPI       -- :: PaymentMethodType   -- ^
  , payment_method         = Just "UPI"   -- :: Maybe Text
  , redirect_after_payment = True          -- :: Bool                -- ^
  , format                 = "format"      -- :: Text                -- ^

  , auth_type              = Nothing       --Just "auth_type" -- :: Maybe Text
  , card_token             = Nothing       -- Just "card_token" -- :: Maybe Text
  , card_security_code     = Nothing -- :: Maybe Text
  , card_number            = Nothing      -- :: Maybe Text
  , card_exp_month         = Nothing   -- :: Maybe Text
  , card_exp_year          = Nothing    -- :: Maybe Text
  , name_on_card           = Nothing -- :: Maybe Text
  , save_to_locker         = Nothing           -- :: Maybe Bool
  , is_emi                 = Nothing           -- :: Maybe Bool
  , emi_bank               = Nothing --Just "emi_bank"     -- :: Maybe Text
  , emi_tenure             = Nothing              -- :: Maybe Int
  , upi_vpa                = Just "upi_vpa"      -- :: Maybe Text
  , txn_type               = Just "UPI_COLLECT"     -- :: Maybe Text
  , direct_wallet_token    = Nothing -- :: Maybe Text
  }

domainUpiCollectTxn = DT.Transaction
  { _order_id = DT.OrderId "some_order_id"
  , _merchant_id = DT.MerchantId "merchant_id"
  , _transaction_type = DT.UPITransaction (UPICollect
    { payment_method = UPI
    , txn_type = UPI_COLLECT
    , upi_vpa = "upi_vpa"
    })
  , _redirect_after_payment = True
  , _format = "format"
  }

--- UPI PAY

apiUpiPAYTxn = AT.Transaction
  { order_id               = "some_order_id"    -- :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            = "merchant_id" -- :: Text                -- ^
  , payment_method_type    = AT.UPI       -- :: PaymentMethodType   -- ^
  , payment_method         = Just "UPI"   -- :: Maybe Text
  , redirect_after_payment = True          -- :: Bool                -- ^
  , format                 = "format"      -- :: Text                -- ^

  , auth_type              = Nothing       --Just "auth_type" -- :: Maybe Text
  , card_token             = Nothing       -- Just "card_token" -- :: Maybe Text
  , card_security_code     = Nothing -- :: Maybe Text
  , card_number            = Nothing      -- :: Maybe Text
  , card_exp_month         = Nothing   -- :: Maybe Text
  , card_exp_year          = Nothing    -- :: Maybe Text
  , name_on_card           = Nothing -- :: Maybe Text
  , save_to_locker         = Nothing           -- :: Maybe Bool
  , is_emi                 = Nothing           -- :: Maybe Bool
  , emi_bank               = Nothing --Just "emi_bank"     -- :: Maybe Text
  , emi_tenure             = Nothing              -- :: Maybe Int
  , upi_vpa                = Nothing      -- :: Maybe Text
  , txn_type               = Just "UPI_PAY"     -- :: Maybe Text
  , direct_wallet_token    = Nothing -- :: Maybe Text
  }

domainUpiPAYTxn = DT.Transaction
  { _order_id = DT.OrderId "some_order_id"
  , _merchant_id = DT.MerchantId "merchant_id"
  , _transaction_type = DT.UPITransaction (UPIPay
    { payment_method = UPI
    , txn_type = UPI_PAY
    })
  , _redirect_after_payment = True
  , _format = "format"
  }

-- WALLET

apiWalletTxn = AT.Transaction
  { order_id               = "some_order_id"    -- :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            = "merchant_id" -- :: Text                -- ^
  , payment_method_type    = AT.WALLET       -- :: PaymentMethodType   -- ^
  , payment_method         = Just "PAYPAL"   -- :: Maybe Text
  , redirect_after_payment = True          -- :: Bool                -- ^
  , format                 = "format"      -- :: Text                -- ^

  , auth_type              = Nothing       --Just "auth_type" -- :: Maybe Text
  , card_token             = Nothing       -- Just "card_token" -- :: Maybe Text
  , card_security_code     = Nothing -- :: Maybe Text
  , card_number            = Nothing      -- :: Maybe Text
  , card_exp_month         = Nothing   -- :: Maybe Text
  , card_exp_year          = Nothing    -- :: Maybe Text
  , name_on_card           = Nothing -- :: Maybe Text
  , save_to_locker         = Nothing           -- :: Maybe Bool
  , is_emi                 = Nothing           -- :: Maybe Bool
  , emi_bank               = Nothing --Just "emi_bank"     -- :: Maybe Text
  , emi_tenure             = Nothing              -- :: Maybe Int
  , upi_vpa                = Nothing      -- :: Maybe Text
  , txn_type               = Nothing     -- :: Maybe Text
  , direct_wallet_token    = Nothing -- :: Maybe Text
  }

domainWalletTxn = DT.Transaction
  { _order_id = DT.OrderId "some_order_id"
  , _merchant_id = DT.MerchantId "merchant_id"
  , _transaction_type = DT.WalletTransaction (WalletPayment
     { payment_method = W.PAYPAL
     })
  , _redirect_after_payment = True
  , _format = "format"
  }

-- Direct WALLET

apiDirectWalletTxn = AT.Transaction
  { order_id               = "some_order_id"    -- :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            = "merchant_id" -- :: Text                -- ^
  , payment_method_type    = AT.WALLET       -- :: PaymentMethodType   -- ^
  , payment_method         = Just "MOBIKWIK"   -- :: Maybe Text
  , redirect_after_payment = True          -- :: Bool                -- ^
  , format                 = "format"      -- :: Text                -- ^

  , auth_type              = Nothing       --Just "auth_type" -- :: Maybe Text
  , card_token             = Nothing       -- Just "card_token" -- :: Maybe Text
  , card_security_code     = Nothing -- :: Maybe Text
  , card_number            = Nothing      -- :: Maybe Text
  , card_exp_month         = Nothing   -- :: Maybe Text
  , card_exp_year          = Nothing    -- :: Maybe Text
  , name_on_card           = Nothing -- :: Maybe Text
  , save_to_locker         = Nothing           -- :: Maybe Bool
  , is_emi                 = Nothing           -- :: Maybe Bool
  , emi_bank               = Nothing --Just "emi_bank"     -- :: Maybe Text
  , emi_tenure             = Nothing              -- :: Maybe Int
  , upi_vpa                = Nothing      -- :: Maybe Text
  , txn_type               = Nothing     -- :: Maybe Text
  , direct_wallet_token    = Just "direct_wallet_token" -- :: Maybe Text
  }

domainDirectWalletTxn = DT.Transaction
  { _order_id = DT.OrderId "some_order_id"
  , _merchant_id = DT.MerchantId "merchant_id"
  , _transaction_type = DT.WalletDirectDebitTransaction (DirectWalletPayment
     { payment_method = WD.MOBIKWIK
     , direct_wallet_token = "direct_wallet_token"
     })
  , _redirect_after_payment = True
  , _format = "format"
  }

-- Netbanking

apiNetbankingTxn = AT.Transaction
  { order_id               = "some_order_id"    -- :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            = "merchant_id" -- :: Text                -- ^
  , payment_method_type    = AT.NB       -- :: PaymentMethodType   -- ^
  , payment_method         = Just "NB_AXIS"   -- :: Maybe Text
  , redirect_after_payment = True          -- :: Bool                -- ^
  , format                 = "format"      -- :: Text                -- ^

  , auth_type              = Nothing       --Just "auth_type" -- :: Maybe Text
  , card_token             = Nothing       -- Just "card_token" -- :: Maybe Text
  , card_security_code     = Nothing -- :: Maybe Text
  , card_number            = Nothing      -- :: Maybe Text
  , card_exp_month         = Nothing   -- :: Maybe Text
  , card_exp_year          = Nothing    -- :: Maybe Text
  , name_on_card           = Nothing -- :: Maybe Text
  , save_to_locker         = Nothing           -- :: Maybe Bool
  , is_emi                 = Nothing           -- :: Maybe Bool
  , emi_bank               = Nothing --Just "emi_bank"     -- :: Maybe Text
  , emi_tenure             = Nothing              -- :: Maybe Int
  , upi_vpa                = Nothing      -- :: Maybe Text
  , txn_type               = Nothing     -- :: Maybe Text
  , direct_wallet_token    = Nothing -- :: Maybe Text
  }

domainNetbankingTxn = DT.Transaction
  { _order_id = DT.OrderId "some_order_id"
  , _merchant_id = DT.MerchantId "merchant_id"
  , _transaction_type = DT.NBTransaction (NBPayment
    { payment_method = NB_AXIS
    })
  , _redirect_after_payment = True
  , _format = "format"
  }

-- Netbanking empty text fields

apiNBTxnEmptyTextFields = AT.Transaction
  { order_id               = ""    -- :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            = "" -- :: Text                -- ^
  , payment_method_type    = AT.NB       -- :: PaymentMethodType   -- ^
  , payment_method         = Just "NB_AXIS"   -- :: Maybe Text
  , redirect_after_payment = True          -- :: Bool                -- ^
  , format                 = ""      -- :: Text                -- ^

  , auth_type              = Nothing       --Just "auth_type" -- :: Maybe Text
  , card_token             = Nothing       -- Just "card_token" -- :: Maybe Text
  , card_security_code     = Nothing -- :: Maybe Text
  , card_number            = Nothing      -- :: Maybe Text
  , card_exp_month         = Nothing   -- :: Maybe Text
  , card_exp_year          = Nothing    -- :: Maybe Text
  , name_on_card           = Nothing -- :: Maybe Text
  , save_to_locker         = Nothing           -- :: Maybe Bool
  , is_emi                 = Nothing           -- :: Maybe Bool
  , emi_bank               = Nothing --Just "emi_bank"     -- :: Maybe Text
  , emi_tenure             = Nothing              -- :: Maybe Int
  , upi_vpa                = Nothing      -- :: Maybe Text
  , txn_type               = Nothing     -- :: Maybe Text
  , direct_wallet_token    = Nothing -- :: Maybe Text
  }

-- Direct WALLET incorrect payment method

apiDirectWalletTxnIncorrectPM = AT.Transaction
  { order_id               = "some_order_id"    -- :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            = "merchant_id" -- :: Text                -- ^
  , payment_method_type    = AT.WALLET       -- :: PaymentMethodType   -- ^
  , payment_method         = Just "IncorrectPM"   -- :: Maybe Text
  , redirect_after_payment = True          -- :: Bool                -- ^
  , format                 = "format"      -- :: Text                -- ^

  , auth_type              = Nothing       --Just "auth_type" -- :: Maybe Text
  , card_token             = Nothing       -- Just "card_token" -- :: Maybe Text
  , card_security_code     = Nothing -- :: Maybe Text
  , card_number            = Nothing      -- :: Maybe Text
  , card_exp_month         = Nothing   -- :: Maybe Text
  , card_exp_year          = Nothing    -- :: Maybe Text
  , name_on_card           = Nothing -- :: Maybe Text
  , save_to_locker         = Nothing           -- :: Maybe Bool
  , is_emi                 = Nothing           -- :: Maybe Bool
  , emi_bank               = Nothing --Just "emi_bank"     -- :: Maybe Text
  , emi_tenure             = Nothing              -- :: Maybe Int
  , upi_vpa                = Nothing      -- :: Maybe Text
  , txn_type               = Nothing     -- :: Maybe Text
  , direct_wallet_token    = Just "direct_wallet_token" -- :: Maybe Text
  }


-- Direct WALLET missing mandatory field

apiDirectWalletTxnNoMandatoryField = AT.Transaction
  { order_id               = "some_order_id"    -- :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            = "merchant_id" -- :: Text                -- ^
  , payment_method_type    = AT.WALLET       -- :: PaymentMethodType   -- ^
  , payment_method         = Nothing   -- :: Maybe Text
  , redirect_after_payment = True          -- :: Bool                -- ^
  , format                 = "format"      -- :: Text                -- ^

  , auth_type              = Nothing       --Just "auth_type" -- :: Maybe Text
  , card_token             = Nothing       -- Just "card_token" -- :: Maybe Text
  , card_security_code     = Nothing -- :: Maybe Text
  , card_number            = Nothing      -- :: Maybe Text
  , card_exp_month         = Nothing   -- :: Maybe Text
  , card_exp_year          = Nothing    -- :: Maybe Text
  , name_on_card           = Nothing -- :: Maybe Text
  , save_to_locker         = Nothing           -- :: Maybe Bool
  , is_emi                 = Nothing           -- :: Maybe Bool
  , emi_bank               = Nothing --Just "emi_bank"     -- :: Maybe Text
  , emi_tenure             = Nothing              -- :: Maybe Int
  , upi_vpa                = Nothing      -- :: Maybe Text
  , txn_type               = Nothing     -- :: Maybe Text
  , direct_wallet_token    = Just "direct_wallet_token" -- :: Maybe Text
  }

-- regular new CARD

apiRegularNewCardTxnFewFailures = AT.Transaction
  { order_id               = "some_order_id"    -- :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            = "" -- :: Text                -- ^
  , payment_method_type    = AT.CARD       -- :: PaymentMethodType   -- ^
  , payment_method         = Just "MISsA"   -- :: Maybe Text
  , redirect_after_payment = True          -- :: Bool                -- ^
  , format                 = "format"      -- :: Text                -- ^

  , auth_type              = Just VISA_CHECKOUT       --Just "auth_type" -- :: Maybe Text
  , card_token             = Nothing       -- Just "card_token" -- :: Maybe Text
  , card_security_code     = Nothing -- :: Maybe Text
  , card_number            = Just ""      -- :: Maybe Text
  , card_exp_month         = Just "card_exp_month"   -- :: Maybe Text
  , card_exp_year          = Just "card_exp_year"    -- :: Maybe Text
  , name_on_card           = Just "name_on_card" -- :: Maybe Text
  , save_to_locker         = Just True           -- :: Maybe Bool
  , is_emi                 = Just True           -- :: Maybe Bool
  , emi_bank               = Just "emi_bank"     -- :: Maybe Text
  , emi_tenure             = Just 5              -- :: Maybe Int
  , upi_vpa                = Nothing      -- :: Maybe Text
  , txn_type               = Nothing     -- :: Maybe Text
  , direct_wallet_token    = Nothing -- :: Maybe Text
  }

spec :: Spec
spec =
  describe "Transaction transformations tests" $ do

    it "API Regular New Card Txn to Domain Regular New Card Txn" $  do
      let (res :: (Validation [Text] DT.Transaction)) = transform apiRegularNewCardTxn
      res `shouldBe` (Success domainRegularNewCardTxn)

    it "API Regular Saved Card Txn to Domain Regular Saved Card Txn" $  do
      let (res :: (Validation [Text] DT.Transaction)) = transform apiRegularSavedCardTxn
      res `shouldBe` (Success domainRegularSavedCardTxn)

    it "API Seamless New Card Txn to Domain Seamless New Card Txn" $  do
      let (res :: (Validation [Text] DT.Transaction)) = transform apiSeamlessNewCardTxn
      res `shouldBe` (Success domainSeamlessNewCardTxn)

    it "API Seamless Saved Card Txn to Domain Seamless Saved Card Txn" $  do
      let (res :: (Validation [Text] DT.Transaction)) = transform apiSeamlessSavedCardTxn
      res `shouldBe` (Success domainSeamlessSavedCardTxn)

    it "API ATM Redirect Card Txn to Domain ATM Redirect Card Txn" $  do
      let (res :: (Validation [Text] DT.Transaction)) = transform apiATMRedirectCardTxn
      res `shouldBe` (Success domainATMRedirectCardTxn)

    it "API UPI Collect Txn to Domain UPI Collect Txn" $  do
      let (res :: (Validation [Text] DT.Transaction)) = transform apiUpiCollectTxn
      res `shouldBe` (Success domainUpiCollectTxn)

    it "API UPI PAY Txn to Domain UPI PAY Txn" $  do
      let (res :: (Validation [Text] DT.Transaction)) = transform apiUpiPAYTxn
      res `shouldBe` (Success domainUpiPAYTxn)

    it "API Wallet Txn to Domain Wallet Txn" $  do
      let (res :: (Validation [Text] DT.Transaction)) = transform apiWalletTxn
      res `shouldBe` (Success domainWalletTxn)

    it "API Direct Wallet Txn to Domain Direct Wallet Txn" $  do
      let (res :: (Validation [Text] DT.Transaction)) = transform apiDirectWalletTxn
      res `shouldBe` (Success domainDirectWalletTxn)

    it "API Netbanking Txn to Domain Netbanking Txn" $  do
      let (res :: (Validation [Text] DT.Transaction)) = transform apiNetbankingTxn
      res `shouldBe` (Success domainNetbankingTxn)

    it "API Netbanking Txn fail with empty text fields" $  do
      let (res :: (Validation [Text] DT.Transaction)) = transform apiNBTxnEmptyTextFields
      res `shouldBe` (Failure ["order_id can't be empty","merchant_id can't be empty","format can't be empty"])

    it "API Direct Wallet Txn fail with incorrect PM" $  do
      let (res :: (Validation [Text] DT.Transaction)) = transform apiDirectWalletTxnIncorrectPM
      res `shouldBe` (Failure ["Can't decode IncorrectPM from field payment_method, should be one of [MOBIKWIK,PAYTM,FREECHARGE,OLAMONEY]"])

    it "API Netbanking Txn fail with missing mandatory field" $  do
      let (res :: (Validation [Text] DT.Transaction)) = transform apiDirectWalletTxnNoMandatoryField
      res `shouldBe` (Failure ["payment_method not present"])

    it "API Regular New Card Txn fail with few Failures" $  do
      let (res :: (Validation [Text] DT.Transaction)) = transform apiRegularNewCardTxnFewFailures
      res `shouldBe` (Failure ["merchant_id can't be empty","Can't decode MISsA from field payment_method, should be one of [VISA,MASTERCARD,MAESTRO,AMEX,RUPAY]","card_number can't be empty","card_security_code not present"])
