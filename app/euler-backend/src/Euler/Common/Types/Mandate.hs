{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Types.Mandate where

import EulerHS.Prelude
import Data.Time

import Euler.Common.Types.Gateway (Gateway)
import Euler.Common.Types.DefaultDate


-- from src/Types/Storage/EC/Mandate/Types.purs
data Mandate = Mandate
  {  id                       :: Maybe Int
  ,  merchantId               :: Text
  ,  endDate                  :: Maybe LocalTime
  ,  startDate                :: Maybe LocalTime
  ,  maxAmount                :: Maybe Double
  ,  merchantCustomerId       :: Maybe Text
  ,  paymentMethod            :: Maybe Text
  ,  paymentMethodType        :: Maybe PaymentMethodType
  ,  status                   :: MandateStatus
  ,  token                    :: Text
  -- TODO: Remove NullOrUndefined later
  ,  mandateId                :: Text
  ,  paymentMethodId          :: Maybe Text
  ,  gateway                  :: Maybe Gateway
  ,  gatewayParams            :: Maybe Text
  ,  authOrderId              :: Maybe Int
  ,  activatedAt              :: Maybe LocalTime
  ,  dateCreated              :: LocalTime
  ,  lastModified             :: LocalTime
  ,  authTxnCardInfo          :: Maybe Text
  ,  currency                 :: Maybe Text
  ,  merchantGatewayAccountId :: Maybe Int
  ,  metadata                 :: Maybe Text
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

defaultMandate :: Mandate
defaultMandate = Mandate
  {  id                        = Nothing -- :: Maybe Int
  ,  merchantId                = "" -- :: Text
  ,  endDate                   = Nothing -- :: Maybe LocalTime
  ,  startDate                 = Nothing -- :: Maybe LocalTime
  ,  maxAmount                 = Nothing -- :: Maybe Double
  ,  merchantCustomerId        = Nothing -- :: Maybe Text
  ,  paymentMethod             = Nothing -- :: Maybe Text
  ,  paymentMethodType         = Nothing -- :: Maybe PaymentMethodType
  ,  status                    = CREATED -- :: MandateStatus
  ,  token                     = "" -- :: Text
  ,  mandateId                 = "" -- :: Text
  ,  paymentMethodId           = Nothing -- :: Maybe Text
  ,  gateway                   = Nothing -- :: Maybe Gateway
  ,  gatewayParams             = Nothing -- :: Maybe Text
  ,  authOrderId               = Nothing -- :: Maybe Int
  ,  activatedAt               = Nothing -- :: Maybe LocalTime
  ,  dateCreated               = defaultDate -- :: LocalTime
  ,  lastModified              = defaultDate -- :: LocalTime
  ,  authTxnCardInfo           = Nothing -- :: Maybe Text
  ,  currency                  = Nothing -- :: Maybe Text
  ,  merchantGatewayAccountId  = Nothing -- :: Maybe Int
  ,  metadata                  = Nothing -- :: Maybe Text
  }


-- from src/Types/Storage/EC/Mandate/Types.purs
data PaymentMethodType = WALLET | UPI | NB | CARD | PAYLATER | CONSUMER_FINANCE | REWARD | CASH | UNKNOWN -- Foreign
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)
data MandateStatus = CREATED | ACTIVE | PAUSED | REVOKED | FAILURE | PENDING
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

-- from src/Types/Storage/EC/TxnDetail/Types.purs
