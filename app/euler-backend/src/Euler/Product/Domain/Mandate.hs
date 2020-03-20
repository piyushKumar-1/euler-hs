{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Mandate where


import           EulerHS.Prelude

import           Euler.Common.Types.Currency (Currency)
import qualified Euler.Common.Types.External.Mandate as M
import           Euler.Common.Types.Gateway
import           Euler.Common.Types.Money

import           Data.Time



newtype MandatePId = MandatePId
  { mandatePId :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Mandate = Mandate
  {  id                       :: MandatePId
  ,  merchantId               :: Text
  ,  endDate                  :: Maybe LocalTime
  ,  startDate                :: Maybe LocalTime
  ,  maxAmount                :: Maybe Money
  ,  merchantCustomerId       :: Maybe Text
  ,  paymentMethod            :: Maybe Text
  ,  paymentMethodType        :: Maybe M.PaymentMethodType
  ,  status                   :: M.MandateStatus
  ,  token                    :: Text
  ,  mandateId                :: Text
  ,  paymentMethodId          :: Maybe Text
  ,  gateway                  :: Maybe Gateway
  ,  gatewayParams            :: Maybe Text
  ,  authOrderId              :: Maybe Int
  ,  activatedAt              :: Maybe LocalTime
  ,  dateCreated              :: LocalTime
  ,  lastModified             :: LocalTime
  ,  authTxnCardInfo          :: Maybe Text
  ,  currency                 :: Maybe Currency
  ,  merchantGatewayAccountId :: Maybe Int
  ,  metadata                 :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)



