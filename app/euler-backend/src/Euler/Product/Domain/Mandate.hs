{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Mandate where


import           EulerHS.Prelude

import           Euler.Common.Types.Gateway
import qualified Euler.Common.Types.Mandate as M
import           Euler.Product.Domain.Money

-- import           Data.Generics.Product.Fields
import           Data.Time



newtype MandateId = MandateId
  { mandateId :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Mandate = Mandate
  {  id                       :: MandateId
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
  ,  authOrderId              :: Maybe Text
  ,  activatedAt              :: Maybe LocalTime
  ,  dateCreated              :: LocalTime
  ,  lastModified             :: LocalTime
  ,  authTxnCardInfo          :: Maybe Text
  ,  currency                 :: Maybe Text
  ,  merchantGatewayAccountId :: Maybe Int
  ,  metadata                 :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)



