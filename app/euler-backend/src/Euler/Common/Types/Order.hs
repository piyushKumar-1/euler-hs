{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Euler.Common.Types.Order where

import EulerHS.Prelude

import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Sqlite
import Database.Beam.MySQL
import Web.FormUrlEncoded
import Web.Internal.HttpApiData

import qualified Data.Text as T (pack, unpack)
import qualified Prelude as P (show)
import qualified Text.Read as TR (readEither)

type OrderId = Text

data OrderType
  = MANDATE_REGISTER
  | MANDATE_PAYMENT
  | ORDER_PAYMENT
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be OrderType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres OrderType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow Sqlite OrderType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow MySQL OrderType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

-- EHS: too generic names for domain data type (NEW, SUCCESS etc.)
-- Should be reworked.
data OrderStatus
  = NEW
  | SUCCESS
  | NOT_FOUND
  | ERROR
  | JUSPAY_DECLINED
  | PENDING_AUTHENTICATION
  | AUTHENTICATION_FAILED
  | AUTHORIZATION_FAILED
  | AUTHORIZING
  | AUTHORIZED
  | CREATED
  | COD_INITIATED
  | VOIDED
  | VOID_INITIATED
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

 -- :set -XUndecidableInstances

instance HasSqlValueSyntax be String => HasSqlValueSyntax be OrderStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres OrderStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow Sqlite OrderStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow MySQL OrderStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow


orderStatusToInt :: OrderStatus -> Int
orderStatusToInt CREATED = 1
orderStatusToInt NEW = 10
orderStatusToInt SUCCESS = 30
orderStatusToInt NOT_FOUND = 40
orderStatusToInt ERROR = -1
orderStatusToInt JUSPAY_DECLINED = 23
orderStatusToInt PENDING_AUTHENTICATION = 15
orderStatusToInt AUTHENTICATION_FAILED = 26
orderStatusToInt AUTHORIZATION_FAILED = 27
orderStatusToInt AUTHORIZING = 28
orderStatusToInt AUTHORIZED = 30
orderStatusToInt VOIDED = 31
orderStatusToInt VOID_INITIATED = 32
-- orderStatusToInt AUTHORIZATION_FAILURE = -1
orderStatusToInt COD_INITIATED = 29


-- from src/Types/Communication/OLTP/OrderStatus.purs
data ClientAuthTokenData = ClientAuthTokenData
  { resourceId    :: Text
  , resourceType  :: Text
  , tokenMaxUsage :: Int
  , source        :: Text
  , usageCount    :: Maybe Int
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

defaultClientAuthTokenData :: ClientAuthTokenData
defaultClientAuthTokenData = ClientAuthTokenData
  { resourceId    = "resourceId"  -- :: Text
  , resourceType  = "resourceType"  -- :: Text
  , tokenMaxUsage = 1  -- :: Int
  , source        = "source"  -- :: Text
  , usageCount    = Nothing  -- :: Maybe Int
  }

-- from src/Types/Communication/OLTP/Order.purs
data OrderTokenExpiryData = OrderTokenExpiryData
  { expiryInSeconds       :: Int
  , tokenMaxUsage         :: Int
  , orderToken            :: Maybe Text
  , currentDateWithExpiry :: Maybe Text
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

defaultOrderTokenExpiryData :: OrderTokenExpiryData
defaultOrderTokenExpiryData = OrderTokenExpiryData
  { expiryInSeconds       = 0 -- :: Int
  , tokenMaxUsage         = 0 -- :: Int
  , orderToken            = Nothing -- :: Maybe Text
  , currentDateWithExpiry = Nothing -- :: Maybe Text
  }

-- EHS: use Money.
-- EHS: use newtypes.
type MandateMaxAmount = Double

-- EHS: domain types.
-- For API & DB types use Types/Mandate/MandateFeature.
data OrderMandate
  = MandateDisabled
  | MandateRequired MandateMaxAmount
  | MandateOptional MandateMaxAmount
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)


data UDF = UDF
  { udf1   :: Maybe Text
  , udf2   :: Maybe Text
  , udf3   :: Maybe Text
  , udf4   :: Maybe Text
  , udf5   :: Maybe Text
  , udf6   :: Maybe Text
  , udf7   :: Maybe Text
  , udf8   :: Maybe Text
  , udf9   :: Maybe Text
  , udf10  :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

emptyUDF :: UDF
emptyUDF = UDF
 { udf1  = Nothing
 , udf2  = Nothing
 , udf3  = Nothing
 , udf4  = Nothing
 , udf5  = Nothing
 , udf6  = Nothing
 , udf7  = Nothing
 , udf8  = Nothing
 , udf9  = Nothing
 , udf10 = Nothing
 }
