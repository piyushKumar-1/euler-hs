{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Euler.Common.Types.Order where

import EulerHS.Prelude

import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Sqlite
import Database.Beam.MySQL
import Web.FormUrlEncoded

import qualified Data.Text as T (unpack)

import qualified Euler.Common.Types.External.Order   as OEx
import qualified Euler.Common.Types.External.Mandate as MEx

type OrderId = Text
type OrderPId = Int

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
  = OrderStatusNew
  | OrderStatusSuccess
  | OrderStatusNotFound
  | OrderStatusError
  | OrderStatusJuspayDeclined
  | OrderStatusPendingAuthentication
  | OrderStatusAuthenticationFailed
  | OrderStatusAuthorizationFailed
  | OrderStatusAuthorizing
  | OrderStatusAuthorized
  | OrderStatusCreated
  | OrderStatusCodInitiated
  | OrderStatusVoided
  | OrderStatusVoidInitiated
  | OrderStatusCaptureInitiated
  | OrderStatusCaptureFailed
  | OrderStatusVoidFailed
  | OrderStatusAutoRefunded
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

toOrderStatusEx :: OrderStatus -> OEx.OrderStatus
toOrderStatusEx OrderStatusNew                   = OEx.NEW
toOrderStatusEx OrderStatusSuccess               = OEx.SUCCESS
toOrderStatusEx OrderStatusNotFound              = OEx.NOT_FOUND
toOrderStatusEx OrderStatusError                 = OEx.ERROR
toOrderStatusEx OrderStatusJuspayDeclined        = OEx.JUSPAY_DECLINED
toOrderStatusEx OrderStatusPendingAuthentication = OEx.PENDING_AUTHENTICATION
toOrderStatusEx OrderStatusAuthenticationFailed  = OEx.AUTHENTICATION_FAILED
toOrderStatusEx OrderStatusAuthorizationFailed   = OEx.AUTHORIZATION_FAILED
toOrderStatusEx OrderStatusAuthorizing           = OEx.AUTHORIZING
toOrderStatusEx OrderStatusAuthorized            = OEx.AUTHORIZED
toOrderStatusEx OrderStatusCreated               = OEx.CREATED
toOrderStatusEx OrderStatusCodInitiated          = OEx.COD_INITIATED
toOrderStatusEx OrderStatusVoided                = OEx.VOIDED
toOrderStatusEx OrderStatusVoidInitiated         = OEx.VOID_INITIATED
toOrderStatusEx OrderStatusCaptureInitiated      = OEx.CAPTURE_INITIATED
toOrderStatusEx OrderStatusCaptureFailed         = OEx.CAPTURE_FAILED
toOrderStatusEx OrderStatusVoidFailed            = OEx.VOID_FAILED
toOrderStatusEx OrderStatusAutoRefunded          = OEx.AUTO_REFUNDED

fromOrderStatusEx :: OEx.OrderStatus -> OrderStatus
fromOrderStatusEx OEx.NEW                    = OrderStatusNew
fromOrderStatusEx OEx.SUCCESS                = OrderStatusSuccess
fromOrderStatusEx OEx.NOT_FOUND              = OrderStatusNotFound
fromOrderStatusEx OEx.ERROR                  = OrderStatusError
fromOrderStatusEx OEx.JUSPAY_DECLINED        = OrderStatusJuspayDeclined
fromOrderStatusEx OEx.PENDING_AUTHENTICATION = OrderStatusPendingAuthentication
fromOrderStatusEx OEx.AUTHENTICATION_FAILED  = OrderStatusAuthenticationFailed
fromOrderStatusEx OEx.AUTHORIZATION_FAILED   = OrderStatusAuthorizationFailed
fromOrderStatusEx OEx.AUTHORIZING            = OrderStatusAuthorizing
fromOrderStatusEx OEx.AUTHORIZED             = OrderStatusAuthorized
fromOrderStatusEx OEx.CREATED                = OrderStatusCreated
fromOrderStatusEx OEx.COD_INITIATED          = OrderStatusCodInitiated
fromOrderStatusEx OEx.VOIDED                 = OrderStatusVoided
fromOrderStatusEx OEx.VOID_INITIATED         = OrderStatusVoidInitiated
fromOrderStatusEx OEx.CAPTURE_INITIATED      = OrderStatusCaptureInitiated
fromOrderStatusEx OEx.CAPTURE_FAILED         = OrderStatusCaptureFailed
fromOrderStatusEx OEx.VOID_FAILED            = OrderStatusVoidFailed
fromOrderStatusEx OEx.AUTO_REFUNDED          = OrderStatusAutoRefunded


toMandateEx :: OrderMandate -> MEx.MandateFeature
toMandateEx  MandateDisabled    = MEx.DISABLED
toMandateEx (MandateRequired _) = MEx.REQUIRED
toMandateEx (MandateOptional _) = MEx.OPTIONAL
toMandateEx MandateReqUndefined = MEx.REQUIRED
toMandateEx MandateOptUndefined = MEx.OPTIONAL

fromMandateEx :: MEx.MandateFeature -> OrderMandate
fromMandateEx MEx.DISABLED = MandateDisabled
fromMandateEx MEx.REQUIRED = MandateReqUndefined
fromMandateEx MEx.OPTIONAL = MandateOptUndefined

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
  | MandateReqUndefined               -- EHS: these two options exist because the flaw of DB table design (lack of max amount)
  | MandateOptUndefined               -- EHS: these two options exist because the flaw of DB table design (lack of max amount)
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)


getOrderType :: OrderMandate -> OrderType
getOrderType (MandateRequired _)   = MANDATE_REGISTER
getOrderType MandateReqUndefined   = MANDATE_REGISTER
getOrderType _                     = ORDER_PAYMENT


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
