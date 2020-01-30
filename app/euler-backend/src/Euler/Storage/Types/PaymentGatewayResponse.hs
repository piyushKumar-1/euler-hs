{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.PaymentGatewayResponse
  ( PaymentGatewayResponseT(..)
  , PaymentGatewayResponse
  -- , Id
  , paymentGatewayResponseEMod
  , defaultPaymentGatewayResponse
  , defaultPaymentGatewayResponse'
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import Euler.Common.Types.DefaultDate
import qualified Database.Beam as B


data PaymentGatewayResponseT f = PaymentGatewayResponse
  { id :: B.C f (Maybe Text)
  , version :: B.C f Int
  , bankCode :: B.C f (Maybe Text)
  , dateCreated :: B.C f (Maybe LocalTime)
  , responseXml :: B.C f (Maybe Text)
  , txnId :: B.C f (Maybe Text)
  , iciciRespCode :: B.C f (Maybe Text)
  , iciciRespMessage :: B.C f (Maybe Text)
  , axisRespCode :: B.C f (Maybe Text)
  , axisRespMessage :: B.C f (Maybe Text)
  , hdfcRespCode :: B.C f (Maybe Text)
  , hdfcRespMessage :: B.C f (Maybe Text)
  , respCode :: B.C f (Maybe Text)
  , respMessage :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table PaymentGatewayResponseT where
  data PrimaryKey PaymentGatewayResponseT f =
    Id (B.C f (Maybe Text)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

-- newtype PaymentGatewayInfo = PaymentGatewayInfo {
--       respCode    :: Maybe Text
--     , respMessage :: Maybe Text
--     , responseXml :: Maybe Text
-- }

type PaymentGatewayResponse = PaymentGatewayResponseT Identity
-- type Id = B.PrimaryKey PaymentGatewayResponseT Identity

deriving instance Show PaymentGatewayResponse
deriving instance Eq PaymentGatewayResponse
deriving instance ToJSON PaymentGatewayResponse
deriving instance FromJSON PaymentGatewayResponse
deriving instance Read PaymentGatewayResponse
deriving instance Ord PaymentGatewayResponse

paymentGatewayResponseEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity PaymentGatewayResponseT)
paymentGatewayResponseEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , bankCode = B.fieldNamed "bank_code"
    , dateCreated = B.fieldNamed "date_created"
    , responseXml = B.fieldNamed "response_xml"
    , txnId = B.fieldNamed "txn_id"
    , iciciRespCode = B.fieldNamed "icici_resp_code"
    , iciciRespMessage = B.fieldNamed "icici_resp_message"
    , axisRespCode = B.fieldNamed "axis_resp_code"
    , axisRespMessage = B.fieldNamed "axis_resp_message"
    , hdfcRespCode = B.fieldNamed "hdfc_resp_code"
    , hdfcRespMessage = B.fieldNamed "hdfc_resp_message"
    , respCode = B.fieldNamed "resp_code"
    , respMessage = B.fieldNamed "resp_message"
    }

defaultPaymentGatewayResponse :: PaymentGatewayResponse
defaultPaymentGatewayResponse = defaultPaymentGatewayResponse' Nothing

defaultPaymentGatewayResponse':: Maybe Text -> PaymentGatewayResponse
defaultPaymentGatewayResponse' id' = PaymentGatewayResponse
  { id =  id' -- Maybe Text
  , version = 0 -- Int
  , axisRespCode =  Nothing -- Maybe Text
  , axisRespMessage =  Nothing -- Maybe Text
  , dateCreated = Just defaultDate -- Maybe LocalTime
  , bankCode =  Nothing -- Maybe Text
  , hdfcRespCode =  Nothing -- Maybe Text
  , hdfcRespMessage =  Nothing -- Maybe Text
  , iciciRespCode =  Nothing -- Maybe Text
  , iciciRespMessage =  Nothing -- Maybe Text
  , respCode =  Nothing -- Maybe Text
  , respMessage =  Nothing -- Maybe Text
  , responseXml =  Nothing -- -- Maybe Text
  , txnId = Just "" -- Maybe Text
  }

