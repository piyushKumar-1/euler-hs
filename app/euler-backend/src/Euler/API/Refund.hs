{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Euler.API.Refund where

import           EulerHS.Prelude

-- import           Data.Aeson
-- import qualified Data.ByteString.Lazy as BSL
-- import           Data.Generics.Product.Fields
-- import qualified Data.HashMap.Strict as HM
-- import qualified Data.Map.Strict as Map
-- import           Data.Semigroup
import qualified Data.Text as T
import           Data.Generics.Product.Fields
-- import qualified Data.Text.Encoding as T
-- import           Data.Time
-- import           Generics.Deriving.Semigroup (gsappenddefault)
-- import           Web.FormUrlEncoded


import           Euler.Common.Types as C
-- import           Euler.Common.Types.External.Order (OrderStatus (..))

import           Euler.Product.Domain as D


-- from src/Types/Communication/OLTP/OrderStatus.purs
data Refund' = Refund'
  {  id                    :: Text -- Foreign
  ,  amount                :: Double
  ,  unique_request_id     :: Text
  ,  ref                   :: Text -- Foreign
  ,  created               :: Text
  ,  status                :: RefundStatus -- Refund.RefundStatus
  ,  error_message         :: Text
  ,  sent_to_gateway       :: Bool
  ,  arn                   :: Text
  ,  initiated_by          :: Text
  ,  internal_reference_id :: Text
  ,  refund_source         :: Text -- Foreign
  ,  refund_type           :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


mapRefund :: D.Refund -> Refund'
mapRefund refund = Refund'
  {  id = blanked $ getField @"referenceId" refund
  ,  amount = C.fromMoney $ getField @"amount" refund
  ,  unique_request_id = blanked $ getField @"uniqueRequestId" refund
  ,  ref = blanked $ getField @"epgTxnId" refund
  ,  created = show $ getField @"dateCreated" refund -- TODO date format
  ,  status = getField @"status" refund --"" ORIG TODO // transform this
  ,  error_message = blanked $ getField @"errorMessage" refund
  ,  sent_to_gateway = getStatus refund
  ,  arn = blanked $ getField @"refundArn" refund
  ,  initiated_by = blanked $ getField @"initiatedBy" refund
  ,  internal_reference_id = getRefId refund
  ,  refund_source = blanked $ getField @"refundSource" refund
  ,  refund_type = blanked $ getField @"refundType" refund
  }
  where
    blanked = fromMaybe T.empty

getStatus :: D.Refund -> Bool
getStatus refund = status == C.SUCCESS || processed || sentToGateway
  where
    status = getField @"status" refund
    processed = getField @"processed" refund
    sentToGateway = fromMaybe False (getField @"sentToGateway" refund)

getRefId :: D.Refund -> Text
getRefId refund
    | gateway == "HDFC" && sentToGateway = internalReferenceId
    | otherwise = mempty
  where
    gateway = getField @"gateway" refund
    sentToGateway = fromMaybe False (getField @"sentToGateway" refund)
    internalReferenceId = fromMaybe mempty $ getField @"internalReferenceId" refund
