module Euler.Product.Domain.Repository.Refund where

import EulerHS.Prelude hiding (id)

import Euler.Common.Types.Refund (RefundStatus (..))
import Euler.Product.Domain.Refund

import Data.Generics.Product.Fields


getStatus :: Refund -> Bool
getStatus refund = status == SUCCESS || processed || sentToGateway
  where
    status = getField @"status" refund
    processed = getField @"processed" refund
    sentToGateway = fromMaybe False (getField @"sentToGateway" refund)

getRefId :: Refund -> Text
getRefId refund
    | gateway == "HDFC" && sentToGateway = internalReferenceId
    | otherwise = mempty
  where
    gateway = getField @"gateway" refund
    sentToGateway = fromMaybe False (getField @"sentToGateway" refund)
    internalReferenceId = fromMaybe mempty $ getField @"internalReferenceId" refund
