module Euler.Services.Version.Refund
  ( RefundService(..)
  , mkRefundService
  )
  where

import EulerHS.Prelude
import qualified EulerHS.Prelude as P

import           Data.Generics.Product.Fields

import Euler.API.Refund (Refund'(..))
import Euler.Common.Types.Refund as Refund

-- move to common types?
-- change to newtype?
type Version = Text

transformRefund :: Version -> Refund' -> Refund'
transformRefund version
  = setInitiatedBy' version
  . setType' version
  . setSource' version
  . setStatus' version

transformRefunds' :: Version -> [Refund'] -> [Refund']
transformRefunds' version rfs = transformRefund version <$> filterRefunds' version rfs

data RefundService = RefundService
  { transformRefunds :: [Refund'] -> [Refund']
  }


mkRefundService :: Version -> RefundService
mkRefundService version = RefundService
  { transformRefunds = transformRefunds' version
  }


setInitiatedBy' :: Version -> Refund' -> Refund'
setInitiatedBy' version
  | version < "2018-09-20" || version == "" = setField @"initiated_by" Nothing -- ""
  | otherwise = P.id

setType' :: Version -> Refund' -> Refund'
setType' version
  | version < "2019-03-12" || version == "" = setField @"refund_type" Nothing -- ""
  | otherwise = P.id

setSource' :: Version -> Refund' -> Refund'
setSource' version
  | version < "2019-03-12" || version == "" = setField @"refund_source" Nothing -- ""
  | otherwise = P.id

setStatus' :: Version -> Refund' -> Refund'
setStatus' version refund
  | version < "2015-01-09" || version == "" = setField @"status" Refund.SUCCESS refund
  | version  < "2017-03-16" && status == MANUAL_REVIEW =
      setField @"status" Refund.PENDING refund
  | otherwise = refund
  where
    status = getField @"status" refund

filterRefunds' :: Version -> [Refund'] -> [Refund']
filterRefunds' version
  | version < "2015-08-18"  || version == "" =
    filter (\refund -> getField @"status" refund /= Refund.FAILURE)
  | otherwise = P.id