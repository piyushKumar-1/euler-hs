module Euler.Version.Handlers.Refund
  ( RefundHandler
  , mkRefundHandler
  , transformRefunds
  )
  where

import EulerHS.Prelude
import qualified EulerHS.Prelude as P

import           Data.Generics.Product.Fields

import Euler.API.Order (Refund'(..))
import Euler.Common.Types.Refund as Refund

-- move to common types?
-- change to newtype?
type Version = Text

transformRefund :: RefundHandler -> Refund' -> Refund'
transformRefund RefundHandler{..}
  = setInitiatedBy
  . setType
  . setSource
  . setStatus

transformRefunds :: RefundHandler -> [Refund'] -> [Refund']
transformRefunds rh@RefundHandler{..} rfs = transformRefund rh <$> filterRefunds rfs

data RefundHandler = RefundHandler
  { setInitiatedBy :: Refund' -> Refund'
  , setType        :: Refund' -> Refund'
  , setSource      :: Refund' -> Refund'
  , setStatus      :: Refund' -> Refund'
  , filterRefunds  :: [Refund'] -> [Refund']
  }


mkRefundHandler :: Version -> RefundHandler
mkRefundHandler version = RefundHandler
  { setInitiatedBy = setInitiatedBy' version
  , setType = setType' version
  , setSource = setSource' version
  , setStatus = setStatus' version
  , filterRefunds = filterRefunds' version
  }

setInitiatedBy' :: Version -> Refund' -> Refund'
setInitiatedBy' version
  | version < "2018-09-20" || version == "" = setField @"initiated_by" ""
  | otherwise = P.id

setType' :: Version -> Refund' -> Refund'
setType' version
  | version < "2019-03-12" || version == "" = setField @"refund_type" ""
  | otherwise = P.id

setSource' :: Version -> Refund' -> Refund'
setSource' version
  | version < "2019-03-12" || version == "" = setField @"refund_source" ""
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