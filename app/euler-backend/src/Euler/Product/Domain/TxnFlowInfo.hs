{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.TxnFlowInfo where

import           EulerHS.Prelude

import qualified Euler.Common.Types as C


data TxnFlowInfo = TxnFlowInfo
  {  flowType     :: Maybe C.ViesFlow
  ,  status       :: Maybe Text
  ,  errorCode    :: Maybe Text
  ,  errorMessage :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
