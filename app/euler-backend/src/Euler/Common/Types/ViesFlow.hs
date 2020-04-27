{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Types.ViesFlow where

import EulerHS.Prelude

data ViesFlow
  = VIES_ENROLLMENT
  | VIES_REPEAT
  | INVALID
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data ViesGatewayAuthReqParams = ViesGatewayAuthReqParams
  { viesReferenceId :: Maybe Text
  , viesInitError :: Maybe Text
  , errorCode :: Maybe Text
  , errorMessage :: Maybe Text
  , flowStatus :: Maybe Text
  , flow :: Maybe ViesFlow
  , crid :: Maybe Text
  , authFlow :: Maybe Text
  -- , errorDump :: Maybe Foreign
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
