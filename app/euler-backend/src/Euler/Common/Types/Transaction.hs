{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Types.Transaction where

import EulerHS.Prelude

data AuthType 
  = ATMPIN
  | THREE_DS
  | OTP
  | VISA_CHECKOUT
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)