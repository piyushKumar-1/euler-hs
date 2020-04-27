{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Euler.Common.Types.Mandate
where

import EulerHS.Prelude
import Data.Data (Data)

import qualified Euler.Common.Types.External.Mandate as MEx

data MandateStatus
  = MandateStatusCreated
  | MandateStatusActive
  | MandateStatusPaused
  | MandateStatusRevoked
  | MandateStatusFailure
  | MandateStatusPending
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Data, Typeable)

fromMandateStatusEx :: MEx.MandateStatus -> MandateStatus
fromMandateStatusEx MEx.CREATED = MandateStatusCreated
fromMandateStatusEx MEx.ACTIVE  = MandateStatusActive
fromMandateStatusEx MEx.PAUSED  = MandateStatusPaused
fromMandateStatusEx MEx.REVOKED = MandateStatusRevoked
fromMandateStatusEx MEx.FAILURE = MandateStatusFailure
fromMandateStatusEx MEx.PENDING = MandateStatusPending

toMandateStatusEx :: MandateStatus -> MEx.MandateStatus
toMandateStatusEx MandateStatusCreated = MEx.CREATED
toMandateStatusEx MandateStatusActive  = MEx.ACTIVE
toMandateStatusEx MandateStatusPaused  = MEx.PAUSED
toMandateStatusEx MandateStatusRevoked = MEx.REVOKED
toMandateStatusEx MandateStatusFailure = MEx.FAILURE
toMandateStatusEx MandateStatusPending = MEx.PENDING
