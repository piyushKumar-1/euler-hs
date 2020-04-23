{-# LANGUAGE InstanceSigs #-}
module Euler.Constant.Feature
  ( -- * Types
    Feature(..)
    -- * Functions
  , showFeature
  , parseFeature
  ) where

import           EulerHS.Prelude
import           WebService.Relude

-- | Represents some kind of features
data Feature
  = EulerOrderStatusCaching      -- ^ EULER_ORDER_STATUS_CACHING
  | TxnStatusRecon               -- ^ TXN_STATUS_RECON
  | UpdateLastSynced             -- ^ UPDATE_LAST_SYNC
  | UseUdf2ForGatewayReferenceId -- ^ USE_UDF2_FOR_GATEWAY_REFERENCE_ID
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Converts FeatureName into screaming snake case
showFeature :: Feature -> Text
showFeature = \case
  EulerOrderStatusCaching      -> "EULER_ORDER_STATUS_CACHING"
  TxnStatusRecon               -> "TXN_STATUS_RECON"
  UpdateLastSynced             -> "UPDATE_LAST_SYNC"
  UseUdf2ForGatewayReferenceId -> "USE_UDF2_FOR_GATEWAY_REFERENCE_ID"

-- | Parse feature from text representation
parseFeature :: Text -> Maybe Feature
parseFeature = inverseMap showFeature