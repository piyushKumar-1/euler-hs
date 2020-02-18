{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.SecondFactor
  ( SecondFactorT (..)
  , SecondFactor
  -- , Id
  , secondFactorEMod
  , defaultSecondFactor
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import qualified Database.Beam as B

data SecondFactorT f = SecondFactor
  { id :: B.C f (Maybe Text)
  , version :: B.C f Int
  , otp :: B.C f (Maybe Text)
  , status :: B.C f Text
  , txnId :: B.C f Text
  , sfType :: B.C f Text -- type is reserved word
  , url :: B.C f (Maybe Text)
  , secondFactorResponse :: B.C f (Maybe Text)
  , dateCreated :: B.C f LocalTime
  , epgTxnId :: B.C f (Maybe Text)
  , lastUpdated :: B.C f LocalTime
  , txnDetailId :: B.C f (Maybe Text)
  , gatewayAuthReqParams :: B.C f (Maybe Text)
  , authenticationAccountId :: B.C f (Maybe Text)
  , canAcceptResponse :: B.C f (Maybe Bool)
  , challengesAttempted :: B.C f (Maybe Int)
  , responseAttempted :: B.C f (Maybe Int)
  }
  deriving (Generic, B.Beamable)

instance B.Table SecondFactorT where
  data PrimaryKey SecondFactorT f =
    Id (B.C f (Maybe Text)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SecondFactor = SecondFactorT Identity
-- type Id = B.PrimaryKey SecondFactorT Identity

deriving instance Show SecondFactor
deriving instance Eq SecondFactor
deriving instance ToJSON SecondFactor
deriving instance FromJSON SecondFactor
deriving instance Read SecondFactor
deriving instance Ord SecondFactor

secondFactorEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity SecondFactorT)
secondFactorEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , otp = B.fieldNamed "otp"
    , status = B.fieldNamed "status"
    , txnId = B.fieldNamed "txn_id"
    , sfType = B.fieldNamed "sf_type"
    , url = B.fieldNamed "url"
    , secondFactorResponse = B.fieldNamed "second_factor_response"
    , dateCreated = B.fieldNamed "date_created"
    , epgTxnId = B.fieldNamed "epg_txn_id"
    , lastUpdated = B.fieldNamed "last_updated"
    , txnDetailId = B.fieldNamed "txn_detail_id"
    , gatewayAuthReqParams = B.fieldNamed "gateway_auth_req_params"
    , authenticationAccountId = B.fieldNamed "authentication_account_id"
    , canAcceptResponse = B.fieldNamed "can_accept_response"
    , challengesAttempted = B.fieldNamed "challenges_attempted"
    , responseAttempted = B.fieldNamed "response_attempted"
    }

defaultSecondFactor :: Maybe Text -> LocalTime -> Text -> Text -> Text -> SecondFactor
defaultSecondFactor id date txnId status t = SecondFactor
  { id = id
  , version = 0
  , otp = Nothing
  , status = status
  , txnId = txnId
  , sfType = t
  , url = Nothing
  , secondFactorResponse = Nothing
  , dateCreated = date
  , epgTxnId = Nothing
  , lastUpdated = date
  , txnDetailId = Nothing
  , gatewayAuthReqParams = Nothing
  , authenticationAccountId = Nothing
  , canAcceptResponse = Nothing
  , challengesAttempted = Nothing
  , responseAttempted = Nothing
  }
