{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.AuthenticationAccount
  ( AuthenticationAccountT(..)
  , AuthenticationAccount
  -- , Id
  , authenticationAccountEMod
  , defaultAuthenticationAccount
  ) where

import EulerHS.Prelude hiding (id)
import qualified Database.Beam as B

data AuthenticationAccountT f = AuthenticationAccount
  { id                        :: B.C f (Maybe Text)
  , version                   :: B.C f Int
  , accountDetails            :: B.C f Text
  , authenticationProvider    :: B.C f Text
  , description               :: B.C f (Maybe Text)
  , disabled                  :: B.C f (Maybe Bool)
  , disabledAt                :: B.C f (Maybe Text)
  , disabledBy                :: B.C f (Maybe Text)
  , maxOtpAttempts            :: B.C f Int
  , maxOtpSendLimit           :: B.C f Int
  , merchantAccountId         :: B.C f Int
  , minTimeResendOtpInSeconds :: B.C f Int
  , testMode                  :: B.C f (Maybe Bool)
  }
  deriving (Generic, B.Beamable)

instance B.Table AuthenticationAccountT where
  data PrimaryKey AuthenticationAccountT f =
    Id (B.C f (Maybe Text)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type AuthenticationAccount = AuthenticationAccountT Identity
-- type Id = B.PrimaryKey AuthenticationAccountT Identity

deriving instance Show AuthenticationAccount
deriving instance Eq AuthenticationAccount
deriving instance ToJSON AuthenticationAccount
deriving instance FromJSON AuthenticationAccount
deriving instance Read AuthenticationAccount
deriving instance Ord AuthenticationAccount

authenticationAccountEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity AuthenticationAccountT)
authenticationAccountEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , accountDetails = B.fieldNamed "account_details"
    , authenticationProvider = B.fieldNamed "authentication_provider"
    , description = B.fieldNamed "description"
    , disabled = B.fieldNamed "disabled"
    , disabledAt = B.fieldNamed "disabled_at"
    , disabledBy = B.fieldNamed "disabled_by"
    , maxOtpAttempts = B.fieldNamed "max_otp_attempts"
    , maxOtpSendLimit = B.fieldNamed "max_otp_send_limit"
    , merchantAccountId = B.fieldNamed "merchant_account_id"
    , minTimeResendOtpInSeconds = B.fieldNamed "min_time_resend_otp_in_seconds"
    , testMode = B.fieldNamed "test_mode"
    }

defaultAuthenticationAccount :: AuthenticationAccount
defaultAuthenticationAccount = AuthenticationAccount
  { id                        = Nothing -- :: B.C f (Maybe Text)
  , version                   = 0 -- :: B.C f Int
  , accountDetails            = "" -- :: B.C f Text
  , authenticationProvider    = "" -- :: B.C f Text
  , description               = Nothing -- :: B.C f (Maybe Text)
  , disabled                  = Nothing -- :: B.C f (Maybe Bool)
  , disabledAt                = Nothing -- :: B.C f (Maybe Text)
  , disabledBy                = Nothing -- :: B.C f (Maybe Text)
  , maxOtpAttempts            = 0 -- :: B.C f Int
  , maxOtpSendLimit           = 0 -- :: B.C f Int
  , merchantAccountId         = 0 -- :: B.C f Int
  , minTimeResendOtpInSeconds = 0 -- :: B.C f Int
  , testMode                  = Nothing -- :: B.C f (Maybe Bool)
  }
