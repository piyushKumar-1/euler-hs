{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Dashboard.Auth.Types
  ( AuthContext(..)
  , LoginContext(..)
  , Role(..)
  , Token(..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Options(..), camelTo2, defaultOptions, genericParseJSON, genericToJSON, object, parseJSON, toJSON, withObject, (.=), (.:), (.:?))
import Data.Char (toUpper)
import Database.Redis (Connection)
import Universum
import Web.JWT (Signer)

-- Things that are needed to decode the token from the client
data AuthContext = AuthContext
  { redisConn :: Connection
  , jwtSecret :: Signer
  }

-- Corresponds to euler-ps Types.Communication.EcDashboard.Login / LoginContext
data LoginContext
  = LoginContextJuspay
  | LoginContextReseller
  | LoginContextMerchant
  deriving (Eq, Show, Generic)

-- drop 14 so we go from LOGIN_CONTEXT_JUSPAY -> JUSPAY
loginContextOptions :: Options
loginContextOptions = defaultOptions { constructorTagModifier = drop 14 . capsAndUnderscore }

instance ToJSON LoginContext where
  toJSON = genericToJSON loginContextOptions

instance FromJSON LoginContext where
  parseJSON = genericParseJSON loginContextOptions

-- Corresponds to euler-ps Types.Storage.EC.Role / Role
data Role
  = RoleUser
  | RoleAdmin
  | RoleCustomer
  | RoleMerchant
  | RoleMerchantAdmin
  | RoleMerchantOperator
  | RoleMerchantSupport
  | RoleSwitchUser
  | RoleReseller
  | RoleResellerOperator
  | RoleResellerSupport
  | RoleJuspaySwitchUser
  | RoleUserAcl
  deriving (Eq, Show, Generic)

roleAuthorityOptions :: Options
roleAuthorityOptions = defaultOptions { constructorTagModifier = capsAndUnderscore }

instance ToJSON Role where
  toJSON = genericToJSON roleAuthorityOptions

instance FromJSON Role where
  parseJSON = genericParseJSON roleAuthorityOptions

-- Corresponds to euler-ps Types.Communication.EcDashboard.Common / Authorization
data Token = Token
  { userId :: Int
  , userName :: String
  , email :: Maybe String
  , roles :: [Role]
  , ownerId :: String
  , loginContext :: LoginContext
  , merchantAccountId :: Maybe Int
  , merchantId :: Maybe String
  , resellerId :: Maybe String
  } deriving (Eq, Show)

-- This one's got a few quirks, so we encode/decode manually
instance ToJSON Token where
  toJSON token = object
    [ "username"          .= userName token
    , "userId"            .= userId token
    , "role"              .= roles token
    , "ownerId"           .= ownerId token
    , "merchantId"        .= merchantId token
    , "merchantAccountId" .= merchantAccountId token
    , "email"             .= email token
    , "context"           .= loginContext token
    ]

instance FromJSON Token where
  parseJSON = withObject "Token" $ \o -> Token
    <$> o .:  "userId"
    <*> o .:  "username"
    <*> o .:? "email"
    <*> o .:  "role"
    <*> o .:  "ownerId"
    <*> o .:  "context"
    <*> o .:? "merchantAccountId"
    <*> o .:? "merchantId"
    <*> o .:? "resellerId"

capsAndUnderscore :: String -> String
capsAndUnderscore = fmap toUpper . camelTo2 '_'
