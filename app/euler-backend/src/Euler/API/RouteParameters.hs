{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Euler.API.RouteParameters
  ( RouteParameters(..)
  -- Headers
  , Authorization(..)
  , OrderId(..)
  , UserAgent(..)
  , Version(..)
  , XAuthScope(..)
  , XAuthSignature(..)
  , XForwardedFor(..)
  , XLoginContext(..)
  , XMerchantAccountId(..)
  , XMerchantId(..)
  , XMerchantRole(..)
  , XMerchantUserName(..)
  , XOrganizationId(..)
  , XOwnerId(..)
  , XResellerId(..)
  , XSDKCustomerId(..)
  , XSDKOrderId(..)
  , XUserId(..)
  -- Source IP
  , SourceIP(..)
  -- Methods
  , collectRPs
  , emptyRPs
  , lookupRP
  , sockAddrToSourceIP
  ) where

import EulerHS.Prelude

import           Data.Coerce
import           Data.Data hiding (typeRep)
import           Network.Socket (SockAddr(..), hostAddressToTuple, hostAddress6ToTuple)
import           Numeric (showHex)
import           Text.Show (showString)
import           Type.Reflection (typeRep)
import           Web.Internal.HttpApiData

import qualified Data.Map as Map


newtype RouteParameters = RouteParameters { unRP :: Map Text Text}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

class RouteParameter a

emptyRPs :: RouteParameters
emptyRPs = RouteParameters mempty

insertRP :: forall a. ((Coercible a Text), Typeable a, RouteParameter a) => a -> RouteParameters -> RouteParameters
insertRP p RouteParameters {..} = RouteParameters $ Map.insert (show $ typeRep @a) (coerce p) unRP

lookupRP :: forall a. Typeable a => RouteParameters -> Maybe Text
lookupRP RouteParameters {..} = Map.lookup (show $ typeRep @a) unRP

class TMClass t  where
  insertRP' :: RouteParameters -> t

instance TMClass RouteParameters where
  insertRP' = id

instance ((Coercible a Text), TMClass r, Typeable a, RouteParameter a) => TMClass (a -> r) where
  insertRP' trmap a = insertRP' (insertRP a trmap)

instance {-# OVERLAPPING #-} ((Coercible a Text), TMClass r, Typeable a, RouteParameter a) => TMClass (Maybe a -> r) where
  insertRP' trmap a = insertRP' (case a of
    Just a' -> insertRP a' trmap
    Nothing -> trmap)

collectRPs :: (TMClass r) => r
collectRPs = insertRP' emptyRPs

-- We should write instances for ToHttpApiData and FromHttpApiData
-- because documentation says:
-- WARNING: Do not derive this using DeriveAnyClass as the generated instance will loop indefinitely.
-- https://hackage.haskell.org/package/servant-0.16.2/docs/Servant-API.html#t:FromHttpApiData

-- HEADERS :

newtype Authorization = Authorization Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData Authorization where toUrlPiece = coerce
instance FromHttpApiData Authorization where parseUrlPiece = Right . coerce

-- EHS: name clash
newtype OrderId = OrderId Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData OrderId where toUrlPiece = coerce
instance FromHttpApiData OrderId where parseUrlPiece = Right . coerce

newtype UserAgent = UserAgent Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData UserAgent where toUrlPiece = coerce
instance FromHttpApiData UserAgent where parseUrlPiece = Right . coerce

newtype Version = Version Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData Version where toUrlPiece = coerce
instance FromHttpApiData Version where parseUrlPiece = Right . coerce

newtype XAuthScope = XAuthScope Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData XAuthScope where toUrlPiece = coerce
instance FromHttpApiData XAuthScope where parseUrlPiece = Right . coerce

newtype XAuthSignature = XAuthSignature Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData XAuthSignature where toUrlPiece = coerce
instance FromHttpApiData XAuthSignature where parseUrlPiece = Right . coerce

newtype XForwardedFor = XForwardedFor Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData XForwardedFor where toUrlPiece = coerce
instance FromHttpApiData XForwardedFor where parseUrlPiece = Right . coerce

newtype XLoginContext = XLoginContext Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData XLoginContext where toUrlPiece = coerce
instance FromHttpApiData XLoginContext where parseUrlPiece = Right . coerce

newtype XMerchantAccountId = XMerchantAccountId Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData XMerchantAccountId where toUrlPiece = coerce
instance FromHttpApiData XMerchantAccountId where parseUrlPiece = Right . coerce

newtype XMerchantId = XMerchantId Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData XMerchantId where toUrlPiece = coerce
instance FromHttpApiData XMerchantId where parseUrlPiece = Right . coerce

newtype XMerchantRole = XMerchantRole Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData XMerchantRole where toUrlPiece = coerce
instance FromHttpApiData XMerchantRole where parseUrlPiece = Right . coerce

newtype XOrganizationId = XOrganizationId Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData XOrganizationId where toUrlPiece = coerce
instance FromHttpApiData XOrganizationId where parseUrlPiece = Right . coerce

newtype XOwnerId = XOwnerId Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData XOwnerId where toUrlPiece = coerce
instance FromHttpApiData XOwnerId where parseUrlPiece = Right . coerce

newtype XMerchantUserName = XMerchantUserName Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData XMerchantUserName where toUrlPiece = coerce
instance FromHttpApiData XMerchantUserName where parseUrlPiece = Right . coerce

newtype XResellerId = XResellerId Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData XResellerId where toUrlPiece = coerce
instance FromHttpApiData XResellerId where parseUrlPiece = Right . coerce

newtype XSDKCustomerId = XSDKCustomerId Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData XSDKCustomerId where toUrlPiece = coerce
instance FromHttpApiData XSDKCustomerId where parseUrlPiece = Right . coerce

newtype XSDKOrderId = XSDKOrderId Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData XSDKOrderId where toUrlPiece = coerce
instance FromHttpApiData XSDKOrderId where parseUrlPiece = Right . coerce

newtype XUserId = XUserId Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData XUserId where toUrlPiece = coerce
instance FromHttpApiData XUserId where parseUrlPiece = Right . coerce


-- Source IP

newtype SourceIP = SourceIP Text
  deriving (Eq, Show, Data, Typeable, RouteParameter)

instance ToHttpApiData SourceIP where toUrlPiece = coerce
instance FromHttpApiData SourceIP where parseUrlPiece = Right . coerce

sockAddrToSourceIP :: SockAddr -> SourceIP
sockAddrToSourceIP sAddr =
  case sAddr of
    SockAddrInet _ h      -> SourceIP
      $ show4tuple $ hostAddressToTuple h
    SockAddrInet6 _ _ h _ -> SourceIP
      $ show8tuple $ hostAddress6ToTuple h
    SockAddrUnix s        -> SourceIP
      $ toText s
    SockAddrCan _         -> SourceIP "" -- SockAddrCan - deprecated: This will be removed in network-3.0
  where
    show4tuple (a,b,c,d) =
      show a <> separator4 <>
      show b <> separator4 <>
      show c <> separator4 <>
      show d
    show8tuple (a,b,c,d,e,f,g,h) = toText $
      showHex a $ separator8 $
      showHex b $ separator8 $
      showHex c $ separator8 $
      showHex d $ separator8 $
      showHex e $ separator8 $
      showHex f $ separator8 $
      showHex g $ separator8 $
      showHex h ""
    separator8 = showString ":"
    separator4 = "."
