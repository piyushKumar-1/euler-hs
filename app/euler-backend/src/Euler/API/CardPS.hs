{-# LANGUAGE DeriveAnyClass #-}

module Euler.API.CardPS where

import EulerHS.Prelude

import Data.Aeson
import Web.FormUrlEncoded


-- Legasy fields.
-- Left is legacy, Right is new ones.
-- cardId = cardToken
-- externalId = cardReference
-- isin = cardIsin

----------------------------------------------------------------------
-- Get Card Info
----------------------------------------------------------------------

-- Types.Communication.OLTP.GetCard

data GetCardRequest = GetCardRequest
  { cardToken :: Text
  , merchantId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)

data GetCardResponse = GetCardResponse
  { card :: GetCardResp
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data GetCardResp = GetCardResp
  { cardNumber :: Text
  , cardFingerprint :: Text
  , cardGlobalFingerprint :: Maybe Text
  , cardExpYear :: Maybe Text
  , cardExpMonth :: Maybe Text
  , nameOnCard :: Maybe Text
  , nickname   :: Maybe Text
  , customerId :: Maybe Text
  , cardToken     :: Text
  , cardReference :: Text
  , merchantId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- CardDetails converted to CardData
-- it is auxiliary type most likely
data CardData = CardData
  { cardNumber :: Text
  , cardExpYear :: Text
  , cardExpMonth :: Text
  , nameOnCard :: Maybe Text
  , cardSecurityCode :: Maybe Text
  , isStoredCard :: Maybe Bool
  , cardType :: Maybe Text
  , cardFingerprint :: Maybe Text
  , cardReference :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)



----------------------------------------------------------------------
-- Add Card
----------------------------------------------------------------------

-- Euler-ps type
data AddCardRequest = AddCardRequest
  { cardNumber :: Text
  , customerId :: Text
  , cardExpMonth :: Text
  , cardExpYear :: Text
  , merchantId :: Text
  , emailAddress :: Text
  , nameOnCard :: Text -- this not required in API docs
  , nickname :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)

-- From euler-ps
data AddCardResponse = AddCardResponse
  { code :: Int
  , status :: Text
  , response :: AddCardResp
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- From euler-ps
data AddCardResp = AddCardResp
  { cardToken :: Text
  , cardReference :: Text
  , cardNumber :: Maybe Text
  , cardFingerprint :: Text
  , cardGlobalFingerprint :: Text
  , duplicate  :: Maybe Text
  , cardExpYear :: Maybe Text
  , cardExpMonth :: Maybe Text
  , nameOnCard :: Maybe Text
  , nickanme :: Maybe Text
  , cardIsin :: Maybe Text
  , customerId :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


-----------------------------------------------------------
-- ListCard
-----------------------------------------------------------

-- Types.Communication.OLTP.ListCard ???

data ListCardRequest = ListCardRequest
  { customerId :: Text
  , merchantId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)

data ListCardResp = ListCardResp
  { cards :: [ListCardResponse]
  , customerId :: Text
  , merchantId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data ListCardResponse = ListCardResponse
  { cardToken :: Text
  , cardReference :: Text
  , cardNumber :: Text
  , cardFingerprint :: Text
  , cardGlobalFingerprint :: Text
  , cardExpYear :: Text
  , cardExpMonth :: Text
  , nameOnCard :: Text
  , nickname :: Text
  , cardIsin :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


----------------------------------------------------------------------
-- Delete Card
----------------------------------------------------------------------

-- Externals.EC.DeleteCards

data CardDeleteReq = CardDeleteReq
  { merchantId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)

data CardDeleteResponse = CardDeleteResponse [CardDeleteResponseScheme]
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data CardDeleteResponseScheme = CardDeleteResponseScheme
  { card_token :: Maybe Text
  , card_reference :: Maybe Text
  , deleted :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


-- Types.Communication.EcDashboard.CRUD.DeleteCards

-- | Delete Card Batch Request
-- data BatchCardDeleteReq = BatchCardDeleteReq
--   { fileName :: Maybe Text
--   , list :: Text
--   , merchantId :: Maybe Text
  -- }

-- TODO: find or realize
{-
newtype Process = Process
 { id :: String
 , shortName :: NullOrUndefined String
 , name :: String
 , "AnchorEntityId" :: NullOrUndefined String
 , anchorEntityName :: NullOrUndefined String
 , anchorEntityInfo :: NullOrUndefined String
 , state :: NullOrUndefined Foreign
 , status :: ProcessStatus
 , "OwnerId" :: NullOrUndefined String
 , "ParentId" :: NullOrUndefined String
 , deadline :: NullOrUndefined Date
 , metaData :: NullOrUndefined Foreign
 , createdAt :: Date
 , updatedAt :: Date
 }
-}
-- type Process = Text -- ??

-- -- | Delete Card Batch Process Status
-- data CardDeleteProcessStatus = CardDeleteProcessStatus
--   { pending :: [Process]
--   , failed :: [Process]
--   , success :: [Process]
--   , details :: Process
--   , summary :: CardDeleteProcessSummary
--   }

-- data CardDeleteProcessSummary = CardDeleteProcessSummary
--   { pendingCount :: Int
--   , failedCount :: Int
--   , successCount :: Int
  -- }


------------------------------------------------------------------------
-- Update Cards
------------------------------------------------------------------------

-- Backend/src/Product/OLTP/Cards.purs

-- data UpdateCardReq = UpdateCardReq
--   { metadata :: Meta
--   , card_reference :: Text
--   , client_auth_token :: Maybe Text
--   , order_id :: Maybe Text
--   , customer_id :: Maybe Text
--   }
--   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- -- Backend/src/Types/Storage/EC/StoredCard.purs
-- data Meta = VISA VisaMeta
--           | UnhandledNetworkDetails Text
--   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- newtype VisaMeta = VisaMeta {
--   visa :: VisaMetaData
--   }
--   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- newtype VisaMetaData = VisaMetaData {
--   vco :: VcoDetails
--   }
--   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- newtype VcoDetails = VcoDetails {
--   enabled :: Bool
--   }
--   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)
