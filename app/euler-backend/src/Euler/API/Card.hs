{-# LANGUAGE DeriveAnyClass #-}

module Euler.API.Card where

import EulerHS.Prelude

import Data.Aeson
-- import Data.Time
-- import Servant
import Web.FormUrlEncoded


----------------------------------------------------------------------
-- Add Card
----------------------------------------------------------------------

-- Types.Communication.OLTP.AddCard

data AddCardRequest = AddCardRequest
  { cardNumber :: Text
  , customerId :: Text
  , cardExpMonth :: Text
  , cardExpYear :: Text
  , merchantId :: Text
  , emailAddress :: Text
  , nameOnCard :: Text
  , nickname :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)

data AddCardResponse = AddCardResponse
  { code :: Int
  , status :: String
  , response :: AddCardResp
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data AddCardResp = AddCardResp
  { cardId :: Text
  , externalId :: Text
  , cardNumber :: Maybe Text
  , cardFingerprint :: Text
  , cardGlobalFingerprint :: Text
  , duplicate  :: Maybe Text
  , cardExpYear :: Maybe Text
  , cardExpMonth :: Maybe Text
  , nameOnCard :: Maybe Text
  , nickanme :: Maybe Text
  , isin :: Maybe Text
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
  { cards :: [CardResp]
  , customerId :: Text
  , merchantId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data CardResp = CardResp
  { cardId :: Text
  , externalId :: Text
  , cardNumber :: Text
  , cardFingerprint :: Text
  , cardGlobalFingerprint :: Text
  , cardExpYear :: Text
  , cardExpMonth :: Text
  , nameOnCard :: Text
  , nickname :: Text
  , isin :: Text
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
data BatchCardDeleteReq = BatchCardDeleteReq
  { fileName :: Maybe Text
  , list :: Text
  , merchantId :: Maybe Text
  }

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
type Process = Text

-- | Delete Card Batch Process Status
data CardDeleteProcessStatus = CardDeleteProcessStatus
  { pending :: [Process]
  , failed :: [Process]
  , success :: [Process]
  , details :: Process
  , summary :: CardDeleteProcessSummary
  }

data CardDeleteProcessSummary = CardDeleteProcessSummary
  { pendingCount :: Int
  , failedCount :: Int
  , successCount :: Int
  }


----------------------------------------------------------------------
-- Card Info
----------------------------------------------------------------------

-- Types.Communication.OLTP.GetCard

data GetCardRequest = GetCardRequest
  { cardId :: Text
  , merchantId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)

data GetCardResponse = GetCardResponse {
  card :: GetCardResp
}

data GetCardResp = GetCardResp
  { cardNumber :: Text
  , cardFingerprint :: Text
  , cardGlobalFingerprint :: Maybe Text
  , cardExpYear :: Maybe Text
  , cardExpMonth :: Maybe Text
  , nameOnCard :: Maybe Text
  , nickname   :: Maybe Text
  , customerId :: Maybe Text
  , cardId     :: Text
  , externalId :: Text
  , merchantId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

------------------------------------------------------------------------
-- Update Cards
------------------------------------------------------------------------

-- Backend/src/Product/OLTP/Cards.purs

data UpdateCardReq = UpdateCardReq
  { metadata :: Meta
  , card_reference :: Text
  , client_auth_token :: Maybe Text
  , order_id :: Maybe Text
  , customer_id :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Backend/src/Types/Storage/EC/StoredCard.purs
data Meta = VISA VisaMeta
          | UnhandledNetworkDetails Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype VisaMeta = VisaMeta {
  visa :: VisaMetaData
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype VisaMetaData = VisaMetaData {
  vco :: VcoDetails
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype VcoDetails = VcoDetails {
  enabled :: Bool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)
