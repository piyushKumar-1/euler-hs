module CreditPlatform.Services.API.Types where

import EulerHS.Prelude
import EulerHS.Language
import EulerHS.Types




type SampleAPI
         = "position"  :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello"     :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

sampleAPI :: Proxy SampleAPI
sampleAPI = Proxy

position :<|> hello :<|> marketing = client sampleAPI

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving (Show, Generic)

instance FromJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving (Show, Generic)

instance FromJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving (Show, Generic)

instance FromJSON Email
