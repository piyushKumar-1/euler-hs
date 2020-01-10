module EulerHS.TestData.API.Client where

import           EulerHS.Prelude

import           Servant.API
import           Servant.Mock (mock)
import           Servant.Server (Server)
import           Servant.API
import           EulerHS.Types

import EulerHS.TestData.Types


type API = "user" :> Get '[JSON] User
      :<|> "book" :> Get '[JSON] Book


port :: Int
port = 8081

api :: Proxy API
api = Proxy

getUser :: EulerClient User
getBook :: EulerClient Book
(getUser :<|> getBook) = client api

server :: Server API
server = mock api Proxy
