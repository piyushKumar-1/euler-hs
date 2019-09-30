module EulerHS.TestData.Framework.Flow where

import           EulerHS.Prelude

import           Servant.API
import           Servant.Client (ClientM, client)
import           Servant.Mock (mock)
import           Servant.Server (Server)

import EulerHS.TestData.Types.Framework.Flow


port :: Int
port = 8081

api :: Proxy API
api = Proxy

getUser :: ClientM User
getBook :: ClientM Book
(getUser :<|> getBook) = client api

server :: Server API
server = mock api Proxy