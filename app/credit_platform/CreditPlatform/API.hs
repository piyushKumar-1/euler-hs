module CreditPlatform.API where

import EulerHS.Prelude

import           Servant
import           Servant.API

import CreditPlatform.Domain as D

type CreditPlatformAPI
  = "authenticate" :> ReqBody '[JSON] D.GSTIN :> Post '[JSON] D.AuthToken
  :<|> "fetch_something" :> Capture "some_key" Text :> Get '[JSON] D.SomeData

creditPlatformServer :: Server CreditPlatformAPI
creditPlatformServer = authenticate :<|> fetchSomething

authenticate :: D.GSTIN -> Handler D.AuthToken
authenticate = error "Not implemented."

fetchSomething :: Text -> Handler D.SomeData
fetchSomething = error "Not implemented."



api :: Proxy CreditPlatformAPI
api = Proxy

creditPlatformApp :: Application
creditPlatformApp = serve api creditPlatformServer
