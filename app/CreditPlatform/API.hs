module CreditPlatform.API where

import EulerHS.Prelude

import CreditPlatform.Domain as D

type CreditPlatformAPI
  = "authenticate" :> Capture "taxID" D.GSTIN :> Get '[JSON] D.AuthToken
  :<|> "fetch_something" :> Capture "some_key" Text :> Get '[JSON] D.SomeData

api :: Proxy CreditPlatformAPI
api = Proxy

creditPlatformServer :: Server CreditPlatformAPI
creditPlatformServer = authenticate :<|> fetchSomething

authenticate :: D.GSTIN -> Handler D.AuthToken
authenticate = error "Not implemented."

fetchSomething :: Text -> Handler D.SomeData
fetchSomething = error "Not implemented."
