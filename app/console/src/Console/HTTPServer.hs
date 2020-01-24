module Console.HTTPServer where

import Universum

import Console.API (AppT(..), QueryAPI, queryAuthContext, queryHandler)
import Dashboard.Auth.Types (AuthContext, Token)
import Dashboard.Query.Backend.BigQuery (BackendConfig)
import Network.Wai (Request)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler)

toHandler :: BackendConfig -> AppT a -> Handler a
toHandler bec = flip runReaderT bec . runAppT

server :: ServerT QueryAPI AppT
server = queryHandler

queryAPI :: Proxy QueryAPI
queryAPI = Proxy

app :: AuthContext -> BackendConfig -> Application
app authCtx bec =
  let pc = Proxy :: Proxy (AuthHandler Request Token ': '[])
      in serveWithContext queryAPI (queryAuthContext authCtx) $
         hoistServerWithContext queryAPI pc (toHandler bec) server
