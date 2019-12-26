module Console.HTTPServer where

import Universum

import Servant

import Console.API (QueryAPI, queryAuthContext, queryHandler)
import Dashboard.Auth.Types (AuthContext, Token)
import Dashboard.Query.Backend
import Dashboard.Query.Config
import Dashboard.Query.Types

server :: QueryBackend qb => qb -> QueryConfiguration -> Token -> Query -> Handler QueryResult
server = queryHandler

queryAPI :: Proxy QueryAPI
queryAPI = Proxy

app :: QueryBackend qb => AuthContext -> qb -> QueryConfiguration -> Application
app authCtx qb qc = serveWithContext queryAPI (queryAuthContext authCtx) (server qb qc)
