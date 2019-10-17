module Console.HTTPServer where

import Universum

import Servant

import Console.API (queryHandler)
import Dashboard.Query.Backend
import Dashboard.Query.Types

type QueryAPI
   = "dashboard" :> "query" :> ReqBody '[ JSON] Query :> Post '[ JSON] QueryResult

server :: QueryBackend qb => qb -> QueryConfiguration -> Server QueryAPI
server = queryHandler

queryAPI :: Proxy QueryAPI
queryAPI = Proxy

app :: QueryBackend qb => qb -> QueryConfiguration -> Application
app qb qc = serve queryAPI $ server qb qc
