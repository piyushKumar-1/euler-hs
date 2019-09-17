module Console.API where

import Universum

import Servant

import Console.Query (runQuery)
import Dashboard.Query.Types (Query, QueryResult)

type QueryAPI
   = "dashboard" :> "query" :> ReqBody '[ JSON] Query :> Post '[ JSON] QueryResult

queryHandler :: Query -> Handler QueryResult
queryHandler q = liftIO $ runQuery q

server :: Server QueryAPI
server = queryHandler

queryAPI :: Proxy QueryAPI
queryAPI = Proxy

app :: Application
app = serve queryAPI server
