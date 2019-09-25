module Console.API where

import Universum

import qualified Data.ByteString.Lazy.Char8 as B
import Servant

import Console.Query (runQuery)
import Dashboard.Query.Types
import Dashboard.Query.Validation

type QueryAPI
   = "dashboard" :> "query" :> ReqBody '[ JSON] Query :> Post '[ JSON] QueryResult

dummyQueryConf :: QueryConfiguration
dummyQueryConf = QueryConfiguration [("table1", TableConfiguration [ ("field1", IntType)
                                                                   , ("field2", StringType)
                                                                   ]
                                     )]

showQueryValidationError :: QueryValidationError -> String
showQueryValidationError (QueryValidationError qve _) =
  case qve of
    (TableError name)         -> "Table: " ++ name
    (SelectFieldError name)   -> "Select field: " ++ name
    (FilterFieldError name)   -> "Filter field: " ++ name
    (GroupByFieldError name)  -> "Group by field" ++ name
    (IntervalFieldError name) -> "Interval field" ++ name
    (FieldError name)         -> "Field: " ++ name

-- FIXME: The error reporting is very basic now to reflect the flattened error that gets
-- returned i.e. [QueryValidationerror], will change once we have a richer, nested error type.
queryHandler :: Query -> Handler QueryResult
queryHandler q =
  validateQuery dummyQueryConf q
  & either
      (throwError . toServerError)
      (const . liftIO . runQuery dummyQueryConf $ q)
  where
    toServerError :: [QueryValidationError] -> ServerError
    toServerError validationErrors =
      let queryError = B.pack . concatMap showQueryValidationError $ validationErrors
      in err400 {errBody = queryError}

server :: Server QueryAPI
server = queryHandler

queryAPI :: Proxy QueryAPI
queryAPI = Proxy

app :: Application
app = serve queryAPI server
