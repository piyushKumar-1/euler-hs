module Console.API where

import Universum

import qualified Data.ByteString.Lazy.Char8 as B
import Servant

import Dashboard.Query.Backend
import Dashboard.Query.Types
import Dashboard.Query.Validation

type QueryAPI
   = "dashboard" :> "query" :> ReqBody '[JSON] Query :> Post '[JSON] QueryResult

showQueryValidationError :: QueryValidationError -> String
showQueryValidationError (QueryValidationError qve _) =
  "Validation failed: " ++ case qve of
    (TableNotFound name)         -> "Table not found: " ++ name
    (SelectFieldNotFound name)   -> "Select field not found " ++ name
    (FilterFieldNotFound name)   -> "Filter field not found: " ++ name
    (GroupByFieldNotFound name)  -> "Group by field not found" ++ name
    (FilterTypeMismatch name)    -> "Filter type mismatch on: " ++ name
    (IntervalFieldNotFound name) -> "Interval field not found: " ++ name
    (SelectOperationNotValid op) -> "Select operation invalid: " ++ show op

-- FIXME: The error reporting is very basic now to reflect the flattened error that gets
-- returned i.e. [QueryValidationerror], will change once we have a richer, nested error type.
queryHandler :: QueryBackend qb => qb -> QueryConfiguration -> Query -> Handler QueryResult
queryHandler qb qc q =
  validateQuery qc q
  & either
      (throwError . toServerError)
      (const . liftIO . runQuery qb qc $ q)
  where
    toServerError :: [QueryValidationError] -> ServerError
    toServerError validationErrors =
      let queryError = B.pack . intercalate ", " $ showQueryValidationError <$> validationErrors
      in err400 {errBody = queryError}

