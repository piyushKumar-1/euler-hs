module Console.API where

import Universum

import qualified Data.ByteString.Lazy.Char8 as B
import Servant

import Dashboard.Query.Backend
import Dashboard.Query.Types
import Dashboard.Query.Validation

type QueryAPI
   = "dashboard" :> "query" :> ReqBody '[JSON] Query :> Post '[JSON] QueryResult

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
      let queryError = B.pack . intercalate ", " $ printQueryValidationError <$> validationErrors
      in err400 {errBody = queryError}

