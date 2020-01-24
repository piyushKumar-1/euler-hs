module Console.API
  ( AppT(..)
  , QueryAPI
  , QueryAPINoAuth
  , queryAuthContext
  , queryHandler
  ) where

import Universum

import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as List
import qualified Prometheus as P
import Network.Wai (Request, requestHeaders)
import Servant
import Servant.Server.Experimental.Auth

import Dashboard.Auth.Types (AuthContext, Token(..), Role(..))
import Dashboard.Auth.Token (lookupToken)
import Dashboard.Metrics.Prometheus
import Dashboard.Query.Backend
import Dashboard.Query.Backend.BigQuery (BackendConfig(..))
import Dashboard.Query.Process
import Dashboard.Query.Types
import Dashboard.Query.Validation

-- This one is only used for tests
type QueryAPINoAuth
  = "dashboard" :> "query" :> ReqBody '[JSON] Query :> Post '[JSON] QueryResult

type QueryAPI
  = AuthProtect "web-token" :> QueryAPINoAuth

type instance AuthServerData (AuthProtect "web-token") = Token

newtype AppT a =
  AppT
    { runAppT :: ReaderT BackendConfig Handler a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadError ServerError,
                MonadThrow, MonadReader BackendConfig)

authHandler :: AuthContext -> AuthHandler Request Token
authHandler authCtx = mkAuthHandler handler
  where
    handler req = do
      let eUuid = maybeToRight
                    "Missing login token"
                    (List.lookup "X-Web-LoginToken" . requestHeaders $ req)
      token <- liftIO $
                 either
                   (return . Left)
                   (fmap (maybeToRight "Invalid login token") . lookupToken authCtx)
                   eUuid

      liftIO $ case token of
        Left _ -> P.incCounter authFailure
        Right _ -> P.incCounter authSuccess
      either throw401 return token

    throw401 msg = throwError $ err401 { errBody = msg }

queryAuthContext :: AuthContext -> Context (AuthHandler Request Token ': '[])
queryAuthContext authCtx = authHandler authCtx :. EmptyContext

-- FIXME: The error reporting is very basic now to reflect the flattened error that gets
-- returned i.e. [QueryValidationerror], will change once we have a richer, nested error type.
queryHandler :: Token -> Query -> AppT QueryResult
queryHandler token q = do
  BackendConfig { qbe = qb , qcfg = qc } <- ask
  -- FIXME: hardcoded role check for now
  if RoleAdmin `List.notElem` roles token
    then
      throwError err403
    else
      case validateQuery qc q of
        Left err -> do
          liftIO $ P.incCounter queryValidationFailed
          throwError . toServerError $ err
        Right _  -> liftIO . map (processResult qc q) . runQuery qb qc $ q

  where
    toServerError :: [QueryValidationError] -> ServerError
    toServerError validationErrors =
      let queryError = B.pack . intercalate ", " $ printQueryValidationError <$> validationErrors
      in err400 {errBody = queryError}
