module CreditPlatform.Server where

import EulerHS.Prelude

import           Servant

import qualified CreditPlatform.Domain as D
import qualified CreditPlatform.Logic.SampleLogger as L
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import qualified EulerHS.Interpreters as R

type CreditPlatformAPI
  = "test" :> Get '[PlainText] Text
  :<|> "authenticate" :> ReqBody '[JSON] D.GSTIN :> Post '[JSON] D.AuthToken
  :<|> "log_message" :> Capture "message" Text :> Post '[JSON] Text
  :<|> EmptyAPI

creditPlatformAPI :: Proxy CreditPlatformAPI
creditPlatformAPI = Proxy

data Env = Env R.FlowRuntime
type FlowHandler = ReaderT Env (ExceptT ServerError IO)
type FlowServer = ServerT CreditPlatformAPI (ReaderT Env (ExceptT ServerError IO))

creditPlatformServer' :: FlowServer
creditPlatformServer' = test :<|> authenticate :<|> logMessage :<|> emptyServer

creditPlatformServer :: Env -> Server CreditPlatformAPI
creditPlatformServer env =
  hoistServer creditPlatformAPI (f env) creditPlatformServer'
  where
    f :: Env -> ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f env' r = do
      eResult <- liftIO $ runExceptT $ runReaderT r env'
      case eResult of
        Left _ -> error "err"       -- TODO: error reporting (internal server error & output to console, to log)
        Right res -> pure res

creditPlatformApp :: Env -> Application
creditPlatformApp = serve creditPlatformAPI . creditPlatformServer

withFlowHandler :: L.Flow a -> FlowHandler a
withFlowHandler flow = do
  Env flowRt <- ask
  lift $ lift $ R.runFlow flowRt flow

test :: FlowHandler Text
test = do
  liftIO $ putStrLn @String "Test method called."
  withFlowHandler (L.logMessageFlow "Test")
  pure "Test."

authenticate :: D.GSTIN -> FlowHandler D.AuthToken
authenticate = error "Not implemented."

logMessage :: Text -> FlowHandler Text
logMessage msg = do
  liftIO $ putStrLn @String "Log Message method called."
  withFlowHandler (L.logMessageFlow msg >> pure msg)
