{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Euler.Server where

import           EulerHS.Prelude

import           Servant
import Data.Time
import qualified EulerHS.Interpreters                   as R
import qualified EulerHS.Language                       as L
import qualified EulerHS.Runtime                        as R

import qualified Euler.API.Transaction                  as ApiTxn
-- import qualified Euler.API.Validators.Transaction       as Txn
-- import qualified Euler.Product.OLTP.Transaction.Decider as Txn

import qualified Euler.API.Order                        as ApiOrder
import qualified Euler.Common.Merchant                  as Merchant

type EulerAPI
  = "test" :> Get '[PlainText] Text
  :<|> "txns" :> ReqBody '[JSON] ApiTxn.Transaction :> Post '[JSON] ApiTxn.TransactionResponse
-- order status
  :<|> "orders" :> Capture "orderId" Text :> Header "Authorization" Text :> Get '[JSON] ApiOrder.OrderStatusResponse
-- order create
  :<|> "orders" :> ReqBody '[FormUrlEncoded, JSON] ApiOrder.OrderCreateRequest :> Post '[JSON] ApiOrder.OrderCreateResponse
-- order update
  :<|> "orders" :> Capture "orderId" Text :> ReqBody '[FormUrlEncoded, JSON] ApiOrder.OrderCreateRequest :> Post '[JSON] ApiOrder.OrderStatusResponse
  :<|> EmptyAPI

eulerAPI :: Proxy EulerAPI
eulerAPI = Proxy

data Env = Env R.FlowRuntime
type FlowHandler = ReaderT Env (ExceptT ServerError IO)
type FlowServer = ServerT EulerAPI (ReaderT Env (ExceptT ServerError IO))

eulerServer' :: FlowServer
eulerServer' = test :<|> txns :<|> orderStatus :<|> orderCreate :<|> orderUpdate :<|> emptyServer

eulerServer :: Env -> Server EulerAPI
eulerServer env = hoistServer eulerAPI (f env) eulerServer'
  where
    f :: Env -> ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f env r = do
      eResult <- liftIO $ (runExceptT $ runReaderT r env ) 
      case eResult of
        Left err  -> throwError err -- $ show err       -- TODO: error reporting (internal server error & output to console, to log)
        Right res -> pure res

eulerBackendApp :: Env -> Application
eulerBackendApp = serve eulerAPI . eulerServer

runFlow :: L.Flow a -> FlowHandler a
runFlow flow = do
  Env flowRt <- ask
  lift $ lift $ R.runFlow flowRt flow

test :: FlowHandler Text
test = do
  liftIO $ putStrLn ("Test method called." :: String)
  runFlow $ L.logInfo "Test" "Test method"
  pure "Test."

txns :: ApiTxn.Transaction -> FlowHandler ApiTxn.TransactionResponse
txns apiTxn = do
  eTxn <- pure () -- transform txnValidator txn
  case eTxn of
    _ -> error "not implemented"
    _           -> error "Validation failed."     -- TODO: error message
  --     deciderResponse <- runFlow $ Txn.decider txn
  --     -- TODO: response
  --     pure Txn.TransactionResponse
  --       { Txn.order_id       = ""
  --       , Txn.txn_id         = ""
  --       , Txn.status         = ""
  --       , Txn.payment        = Txn.PaymentAuth $ Txn.Authentication "" ""
  --       , Txn.authentication = ""
  --       , Txn.method         = ""
  --       , Txn.url            = ""
  --       , Txn.params         = ""
  --       }

orderStatus :: Text -> Maybe Text -> FlowHandler ApiOrder.OrderStatusResponse
orderStatus orderId apiKey' = do
  res <- case apiKey' of
           Nothing -> do
            liftIO $ putStrLn ("No apikey header" :: String)
            throwError err403 { errBody = " no api key " }
           Just apiKey -> do
            pure ApiOrder.defaultOrderStatusResponse
  pure res

orderCreate :: ApiOrder.OrderCreateRequest -> FlowHandler ApiOrder.OrderCreateResponse
orderCreate ordReq = do
  res <- do
    liftIO $ putStrLn ("orderCreateStart" :: String)
    pure ApiOrder.defaultOrderCreateResponse
  pure res

orderUpdate :: Text -> ApiOrder.OrderCreateRequest -> FlowHandler ApiOrder.OrderStatusResponse
orderUpdate orderId ordReq = do
  res <- do
    liftIO $ putStrLn ("orderUpdateStart" :: String)
    pure ApiOrder.defaultOrderStatusResponse
  pure res