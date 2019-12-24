{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Euler.Server where

import           EulerHS.Prelude

import           Servant
import           Data.Time

import qualified Data.Aeson as A
import qualified Data.Text  as Text

import qualified EulerHS.Interpreters                   as R
import qualified EulerHS.Language                       as L
import qualified EulerHS.Runtime                        as R
import qualified EulerHS.Types                          as T

import qualified Euler.API.Transaction                  as ApiTxn
-- import qualified Euler.API.Validators.Transaction       as Txn
-- import qualified Euler.Product.OLTP.Transaction.Decider as Txn

import qualified Euler.API.Order                        as ApiOrder
import qualified Euler.API.Payment                      as ApiPayment
import qualified Euler.Common.Types.Merchant            as Merchant
import qualified Euler.Playback.Types                   as PB
import qualified Euler.Playback.Service                 as PB (writeMethodRecordingDescription)
import qualified Data.ByteString.Lazy                   as BSL
import qualified Prometheus as P
import           WebService.ContentType
                 (JavascriptWrappedJSON, mkDynContentTypeMiddleware, throwJsonError)
import           WebService.FlexCasing
                 (QueryParamC, mkFlexCaseMiddleware)
import           WebService.PostRewrite
                 (mkPostToGetMiddleware)
import           Network.Wai.Middleware.Routed
                 (routedMiddleware)

type EulerAPI
  = "test" :> Get '[PlainText] Text
  :<|> "txns" :> ReqBody '[JSON] ApiTxn.Transaction :> Post '[JSON] ApiTxn.TransactionResponse
-- order status
  :<|> "orders" :> Capture "orderId" Text :> Header "Authorization" Text :> Get '[JSON] ApiOrder.OrderStatusResponse
-- order create
  :<|> "orders" :> ReqBody '[FormUrlEncoded, JSON] ApiOrder.OrderCreateRequest :> Post '[JSON] ApiOrder.OrderCreateResponse
-- order update
  :<|> "orders" :> Capture "orderId" Text :> ReqBody '[FormUrlEncoded, JSON] ApiOrder.OrderCreateRequest :> Post '[JSON] ApiOrder.OrderStatusResponse
-- payment status endpoint (flexible casing showcase)
  :<|> "orders" :> "payment-status" :> QueryParamC "orderId" String :> QueryParamC "merchantId" String :> QueryParamC "callback" String :> QueryParamC "casing" String :> Get '[JSON, JavascriptWrappedJSON] ApiPayment.PaymentStatusResponse
  :<|> "metrics" :> Get '[OctetStream] ByteString
  :<|> EmptyAPI

eulerAPI :: Proxy EulerAPI
eulerAPI = Proxy

data Env = Env
  { envFlowRt         :: R.FlowRuntime
  , envRecorderParams :: Maybe PB.RecorderParams
  }

type FlowHandler = ReaderT Env (ExceptT ServerError IO)
type FlowServer = ServerT EulerAPI (ReaderT Env (ExceptT ServerError IO))

eulerServer' :: FlowServer
eulerServer' =
    test          :<|>
    txns          :<|>
    orderStatus   :<|>
    orderCreate   :<|>
    orderUpdate   :<|>
    paymentStatus :<|>
    metrics       :<|>
    emptyServer

eulerServer :: Env -> Server EulerAPI
eulerServer env = hoistServer eulerAPI (f env) $ eulerServer'
  where
    f :: Env -> ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f env r = do
      eResult <- liftIO $ (runExceptT $ runReaderT r env )
      case eResult of
        Left err  -> throwError err -- TODO: error reporting (internal server error & output to console, to log)
        Right res -> pure res

eulerBackendApp :: Env -> Application
eulerBackendApp env =
  let
    flexCaseMiddleware = routedMiddleware (["orders", "payment-status"] ==)
      (mkPostToGetMiddleware
      . mkFlexCaseMiddleware "orderId"
      . mkDynContentTypeMiddleware "callback")
  in
    flexCaseMiddleware (serve eulerAPI (eulerServer env))

runFlow :: (ToJSON a) => PB.FlowTag -> A.Value -> L.Flow a -> FlowHandler a
runFlow flowTag req flow = do
  Env {..} <- ask
  runningMode <- case envRecorderParams of
    Nothing -> pure T.RegularMode
    Just PB.RecorderParams{..} -> do
      recordingMVar <- newMVar mempty
      forkedRecordingsVar <- newMVar mempty
      let recording = T.Recording recordingMVar forkedRecordingsVar
      pure $ T.RecordingMode $ T.RecorderRuntime flowTag recording disableEntries
  let newRt = envFlowRt{ R._runMode = runningMode}
  let mc = PB.MethodConfigs
        { mcRawBody = ""
        , mcQueryParams = mempty
        , mcRouteParams = mempty
        , mcRawHeaders = mempty
        , mcMethodUrl = ""
        , mcSourceIP = ""
        , mcUserAgent = ""
        }
  res <- try $ lift $ lift $ R.runFlow newRt flow
  let jsonRes = case res of
        Left err ->  toJSON @String $ show err
        Right r ->  toJSON r
  _ <- lift $ lift $ case (runningMode, envRecorderParams) of
    (T.RecordingMode T.RecorderRuntime{..}, Just envParams) -> do
      entries' <- T.awaitRecording $ recording
      let mr = PB.MethodRecording
            { mrJsonRequest = req
            , mrJsonResponse = jsonRes
            , mrEntries = entries'
            , mrMethodConfigs = mc
            , mrSessionId = "SomeSessionId"
            , mrParameters = mempty
            }
      let mrd = PB.MethodRecordingDescription
            { methodName = toString flowTag
            , methodRecording = mr
            }
      PB.writeMethodRecordingDescription (PB.recordingsStorage envParams) mrd (toString flowTag) "sessionId"
    _ -> pure ()
  case res of
    Left e -> throwError e
    Right r -> pure r

testFlow2 :: Map String String -> L.Flow Text
testFlow2  _ = do
  L.runSysCmd "echo hello"
  L.forkFlow "f1" $ L.logInfo ("from f1" :: Text) "hellofrom forked flow"
  res <- L.runIO $ do
    putTextLn "text from runio"
    pure ("text from runio" :: Text)
  pure res

-- TODO > Move to somewhere
type NoReqBody = ()

noReqBody :: NoReqBody
noReqBody = ()

noReqBodyJSON :: A.Value
noReqBodyJSON = toJSON noReqBody

type RequestParameters = Map String String

getMethod ::
  ( FromJSON resp)
  => (RequestParameters -> L.Flow resp)
  -> NoReqBody
  -> RequestParameters
  -> L.Flow resp
getMethod f _ p = f p
-- TODO <

test :: FlowHandler Text
test = do
  liftIO $ putStrLn ("Test method called." :: String)
  runFlow "testFlow2" noReqBodyJSON $ (getMethod testFlow2) noReqBody mempty
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

paymentStatus ::
  Maybe String ->  -- orderId
  Maybe String ->  -- merchantId
  Maybe String ->  -- callback
  Maybe String ->  -- casing
  FlowHandler ApiPayment.PaymentStatusResponse
paymentStatus (Just orderId) (Just merchantId) callback (Just casing) = do
  when (casing == "unsupported") $ throwJsonError err400 $ ApiPayment.defaultJsonError
  return $ ApiPayment.PaymentStatusResponse {
    payload = ApiPayment.defaultPaymentStatus,
    caseStyle = Text.pack casing,
    callback = Text.pack <$> callback
  }
paymentStatus _ _ _ _ = throwError err500
--paymentStatus _ _ _ _ = throwJsonError err400 $ ApiPayment.defaultJsonError

metrics :: FlowHandler ByteString
metrics = BSL.toStrict <$> P.exportMetricsAsText
