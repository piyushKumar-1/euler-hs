{-# LANGUAGE ScopedTypeVariables #-}

module Euler.Server where

import           EulerHS.Prelude

import qualified Control.Exception.Safe as CES (catches)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy                   as BSL
import           Data.Coerce (coerce)
import qualified Data.Text  as Text
import           Network.Socket (SockAddr(..))
import qualified Prometheus as P
import           Servant

import qualified EulerHS.Interpreters                   as R
import qualified EulerHS.Language                       as L
import qualified EulerHS.Runtime                        as R
import qualified EulerHS.Types                          as T

import qualified Euler.API.Transaction                  as ApiTxn
-- import qualified Euler.API.Validators.Transaction       as Txn
-- import qualified Euler.Product.OLTP.Transaction.Decider as Txn

import qualified Euler.API.Order                        as ApiOrder
import qualified Euler.API.Payment                      as ApiPayment
import           Euler.API.RouteParameters
import qualified Euler.Common.Errors.ErrorsMapping      as EMap
import qualified Euler.Playback.Types                   as PB
import qualified Euler.Playback.Service                 as PB (writeMethodRecordingDescription)
import qualified Euler.Playback.MethodPlayer            as PB (getMethod, noReqBody, noReqBodyJSON)
import qualified Euler.Product.Domain                   as D
import qualified Euler.Product.OLTP.Order.OrderStatus   as OrderStatus
import qualified Euler.Product.OLTP.Order.Create        as OrderCreate
import qualified Euler.Product.OLTP.Order.Update        as OrderUpdate
import qualified Euler.Product.OLTP.Services.AuthConf   as Auth

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

-- Transactions (Payments) API
  :<|> "txns"
      :> ReqBody '[JSON] ApiTxn.Transaction
      :> Post '[JSON] ApiTxn.TransactionResponse

-- Order API

-- Order status
  :<|> "orders"
      :> OrderStatusGetEndpoint
      -- :> Capture "orderId" Text
      -- :> Header "Authorization" Text
      -- :> Get '[JSON] ApiOrder.OrderStatusResponse

-- Order create
  :<|> "orders"
      :> OrderCreateEndpoint
--      :> ReqBody '[FormUrlEncoded, JSON] ApiOrder.OrderCreateRequest
--      :> Post '[JSON] ApiOrder.OrderCreateResponse

-- Order update
  :<|> "orders" :> OrderUpdateEndpoint
     -- :> Capture "orderId" Text
     -- :> ReqBody '[FormUrlEncoded, JSON] ApiOrder.OrderUpdateRequest
     -- :> Post '[JSON] ApiOrder.OrderStatusResponse

-- Other APIS
-- payment status endpoint (flexible casing showcase)
  :<|> "orders"
      :> "payment-status"
      :> QueryParamC "orderId" String
      :> QueryParamC "merchantId" String
      :> QueryParamC "callback" String
      :> QueryParamC "casing" String
      :> Get '[JSON, JavascriptWrappedJSON] ApiPayment.PaymentStatusResponse

  :<|> "metrics"
      :> Get '[OctetStream] ByteString

  :<|> "remoteip"
      :> Header "User-Agent" Text
      :> RemoteHost
      :> Get '[JSON] Text

  :<|> EmptyAPI

eulerAPI :: Proxy EulerAPI
eulerAPI = Proxy

data Acc = Acc {}

data Env = Env
  { envFlowRt         :: R.FlowRuntime
  , envRecorderParams :: Maybe PB.RecorderParams
  }

type FlowHandler = ReaderT Env (ExceptT ServerError IO)
type FlowServer' api = ServerT api FlowHandler
type FlowServer      = FlowServer' EulerAPI

eulerServer' :: FlowServer
eulerServer' =
    test          :<|>
    txns          :<|>

    orderStatus   :<|>
    orderCreate   :<|>
    orderUpdate   :<|>

    paymentStatus :<|>
    metrics       :<|>
    remoteip      :<|>
    emptyServer

----------------------------------------------------------------------

eulerServer_ :: HasServer (api :: *) '[] => Proxy api -> FlowServer' api -> Env -> Server api
eulerServer_ api serv env = hoistServer api (f env) serv
  where
    f :: Env -> ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f env' r = do
      eResult <- liftIO $ (runExceptT $ runReaderT r env')
      case eResult of
        Left err  -> throwError err
        Right res -> pure res

eulerServer :: Env -> Server EulerAPI
eulerServer = eulerServer_ eulerAPI eulerServer'

eulerBackendApp :: Env -> Application
eulerBackendApp env =
  let
    flexCaseMiddleware = routedMiddleware (["orders", "payment-status"] ==)
      (mkPostToGetMiddleware
      . mkFlexCaseMiddleware "orderId"
      . mkDynContentTypeMiddleware "callback")
  in
    flexCaseMiddleware (serve eulerAPI (eulerServer env))

runFlow :: (ToJSON a) => PB.FlowTag -> RouteParameters -> A.Value -> L.Flow a -> FlowHandler a
runFlow flowTag rps req flow = do
  Env {..} <- ask
  runningMode <- case envRecorderParams of
    Nothing -> pure T.RegularMode
    Just PB.RecorderParams{..} -> do
      recordingMVar <- newMVar mempty
      safeRecordingsVar <- newMVar mempty
      forkedRecordingsVar <- newMVar mempty
      let recording = T.Recording recordingMVar safeRecordingsVar forkedRecordingsVar
      pure $ T.RecordingMode $ T.RecorderRuntime flowTag recording disableEntries
  let newRt = envFlowRt{ R._runMode = runningMode}
  let mc = PB.MethodConfigs
        { mcRawBody = ""
        , mcQueryParams = mempty
        , mcRouteParams = coerce rps
        , mcRawHeaders = mempty
        , mcMethodUrl = ""
        , mcSourceIP = ""
        , mcUserAgent = ""
        }
  res <- try (lift $ lift $ R.runFlow newRt flow `CES.catches` EMap.handlers)
  jsonRes <- case res of
    Left err -> pure $ toJSON @String $ show err
    Right r -> pure $ toJSON r
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

test :: FlowHandler Text
test = do
  liftIO $ putStrLn ("Test method called." :: String)
  pure "Test."

txns :: ApiTxn.Transaction -> FlowHandler ApiTxn.TransactionResponse
txns _ = do
  eTxn <- pure () -- transform txnValidator txn
  case eTxn of
    -- _ -> error "not implemented"
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

type OrderStatusGetEndpoint =
     -- URL parameters
     Capture "orderId" OrderId
     -- Headers
  -- EHS: use common constants for headers?
  :> Header "Authorization" Authorization
  -- EHS: most likely this header is not used in order status
  :> Header "X-Auth-Scope" XAuthScope
  :> Header "X-Forwarded-For" XForwardedFor
  :> Header "client_auth_token" ClientAuthToken
  :> Header "options.add_full_gateway_response" AddFullGatewayResponseOption
  :> Get '[JSON] ApiOrder.OrderStatusResponse

orderStatus ::
     OrderId
  -> Maybe Authorization
  -> Maybe XAuthScope
  -> Maybe XForwardedFor
  -> Maybe ClientAuthToken
  -> Maybe AddFullGatewayResponseOption
  -> FlowHandler ApiOrder.OrderStatusResponse
orderStatus orderId mbAuth mbXAuthScope mbXForwarderFor mbClientAuthToken mbSendFullPgr = do
  let rps = collectRPs
              orderId
              mbAuth
              mbXAuthScope
              mbXForwarderFor
              mbClientAuthToken
              mbSendFullPgr

  runFlow "orderStatusHandle" rps PB.noReqBodyJSON
        $ Auth.withAuth Auth.mkKeyTokenAuthService
          (PB.getMethod OrderStatus.orderStatus) rps PB.noReqBody


-- EHS: Extract from here.
type OrderCreateEndpoint
  --  Headers
  =   Header "Authorization" Authorization
  :>  Header "Version" Version
  :>  Header "User-Agent" UserAgent
  :>  Header "X-Auth-Scope" XAuthScope
  :>  Header "X-Forwarded-For" XForwardedFor
  --  Remote host
  :>  RemoteHost
  --  POST Body
  :>  ReqBody '[FormUrlEncoded, JSON] ApiOrder.OrderCreateRequest
  --  Response
  :>  Post '[JSON] ApiOrder.OrderCreateResponse

orderCreate
  :: Maybe Authorization
  -- Header
  -> Maybe Version
  -> Maybe UserAgent
  -> Maybe XAuthScope
  -> Maybe XForwardedFor
  -- Remote IP
  -> SockAddr
  -- Response
  -> ApiOrder.OrderCreateRequest
  -> FlowHandler ApiOrder.OrderCreateResponse
orderCreate auth version uagent xauthscope xforwarderfor sockAddr ordReq = do

  -- EHS: review this function
  let rps = collectRPs
               auth
               version
               uagent
               xauthscope
               xforwarderfor
               (sockAddrToSourceIP sockAddr)

--  let eValidated = VO.transApiOrdCreateToOrdCreateT ordReq
  -- EHS: TODO: move validations from validateOrderParams
  -- validateOrderParams orderCreateReq

  -- EHS: function `orderCreate` is also about validation.
--  case eValidated of
--    V.Failure err -> error "Not implemented"   -- TODO: EHS: return error.
--    V.Success validatedOrder ->
  runFlow "orderCreate" rps (toJSON ordReq)
          $ Auth.withAuth Auth.mkKeyAuthService OrderCreate.orderCreate rps ordReq -- validatedOrder


type OrderUpdateEndpoint =
  -- URL parameters
      Capture "orderId" OrderId
  --  Headers
  :>  Header "Authorization" Authorization
  :>  Header "Version" Version
  :>  Header "User-Agent" UserAgent
  :>  Header "X-Auth-Scope" XAuthScope
  :>  Header "X-Forwarded-For" XForwardedFor
  --  Remote host
  :>  RemoteHost
  --  POST Body
  :>  ReqBody '[FormUrlEncoded, JSON] ApiOrder.OrderUpdateRequest
  --  Response
  :>  Post '[JSON] ApiOrder.OrderStatusResponse

orderUpdate
-- URL parameters
   ::  OrderId
  -- Headers
  -> Maybe Authorization
  -> Maybe Version
  -> Maybe UserAgent
  -> Maybe XAuthScope
  -> Maybe XForwardedFor
  -- Remote IP
  -> SockAddr
  -- Request
  -> ApiOrder.OrderUpdateRequest
  -- Response
  ->  FlowHandler ApiOrder.OrderStatusResponse
orderUpdate orderId auth version uagent xauthscope xforwarderfor sockAddr req = do
  let rps = collectRPs
             auth
             version
             uagent
             xauthscope
             xforwarderfor
             (sockAddrToSourceIP sockAddr)
             orderId
  runFlow "orderUpdate" rps (toJSON req) $ Auth.withAuth Auth.mkKeyAuthService OrderUpdate.runOrderUpdate rps req



paymentStatus
  :: Maybe String -- ^ orderId
  -> Maybe String -- ^ merchantId
  -> Maybe String -- ^ callback
  -> Maybe String -- ^ casing
  -> FlowHandler ApiPayment.PaymentStatusResponse
paymentStatus (Just _) (Just _) callback (Just casing) = do
  when (casing == "unsupported") $ throwJsonError err400 $ ApiPayment.defaultJsonError
  return $ ApiPayment.PaymentStatusResponse
    { payload = ApiPayment.defaultPaymentStatus
    , caseStyle = Text.pack casing
    , callback = Text.pack <$> callback
    }
paymentStatus _ _ _ _ = throwError err500
--paymentStatus _ _ _ _ = throwJsonError err400 $ ApiPayment.defaultJsonError

metrics :: FlowHandler ByteString
metrics = BSL.toStrict <$> P.exportMetricsAsText

remoteip :: Maybe Text -> SockAddr -> FlowHandler Text
remoteip uagent remIp =
  pure $ show uagent <> " \n " <> (show $ sockAddrToSourceIP remIp)
