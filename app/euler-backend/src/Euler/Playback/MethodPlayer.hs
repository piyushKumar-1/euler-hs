module Euler.Playback.MethodPlayer
  ( runMethodPlayer
  --, getMethod
  , noReqBody
  , noReqBodyJSON
  ) where

import EulerHS.Prelude

import           EulerHS.Interpreters (runFlow)
import           EulerHS.Language
import           EulerHS.Runtime
import           EulerHS.Types

import           Control.Exception               (throwIO)
import qualified Control.Exception.Safe as CES (catches, Handler(..))
import qualified Data.Aeson as A (Result(..), fromJSON, Value)
import           Data.Coerce (coerce)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Database.Redis as RD

import           Euler.API.RouteParameters
import qualified Euler.AppEnv as AppEnv
import qualified Euler.Common.Errors.ErrorsMapping as EMap
import           Euler.Playback.Types
import qualified Euler.Product.Domain              as D
import qualified Euler.Product.OLTP.Order.Create        as OrderCreate
import qualified Euler.Product.OLTP.Services.AuthConf   as Auth



-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- EHS: FIXME how to pass this environment to player?
appEnv :: AppEnv.AppEnv
appEnv = undefined
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------


-- EHS: Player should know nothing about methods. Should not depend on APIs
runMethodPlayer
  :: String
  -> MethodRecording
  -> PlayerParams
  -> IO MethodPlayerResult

-- old style -- direct dependency
runMethodPlayer "orderCreate"       = withMethodPlayer (Auth.withAuth Auth.mkKeyAuthService OrderCreate.orderCreate)

-- new style -- get handlers from AppEnv
runMethodPlayer "oderUpdate"        = withMethodPlayer $ AppEnv.orderUpdateMethod appEnv
runMethodPlayer "orderStatusHandle" = withMethodPlayer $ AppEnv.orderStatusMethod appEnv

--
runMethodPlayer methodName          = \_ _ -> pure $ Left $ MethodNotSupported methodName


type NoReqBody = ()

noReqBody :: NoReqBody
noReqBody = ()

noReqBodyJSON :: A.Value
noReqBodyJSON = toJSON noReqBody

--getMethod ::
--  ( FromJSON resp)
--  => (RouteParameters -> D.MerchantAccount -> Flow resp)
--  -> RouteParameters
--  -> NoReqBody
--  -> D.MerchantAccount
--  -> Flow resp
--getMethod f p _ m = f p m

withMethodPlayer
  :: (FromJSON req, FromJSON resp, ToJSON resp, Eq resp, Show resp)
  =>  ( RouteParameters -> req ->  Flow resp)
  -> MethodRecording
  -> PlayerParams
  -> IO MethodPlayerResult
withMethodPlayer methodF MethodRecording{..} PlayerParams{..} = do
  let eDecoded = A.fromJSON mrJsonRequest
  case eDecoded of
    A.Error err  -> pure $ Left $ RequestDecodingError err
    A.Success req -> do
      options    <- newMVar mempty -- initStartupOptions
      stepVar    <- newMVar 0
      errorMVar <- newMVar Nothing
      safeFlowMVar <- newMVar mempty
      forkedFlowErrorsVar <- newMVar mempty
      let rerrorVar = ReplayErrors errorMVar safeFlowMVar forkedFlowErrorsVar
      let MethodConfigs{..} = mrMethodConfigs
      let playerRt = PlayerRuntime
            { resRecording         = mrEntries -- :: ResultRecording
            , rerror               = rerrorVar -- :: ReplayErrors
            , stepMVar             = stepVar -- :: MVar Int
            , disableVerify        = ppDisableVerify -- :: [String]
            , disableMocking       = ppDisableMocking -- :: [String]
            , skipEntries          = ppSkipEntries -- :: [String]
            , entriesFiltered      = False -- :: Bool
            , flowGUID             = "main flow" -- :: Text
            }
      coreRt <- createCoreRuntime =<< createVoidLoggerRuntime
      httpManager <- newManager tlsManagerSettings
      kvdbConnectionsVar <- newMVar mempty
      sqldbConnectionsVar <- newMVar mempty
      pubSubController  <- RD.newPubSubController [] []
      let flowRt = FlowRuntime
            { _coreRuntime = coreRt
            , _defaultHttpClientManager = httpManager
            , _httpClientManagers       = mempty
            , _options = options
            , _kvdbConnections = kvdbConnectionsVar
            , _runMode = ReplayingMode playerRt
            , _sqldbConnections = sqldbConnectionsVar
            , _pubSubController         = pubSubController
            , _pubSubConnection         = Nothing
            }
      let method = methodF (coerce mcRouteParams) req  -- mr.parameters
      eResult :: Either SomeException resp <- try $ runFlow flowRt method `CES.catches` EMap.handlers
      case eResult of
        Right eResult' -> do

          let responseCheckResult = case ppResponseCheckMode of
                VerifyResponse   -> verifyResponse mrJsonResponse eResult'
                NoVerifyResponse -> ResponseSkipped

          resultPlayerError <- awaitErrors rerrorVar

          case flattenErrors resultPlayerError of
            [] -> pure $ Right $ PlaybackSucceeded $ responseCheckResult
            [x] -> pure $ Right $ PlaybackFailed x
            errList -> pure $ Left $ ForkedFlowsFailed $ tail errList
        Left ex -> do
          throwIO ex `CES.catches`
            [ CES.Handler (\(ReplayingException rEx) -> pure $ Right $ PlaybackFailed rEx)
            , CES.Handler (\ (e :: SomeException) -> do
                let (ee :: String) = show e
                responseCheckResult <- case ppResponseCheckMode of
                  VerifyResponse   -> pure $ verifyResponse mrJsonResponse ee
                  NoVerifyResponse -> pure ResponseSkipped
                resultPlayerError <- awaitErrors rerrorVar
                case flattenErrors resultPlayerError of
                  [] -> pure $ Right $ PlaybackSucceeded $ responseCheckResult
                  [x] -> pure $ Right $ PlaybackFailed x
                  errList -> pure $ Left $ ForkedFlowsFailed $ tail errList
                )
            ]
  where
    verifyResponse jsonResponse eResp =
      case A.fromJSON jsonResponse of
        A.Error err -> ResponseDecodingError $ show err
        A.Success recordedResp -> if recordedResp == eResp
              then ResponseOk
              else ResponseMismatch (show recordedResp) (show eResp)
