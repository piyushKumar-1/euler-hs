module Euler.Playback.MethodPlayer
  ( runMethodPlayer
  ) where

import EulerHS.Prelude

import           EulerHS.Interpreters (runFlow)
import           EulerHS.Language
import           EulerHS.Runtime
import           EulerHS.Types

import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Data.Aeson as A (Result(..), fromJSON)

import Euler.Playback.Types


runMethodPlayer
  :: String
  -> MethodRecording
  -> PlayerParams
  -> IO MethodPlayerResult
runMethodPlayer "testFlow2"        = withMethodPlayer (getMethod testFlow2)
runMethodPlayer methodName         = \_ _ -> pure $ Left $ MethodNotSupported methodName


getMethod :: ( FromJSON resp) => (t1 -> Flow resp) -> () -> t1 -> Flow resp
getMethod f r p = f p

testFlow2 ::  Map String String -> Flow Text
testFlow2  _ = do
  runSysCmd "echo hello"
  forkFlow "f1" $ logInfo tag "hellofrom forked flow"
  res <- runIO $ do
    putTextLn "text from runio"
    pure ("text from runio" :: Text)
  pure res
  where
    tag :: String
    tag = "from f1"

withMethodPlayer
  :: (FromJSON req, FromJSON resp, ToJSON resp, Eq resp, Show resp)
  =>  ( req -> Map String String -> Flow resp)
  -> MethodRecording
  -> PlayerParams
  -> IO MethodPlayerResult
withMethodPlayer methodF mr@MethodRecording{..} PlayerParams{..} = do
  let eDecoded = A.fromJSON mrJsonRequest
  case eDecoded of
    A.Error err  -> pure $ Left $ RequestDecodingError err
    A.Success req -> do
      options    <- newMVar mempty -- initStartupOptions
      stepVar    <- newMVar 0
      errorMVar <- newMVar Nothing
      forkedFlowErrorsVar <- newMVar mempty
      let rerrorVar = ReplayErrors errorMVar forkedFlowErrorsVar
      let mCfgs@MethodConfigs{..} = mrMethodConfigs
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
      httpManager <- newManager tlsManagerSettings >>= newMVar
      kvdbConnectionsVar <- newMVar mempty
      sqldbConnectionsVar <- newMVar mempty
      let flowRt = FlowRuntime
            { _coreRuntime = coreRt
            , _httpClientManager = httpManager
            , _options = options
            , _kvdbConnections = kvdbConnectionsVar
            , _runMode = ReplayingMode playerRt
            , _sqldbConnections = sqldbConnectionsVar
            }
      let method = methodF req mempty -- mr.parameters
      eResult <- try $ runFlow flowRt method
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

        Left (ReplayingException err)-> pure $ Right $ PlaybackFailed err
  where
    verifyResponse jsonResponse eResp =
      case A.fromJSON jsonResponse of
        A.Error err -> ResponseDecodingError $ show err
        A.Success recordedResp -> if recordedResp == eResp
              then ResponseOk
              else ResponseMismatch (show recordedResp) (show eResp)