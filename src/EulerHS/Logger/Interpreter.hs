{-# LANGUAGE BangPatterns #-}

module EulerHS.Logger.Interpreter
  (
    -- * Core Logger Interpreter
    runLogger
  )
where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.IORef (readIORef)
import           EulerHS.Common (FlowGUID)
import           EulerHS.Logger.Language (Logger, LoggerMethod (LogMessage))
import qualified EulerHS.Logger.Runtime as R
import qualified EulerHS.Logger.TinyLogger as Impl
import qualified EulerHS.Logger.Types as T
import           EulerHS.Prelude hiding (readIORef)

interpretLogger :: Maybe FlowGUID -> R.LoggerRuntime -> LoggerMethod a -> IO a

-- Memory logger
interpretLogger
  mbFlowGuid
  (R.MemoryLoggerRuntime flowFormatter logContext logLevel logsVar cntVar)
  (LogMessage msgLogLvl tag msg next) =

  fmap next $
    case compare logLevel msgLogLvl of
      GT -> pure ()
      _  -> do
        formatter <- flowFormatter mbFlowGuid
        !msgNum   <- R.incLogCounter cntVar
        x <- readIORef logContext
        let msgBuilder = formatter $ T.PendingMsg mbFlowGuid msgLogLvl tag msg msgNum x
        let !m = case msgBuilder of
              T.SimpleString str -> T.pack str
              T.SimpleText txt -> txt
              T.SimpleBS bs -> T.decodeUtf8 bs
              T.SimpleLBS lbs -> T.decodeUtf8 $ LBS.toStrict lbs
              T.MsgBuilder bld -> T.decodeUtf8 $ LBS.toStrict $ T.builderToByteString bld
              T.MsgTransformer _ -> error "Msg -> Msg not supported for memory logger."
        MVar.modifyMVar logsVar $ \(!lgs) -> pure (m : lgs, ())

-- Regular logger
interpretLogger
  mbFlowGuid
  (R.LoggerRuntime flowFormatter logContext logLevel _ _ cntVar _ handle severityCounterHandle)
  (LogMessage msgLogLevel tag msg next) =

  fmap next $
    case compare logLevel msgLogLevel of
      GT -> pure ()
      _  -> do
        msgNum    <- R.incLogCounter cntVar
        x <- readIORef logContext
        Impl.sendPendingMsg flowFormatter handle $ T.PendingMsg mbFlowGuid msgLogLevel tag msg msgNum x
        case severityCounterHandle of
          Nothing -> pure ()
          Just scHandle -> scHandle.incCounter msgLogLevel

runLogger :: Maybe FlowGUID -> R.LoggerRuntime -> Logger a -> IO a
runLogger mbFlowGuid loggerRt = foldF (interpretLogger mbFlowGuid loggerRt)
