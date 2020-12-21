{-# LANGUAGE BangPatterns #-}

module EulerHS.Core.Logger.Interpreter
  (
    -- * Core Logger Interpreter
    runLogger
  )
where

import           EulerHS.Prelude

import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.Logger.Entries as E
import qualified EulerHS.Core.Logger.ImplMimicPSBad.TinyLogger as Impl
import qualified EulerHS.Core.Playback.Machine as P
import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Types as T
import qualified Control.Concurrent.MVar as MVar


interpretLogger :: T.RunMode -> R.LoggerRuntime -> L.LoggerMethod a -> IO a
interpretLogger
  runMode
  (R.MemoryLoggerRuntime formatter _ cfgLogLvl mvar)
  (L.LogMessage msgLogLvl tag msg next) =

  fmap next $ P.withRunMode runMode (E.mkLogMessageEntry msgLogLvl tag msg) $
    case compare cfgLogLvl msgLogLvl of
      GT -> pure ()
      _  -> do
        !msgNum <- R.incLogCounter cntVar
        let !m = formatter $ T.PendingMsg msgLogLvl tag msg msgNum
        MVar.modifyMVar mvar $ \(!lgs) -> pure (m : lgs, ())

interpretLogger runMode
  (R.LoggerRuntime formatter cfgLogLvl _ cntVar handle)
  (L.LogMessage msgLogLvl tag msg next) =

  fmap next $ P.withRunMode runMode (E.mkLogMessageEntry msgLogLvl tag msg) $
    case compare cfgLogLvl msgLogLvl of
      GT -> pure ()
      _  -> do
        msgNum <- R.incLogCounter cntVar
        Impl.sendPendingMsg formatter handle $ T.PendingMsg msgLogLvl tag msg msgNum

runLogger :: T.RunMode -> R.LoggerRuntime -> L.Logger a -> IO a
runLogger runMode loggerRt = foldF (interpretLogger runMode loggerRt)
