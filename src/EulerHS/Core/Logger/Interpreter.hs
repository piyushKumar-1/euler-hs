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
import qualified EulerHS.Core.Types as D
import qualified Control.Concurrent.MVar as MVar


interpretLogger :: D.RunMode -> R.LoggerRuntime -> L.LoggerMethod a -> IO a
interpretLogger runMode (R.MemoryLoggerRuntime _ cfgLogLvl mvar) (L.LogMessage formatter msgLogLvl tag msg next) =
  fmap next $ P.withRunMode runMode (E.mkLogMessageEntry msgLogLvl tag msg) $
    case compare cfgLogLvl msgLogLvl of
      GT -> pure ()
      _  -> do
        !msgNum <- R.incLogCounter cntVar
        let !m = formatter msgLogLvl tag msg msgNum
        MVar.modifyMVar mvar $ \(!lgs) -> pure (m : lgs, ())

interpretLogger runMode (R.LoggerRuntime _ cfgLogLvl _ cntVar handle) (L.LogMessage formatter msgLogLvl tag msg next) =
  fmap next $ P.withRunMode runMode (E.mkLogMessageEntry msgLogLvl tag msg) $
    case compare cfgLogLvl msgLogLvl of
      GT -> pure ()
      _  -> do
        msgNum <- R.incLogCounter cntVar
        Impl.sendPendingMsg handle formatter $ D.PendingMsg msgLogLvl tag msg msgNum

runLogger :: D.RunMode -> R.LoggerRuntime -> L.Logger a -> IO a
runLogger runMode loggerRt = foldF (interpretLogger runMode loggerRt)
