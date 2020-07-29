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


interpretLogger :: D.RunMode -> R.LoggerRuntime -> L.LoggerMethod a -> IO a
interpretLogger runMode (R.MemoryLoggerRuntime cfgLogLvl mvar) (L.LogMessage msgLogLvl tag msg next) =
  fmap next $ P.withRunMode runMode (E.mkLogMessageEntry msgLogLvl tag msg) $
    case compare cfgLogLvl msgLogLvl of
      GT -> pure ()
      _  -> do
        !lgs <- takeMVar mvar
        let m = "" +|| msgLogLvl ||+ " " +| tag |+ " " +| msg |+ ""
        putMVar mvar (m : lgs)

interpretLogger runMode (R.LoggerRuntime cfgLogLvl ctx handle) (L.LogMessage msgLogLvl tag msg next) =
  fmap next $ P.withRunMode runMode (E.mkLogMessageEntry msgLogLvl tag msg) $
    case compare cfgLogLvl msgLogLvl of
      GT -> pure ()
      _  -> Impl.sendPendingMsg handle $ D.PendingMsg msgLogLvl tag msg ctx

runLogger :: D.RunMode -> R.LoggerRuntime -> L.Logger a -> IO a
runLogger runMode loggerRt = foldF (interpretLogger runMode loggerRt)
