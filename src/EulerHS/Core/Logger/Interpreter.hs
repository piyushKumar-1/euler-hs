{-# LANGUAGE BangPatterns #-}

module EulerHS.Core.Logger.Interpreter
  (
    -- * Core Logger Interpreter
    runLogger
  )
where

import           EulerHS.Prelude

import qualified EulerHS.Core.Language as L
-- import qualified EulerHS.Core.Logger.Entries as E
import qualified EulerHS.Core.Logger.ImplMimicPSBad.TinyLogger as Impl
-- import qualified EulerHS.Core.Playback.Machine as P
import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Types as D
import qualified Control.Concurrent.MVar as MVar


interpretLogger :: R.LoggerRuntime -> L.LoggerMethod a -> IO a
interpretLogger (R.MemoryLoggerRuntime cfgLogLvl mvar) (L.LogMessage msgLogLvl tag msg next) =
  -- fmap next $ P.withRunMode runMode (E.mkLogMessageEntry msgLogLvl tag msg) $
  fmap next $
    case compare cfgLogLvl msgLogLvl of
      GT -> pure ()
      -- _  -> do
        -- !lgs <- takeMVar mvar
        -- putMVar mvar (m : lgs)
      _  -> MVar.modifyMVar mvar $ \(!lgs) -> do
        let m = "" +|| msgLogLvl ||+ " " +| tag |+ " " +| msg |+ ""
        pure (m : lgs, ())

interpretLogger (R.LoggerRuntime cfgLogLvl _ cnt ctx1 ctx2 _ handle) (L.LogMessage msgLogLvl tag msg next) =
  -- fmap next $ P.withRunMode runMode (E.mkLogMessageEntry msgLogLvl tag msg) $
  fmap next $
    case compare cfgLogLvl msgLogLvl of
      GT -> pure ()
      _  -> do
        msgNum <- R.incLogCounter cnt
        Impl.sendPendingMsg handle $ D.PendingMsg msgLogLvl tag msg msgNum ctx1 ctx2

runLogger :: R.LoggerRuntime -> L.Logger a -> IO a
runLogger loggerRt = foldF (interpretLogger loggerRt)
