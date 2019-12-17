{-# LANGUAGE BangPatterns #-}

module EulerHS.Core.Logger.Interpreter
  (
    -- * Core Logger Interpreter
    runLogger
  )
where

import           EulerHS.Prelude

import qualified EulerHS.Core.Types         as D
import qualified EulerHS.Core.Language      as L
import qualified EulerHS.Core.Runtime       as R
import qualified EulerHS.Core.Logger.Impl.TinyLogger as Impl
import qualified EulerHS.Core.Playback.Machine as P
import qualified EulerHS.Core.Logger.Entries as E


interpretLogger :: D.RunMode -> R.LoggerRuntime -> L.LoggerMethod a -> IO a
interpretLogger runMode (R.MemoryLoggerRuntime mvar) (L.LogMessage level tag msg next) =
  fmap next $ P.withRunMode runMode (E.mkLogMessageEntry level tag msg) $ do
    !lgs <- takeMVar mvar
    let m = "" +|| level ||+ " " +| tag |+ " " +| msg |+ ""
    putMVar mvar (m : lgs)

interpretLogger runMode (R.LoggerRuntime handle) (L.LogMessage level tag msg next) =
  fmap next $ P.withRunMode runMode (E.mkLogMessageEntry level tag msg) $
    Impl.sendPendingMsg handle $ D.PendingMsg level tag msg

runLogger :: D.RunMode -> R.LoggerRuntime -> L.Logger a -> IO a
runLogger runMode loggerRt = foldF (interpretLogger runMode loggerRt)
