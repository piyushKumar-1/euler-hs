module EulerHS.Core.Logger.Interpreter where

import           EulerHS.Prelude

import qualified EulerHS.Core.Types         as D
import qualified EulerHS.Core.Language      as L
import qualified EulerHS.Core.Runtime       as R
import qualified EulerHS.Core.Logger.Impl.TinyLogger as Impl


interpretLogger :: R.LoggerRuntime -> L.LoggerMethod a -> IO a
interpretLogger (R.LoggerRuntime handle) (L.LogMessage level tag msg next) = do
  Impl.sendPendingMsg handle $ D.PendingMsg level tag msg
  pure $ next ()

runLogger :: R.LoggerRuntime -> L.Logger a -> IO a
runLogger loggerRt = foldF (interpretLogger loggerRt)
