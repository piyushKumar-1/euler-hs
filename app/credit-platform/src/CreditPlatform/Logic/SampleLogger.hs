module CreditPlatform.Logic.SampleLogger where

import EulerHS.Prelude

import qualified EulerHS.Language as L


logMessageFlow :: Text -> L.Flow ()
logMessageFlow msg = do
  L.logInfo    @String "SampleFlow" msg
  L.logWarning @String "SampleFlow" msg
  L.logDebug   @String "SampleFlow" msg
  L.logError   @String "SampleFlow" msg
