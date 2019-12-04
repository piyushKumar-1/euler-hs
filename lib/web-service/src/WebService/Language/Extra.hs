module WebService.Language.Extra where

import EulerHS.Prelude

import qualified EulerHS.Language as L
import qualified EulerHS.Types as T
import qualified WebService.Types as WST

throwOnFailedWithLog :: Show e => Either e a -> (Text -> WST.AppException) -> Text -> L.Flow ()
throwOnFailedWithLog (Left err) mkException msg = do
  L.logError "" $ msg <> " " <> show err <> ""
  L.throwException $ mkException $ msg <> " " <> show err <> ""
