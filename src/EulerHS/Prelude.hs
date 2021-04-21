{-# OPTIONS -fno-warn-orphans #-}

module EulerHS.Prelude
  -- TODO: This entire export lists needs to be explicit
  ( module X
  , liftFC
  , catchAny
  -- JSON
  , stripLensPrefixOptions
  , stripAllLensPrefixOptions
  , jsonSetField
  , encodeJSON
  , decodeJSON
  ) where

import           Control.Concurrent as X (ThreadId, forkIO, killThread,
                                          threadDelay)
import           Control.Concurrent.STM as X (retry)
import           Control.Concurrent.STM.TChan as X (TChan, newTChan, newTChanIO,
                                                    readTChan, tryReadTChan,
                                                    writeTChan)
import           Control.Concurrent.STM.TMVar as X (TMVar, newEmptyTMVar,
                                                    newEmptyTMVarIO, newTMVar,
                                                    newTMVarIO, putTMVar,
                                                    readTMVar, takeTMVar,
                                                    tryReadTMVar)
import           Control.Concurrent.STM.TVar as X (modifyTVar)
import           Control.Lens as X (at, (.=))
import           Control.Lens.TH as X (makeFieldsNoPrefix, makeLenses)
import           Control.Monad as X (liftM)
import           Control.Monad.Free as X (Free (..), foldFree, liftF)
import           Control.Monad.Free.Church as X (F (..), foldF, fromF, iter,
                                                 iterM, retract)
import qualified Control.Monad.Free.Church as CF
import qualified Control.Monad.Free.Class as MF
import           Control.Newtype.Generics as X (Newtype, O, pack, unpack)
import           Data.Aeson as X (FromJSON, FromJSONKey, ToJSON, ToJSONKey,
                                  genericParseJSON, genericToJSON, parseJSON,
                                  toJSON)
import           Data.Kind as X (Type)
import           Data.Maybe as X (fromJust)
import           Data.Serialize as X (Serialize)
import           EulerHS.Extra.Aeson (decodeJSON, encodeJSON, jsonSetField,
                                      stripAllLensPrefixOptions,
                                      stripLensPrefixOptions)
import           Fmt as X ((+|), (+||), (|+), (||+))
import           GHC.Base as X (until)
import           Text.Read as X (read, readsPrec)
import           Universum (catchAny)
import           Universum as X hiding (All, Option, Set, Type, catchAny, head,
                                 init, last, set, tail, trace)
import           Universum.Unsafe as X (head, init, last, tail, (!!))

-- Lift for Church encoded Free
liftFC :: (Functor f, MF.MonadFree f m) => f a -> m a
liftFC = CF.liftF
