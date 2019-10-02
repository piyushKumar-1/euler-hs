{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}


module EulerHS.Tests.Framework.Interpreters.TestInterpreter where

import           EulerHS.Prelude -- hiding (view, (^.))
import           Data.Aeson                      (encode, decode)
import Servant.Client (ClientError(..))
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import qualified EulerHS.Interpreters as R

import EulerHS.TestData.Types.Framework.Flow
import Unsafe.Coerce

import Control.Lens
import Data.Generics.Product.Fields
import GHC.Generics
import EulerHS.TestData.Types.Interpreters.TestInterpreter


-- takeMockedVal ::forall (x :: Symbol) a.  Proxy x -> MockedValues -> IO a
-- takeMockedVal _ mmv = do
--   mv <- takeMVar mmv
--   (v,t) <- case (getField @x mv) of
--     [] -> error "empty MockedValues "
--     (x:xs) -> pure (x,xs)
--   putMVar mmv $ setField @x t mv
--   pure v

runFlowWithTestInterpreter :: MockedValues -> R.FlowRuntime -> L.Flow a -> IO a
runFlowWithTestInterpreter mv flowRt = foldF (interpretFlowMethod mv flowRt)

interpretFlowMethod :: MockedValues -> R.FlowRuntime -> L.FlowMethod a -> IO a

interpretFlowMethod mmv _ (L.RunIO ioAct next) = do
  mv <- takeMVar mmv
  (v,t) <- case (getField @"mockedRunIO" mv) of
    [] -> error "empty mockedRunIO "
    (x:xs) -> pure (x,xs)
  putMVar mmv $ setField @"mockedRunIO" t mv
  next <$> (pure $ unsafeCoerce v)

interpretFlowMethod mmv rt (L.CallServantAPI bUrl clientAct next) = do
  mv <- takeMVar mmv
  (v,t) <- case (getField @"mockedCallServantAPI" mv) of
    [] -> error "empty mockedCallServantAPI "
    (x:xs) -> pure (x,xs)
  putMVar mmv $ setField @"mockedCallServantAPI" t mv
  next <$> (pure $ unsafeCoerce v)

interpretFlowMethod mmv R.FlowRuntime {..} (L.GetOption k next) = do
  mv <- takeMVar mmv
  (v,t) <- case (getField @"mockedGetOption" mv) of
    [] -> error "empty mockedGetOption "
    (x:xs) -> pure (x,xs)
  putMVar mmv $ setField @"mockedGetOption" t mv
  next <$> (pure $ decode v)

interpretFlowMethod mv R.FlowRuntime {..} (L.SetOption k v next) =
  next <$> pure ()

interpretFlowMethod mmv _ (L.GenerateGUID next) = do
  mv <- takeMVar mmv
  (v,t) <- case (getField @"mockedGenerateGUID" mv) of
    [] -> error "empty mockedGenerateGUID "
    (x:xs) -> pure (x,xs)
  putMVar mmv $ setField @"mockedGenerateGUID" t mv
  next <$> (pure v)

interpretFlowMethod mmv _ (L.RunSysCmd cmd next) = do
  mv <- takeMVar mmv
  (v,t) <- case (getField @"mockedRunSysCmd" mv) of
    [] -> error "empty mockedRunSysCmd "
    (x:xs) -> pure (x,xs)
  putMVar mmv $ setField @"mockedRunSysCmd" t mv
  next <$> (pure v)

interpretFlowMethod mv _ _ = error "not yet supported."

