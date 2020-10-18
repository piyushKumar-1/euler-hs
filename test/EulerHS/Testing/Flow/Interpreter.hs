{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module EulerHS.Testing.Flow.Interpreter where

import           Data.Aeson (decode)
import           Data.Generics.Product.Fields
import qualified EulerHS.Language as L
import           EulerHS.Prelude
import qualified EulerHS.Runtime as R
import           EulerHS.Testing.Types
import           GHC.TypeLits
import           Type.Reflection (typeRep)
import           Unsafe.Coerce

runFlowWithTestInterpreter :: FlowMockedValues -> R.FlowRuntime -> L.Flow a -> IO a
runFlowWithTestInterpreter mv flowRt (L.Flow comp) = foldF (interpretFlowMethod mv flowRt) comp

interpretFlowMethod :: FlowMockedValues -> R.FlowRuntime -> L.FlowMethod a -> IO a

interpretFlowMethod mmv _ (L.RunIO descr ioAct next) = do
  v <- takeMockedVal @"mockedRunIO" mmv
  next <$> (pure $ unsafeCoerce v)

interpretFlowMethod mmv rt (L.CallServantAPI mbMgrSel bUrl clientAct next) = do
  v <- takeMockedVal @"mockedCallServantAPI" mmv
  next <$> (pure $ unsafeCoerce v)

interpretFlowMethod mmv R.FlowRuntime {..} (L.GetOption k next) = do
  v <- takeMockedVal @"mockedGetOption" mmv
  next <$> (pure $ decode v)

interpretFlowMethod mv R.FlowRuntime {..} (L.SetOption k v next) =
  next <$> pure ()

interpretFlowMethod mmv _ (L.GenerateGUID next) = do
  v <- takeMockedVal @"mockedGenerateGUID" mmv
  next <$> (pure v)

interpretFlowMethod mmv _ (L.RunSysCmd cmd next) = do
  v <- takeMockedVal @"mockedRunSysCmd" mmv
  next <$> (pure v)

interpretFlowMethod mv _ _ = error "not yet supported."


takeMockedVal ::forall (f :: Symbol) a r
  .  (KnownSymbol f, Typeable r, HasField' f r [a])
  => MVar r -> IO a
takeMockedVal mmv = do
  mv <- takeMVar mmv
  (v,t) <- case (getField @f mv) of
    [] -> error $ "empty " <> (show $ typeRep @f) <> " in " <> (show $ typeRep @r)
    (x:xs) -> pure (x,xs)
  putMVar mmv $ setField @f t mv
  pure v
