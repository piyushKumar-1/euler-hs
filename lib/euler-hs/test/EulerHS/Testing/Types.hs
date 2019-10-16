{-# LANGUAGE DeriveDataTypeable #-}
module EulerHS.Testing.Types where

import EulerHS.Prelude
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Redis  as R
import Data.Data

data FlowMockedValues' = FlowMockedValues'
  { mockedCallServantAPI :: [Any]
  , mockedRunIO :: [Any]
  , mockedGetOption :: [BSL.ByteString]
  , mockedGenerateGUID :: [Text]
  , mockedRunSysCmd :: [String]
  } deriving (Generic, Typeable)

data KVDBMockedValues' = KVDBMockedValues'
  { kvdbSet    :: [ R.Status]
  , kvdbGet    :: [ (Maybe ByteString)]
  , kvdbExists :: [ Bool]
  , kvdbDel    :: [ Integer]
  , kvdbExpire :: [ Bool]
  , kvdbIncr   :: [ Integer]
  , kvdbHSet   :: [ Bool]
  , kvdbHGet   :: [ (Maybe ByteString)]
  } deriving (Generic, Typeable)

type FlowMockedValues = MVar FlowMockedValues'
type KVDBMockedValues = MVar (KVDBMockedValues' )
