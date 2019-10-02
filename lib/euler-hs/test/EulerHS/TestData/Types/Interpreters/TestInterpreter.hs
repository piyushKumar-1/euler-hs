module EulerHS.TestData.Types.Interpreters.TestInterpreter where

import EulerHS.Prelude
import qualified Data.ByteString.Lazy as BSL

data MockedValues' = MockedValues 
  { mockedCallServantAPI :: [Any]
  , mockedRunIO :: [Any]
  , mockedGetOption :: [BSL.ByteString]
  , mockedGenerateGUID :: [Text]
  , mockedRunSysCmd :: [String]
  } deriving (Generic)

type MockedValues = MVar MockedValues'