module EulerHS.TestData.Scenarios.Scenario1 where

import           EulerHS.Prelude hiding (getOption)
import           EulerHS.Language

import           EulerHS.TestData

import           Servant.Client (BaseUrl(..), Scheme(..))
import qualified Data.ByteString.Lazy as BSL
import EulerHS.TestData.Types.Interpreters.TestInterpreter
import Unsafe.Coerce
import           Data.Aeson                      (encode)

mkUrl :: String -> BaseUrl
mkUrl host = BaseUrl Http host port ""

user :: Any
user = unsafeCoerce $ Right $ User "John" "Snow" "00000000-0000-0000-0000-000000000000"

localGUID :: Any
localGUID = unsafeCoerce "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"

lhost :: BSL.ByteString
lhost = encode ("localhost" :: String)

scenario1MockedValues :: MockedValues'
scenario1MockedValues = MockedValues
  { mockedCallServantAPI = [user]
  , mockedRunIO = [localGUID]
  , mockedGetOption = [lhost]
  , mockedGenerateGUID = ["00000000-0000-0000-0000-000000000000"]
  , mockedRunSysCmd = ["Neo"]
  }

testScenario1 :: Flow User
testScenario1 = do
  localUserName <- runSysCmd "whoami"
  localGUID <- runIO $ (undefined :: IO String)
  guid <- generateGUID
  url <- maybe (mkUrl "localhost") mkUrl <$> getOption UrlKey
  res <- callServantAPI url getUser
  case res of
    Right u ->  if localGUID /= userGUID u then pure u
                     else pure $ User localUserName "" $ toString guid
    _ -> pure $ User localUserName "Smith" $ toString guid
