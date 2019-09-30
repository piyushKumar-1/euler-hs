module EulerHS.TestData.Scenarios.Scenario1 where

import           EulerHS.Prelude hiding (getOption)
import           EulerHS.Language

import           EulerHS.TestData

import           Servant.Client (BaseUrl(..), Scheme(..))

mkUrl :: String -> BaseUrl
mkUrl host = BaseUrl Http host port ""

getLocalGUID :: IO String
getLocalGUID = pure "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"

testScenario1 :: Flow User
testScenario1 = do
  localUserName <- runSysCmd "whoami"
  localGUID <- runIO getLocalGUID
  guid <- generateGUID
  url <- maybe (mkUrl "localhost") mkUrl <$> getOption UrlKey
  res <- callServantAPI url getUser
  case res of
    Right user ->  if localGUID /= userGUID user then pure user
                     else pure $ User localUserName "" $ toString guid
    _ -> pure $ User localUserName "Smith" $ toString guid
