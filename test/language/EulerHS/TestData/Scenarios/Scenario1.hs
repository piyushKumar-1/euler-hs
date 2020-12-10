{-# OPTIONS -fno-warn-deprecations #-}
module EulerHS.TestData.Scenarios.Scenario1 where

import           EulerHS.Language
import           EulerHS.Prelude hiding (getOption)
import           Servant.Client (BaseUrl (..), Scheme (..))

import           EulerHS.TestData.API.Client
import           EulerHS.TestData.Types

mkUrl :: String -> BaseUrl
mkUrl host = BaseUrl Http host port ""

testScenario1 :: Flow User
testScenario1 = do
  localUserName <- runSysCmd "whoami"
  localGUID <- runIO $ (undefined :: IO String)
  guid <- generateGUID
  url <- maybe (mkUrl "localhost") mkUrl <$> getOption UrlKey
  res <- callServantAPI Nothing url getUser
  case res of
    Right u ->  if localGUID /= userGUID u then pure u
                     else pure $ User localUserName "" $ toString guid
    _ -> pure $ User localUserName "Smith" $ toString guid
