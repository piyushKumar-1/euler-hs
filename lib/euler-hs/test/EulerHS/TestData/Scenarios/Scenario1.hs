{-# OPTIONS -fno-warn-deprecations #-}
module EulerHS.TestData.Scenarios.Scenario1 where

import           EulerHS.Prelude hiding (getOption)
import           EulerHS.Language
import           Servant.Client                  (BaseUrl(..), Scheme(..))

import           EulerHS.TestData.Types
import           EulerHS.TestData.API.Client

mkUrl :: String -> BaseUrl
mkUrl host = BaseUrl Http host port ""

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
