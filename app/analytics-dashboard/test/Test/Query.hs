module Test.Query where

import Universum

import Test.Hspec

import Test.Fixtures (withConsoleServer, testPort)
import Console.API (app, queryAPI)
import Console.Query (dummyResult, ts)
import qualified Dashboard.Query.Types as QT

import Servant.Client (client, runClientM, mkClientEnv, parseBaseUrl)
import Network.HTTP.Client (newManager, defaultManagerSettings)

dummyQuery :: QT.Query
dummyQuery = QT.Query (QT.Selection
                        (QT.COUNT, QT.All))
                        "table1"
                        (QT.Interval { start = ts
                                     , stop = ts
                                     , step = Just $ QT.Milliseconds 3928})
                      (QT.Filter [])
                      (QT.GroupBy [])

incorrectQuery :: QT.Query
incorrectQuery = QT.Query (QT.Selection
                            (QT.COUNT, QT.All))
                            "table666"
                            (QT.Interval { start = ts
                                         , stop = ts
                                         , step = Just $ QT.Milliseconds 3928})
                            (QT.Filter []) (QT.GroupBy [])

-- FIXME: API tests should yuse hspec-wai so that we can check on Response body
-- and status code without having to constrcut responses ourselves
specs :: Spec
specs = describe "Query API" $ do
      let queryClient = client queryAPI
      baseUrl <- runIO $ parseBaseUrl $ "http://localhost:" ++ show testPort
      manager <- runIO $ newManager defaultManagerSettings
      let clientEnv = mkClientEnv manager baseUrl

      it "should return a queryresult when a query is given" $ do
        result <- runClientM (queryClient dummyQuery) clientEnv
        result `shouldBe` Right dummyResult

      -- FIXME: Use hspec-wai and check for a 400 here and responseBody
      it "should return a failure when an incorrect query is given" $ do
        result <- runClientM (queryClient incorrectQuery) clientEnv
        result `shouldNotBe` Right dummyResult
