module Test.Query where

import Universum

import Test.Hspec
import Data.Time.Clock
import Data.Time.Calendar (Day(ModifiedJulianDay))

import Test.Fixtures (withConsoleServer, testPort)
import Console.API (app, queryAPI)
import Console.Query (dummyResult)
import qualified Dashboard.Query.Types as QT

import Servant.Client (client, runClientM, mkClientEnv, parseBaseUrl)
import Network.HTTP.Client (newManager, defaultManagerSettings)

-- The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17
-- which will come up as a negative Unix timestamp
ts :: QT.Timestamp
ts = QT.Timestamp $ UTCTime {
        utctDay = ModifiedJulianDay (160 * 365) -- 160 years since 1858 i.e. 2018,
      , utctDayTime = secondsToDiffTime 0
      }

dummyQuery :: QT.Query
dummyQuery = QT.Query (QT.Selection
                        [(Just QT.COUNT, QT.All)])
                        "table1"
                        (QT.Interval { start = ts
                                     , stop = ts
                                     , step = Just $ QT.Milliseconds 15
                                     , field = "field1"})
                      (QT.Filter [])
                      (QT.GroupBy [])

incorrectQuery :: QT.Query
incorrectQuery = QT.Query (QT.Selection
                            [(Just QT.COUNT, QT.All)])
                            "table666"
                            (QT.Interval { start = ts
                                         , stop = ts
                                         , step = Just $ QT.Milliseconds 15
                                         , field = "field1"})
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
