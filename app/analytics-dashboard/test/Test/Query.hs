module Test.Query where

import Universum

import Data.Maybe (fromJust)
import Data.Time.ISO8601 (parseISO8601)
import Test.Hspec

import Test.Fixtures (testPort)
import Console.API (queryAPI)
import qualified Dashboard.Query.Types as QT

import Servant.Client (client, runClientM, mkClientEnv, parseBaseUrl)
import Network.HTTP.Client (newManager, defaultManagerSettings)

makeTimestamp :: String -> QT.Timestamp
makeTimestamp = QT.Timestamp . fromJust . parseISO8601

testQuery :: QT.Query
testQuery = QT.Query (QT.Selection [(Just QT.SUM, QT.Field "amount")])
                     "godel-big-q.express_checkout.express_checkout20190927"
                     (QT.Interval { start = makeTimestamp "2019-09-27T10:00:00Z"
                                  , stop  = makeTimestamp "2019-09-27T12:00:00Z"
                                  , step  = Just . QT.Milliseconds $ 5 * 60 * 1000
                                  , field = "order_last_modified"
                                  })
                     (QT.Filter [("merchant_id", QT.EQUAL, QT.StringValue "com.swiggy")])
                     (QT.GroupBy ["gateway", "card_type"])

testResultRow :: QT.QueryResultRow
testResultRow = QT.QueryResultRow ts1 ts2 [QT.FloatValue 75557.0]
  where
    ts1 = QT.Timestamp . fromJust . parseISO8601 $ "2019-09-27T10:05:00Z"
    ts2 = QT.Timestamp . fromJust . parseISO8601 $ "2019-09-27T10:10:00Z"

incorrectQuery :: QT.Query
incorrectQuery = QT.Query (QT.Selection
                            [(Just QT.COUNT, QT.All)])
                            "bad_table"
                            (QT.Interval { start = makeTimestamp "1970-01-01T00:00:00Z"
                                         , stop  = makeTimestamp "1970-01-01T00:10:00Z"
                                         , step  = Just $ QT.Milliseconds 15
                                         , field = "field1"})
                            (QT.Filter []) (QT.GroupBy [])

specs :: Spec
specs = describe "Query API" $ do
      let queryClient = client queryAPI
      baseUrl <- runIO . parseBaseUrl $ "http://localhost:" ++ show testPort
      manager <- runIO . newManager $ defaultManagerSettings
      let clientEnv = mkClientEnv manager baseUrl

      it "should return a queryresult when a query is given" $ do
        result <- runClientM (queryClient testQuery) clientEnv
        map (\(QT.QueryResult (x:xs)) -> x) result `shouldBe` Right testResultRow

      -- FIXME: Use hspec-wai and check for a 400 here and responseBody
      it "should return a failure when an incorrect query is given" $ do
        result <- runClientM (queryClient incorrectQuery) clientEnv
        result `shouldSatisfy` isLeft
